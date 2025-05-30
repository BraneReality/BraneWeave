use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::{Rc, Weak},
};

use raylib::prelude::*;

const GRAPH_BACKGROUND: Color = Color {
    r: 25,
    g: 77,
    b: 51,
    a: 255,
};
const GUI_BACKGROUND: Color = Color {
    r: 32,
    g: 96,
    b: 32,
    a: 255,
};

const BORDER_COLOR: Color = Color {
    r: 64,
    g: 191,
    b: 128,
    a: 255,
};

const DRAG_BORDER_WIDTH: i32 = 4i32;

struct UISizeConstraints {
    pub min_size: Vector2,
    pub max_size: Vector2,
    pub preferred_size: Option<Vector2>,
}

trait UISize {
    fn size_constraints() -> UISizeConstraints;
}

trait UIClickable {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool;
    fn consume_mouse_up(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool;
}

pub struct UIContext {
    pub add_node_popup: Option<()>,
    pub node_graph: NodeGraphUI,
    pub inspector_sidebar: InspectorSidebarUI,
}

impl UIContext {
    pub fn new() -> UIContext {
        UIContext {
            add_node_popup: None,
            node_graph: NodeGraphUI {
                nodes: Default::default(),
                currently_selected: Default::default(),
                id_counter: Cell::new(0usize),
            },
            inspector_sidebar: InspectorSidebarUI {
                width: 400f32,
                is_border_clicked: false,
            },
        }
    }

    pub fn draw(&mut self, d: &mut RaylibDrawHandle) {
        if d.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            let res = self.consume_mouse_down(d.get_mouse_position(), d);
            println!("mouse down consumed: {}", res);
        }
        if d.is_mouse_button_released(MouseButton::MOUSE_BUTTON_LEFT) {
            let res = self.consume_mouse_up(d.get_mouse_position(), d);
            println!("mouse up consumed: {}", res);
        }

        self.inspector_sidebar.update_width(d);
        let graph_rect = Rectangle {
            x: 0f32,
            y: 0f32,
            width: d.get_render_width() as f32 - self.inspector_sidebar.width,
            height: d.get_render_height() as f32,
        };
        self.node_graph.draw(d, graph_rect);
        self.inspector_sidebar.draw(d, &self.node_graph);
    }
}

impl UIClickable for UIContext {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        self.inspector_sidebar.consume_mouse_down(click_pos, d)
            || self.node_graph.consume_mouse_down(click_pos, d)
    }

    fn consume_mouse_up(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        self.inspector_sidebar.consume_mouse_up(click_pos, d)
            || self.node_graph.consume_mouse_up(click_pos, d)
    }
}

pub type GraphNodeId = usize;

pub struct NodeGraphUI {
    nodes: RefCell<HashMap<GraphNodeId, Rc<RefCell<GraphNode>>>>,
    currently_selected: RefCell<Weak<RefCell<GraphNode>>>,
    id_counter: Cell<GraphNodeId>,
}

impl NodeGraphUI {
    fn draw(&self, d: &mut RaylibDrawHandle, rect: Rectangle) {
        d.draw_rectangle_rec(rect, GRAPH_BACKGROUND);
        for (_id, node) in self.nodes.borrow().iter() {
            node.borrow_mut().draw(d, self);
        }
    }

    fn selected(&self) -> Option<Rc<RefCell<GraphNode>>> {
        return self.currently_selected.borrow().upgrade();
    }

    fn set_selected(&self, node: Option<GraphNodeId>) {
        //callbacks here?
        *self.currently_selected.borrow_mut() = match node {
            Some(id) => self
                .nodes
                .borrow()
                .get(&id)
                .map(|node| Rc::downgrade(node))
                .unwrap_or(Weak::new()),
            None => Weak::new(),
        };
    }

    pub fn new_node(&self) -> Weak<RefCell<GraphNode>> {
        let id = self.id_counter.get();
        self.id_counter.set(id + 1);
        let node = Rc::new(RefCell::new(GraphNode {
            id,
            pos: Vector2::new(0f32, 0f32),
            is_clicked: false,
        }));
        let ret = Rc::downgrade(&node);
        self.nodes.borrow_mut().insert(id, node);
        ret
    }
}

impl UIClickable for NodeGraphUI {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        for (_id, node) in self.nodes.borrow().iter() {
            let mut node = node.borrow_mut();
            if node.consume_mouse_down(click_pos, d) {
                self.set_selected(Some(node.id));
                return true;
            }
        }
        //self.set_selected(None);
        false
    }

    fn consume_mouse_up(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        for (_id, node) in self.nodes.borrow().iter() {
            if node.borrow_mut().consume_mouse_up(click_pos, d) {
                return true;
            }
        }
        false
    }
}

pub struct InspectorSidebarUI {
    pub width: f32,
    pub is_border_clicked: bool,
}

impl InspectorSidebarUI {
    fn border_rect(&self, d: &RaylibDrawHandle) -> Rectangle {
        let edge = d.get_render_width() as f32 - self.width;
        Rectangle {
            x: edge,
            y: 0f32,
            width: DRAG_BORDER_WIDTH as f32,
            height: d.get_render_height() as f32,
        }
    }

    fn background_rect(&self, d: &mut RaylibDrawHandle) -> Rectangle {
        let edge = d.get_render_width() as f32 - self.width;
        Rectangle {
            x: edge,
            y: 0f32,
            width: self.width,
            height: d.get_render_height() as f32,
        }
    }

    fn update_width(&mut self, d: &mut RaylibDrawHandle) {
        if self.is_border_clicked {
            self.width += -d.get_mouse_delta().x;
        }
    }

    fn draw(&mut self, d: &mut RaylibDrawHandle, graph: &NodeGraphUI) {
        let area = self.background_rect(d);
        d.draw_rectangle_rec(area, GUI_BACKGROUND);
        let border = self.border_rect(d);
        d.draw_rectangle_rec(border, BORDER_COLOR);

        d.draw_text_ex(
            d.get_font_default(),
            "inspector",
            Vector2::new(area.x + 12f32, area.y + 12f32),
            20f32,
            2f32,
            Color::WHITE,
        );

        d.draw_text_ex(
            d.get_font_default(),
            format!(
                "Currently selected: {}",
                graph
                    .selected()
                    .map(|node| node.borrow().id.to_string())
                    .unwrap_or("None".to_string())
            )
            .as_str(),
            Vector2::new(area.x + 12f32, area.y + 12f32 + 32f32),
            20f32,
            2f32,
            Color::WHITE,
        );
    }
}

impl UIClickable for InspectorSidebarUI {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        self.is_border_clicked = self.border_rect(d).check_collision_point_rec(click_pos);
        self.is_border_clicked
    }

    fn consume_mouse_up(&mut self, _click_pos: Vector2, _d: &RaylibDrawHandle) -> bool {
        self.is_border_clicked = false;
        false
    }
}

pub struct GraphNode {
    pub id: GraphNodeId,
    pub pos: Vector2,
    pub is_clicked: bool,
}

impl GraphNode {
    fn rect(&self, _d: &RaylibDrawHandle) -> Rectangle {
        Rectangle {
            x: self.pos.x,
            y: self.pos.y,
            width: 500f32,
            height: 200f32,
        }
    }

    fn draw(&mut self, d: &mut RaylibDrawHandle, graph: &NodeGraphUI) {
        if d.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {}
        if d.is_mouse_button_released(MouseButton::MOUSE_BUTTON_LEFT) {
            if self.is_clicked {
                graph.set_selected(Some(self.id));
            }
            self.is_clicked = false;
        }

        if self.is_clicked {
            let delta = d.get_mouse_delta();
            self.pos.x += delta.x;
            self.pos.y += delta.y;
        }

        let rect = self.rect(d);
        d.draw_rectangle_rounded(rect, 0.2f32, 4, GUI_BACKGROUND);
    }
}

impl UIClickable for GraphNode {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool {
        self.is_clicked = self.rect(d).check_collision_point_rec(click_pos);
        self.is_clicked
    }

    fn consume_mouse_up(&mut self, _click_pos: Vector2, _d: &RaylibDrawHandle) -> bool {
        self.is_clicked = false;
        false
    }
}
