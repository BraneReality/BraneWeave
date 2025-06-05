use branec::ProjectCtx;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::{Rc, Weak},
    sync::Arc,
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

trait UIClickable {
    fn consume_mouse_down(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool;
    fn consume_mouse_up(&mut self, click_pos: Vector2, d: &RaylibDrawHandle) -> bool;
}

pub struct UIContext {
    pub add_node_popup: Option<()>,
    pub node_graph: NodeGraphUI,
    pub inspector_sidebar: InspectorSidebarUI,
}

/// Allignment along an axis
#[derive(Copy, Clone)]
pub enum TextAlign {
    TopLeft,
    TopCenter,
    TopRight,
    CenterLeft,
    Center,
    CenterRight,
    BottomLeft,
    BottomCenter,
    BottomRight,
}

/// How to draw this particular element
/// every type of draw is contained in this enum,
/// all more complicated draw types are composed of multiple UIElements
#[derive(Clone)]
pub enum UIContent {
    /// For when you want a layout only element
    None,
    /// Simple rect with a color
    Rect(Color),
    /// Rect with round corners
    Rounded { color: Color, corner_radius: f32 },
    /// Text
    Text {
        color: Color,
        text: String,
        align: TextAlign,
        font_size: f32,
        font: Arc<WeakFont>,
    },
}

#[derive(Clone, Copy)]
pub enum UIEvent {
    MouseDown,
    MouseUp,
    MouseDrag,
}

/// Layout sizing options
#[derive(Clone, Copy)]
pub enum Sizing {
    /// Retain an exact size
    Fixed(f32),
    /// Prefer a target size, but shrink if less space is available down to an optional min
    Prefer { target: f32, min: Option<f32> },
    /// Shrink to content size, with an optional min size
    Fit(Option<f32>),
    /// Grow to fill available space, with an optional max size
    Grow(Option<f32>),
}

/// Universal way to represent border sizes
#[derive(Clone, Copy, Default)]
pub struct BorderSizes {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}

impl BorderSizes {
    pub fn uniform(size: f32) -> BorderSizes {
        BorderSizes {
            left: size,
            top: size,
            right: size,
            bottom: size,
        }
    }

    /// Sum of left + right
    pub fn width(&self) -> f32 {
        self.left + self.right
    }

    /// Sum of top + bottom
    pub fn height(&self) -> f32 {
        self.top + self.bottom
    }

    fn axis_width(&self, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => self.width(),
            LayoutDir::Decending | LayoutDir::Ascending => self.height(),
        }
    }

    fn axis_height(&self, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => self.height(),
            LayoutDir::Decending | LayoutDir::Ascending => self.width(),
        }
    }

    fn axis_start(&self, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight => self.left,
            LayoutDir::Decending => self.top,
            LayoutDir::Ascending => self.bottom,
            LayoutDir::RightLeft => self.right,
        }
    }

    fn axis_end(&self, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight => self.right,
            LayoutDir::Decending => self.bottom,
            LayoutDir::Ascending => self.top,
            LayoutDir::RightLeft => self.left,
        }
    }
}

/// Direction of item layout
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LayoutDir {
    /// Layout elements from left to right
    LeftRight,
    /// Start placing elements at the top of the box and then add new elements
    /// underneath old ones
    Decending,
    /// Start placing elements at the bottom and then stack them upwards
    Ascending,
    /// Layout elements starting from the right, and then moving to the left
    RightLeft,
}

impl LayoutDir {
    /// Get a perpendicular direction to the current.
    pub fn perpendicular(self) -> LayoutDir {
        match self {
            LayoutDir::LeftRight => Self::Decending,
            LayoutDir::Decending => Self::LeftRight,
            LayoutDir::Ascending => Self::LeftRight,
            LayoutDir::RightLeft => Self::Decending,
        }
    }

    pub fn is_axis_same(self, other: LayoutDir) -> bool {
        other.perpendicular() == self.perpendicular()
    }
}

/// Alignment of items
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LayoutAlign {
    /// Allign elements to the start of an axis
    Start,
    /// Center elements along the direction axis
    Center,
    /// Align elements to the end of an axis
    End,
}

/// Description of how an element should interact with the layout system
#[derive(Clone)]
pub struct Layout {
    /// Width behavior
    pub width: Sizing,
    /// Height behavior
    pub height: Sizing,
    /// Space to insert outside/around this element
    pub padding: BorderSizes,
    /// Space to insert within this element around it's borders
    pub spacing: f32,
    /// The direction to order chilren along
    pub direction: LayoutDir,
    /// How to align children perpendicular to a direction axis
    pub align: LayoutAlign,
}

impl Layout {
    fn axis_width(&self, dir: LayoutDir) -> Sizing {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => self.width,
            LayoutDir::Decending | LayoutDir::Ascending => self.height,
        }
    }

    fn axis_height(&self, dir: LayoutDir) -> Sizing {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => self.height,
            LayoutDir::Decending | LayoutDir::Ascending => self.width,
        }
    }
}

/// A rendered UI element
pub struct UIElement {
    /// The computed position of this element
    /// this generally should not be set directly as it gets overwritten by compute_layout()
    /// influence this through layout instead
    pub rect: Rectangle,
    /// What this element renders
    pub content: UIContent,
    /// The layout options for this element
    pub layout: Layout,
    /// elements to render within this one
    pub children: Vec<UIElement>,
    /// Subscribe to all events, function is expectd to self-filter and return true to
    /// indicate that an event is consumed.
    /// Events start at children and propigate upwards
    pub on_event: Option<Box<dyn FnMut(&mut Self, UIEvent) -> bool>>,
}

impl UIElement {
    /// Update the layout for this element and all it's children
    ///
    /// Should only be called from root node
    pub fn compute_layout(&mut self) {
        self.compute_fit_widths();
        self.compute_dynamic_widths();
        //self.compute_text_wrap();
        self.compute_fit_heights();
        self.compute_dynamic_heights();
        self.compute_positions();
    }

    fn axis_width(rect: &Rectangle, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => rect.width,
            LayoutDir::Decending | LayoutDir::Ascending => rect.height,
        }
    }

    fn axis_width_mut(rect: &mut Rectangle, dir: LayoutDir) -> &mut f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => &mut rect.width,
            LayoutDir::Decending | LayoutDir::Ascending => &mut rect.height,
        }
    }

    fn axis_height(rect: &Rectangle, dir: LayoutDir) -> f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => rect.height,
            LayoutDir::Decending | LayoutDir::Ascending => rect.width,
        }
    }

    fn axis_height_mut(rect: &mut Rectangle, dir: LayoutDir) -> &mut f32 {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => &mut rect.height,
            LayoutDir::Decending | LayoutDir::Ascending => &mut rect.width,
        }
    }

    fn axis_pos(rect: &mut Rectangle, dir: LayoutDir) -> (&mut f32, &mut f32) {
        match dir {
            LayoutDir::LeftRight | LayoutDir::RightLeft => (&mut rect.x, &mut rect.y),
            LayoutDir::Decending | LayoutDir::Ascending => (&mut rect.y, &mut rect.x),
        }
    }

    fn compute_fit_size(&mut self, axis: LayoutDir) {
        for child in self.children.iter_mut() {
            child.compute_fit_size(axis);
        }

        let layout = &self.layout;
        let width = Self::axis_width_mut(&mut self.rect, axis);

        let mut const_width = layout.padding.axis_width(axis);
        if let UIContent::Text {
            color: _,
            text,
            align: _,
            font_size,
            font,
        } = &self.content
        {
            let text_size = font.measure_text(text, *font_size, 1f32);
            if axis.is_axis_same(LayoutDir::LeftRight) {
                const_width += text_size.x
            } else {
                const_width += text_size.y
            }
        }

        match self.layout.axis_width(axis) {
            Sizing::Fixed(size) => {
                *width = size;
            }
            Sizing::Fit(min) => {
                if layout.direction.is_axis_same(axis) {
                    *width = const_width;
                    for child in self.children.iter() {
                        *width += Self::axis_width(&child.rect, axis);
                    }
                    *width += (self.children.len() as f32 - 1f32).max(0f32) * layout.spacing;
                } else {
                    *width = 0f32;
                    for child in self.children.iter() {
                        *width = width.max(Self::axis_width(&child.rect, axis));
                    }
                    *width += const_width;
                }
                if let Some(min) = min {
                    *width = width.max(min);
                }
            }
            Sizing::Grow(_) | Sizing::Prefer { target: _, min: _ } => (), // Computed later
        }
    }

    fn compute_fit_widths(&mut self) {
        self.compute_fit_size(LayoutDir::LeftRight);
    }

    fn compute_fit_heights(&mut self) {
        self.compute_fit_size(LayoutDir::Decending);
    }

    fn compute_dynamic_size(&mut self, axis: LayoutDir) {
        let mut remaining_width = Self::axis_width(&self.rect, axis);
        remaining_width -= self.layout.padding.axis_width(axis);

        if !self.layout.direction.is_axis_same(axis) {
            for child in self.children.iter_mut() {
                match child.layout.axis_width(axis) {
                    Sizing::Fixed(_) => (),
                    Sizing::Fit(_) => (),
                    Sizing::Grow(min) => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
                        *width = remaining_width.max(min.unwrap_or(0f32));
                    }
                    Sizing::Prefer { target, min: _ } => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
                        *width = target.min(remaining_width);
                    }
                }
            }
            return;
        }
        remaining_width -= (self.children.len() as f32 - 1f32).max(0f32) * self.layout.spacing;

        // I'm doing this in this super convoluted way to test out how it might look ported to BraneScript, because of functional programming things
        let child_count = self.children.len();
        let (mut remaining_width, mut grow_children, mut shrink_children) =
            self.children.iter_mut().fold(
                (remaining_width, Vec::new(), Vec::new()),
                |(remaining_width, mut grow, mut shrink), child| match child.layout.axis_width(axis)
                {
                    Sizing::Fixed(size) => (remaining_width - size, grow, shrink),
                    Sizing::Fit(_) => (
                        remaining_width - Self::axis_width(&child.rect, axis),
                        grow,
                        shrink,
                    ),
                    Sizing::Grow(_) => {
                        *Self::axis_width_mut(&mut child.rect, axis) = 0f32;
                        grow.push(child);
                        (remaining_width, grow, shrink)
                    }
                    Sizing::Prefer { target, min: _ } => {
                        *Self::axis_width_mut(&mut child.rect, axis) = target;
                        shrink.push(child);
                        (remaining_width - target, grow, shrink)
                    }
                },
            );

        while !grow_children.is_empty() && remaining_width > 0f32 {
            let (smallest, _, width_to_add) = grow_children.iter().fold(
                (
                    Self::axis_width(&grow_children[0].rect, axis),
                    f32::INFINITY,
                    remaining_width,
                ),
                |(smallest, second_smallest, wta), child| {
                    let width = Self::axis_width(&child.rect, axis);
                    if width < smallest {
                        (width, smallest, wta)
                    } else if width > smallest {
                        (
                            smallest,
                            second_smallest.min(width),
                            second_smallest - smallest,
                        )
                    } else {
                        (smallest, second_smallest, wta)
                    }
                },
            );

            let width_to_add = width_to_add.min(remaining_width / (grow_children.len() as f32));

            grow_children = grow_children
                .into_iter()
                .filter_map(|child: &mut UIElement| {
                    let width = Self::axis_width(&child.rect, axis);
                    if width != smallest {
                        return Some(child);
                    }
                    let max_width = match child.layout.axis_width(axis) {
                        Sizing::Grow(max) => max.unwrap_or(f32::INFINITY),
                        _ => unreachable!(),
                    };
                    let rect = &mut child.rect;
                    let width = Self::axis_width_mut(rect, axis);
                    let old_width = *width;

                    *width += width_to_add;
                    let bound_hit = *width >= max_width;
                    if bound_hit {
                        *width = max_width;
                    }
                    remaining_width -= *width - old_width;
                    if bound_hit { None } else { Some(child) }
                })
                .collect();
        }

        while !shrink_children.is_empty() && remaining_width < 0f32 {
            let (largest, _, width_to_add) = shrink_children.iter().fold(
                (
                    Self::axis_width(&shrink_children[0].rect, axis),
                    0f32,
                    remaining_width,
                ),
                |(largest, second_largest, wta), child| {
                    let width = Self::axis_width(&child.rect, axis);
                    if width > largest {
                        (width, largest, wta)
                    } else if width < largest {
                        (largest, second_largest.max(width), second_largest - largest)
                    } else {
                        (largest, second_largest, wta)
                    }
                },
            );

            let width_to_add = width_to_add.max(remaining_width / shrink_children.len() as f32);

            shrink_children = shrink_children
                .into_iter()
                .filter_map(|child| {
                    let width = Self::axis_width(&child.rect, axis);
                    if width != largest {
                        return Some(child);
                    }
                    let min_width = match child.layout.axis_width(axis) {
                        Sizing::Prefer { target: _, min } => min.unwrap_or(0f32),
                        _ => unreachable!(),
                    };
                    let width = Self::axis_width_mut(&mut child.rect, axis);
                    let old_width = *width;

                    *width += width_to_add;
                    let bound_hit = *width <= min_width;
                    if bound_hit {
                        *width = min_width;
                    }
                    remaining_width -= *width - old_width;
                    if bound_hit { None } else { Some(child) }
                })
                .collect();
        }
    }

    fn compute_dynamic_widths(&mut self) {
        self.compute_dynamic_size(LayoutDir::LeftRight);
        for child in self.children.iter_mut() {
            child.compute_dynamic_size(LayoutDir::LeftRight);
        }
    }

    fn compute_dynamic_heights(&mut self) {
        self.compute_dynamic_size(LayoutDir::Decending);
        for child in self.children.iter_mut() {
            child.compute_dynamic_size(LayoutDir::Decending);
        }
    }

    fn compute_positions(&mut self) {
        let dir = self.layout.direction;
        // Layout along axis direction
        match dir {
            LayoutDir::LeftRight | LayoutDir::Decending => {
                let mut last_x_pos =
                    *Self::axis_pos(&mut self.rect, dir).0 + self.layout.padding.axis_start(dir);
                let y_pos = *Self::axis_pos(&mut self.rect, dir).1;
                for child in self.children.iter_mut() {
                    let height = Self::axis_height(&child.rect, dir);
                    let width = Self::axis_width(&child.rect, dir);
                    let (x, y) = Self::axis_pos(&mut child.rect, dir);
                    *x = last_x_pos;
                    last_x_pos += width + self.layout.spacing;

                    let h_start = y_pos + self.layout.padding.axis_start(dir.perpendicular());
                    *y = match self.layout.align {
                        LayoutAlign::Start => h_start,
                        LayoutAlign::Center => {
                            let content = Self::axis_height(&self.rect, dir)
                                - self.layout.padding.axis_height(dir);
                            content / 2f32 - height / 2f32 + h_start
                        }
                        LayoutAlign::End => {
                            let content = Self::axis_height(&self.rect, dir)
                                - self.layout.padding.axis_height(dir);
                            h_start + content - height
                        }
                    };
                }
            }
            LayoutDir::RightLeft | LayoutDir::Ascending => {
                let mut last_x_pos =
                    *Self::axis_pos(&mut self.rect, dir).0 + self.layout.padding.axis_end(dir);
                let y_pos = *Self::axis_pos(&mut self.rect, dir).1;
                for child in self.children.iter_mut().rev() {
                    let height = Self::axis_height(&child.rect, dir);
                    let width = Self::axis_width(&child.rect, dir);
                    let (x, y) = Self::axis_pos(&mut child.rect, dir);
                    *x = last_x_pos;
                    last_x_pos += width + self.layout.spacing;

                    let h_start = y_pos + self.layout.padding.axis_end(dir.perpendicular());
                    *y = match self.layout.align {
                        LayoutAlign::Start => h_start,
                        LayoutAlign::Center => {
                            let content = Self::axis_height(&self.rect, dir)
                                - self.layout.padding.axis_height(dir);
                            content / 2f32 - height / 2f32 + h_start
                        }
                        LayoutAlign::End => {
                            let content = Self::axis_height(&self.rect, dir)
                                - self.layout.padding.axis_height(dir);
                            h_start + content - height
                        }
                    };
                }
            }
        };

        for child in self.children.iter_mut() {
            child.compute_positions();
        }
    }

    pub fn draw(&self, d: &mut RaylibDrawHandle) {
        match &self.content {
            UIContent::None => (),
            UIContent::Rect(color) => d.draw_rectangle_rec(self.rect, color),
            UIContent::Rounded {
                color,
                corner_radius,
            } => d.draw_rectangle_rounded(
                self.rect,
                (corner_radius * 2f32) / self.rect.width.min(self.rect.height),
                5,
                color,
            ),
            UIContent::Text {
                color,
                text,
                align,
                font_size,
                font, // TODO alignment and wrapping
            } => d.draw_text_ex(
                font.as_ref(),
                &text,
                Vector2::new(
                    self.rect.x + self.layout.padding.left,
                    self.rect.y + self.layout.padding.top,
                ),
                *font_size,
                1f32,
                color,
            ),
        }
        for child in self.children.iter() {
            child.draw(d);
        }
    }
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

    pub fn draw(&mut self, d: &mut RaylibDrawHandle, proj: &mut ProjectCtx) {
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
        self.node_graph.draw(d, graph_rect, proj);
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
    fn draw(&self, d: &mut RaylibDrawHandle, rect: Rectangle, proj: &ProjectCtx) {
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
