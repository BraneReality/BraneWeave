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
    Text(TextElementContent),
}

#[derive(Clone)]
pub struct TextElementContent {
    pub color: Color,
    pub text: String,
    pub font_size: f32,
    pub wrap: bool,
    pub font: Arc<WeakFont>,
}

#[derive(Clone, Copy)]
pub enum UIEvent {
    MouseDown,
    MouseUp,
    MouseDrag,
}

/// Layout sizing options
#[derive(Clone, Copy, Debug)]
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
    pub fn text_element(text: &TextElementContent) -> Layout {
        let target = text.font.measure_text(&text.text, text.font_size, 1f32).x;
        let segments = text.text.split([' ']);
        let min = segments.into_iter().fold(None, |largest, seg| {
            Some(
                largest
                    .unwrap_or(0f32)
                    .max(text.font.measure_text(&seg, text.font_size, 1f32).x),
            )
        });
        let sizing = Sizing::Prefer { target, min };
        println!("Computed text layout: {:?}", sizing);

        Layout {
            width: sizing,
            height: Sizing::Fit(None),
            padding: BorderSizes::uniform(2f32),
            spacing: 4f32,
            direction: LayoutDir::LeftRight,
            align: LayoutAlign::Center,
        }
    }

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
    pub wrapped_text: Option<String>,
    /// What this element renders
    pub content: UIContent,
    /// The layout options for this element
    pub layout: Layout,
    /// elements to render within this one
    pub children: Vec<Rc<RefCell<UIElement>>>,
    /// Subscribe to all events, function is expectd to self-filter and return true to
    /// indicate that an event is consumed.
    /// Events start at children and propigate upwards
    pub on_event: Option<Box<dyn FnMut(&mut Self, UIEvent) -> bool>>,
}

impl UIElement {
    #[must_use]
    pub fn new(content: UIContent) -> UIElementBuilder {
        UIElementBuilder {
            content,
            width: None,
            height: None,
            padding: None,
            spacing: None,
            direction: None,
            align: None,
            children: Vec::default(),
            on_event: None,
        }
    }

    pub fn new_text(
        font: Arc<WeakFont>,
        text: String,
        font_size: f32,
        wrap: bool,
        color: Color,
    ) -> UIElementBuilder {
        Self::new(UIContent::Text(TextElementContent {
            color,
            text,
            font_size,
            wrap,
            font,
        }))
    }

    /// Update the layout for this element and all it's children
    ///
    /// Should only be called from root node
    pub fn compute_layout(&mut self) {
        self.compute_fit_widths();
        self.compute_dynamic_widths();
        self.compute_text_wrap();
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
            child.borrow_mut().compute_fit_size(axis);
        }

        let layout = &self.layout;
        let width = Self::axis_width_mut(&mut self.rect, axis);

        let mut const_width = layout.padding.axis_width(axis);
        if let UIContent::Text(te) = &self.content {
            if axis.is_axis_same(LayoutDir::LeftRight) {
                if !te.wrap {
                    let text_size = te.font.measure_text(&te.text, te.font_size, 1f32);
                    const_width += text_size.x;
                }
            } else {
                let text_size = te.font.measure_text(
                    self.wrapped_text.as_ref().unwrap_or(&te.text),
                    te.font_size,
                    1f32,
                );
                const_width += text_size.y;
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
                        *width += Self::axis_width(&child.borrow().rect, axis);
                    }
                    *width += (self.children.len() as f32 - 1f32).max(0f32) * layout.spacing;
                } else {
                    *width = 0f32;
                    for child in self.children.iter() {
                        *width = width.max(Self::axis_width(&child.borrow().rect, axis));
                    }
                    *width += const_width;
                }
                if let Some(min) = min {
                    *width = width.max(min);
                }
            }
            Sizing::Grow(_) => (), // Computed
            Sizing::Prefer { target: _, min } => {
                *width = const_width + min.unwrap_or(0f32);
            }
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
                let mut child = child.borrow_mut();
                let sizing = child.layout.axis_width(axis);
                println!("tangent sizing for child: {:?}", sizing);
                match sizing {
                    Sizing::Fixed(_) => (),
                    Sizing::Fit(_) => (),
                    Sizing::Grow(min) => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
                        *width = remaining_width.max(min.unwrap_or(0f32));
                    }
                    Sizing::Prefer { target, min } => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
                        println!(
                            "computing prefered height. prefered={}, remaining={}",
                            target, remaining_width
                        );
                        *width = target.min(remaining_width).max(min.unwrap_or(0f32));
                    }
                }
            }
            return;
        }
        remaining_width -= (self.children.len() as f32 - 1f32).max(0f32) * self.layout.spacing;

        // I'm doing this in this super convoluted way to test out how it might look ported to BraneScript, because of functional programming things
        let (mut remaining_width, mut grow_children, mut shrink_children) =
            self.children.iter().fold(
                (remaining_width, Vec::new(), Vec::new()),
                |(remaining_width, mut grow, mut shrink), child| {
                    let sizing = child.borrow().layout.axis_width(axis);
                    println!("axis sizing for child: {:?}", sizing);
                    match sizing {
                        Sizing::Fixed(size) => (remaining_width - size, grow, shrink),
                        Sizing::Fit(_) => (
                            remaining_width - Self::axis_width(&child.borrow().rect, axis),
                            grow,
                            shrink,
                        ),
                        Sizing::Grow(_) => {
                            *Self::axis_width_mut(&mut child.borrow_mut().rect, axis) = 0f32;
                            grow.push(child.clone());
                            (remaining_width, grow, shrink)
                        }
                        Sizing::Prefer { target, min: _ } => {
                            println!("adding shrink element");
                            *Self::axis_width_mut(&mut child.borrow_mut().rect, axis) = target;
                            shrink.push(child.clone());
                            (remaining_width - target, grow, shrink)
                        }
                    }
                },
            );

        while !grow_children.is_empty() && remaining_width > 0.001f32 {
            let (smallest, _, width_to_add) = grow_children.iter().fold(
                (
                    Self::axis_width(&grow_children[0].borrow().rect, axis),
                    f32::INFINITY,
                    remaining_width,
                ),
                |(smallest, second_smallest, wta), child| {
                    let width = Self::axis_width(&child.borrow().rect, axis);
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
                .filter_map(|child| {
                    let width = Self::axis_width(&child.borrow().rect, axis);
                    if width != smallest {
                        return Some(child.clone());
                    }
                    let max_width = match child.borrow().layout.axis_width(axis) {
                        Sizing::Grow(max) => max.unwrap_or(f32::INFINITY),
                        _ => unreachable!(),
                    };
                    let mut c = child.borrow_mut();
                    let width = Self::axis_width_mut(&mut c.rect, axis);
                    let old_width = *width;

                    *width += width_to_add;
                    let bound_hit = *width >= max_width;
                    if bound_hit {
                        *width = max_width;
                    }

                    remaining_width -= *width - old_width;
                    if bound_hit { None } else { Some(child.clone()) }
                })
                .collect();
        }

        if !shrink_children.is_empty() {
            println!("shrinkable elements, remaining width: {}", remaining_width)
        }

        while !shrink_children.is_empty() && remaining_width < -0.001f32 {
            let (largest, _, width_to_add) = shrink_children.iter().fold(
                (
                    Self::axis_width(&shrink_children[0].borrow().rect, axis),
                    0f32,
                    remaining_width,
                ),
                |(largest, second_largest, wta), child| {
                    let width = Self::axis_width(&child.borrow().rect, axis);
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
                    let width = Self::axis_width(&child.borrow().rect, axis);
                    if width != largest {
                        println!(
                            "skipping shrink for child with size {} needed {}",
                            width, largest
                        );
                        return Some(child.clone());
                    }
                    let min_width = match child.borrow().layout.axis_width(axis) {
                        Sizing::Prefer { target: _, min } => min.unwrap_or(0f32),
                        _ => unreachable!(),
                    };
                    let mut c = child.borrow_mut();
                    let width = Self::axis_width_mut(&mut c.rect, axis);
                    let old_width = *width;

                    *width += width_to_add;
                    let bound_hit = *width <= min_width;
                    if bound_hit {
                        *width = min_width;
                    }
                    println!(
                        "shrinking {} to {}, remaining was {} is now {}",
                        old_width,
                        *width,
                        remaining_width,
                        remaining_width - (*width - old_width)
                    );
                    remaining_width -= *width - old_width;
                    if bound_hit { None } else { Some(child.clone()) }
                })
                .collect();
        }
    }

    fn compute_dynamic_widths(&mut self) {
        println!("computing dynamic child widths");
        self.compute_dynamic_size(LayoutDir::LeftRight);
        for child in self.children.iter_mut() {
            println!("walking child node");
            child.borrow_mut().compute_dynamic_widths();
        }
    }

    fn compute_dynamic_heights(&mut self) {
        println!("computing dynamic child heights");
        self.compute_dynamic_size(LayoutDir::Decending);
        for child in self.children.iter_mut() {
            println!("walking child node");
            child.borrow_mut().compute_dynamic_heights();
        }
    }

    fn compute_text_wrap(&mut self) {
        for child in self.children.iter() {
            child.borrow_mut().compute_text_wrap();
        }

        if let UIContent::Text(te) = &self.content {
            if !te.wrap {
                return;
            }
            let max_width = self.rect.width;

            let mut break_start = 0;
            let mut wrapped_text = String::new();
            for i in 0..te.text.len() {
                if break_start > (i + 1) {
                    continue;
                }
                let line = &te.text[break_start..(i + 1)];
                let width = te.font.measure_text(line, te.font_size, 1f32).x;
                if width > max_width {
                    let mut break_end = i;
                    let mut next_break_start = i;
                    let can_break;
                    loop {
                        let c = te.text.chars().nth(break_end).unwrap();
                        if c == ' ' {
                            next_break_start = break_end + 1;
                            can_break = true;
                            break;
                        }
                        if c == '-' && break_end != i {
                            break_end = (break_end + 1).min(te.text.len());
                            next_break_start = break_end;
                            can_break = true;
                            break;
                        }
                        if break_end == 0 {
                            break_end = break_start;
                            can_break = false;
                            break;
                        }
                        break_end -= 1;
                    }
                    if can_break && break_end > break_start {
                        if wrapped_text.is_empty() {
                            wrapped_text = (&te.text[break_start..break_end]).into();
                        } else {
                            wrapped_text =
                                format!("{}\n{}", wrapped_text, &te.text[break_start..break_end]);
                        }
                        break_start = next_break_start;
                    }
                }
            }

            let rem = &te.text[break_start..te.text.len()];
            if rem.len() > 0 {
                wrapped_text = format!("{}\n{}", wrapped_text, rem);
            }

            if wrapped_text.is_empty() {
                self.wrapped_text = None;
            } else {
                self.wrapped_text = Some(wrapped_text);
            }
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
                    let height = Self::axis_height(&child.borrow().rect, dir);
                    let width = Self::axis_width(&child.borrow().rect, dir);
                    let mut c = child.borrow_mut();
                    let (x, y) = Self::axis_pos(&mut c.rect, dir);
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
                    let height = Self::axis_height(&child.borrow().rect, dir);
                    let width = Self::axis_width(&child.borrow().rect, dir);
                    let mut c = child.borrow_mut();
                    let (x, y) = Self::axis_pos(&mut c.rect, dir);
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
            child.borrow_mut().compute_positions();
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
            UIContent::Text(te) => d.draw_text_ex(
                te.font.as_ref(),
                self.wrapped_text.as_ref().unwrap_or(&te.text),
                Vector2::new(
                    self.rect.x + self.layout.padding.left,
                    self.rect.y + self.layout.padding.top,
                ),
                te.font_size,
                1f32,
                te.color,
            ),
        }
        for child in self.children.iter() {
            child.borrow().draw(d);
        }
    }
}

pub struct UIElementBuilder {
    content: UIContent,
    width: Option<Sizing>,
    height: Option<Sizing>,
    padding: Option<BorderSizes>,
    spacing: Option<f32>,
    direction: Option<LayoutDir>,
    align: Option<LayoutAlign>,
    children: Vec<Rc<RefCell<UIElement>>>,
    on_event: Option<Box<dyn FnMut(&mut UIElement, UIEvent) -> bool>>,
}

impl UIElementBuilder {
    pub fn build(self) -> UIElement {
        let layout = if let UIContent::Text(te) = &self.content {
            Layout::text_element(te)
        } else {
            Layout {
                width: self.width.unwrap_or(Sizing::Fit(None)),
                height: self.height.unwrap_or(Sizing::Fit(None)),
                padding: self.padding.unwrap_or(BorderSizes::uniform(0f32)),
                spacing: self.spacing.unwrap_or(0f32),
                direction: self.direction.unwrap_or(LayoutDir::LeftRight),
                align: self.align.unwrap_or(LayoutAlign::Start),
            }
        };
        UIElement {
            rect: Rectangle::default(),
            wrapped_text: None,
            content: self.content,
            layout,
            children: self.children,
            on_event: self.on_event,
        }
    }

    /// Set both width and height to sizing
    #[must_use]
    pub fn sizing(mut self, sizing: Sizing) -> Self {
        self.width = Some(sizing);
        self.height = Some(sizing);
        self
    }

    #[must_use]
    pub fn width(mut self, sizing: Sizing) -> Self {
        self.width = Some(sizing);
        self
    }

    #[must_use]
    pub fn height(mut self, sizing: Sizing) -> Self {
        self.height = Some(sizing);
        self
    }

    #[must_use]
    pub fn padding(mut self, padding: BorderSizes) -> Self {
        self.padding = Some(padding);
        self
    }

    /// Set padding on all sides to the same value
    #[must_use]
    pub fn uniform_padding(mut self, padding: f32) -> Self {
        self.padding = Some(BorderSizes::uniform(padding));
        self
    }

    /// Space between child UIElements
    #[must_use]
    pub fn spacing(mut self, spacing: f32) -> Self {
        self.spacing = Some(spacing);
        self
    }

    /// Direction axis to layout children on
    #[must_use]
    pub fn direction(mut self, direction: LayoutDir) -> Self {
        self.direction = Some(direction);
        self
    }

    /// How to align chilren tangentally to the direction axis
    #[must_use]
    pub fn align(mut self, align: LayoutAlign) -> Self {
        self.align = Some(align);
        self
    }

    /// set an event handler for all UI events
    #[must_use]
    pub fn on_event(
        mut self,
        callback: impl FnMut(&mut UIElement, UIEvent) -> bool + 'static,
    ) -> Self {
        self.on_event = Some(Box::new(callback));
        self
    }

    pub fn add_child(&mut self, child: UIElement) -> Weak<RefCell<UIElement>> {
        let el = Rc::new(RefCell::new(child));
        let ret = Rc::downgrade(&el);
        self.children.push(el);
        ret
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
