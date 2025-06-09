use branec::{hir, queries};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::{Rc, Weak},
    sync::Arc,
};

use raylib::prelude::*;

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

#[derive(Clone, Copy, Debug)]
pub enum UIEvent {
    MouseDown(Vector2, MouseButton),
    MouseUp(Vector2, MouseButton),
    MouseDrag(Vector2),
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
    pub on_event: Option<Box<dyn FnMut(UIEvent, bool) -> bool>>,
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
                match sizing {
                    Sizing::Fixed(_) => (),
                    Sizing::Fit(_) => (),
                    Sizing::Grow(min) => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
                        *width = remaining_width.max(min.unwrap_or(0f32));
                    }
                    Sizing::Prefer { target, min } => {
                        let width = Self::axis_width_mut(&mut child.rect, axis);
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
                    remaining_width -= *width - old_width;
                    if bound_hit { None } else { Some(child.clone()) }
                })
                .collect();
        }
    }

    fn compute_dynamic_widths(&mut self) {
        self.compute_dynamic_size(LayoutDir::LeftRight);
        for child in self.children.iter_mut() {
            child.borrow_mut().compute_dynamic_widths();
        }
    }

    fn compute_dynamic_heights(&mut self) {
        self.compute_dynamic_size(LayoutDir::Decending);
        for child in self.children.iter_mut() {
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

    // Return all chilren that have an event listener
    pub fn get_event_targets(&self, targets: &mut Vec<(UIRef, bool)>, pos: Option<Vector2>) {
        for child in self.children.iter() {
            let c = child.borrow();

            c.get_event_targets(targets, pos);
            if let Some(_) = &c.on_event {
                targets.push((
                    child.clone(),
                    if let Some(pos) = pos {
                        c.rect.check_collision_point_rec(pos)
                    } else {
                        false
                    },
                ));
            }
        }
    }

    pub fn send_event(&mut self, event: UIEvent, pos: Option<Vector2>) -> bool {
        // TEMPORARY SOLUTION
        let mut targets = Vec::new();
        self.get_event_targets(&mut targets, pos);

        for target in targets {
            let mut callback = { target.0.borrow_mut().on_event.take() }.unwrap();
            let consumed = (callback)(event, target.1);
            target.0.borrow_mut().on_event = Some(callback);
            if consumed {
                return true;
            }
        }

        if let Some(on_event) = &mut self.on_event {
            (*on_event)(
                event,
                if let Some(pos) = pos {
                    self.rect.check_collision_point_rec(pos)
                } else {
                    false
                },
            );
        }
        false
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
    on_event: Option<Box<dyn FnMut(UIEvent, bool) -> bool>>,
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
    pub fn on_event(mut self, callback: impl FnMut(UIEvent, bool) -> bool + 'static) -> Self {
        self.on_event = Some(Box::new(callback));
        self
    }

    #[must_use]
    pub fn child(mut self, child: UIElement) -> Self {
        let el = Rc::new(RefCell::new(child));
        self.children.push(el);
        self
    }

    pub fn add_child(&mut self, child: UIElement) -> WeakUIRef {
        let el = Rc::new(RefCell::new(child));
        let ret = Rc::downgrade(&el);
        self.children.push(el);
        ret
    }

    pub fn add_child_ref(&mut self, child: UIRef) {
        self.children.push(child);
    }
}

type WeakUIRef = Weak<RefCell<UIElement>>;
type UIRef = Rc<RefCell<UIElement>>;

#[allow(unused)]
pub mod style {
    use std::{
        cell::LazyCell,
        rc::Rc,
        sync::{Arc, LazyLock, RwLock},
    };

    use raylib::{color::Color, text::WeakFont};

    pub const TEXT: Color = Color::new(255, 255, 255, 255);
    pub const TEXT_DISABLED: Color = Color::new(128, 128, 128, 255);
    pub const WINDOW_BG: Color = Color::new(15, 15, 15, 240);
    pub const CHILD_BG: Color = Color::new(0, 0, 0, 0);
    pub const POPUP_BG: Color = Color::new(20, 20, 20, 240);
    pub const BORDER: Color = Color::new(110, 128, 110, 128);
    pub const BORDER_SHADOW: Color = Color::new(0, 0, 0, 0);
    pub const FRAME_BG: Color = Color::new(41, 122, 74, 138);
    pub const FRAME_BG_HOVERED: Color = Color::new(66, 250, 151, 102);
    pub const FRAME_BG_ACTIVE: Color = Color::new(66, 250, 151, 171);
    pub const TITLE_BG: Color = Color::new(10, 10, 10, 255);
    pub const TITLE_BG_ACTIVE: Color = Color::new(41, 122, 74, 255);
    pub const TITLE_BG_COLLAPSED: Color = Color::new(0, 0, 0, 130);
    pub const MENU_BAR_BG: Color = Color::new(36, 36, 36, 255);
    pub const SCROLLBAR_BG: Color = Color::new(5, 5, 5, 135);
    pub const SCROLLBAR_GRAB: Color = Color::new(79, 79, 79, 255);
    pub const SCROLLBAR_GRAB_HOVERED: Color = Color::new(104, 104, 104, 255);
    pub const SCROLLBAR_GRAB_ACTIVE: Color = Color::new(130, 130, 130, 255);
    pub const CHECK_MARK: Color = Color::new(66, 250, 151, 255);
    pub const SLIDER_GRAB: Color = Color::new(61, 224, 133, 255);
    pub const SLIDER_GRAB_ACTIVE: Color = Color::new(66, 250, 151, 255);
    pub const BUTTON: Color = Color::new(66, 250, 151, 255);
    pub const BUTTON_HOVERED: Color = Color::new(66, 250, 151, 179);
    pub const BUTTON_ACTIVE: Color = Color::new(15, 250, 135, 255);
    pub const HEADER: Color = Color::new(66, 250, 151, 79);
    pub const HEADER_HOVERED: Color = Color::new(66, 250, 151, 102);
    pub const HEADER_ACTIVE: Color = Color::new(66, 250, 151, 255);
    pub const SEPARATOR: Color = BORDER;
    pub const SEPARATOR_HOVERED: Color = Color::new(26, 191, 102, 199);
    pub const SEPARATOR_ACTIVE: Color = Color::new(26, 191, 102, 255);
    pub const RESIZE_GRIP: Color = Color::new(66, 250, 151, 51);
    pub const RESIZE_GRIP_HOVERED: Color = Color::new(66, 250, 151, 171);
    pub const RESIZE_GRIP_ACTIVE: Color = Color::new(66, 250, 151, 242);
    // Complex ImLerp-based TAB colors omitted — can be interpolated in code
    pub const DOCKING_EMPTY_BG: Color = Color::new(51, 51, 51, 255);
    pub const PLOT_LINES: Color = Color::new(156, 156, 156, 255);
    pub const PLOT_LINES_HOVERED: Color = Color::new(255, 110, 89, 255);
    pub const PLOT_HISTOGRAM: Color = Color::new(230, 179, 0, 255);
    pub const PLOT_HISTOGRAM_HOVERED: Color = Color::new(255, 153, 0, 255);
    pub const TABLE_HEADER_BG: Color = Color::new(48, 51, 48, 255);
    pub const TABLE_BORDER_STRONG: Color = Color::new(79, 89, 79, 255);
    pub const TABLE_BORDER_LIGHT: Color = Color::new(58, 64, 58, 255);
    pub const TABLE_ROW_BG: Color = Color::new(0, 0, 0, 0);
    pub const TABLE_ROW_BG_ALT: Color = Color::new(255, 255, 255, 15);
    pub const TEXT_SELECTED_BG: Color = Color::new(66, 250, 151, 89);
    pub const DRAG_DROP_TARGET: Color = Color::new(51, 255, 51, 230);
    pub const NAV_HIGHLIGHT: Color = Color::new(66, 250, 151, 255);
    pub const NAV_WINDOWING_HIGHLIGHT: Color = Color::new(255, 255, 255, 179);
    pub const NAV_WINDOWING_DIM_BG: Color = Color::new(50, 50, 50, 255);
    pub const MODAL_WINDOW_DIM_BG: Color = Color::new(204, 204, 204, 89);

    pub const FN_BACKGROUND: Color = Color::new(77, 145, 255, 255);
}

pub enum FollowMode {
    // Smoothly move towards target element
    Smooth(f32),
    // No smoothing
    Snap,
}

pub struct PlaceholderUI {
    pub position: WeakUIRef,
    pub content: WeakUIRef,
    pub follow_mode: FollowMode,
}

pub struct UIContext {
    pub project: Arc<RefCell<hir::Project>>,
    pub graph: hir::GraphId,

    pub root: UIElement,
    pub graph_content: UIElement,

    /// Node adding
    pub left_sidebar: UIRef,
    pub left_sidebar_content: UIRef,
    /// Node props
    pub right_sidebar: UIRef,
    pub right_sidebar_border: UIRef,
    pub right_sidebar_content: UIRef,
    /// Node editing area
    pub graph_placeholder: UIRef,
    pub graph_y_scrollbar: UIRef,
    pub graph_x_scrollbar: UIRef,
    //pub node_graph: NodeGraphUI,
    //pub inspector_sidebar: InspectorSidebarUI,
    pub last_mouse_pos: Vector2,
    pub mouse_down_last_frame: bool,

    // Element ref cache
    pub functions: HashMap<hir::ItemId, UIRef>,
    pub blocks: HashMap<hir::BlockId, Rc<RefCell<BlockUI>>>,

    pub placeholders: Vec<PlaceholderUI>,

    pub popup: Rc<AddBlockNodePopupCtx>,

    header_font: Arc<WeakFont>,
}

impl UIContext {
    pub fn new(
        rl: &mut RaylibHandle,
        thread: &RaylibThread,
        graph: hir::GraphId,
        project: Arc<RefCell<hir::Project>>,
    ) -> UIContext {
        let header_font = Arc::new(
            rl.load_font_ex(
                &thread,
                "fonts/JetBrains/JetBrainsMonoNerdFont-Bold.ttf",
                30,
                None,
            )
            .expect("Couldn't find font")
            .make_weak(),
        );

        let mut root = UIElement::new(UIContent::None);

        let mut left_sidebar = UIElement::new(UIContent::Rect(style::NAV_WINDOWING_DIM_BG))
            .width(Sizing::Fixed(300f32))
            .height(Sizing::Grow(None));

        let left_sidebar_content = left_sidebar
            .add_child(
                UIElement::new(UIContent::None)
                    .width(Sizing::Grow(None))
                    .height(Sizing::Grow(None))
                    .build(),
            )
            .upgrade()
            .unwrap();

        left_sidebar.add_child(
            UIElement::new(UIContent::Rect(style::BUTTON))
                .width(Sizing::Fixed(5f32))
                .height(Sizing::Grow(None))
                .build(),
        );

        let left_sidebar = root.add_child(left_sidebar.build()).upgrade().unwrap();

        let mut graph_area = UIElement::new(UIContent::None)
            .width(Sizing::Grow(None))
            .height(Sizing::Grow(None));

        let mut graph_sub = UIElement::new(UIContent::None)
            .width(Sizing::Grow(None))
            .height(Sizing::Grow(None))
            .direction(LayoutDir::Ascending);

        let mut graph_x_scrollbar = UIElement::new(UIContent::Rect(style::WINDOW_BG))
            .width(Sizing::Grow(None))
            .uniform_padding(3f32);

        graph_x_scrollbar.add_child(
            UIElement::new(UIContent::Rounded {
                color: style::BUTTON,
                corner_radius: 4f32,
            })
            .width(Sizing::Grow(None))
            .height(Sizing::Fixed(8f32))
            .build(),
        );

        let graph_x_scrollbar = graph_sub
            .add_child(graph_x_scrollbar.build())
            .upgrade()
            .unwrap();

        let graph_placeholder = graph_sub
            .add_child(
                UIElement::new(UIContent::None)
                    .width(Sizing::Grow(None))
                    .height(Sizing::Grow(None))
                    .build(),
            )
            .upgrade()
            .unwrap();

        graph_area.add_child(graph_sub.build());

        let mut graph_y_scrollbar = UIElement::new(UIContent::Rect(style::WINDOW_BG))
            .height(Sizing::Grow(None))
            .uniform_padding(3f32);

        graph_y_scrollbar.add_child(
            UIElement::new(UIContent::Rounded {
                color: style::BUTTON,
                corner_radius: 4f32,
            })
            .height(Sizing::Grow(None))
            .width(Sizing::Fixed(8f32))
            .build(),
        );

        let graph_y_scrollbar = graph_area
            .add_child(graph_y_scrollbar.build())
            .upgrade()
            .unwrap();

        root.add_child(graph_area.build());

        let mut right_sidebar = UIElement::new(UIContent::Rect(style::NAV_WINDOWING_DIM_BG))
            .width(Sizing::Prefer {
                target: 500f32,
                min: Some(200f32),
            })
            .height(Sizing::Grow(None));

        let right_sidebar_border = right_sidebar
            .add_child(
                UIElement::new(UIContent::Rect(style::BUTTON))
                    .width(Sizing::Fixed(5f32))
                    .height(Sizing::Grow(None))
                    .build(),
            )
            .upgrade()
            .unwrap();

        let mut right_sidebar_content = UIElement::new(UIContent::None)
            .align(LayoutAlign::Center)
            .uniform_padding(10f32)
            .width(Sizing::Grow(None))
            .height(Sizing::Grow(None));

        right_sidebar_content.add_child(
            UIElement::new(UIContent::Rounded {
                color: style::BUTTON,
                corner_radius: 20f32,
            })
            .child(
                UIElement::new_text(
                    header_font.clone(),
                    "Run".into(),
                    40f32,
                    false,
                    Color::WHITE,
                )
                .build(),
            )
            .on_event({
                let project = project.clone();
                move |event, in_bounds| match event {
                    UIEvent::MouseDown(_, _) => false,
                    UIEvent::MouseUp(_, mouse_button) => {
                        if in_bounds && mouse_button == MouseButton::MOUSE_BUTTON_LEFT {
                            //compile
                            println!("COMPILING");
                            match ::branec::lower_hir(&*project.borrow()) {
                                Ok(modules) => {
                                    println!("compiled modules:");

                                    let backend =
                                        ::brane_backend_llvm::LLVMJitBackend::new().unwrap();
                                    for module in modules {
                                        println!("{}", module);
                                        println!("staging: {:#?}", backend.stage_module(module));
                                    }
                                    if let Err(err) = backend.process_modules() {
                                        println!("error loading modules: {:?}", err);
                                        return true;
                                    }

                                    println!("Loaded functions:");
                                    let functions = backend.functions.read().unwrap();
                                    for f in functions.iter() {
                                        println!("{}", f.0)
                                    }

                                    let main_fn: fn(*const *mut u8, i32, f32, f32) -> f32 = unsafe {
                                        std::mem::transmute(
                                            *functions
                                                .get("main")
                                                .expect("didn't find main function"),
                                        )
                                    };

                                    let mut stack_page = [0u32; u16::MAX as usize / 4];
                                    let bindings_page =
                                        [stack_page.as_mut_ptr() as *mut u8; u16::MAX as usize];

                                    let ret = main_fn(bindings_page.as_ptr(), 0i32, 2f32, 2f32);
                                    println!("return value: {}", ret);

                                    println!("return values: ");
                                    for i in 0..10 {
                                        let ret = unsafe {
                                            *((stack_page.as_mut_ptr() as *mut f32).add(i))
                                        };
                                        println!("{}", ret);
                                    }
                                }
                                Err(err) => println!("Failed to compile: {:?}", err),
                            };

                            true
                        } else {
                            false
                        }
                    }
                    UIEvent::MouseDrag(vector2) => false,
                }
            })
            .build(),
        );

        let right_sidebar_content = right_sidebar
            .add_child(right_sidebar_content.build())
            .upgrade()
            .unwrap();

        let right_sidebar = root.add_child(right_sidebar.build()).upgrade().unwrap();

        let root = root.build();

        let graph_content =
            UIElement::new(UIContent::Rect(style::NAV_WINDOWING_DIM_BG)).uniform_padding(15f32);

        let graph_content = graph_content.build();

        let popup = AddBlockNodePopupCtx::new(header_font.clone(), project.clone());

        let ctx = UIContext {
            root,
            graph,
            graph_content,
            left_sidebar,
            left_sidebar_content,
            right_sidebar,
            right_sidebar_content,
            right_sidebar_border,
            graph_placeholder,
            graph_y_scrollbar,
            graph_x_scrollbar,
            last_mouse_pos: Vector2::new(0f32, 0f32),
            mouse_down_last_frame: false,
            functions: Default::default(),
            blocks: Default::default(),
            project,
            header_font,
            placeholders: Vec::new(),
            popup,
        };
        ctx.init_events();
        ctx
    }

    fn init_events(&self) {
        self.right_sidebar_border.borrow_mut().on_event = Some({
            let mut is_clicked = false;
            let sidebar = Rc::downgrade(&self.right_sidebar);
            let mut drag_width = 0f32;
            Box::new(move |ui_event, in_bounds| match ui_event {
                UIEvent::MouseDown(_, button) => {
                    if button == MouseButton::MOUSE_BUTTON_LEFT {
                        is_clicked = in_bounds;
                        drag_width = sidebar.upgrade().unwrap().borrow().rect.width;
                        in_bounds
                    } else {
                        false
                    }
                }
                UIEvent::MouseUp(_, button) => {
                    if is_clicked && button == MouseButton::MOUSE_BUTTON_LEFT {
                        drag_width = sidebar.upgrade().unwrap().borrow().rect.width;
                        let sidebar = sidebar.upgrade().unwrap();
                        let mut sidebar = sidebar.borrow_mut();
                        if let Sizing::Prefer {
                            target: _,
                            min: Some(min),
                        } = sidebar.layout.width
                        {
                            sidebar.layout.width = Sizing::Prefer {
                                target: min.max(drag_width),
                                min: Some(min),
                            };
                        }
                        is_clicked = false;
                    }
                    false
                }
                UIEvent::MouseDrag(delta) => {
                    if is_clicked {
                        let sidebar = sidebar.upgrade().unwrap();
                        let mut sidebar = sidebar.borrow_mut();
                        let Sizing::Prefer {
                            target: _,
                            min: Some(min),
                        } = sidebar.layout.width
                        else {
                            unreachable!()
                        };

                        drag_width -= delta.x;
                        sidebar.layout.width = Sizing::Prefer {
                            target: min.max(drag_width),
                            min: Some(min),
                        };
                    }
                    is_clicked
                }
            })
        });
    }

    pub fn send_event(&mut self, event: UIEvent, pos: Option<Vector2>) -> bool {
        let popup_res = { self.popup.send_event(event, pos) };
        popup_res || self.root.send_event(event, pos) || self.graph_content.send_event(event, pos)
    }

    pub fn draw(&mut self, d: &mut RaylibDrawHandle) {
        let mouse_pos = d.get_mouse_position();
        if d.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_LEFT) {
            self.send_event(
                UIEvent::MouseDown(mouse_pos, MouseButton::MOUSE_BUTTON_LEFT),
                Some(mouse_pos),
            );
        }

        if d.is_mouse_button_pressed(MouseButton::MOUSE_BUTTON_RIGHT) {
            self.send_event(
                UIEvent::MouseDown(mouse_pos, MouseButton::MOUSE_BUTTON_RIGHT),
                Some(mouse_pos),
            );
        }
        if d.is_mouse_button_released(MouseButton::MOUSE_BUTTON_LEFT) {
            self.send_event(
                UIEvent::MouseUp(mouse_pos, MouseButton::MOUSE_BUTTON_LEFT),
                Some(mouse_pos),
            );
        }
        if d.is_mouse_button_released(MouseButton::MOUSE_BUTTON_RIGHT) {
            self.send_event(
                UIEvent::MouseUp(mouse_pos, MouseButton::MOUSE_BUTTON_RIGHT),
                Some(mouse_pos),
            );
        }

        let mouse_down = d.is_mouse_button_down(MouseButton::MOUSE_BUTTON_LEFT);
        if mouse_down && self.mouse_down_last_frame {
            let delta = d.get_mouse_position() - self.last_mouse_pos;
            self.send_event(UIEvent::MouseDrag(delta), Some(self.last_mouse_pos));
        }
        self.mouse_down_last_frame = mouse_down;
        self.last_mouse_pos = d.get_mouse_position();

        // Update content
        self.graph_content.children.clear();
        {
            let project = self.project.clone();
            let project = project.borrow();
            let graph = project.graph(self.graph).unwrap();
            for item in graph.iter() {
                use ::branec::hir::Item::*;
                match item {
                    Pipe(_) => todo!(),
                    Trait(_) => todo!(),
                    TraitImpl(_) => todo!(),
                    Fn(function) => {
                        let ui = self.get_fn_ui(function);
                        self.graph_content.children.push(ui)
                    }
                }
            }
        }

        for (_id, graph_block) in self.blocks.iter_mut() {
            graph_block.borrow_mut().update_data(self.project.clone());
        }

        self.root.layout.width = Sizing::Fixed(d.get_render_width() as f32);
        self.root.layout.height = Sizing::Fixed(d.get_render_height() as f32);
        self.root.compute_layout();

        {
            let gp = self.graph_placeholder.borrow();
            self.graph_content.layout.width = Sizing::Fit(Some(gp.rect.width));
            self.graph_content.layout.height = Sizing::Fit(Some(gp.rect.height));
            self.graph_content.rect.x = gp.rect.x;
            self.graph_content.rect.y = gp.rect.y;
            self.graph_content.compute_layout();
            self.graph_content.draw(d);

            for (_id, graph_block) in self.blocks.iter_mut() {
                graph_block
                    .borrow_mut()
                    .draw_node_connections(d, self.project.clone());
            }
        }

        self.root.draw(d);
        self.popup.draw(d);
    }

    fn get_fn_ui(&mut self, function: &hir::Fn) -> UIRef {
        if let Some(ui) = self.functions.get(&function.id) {
            ui.clone()
        } else {
            let mut fn_ui = UIElement::new(UIContent::Rounded {
                color: style::FN_BACKGROUND,
                corner_radius: 20f32,
            })
            .direction(LayoutDir::Decending)
            .padding(BorderSizes {
                left: 20f32,
                top: 12f32,
                right: 20f32,
                bottom: 12f32,
            });

            fn_ui.add_child(
                UIElement::new_text(
                    self.header_font.clone(),
                    function.ident.clone(),
                    30f32,
                    false,
                    Color::WHITE,
                )
                .build(),
            );

            fn_ui.add_child_ref(self.get_block_ui(function.body, &function.sig));

            let fn_ui = Rc::new(RefCell::new(fn_ui.build()));
            self.functions.insert(function.id, fn_ui.clone());
            fn_ui
        }
    }

    fn get_block_ui(&mut self, block: hir::BlockId, sig: &hir::BlockSig) -> UIRef {
        if let Some(ui) = self.blocks.get(&block) {
            return ui.borrow().root.clone();
        }

        let block_ui = BlockUI::new(
            block,
            sig,
            self.graph,
            self.header_font.clone(),
            self.popup.clone(),
            self.project.clone(),
        );
        let ui = block_ui.borrow().root.clone();
        self.blocks.insert(block, block_ui);
        ui
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum GraphHandleId {
    BlockOutput(usize),
    BlockInput(usize),
    NodeInput(hir::NodeId, usize),
    NodeOutput(hir::NodeId, usize),
}

impl GraphHandleId {
    pub fn is_input_source(self) -> bool {
        match self {
            GraphHandleId::BlockInput(_) | GraphHandleId::NodeOutput(_, _) => true,
            GraphHandleId::NodeInput(_, _) | GraphHandleId::BlockOutput(_) => false,
        }
    }

    pub fn is_output_sink(self) -> bool {
        !self.is_input_source()
    }

    pub fn as_input_output(a: Self, b: Self) -> Option<(Self, Self)> {
        if a.is_input_source() == b.is_input_source() {
            return None;
        }
        if b.is_input_source() {
            Some((b, a))
        } else {
            Some((a, b))
        }
    }

    pub fn as_local_value(self) -> Option<hir::LocalValue> {
        match self {
            GraphHandleId::BlockOutput(_) => None,
            GraphHandleId::BlockInput(index) => Some(hir::LocalValue::BlockInput { index }),
            GraphHandleId::NodeInput(_, _) => None,
            GraphHandleId::NodeOutput(node, index) => {
                Some(hir::LocalValue::NodeOutput { node, index })
            }
        }
    }
}

pub struct BlockUI {
    pub root: UIRef,
    pub content: UIRef,
    pub block: hir::BlockId,
    pub graph_id: hir::GraphId,
    /// Node ui element / what colum it's in
    pub nodes: HashMap<hir::NodeId, NodeUI>,
    pub input_edges: Vec<GraphEdgeHandle>,
    pub output_edges: Vec<GraphEdgeHandle>,
    preview_drag: Option<GraphHandleId>,
    header_font: Arc<WeakFont>,
}

impl BlockUI {
    pub fn new(
        block: hir::BlockId,
        sig: &hir::BlockSig,
        graph_id: hir::GraphId,
        header_font: Arc<WeakFont>,
        popup: Rc<AddBlockNodePopupCtx>,
        proj: Arc<RefCell<hir::Project>>,
    ) -> Rc<RefCell<Self>> {
        let mut root = UIElement::new(UIContent::None).align(LayoutAlign::Center);

        let mut inputs = UIElement::new(UIContent::None).direction(LayoutDir::Decending);
        let mut outputs = UIElement::new(UIContent::None)
            .direction(LayoutDir::Decending)
            .align(LayoutAlign::End);

        let mut input_edges = Vec::new();
        let mut output_edges = Vec::new();

        for input in sig.inputs.iter() {
            let handle = make_edge_handle(
                true,
                input.ident.clone(),
                style::BUTTON,
                header_font.clone(),
            );
            inputs.add_child_ref(handle.root.clone());
            input_edges.push(handle);
        }

        for output in sig.outputs.iter() {
            let handle = make_edge_handle(
                false,
                output.ident.clone(),
                style::BUTTON,
                header_font.clone(),
            );
            outputs.add_child_ref(handle.root.clone());
            output_edges.push(handle);
        }

        root.add_child(inputs.build());
        let content = root
            .add_child(
                UIElement::new(UIContent::Rounded {
                    color: style::NAV_WINDOWING_DIM_BG,
                    corner_radius: 20f32,
                })
                .direction(LayoutDir::RightLeft)
                .spacing(25f32)
                .uniform_padding(10f32)
                .height(Sizing::Fit(Some(100f32)))
                .width(Sizing::Fit(Some(400f32)))
                .build(),
            )
            .upgrade()
            .unwrap();
        root.add_child(outputs.build());

        let root = Rc::new(RefCell::new(root.build()));

        let block = Rc::new(RefCell::new(Self {
            block,
            graph_id,
            root,
            content,
            nodes: HashMap::new(),
            header_font,
            input_edges,
            output_edges,
            preview_drag: None,
        }));

        block.borrow_mut().root.borrow_mut().on_event = Some({
            let block = Rc::downgrade(&block);
            let proj = Arc::downgrade(&proj);
            Box::new(move |ui_event, _in_bounds| {
                let Some(block_ptr) = block.upgrade() else {
                    return false;
                };

                let mut block = block_ptr.borrow_mut();
                let Some(proj) = proj.upgrade() else {
                    return false;
                };
                match ui_event {
                    UIEvent::MouseDown(pos, button) => match button {
                        MouseButton::MOUSE_BUTTON_LEFT => block.try_start_drag(pos),
                        MouseButton::MOUSE_BUTTON_RIGHT => {
                            popup.clear_menu();

                            for (graph_id, graph) in proj.borrow().graphs.iter() {
                                for item in graph.iter() {
                                    match item {
                                        hir::Item::Pipe(_) => (),
                                        hir::Item::Fn(func) => {
                                            popup.add_menu_item(func.ident.clone(), {
                                                let proj = proj.clone();
                                                let block = block_ptr.clone();
                                                let graph_id = *graph_id;
                                                let func_id = func.id;
                                                move || {
                                                    let block = block.borrow();
                                                    let mut proj = proj.borrow_mut();
                                                    let graph =
                                                        proj.graph_mut(block.graph_id).unwrap();
                                                    let block_data =
                                                        graph.block_mut(block.block).unwrap();
                                                    block_data.add_node(
                                                        hir::DefId {
                                                            graph: graph_id,
                                                            item: func_id,
                                                        },
                                                        vec![],
                                                    );
                                                }
                                            });
                                        }
                                        hir::Item::Trait(_) => (),
                                        hir::Item::TraitImpl(_) => (),
                                    }
                                }
                            }

                            popup.show(pos);

                            true
                        }
                        _ => false,
                    },
                    UIEvent::MouseUp(pos, button) => {
                        if button == MouseButton::MOUSE_BUTTON_LEFT {
                            if let Some(preview) = block.preview_drag.take() {
                                if let Some(dropped_on) = block.get_handle_at(pos) {
                                    if dropped_on != preview {
                                        block.connect_edges(
                                            preview,
                                            dropped_on,
                                            &mut *proj.borrow_mut(),
                                        );
                                    }
                                } else {
                                    block.set_local_value(
                                        preview,
                                        hir::LocalValue::BrokenRef,
                                        &mut *proj.borrow_mut(),
                                    );
                                }

                                if let Some(h) = block.get_handle_mut(preview) {
                                    h.preview_pos = None;
                                }
                            }
                        }
                        false
                    }
                    UIEvent::MouseDrag(delta) => {
                        if let Some(preview) = block.preview_drag {
                            if let Some(h) = block.get_handle_mut(preview) {
                                if let Some(pos) = &mut h.preview_pos {
                                    *pos += delta;
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    }
                }
            })
        });

        block
    }

    fn connect_edges(
        &mut self,
        a: GraphHandleId,
        b: GraphHandleId,
        proj: &mut hir::Project,
    ) -> bool {
        let Some((input, output)) = GraphHandleId::as_input_output(a, b) else {
            return false;
        };

        let local_value = input.as_local_value().unwrap();

        self.set_local_value(output, local_value, proj)
    }

    fn set_local_value(
        &mut self,
        output: GraphHandleId,
        value: hir::LocalValue,
        proj: &mut hir::Project,
    ) -> bool {
        match output {
            GraphHandleId::BlockOutput(output) => {
                let graph = proj.graph_mut(self.graph_id).unwrap();
                let block = graph.block_mut(self.block).unwrap();
                while block.outputs.len() <= output {
                    block.outputs.push(hir::LocalValue::BrokenRef);
                }
                block.outputs[output] = value;
                true
            }
            GraphHandleId::NodeInput(node, input) => {
                let graph = proj.graph_mut(self.graph_id).unwrap();
                let block = graph.block_mut(self.block).unwrap();
                let node = block.nodes.get_mut(&node).unwrap();
                while node.inputs.len() <= input {
                    node.inputs.push(hir::LocalValue::BrokenRef);
                }
                node.inputs[input] = value;
                true
            }
            _ => false,
        }
    }

    fn get_handle_at(&self, mouse_pos: Vector2) -> Option<GraphHandleId> {
        for (i, input) in self.input_edges.iter().enumerate() {
            if input
                .handle
                .borrow()
                .rect
                .check_collision_point_rec(mouse_pos)
            {
                return Some(GraphHandleId::BlockInput(i));
            }
        }

        for (i, output) in self.output_edges.iter().enumerate() {
            if output
                .handle
                .borrow()
                .rect
                .check_collision_point_rec(mouse_pos)
            {
                return Some(GraphHandleId::BlockOutput(i));
            }
        }

        for (id, node) in self.nodes.iter() {
            for (i, input) in node.input_edges.iter().enumerate() {
                if input
                    .handle
                    .borrow()
                    .rect
                    .check_collision_point_rec(mouse_pos)
                {
                    return Some(GraphHandleId::NodeInput(*id, i));
                }
            }

            for (i, output) in node.output_edges.iter().enumerate() {
                if output
                    .handle
                    .borrow()
                    .rect
                    .check_collision_point_rec(mouse_pos)
                {
                    return Some(GraphHandleId::NodeOutput(*id, i));
                }
            }
        }
        None
    }

    fn get_handle(&self, id: GraphHandleId) -> Option<&GraphEdgeHandle> {
        match id {
            GraphHandleId::BlockOutput(i) => self.output_edges.get(i),
            GraphHandleId::BlockInput(i) => self.input_edges.get(i),
            GraphHandleId::NodeInput(node, i) => self
                .nodes
                .get(&node)
                .map(|node| node.input_edges.get(i))
                .flatten(),
            GraphHandleId::NodeOutput(node, i) => self
                .nodes
                .get(&node)
                .map(|node| node.output_edges.get(i))
                .flatten(),
        }
    }

    fn get_handle_mut(&mut self, id: GraphHandleId) -> Option<&mut GraphEdgeHandle> {
        match id {
            GraphHandleId::BlockOutput(i) => self.output_edges.get_mut(i),
            GraphHandleId::BlockInput(i) => self.input_edges.get_mut(i),
            GraphHandleId::NodeInput(node, i) => self
                .nodes
                .get_mut(&node)
                .map(|node| node.input_edges.get_mut(i))
                .flatten(),
            GraphHandleId::NodeOutput(node, i) => self
                .nodes
                .get_mut(&node)
                .map(|node| node.output_edges.get_mut(i))
                .flatten(),
        }
    }

    pub fn try_start_drag(&mut self, mouse_pos: Vector2) -> bool {
        println!("Trying start drag");
        let Some(handle_id) = self.get_handle_at(mouse_pos) else {
            return false;
        };
        println!("Found handle");

        self.preview_drag = Some(handle_id);
        let handle = self.get_handle_mut(handle_id).unwrap();
        handle.preview_pos = Some(mouse_pos);
        true
    }

    pub fn update_data(&mut self, proj: Arc<RefCell<hir::Project>>) {
        self.content.borrow_mut().children.clear();

        let p = proj.borrow();
        let graph = p.graph(self.graph_id).unwrap();
        let block = graph.block(self.block).unwrap();
        for (id, _node) in block.nodes.iter() {
            let depth = queries::node_max_depth(*id, block);
            let depth_index = match depth {
                queries::NodeDepth::Orphaned(depth) => depth,
                queries::NodeDepth::InTree(depth) => depth,
            };
            while depth_index >= { self.content.borrow().children.len() } {
                self.content
                    .borrow_mut()
                    .children
                    .push(Rc::new(RefCell::new(
                        UIElement::new(UIContent::None)
                            .direction(LayoutDir::Decending)
                            .spacing(10f32)
                            .build(),
                    )))
            }

            let node_ui = self.get_node_ui(*id, block, proj.clone());
            self.content.borrow_mut().children[depth_index]
                .borrow_mut()
                .children
                .push(node_ui);
        }
    }

    pub fn get_node_ui(
        &mut self,
        node_id: hir::NodeId,
        block: &hir::Block,
        proj: Arc<RefCell<hir::Project>>,
    ) -> UIRef {
        if let Some(ui) = self.nodes.get(&node_id) {
            return ui.root.clone();
        }
        let ui = NodeUI::new(node_id, block, self.header_font.clone(), proj.clone());
        self.nodes.insert(node_id, ui);
        self.get_node_ui(node_id, block, proj)
    }

    pub fn draw_node_connections(
        &mut self,
        d: &mut RaylibDrawHandle,
        proj: Arc<RefCell<hir::Project>>,
    ) {
        let proj = proj.borrow();
        let graph = proj.graph(self.graph_id).unwrap();
        let block = graph.block(self.block).unwrap();

        for (output_index, output) in block.outputs.iter().enumerate() {
            let Some(end) = self.output_edges.get(output_index) else {
                continue;
            };

            match output {
                hir::LocalValue::BrokenRef => (),
                hir::LocalValue::BlockInput { index } => {
                    if let Some(start) = self.input_edges.get(*index) {
                        Self::draw_connection(d, &start.handle, &end.handle, style::BUTTON);
                    }
                }
                hir::LocalValue::NodeOutput { node, index } => {
                    if let Some(start) = self
                        .nodes
                        .get(node)
                        .map(|node| node.output_edges.get(*index))
                        .flatten()
                    {
                        Self::draw_connection(d, &start.handle, &end.handle, style::BUTTON);
                    }
                }
            }
        }

        for (id, node) in self.nodes.iter() {
            let Some(node_data) = block.nodes.get(id) else {
                continue;
            };
            for (input_index, input) in node_data.inputs.iter().enumerate() {
                let Some(end) = node.input_edges.get(input_index) else {
                    continue;
                };

                match input {
                    hir::LocalValue::BrokenRef => (),
                    hir::LocalValue::BlockInput { index } => {
                        if let Some(start) = self.input_edges.get(*index) {
                            Self::draw_connection(d, &start.handle, &end.handle, style::BUTTON);
                        }
                    }
                    hir::LocalValue::NodeOutput { node, index } => {
                        if let Some(start) = self
                            .nodes
                            .get(node)
                            .map(|node| node.output_edges.get(*index))
                            .flatten()
                        {
                            Self::draw_connection(d, &start.handle, &end.handle, style::BUTTON);
                        }
                    }
                }
            }
        }

        if let Some(preview) = self.preview_drag {
            let Some(handle) = self.get_handle(preview) else {
                self.preview_drag = None;
                println!("preview handle invalid");
                return;
            };
            let start = handle.handle.borrow().rect;
            let start = Vector2::new(start.x + start.width / 2f32, start.y + start.height / 2f32);
            let Some(end) = &handle.preview_pos else {
                self.preview_drag = None;
                println!("preview pos not set");
                return;
            };

            d.draw_line_bezier(start, end, 3f32, style::BUTTON);
        }
    }

    fn draw_connection(d: &mut RaylibDrawHandle, start: &UIRef, end: &UIRef, color: Color) {
        let start = start.borrow().rect;
        let start = Vector2::new(start.x + start.width / 2f32, start.y + start.height / 2f32);
        let end = end.borrow().rect;
        let end = Vector2::new(end.x + end.width / 2f32, end.y + end.height / 2f32);
        d.draw_line_bezier(start, end, 3f32, color);
    }
}

pub struct NodeUI {
    pub root: UIRef,
    pub title: UIRef,
    pub inputs: UIRef,
    pub outputs: UIRef,
    pub node: hir::NodeId,
    pub input_edges: Vec<GraphEdgeHandle>,
    pub output_edges: Vec<GraphEdgeHandle>,
}

pub struct GraphEdgeHandle {
    pub root: UIRef,
    pub ident: UIRef,
    pub handle: UIRef,
    pub preview_pos: Option<Vector2>,
}

impl NodeUI {
    pub fn new(
        node_id: hir::NodeId,
        block: &hir::Block,
        header_font: Arc<WeakFont>,
        proj: Arc<RefCell<hir::Project>>,
    ) -> Self {
        let node = block.nodes.get(&node_id).unwrap();

        let mut ui = UIElement::new(UIContent::Rounded {
            color: style::FN_BACKGROUND,
            corner_radius: 15f32,
        })
        .direction(LayoutDir::Decending)
        .uniform_padding(5f32);

        let proj = proj.borrow();
        let (ident, sig) = match proj.get_item(node.expr) {
            Some(item) => match item {
                hir::Item::Pipe(_) => todo!(),
                hir::Item::Fn(function) => (function.ident.clone(), Some(&function.sig)),
                hir::Item::Trait(_) => todo!(),
                hir::Item::TraitImpl(_) => todo!(),
            },
            None => ("broken ref".into(), None),
        };

        let title = ui
            .add_child(
                UIElement::new_text(
                    header_font.clone(),
                    ident.clone(),
                    20f32,
                    false,
                    Color::WHITE,
                )
                .build(),
            )
            .upgrade()
            .unwrap();

        let mut connections = UIElement::new(UIContent::None);

        let mut inputs = UIElement::new(UIContent::None).direction(LayoutDir::Decending);
        let mut outputs = UIElement::new(UIContent::None)
            .direction(LayoutDir::Decending)
            .align(LayoutAlign::End);

        let mut input_edges = Vec::new();
        let mut output_edges = Vec::new();

        if let Some(sig) = sig {
            for input in sig.inputs.iter() {
                let handle = make_edge_handle(
                    false,
                    input.ident.clone(),
                    style::BUTTON,
                    header_font.clone(),
                );
                inputs.add_child_ref(handle.root.clone());
                input_edges.push(handle);
            }

            for output in sig.outputs.iter() {
                let handle = make_edge_handle(
                    true,
                    output.ident.clone(),
                    style::BUTTON,
                    header_font.clone(),
                );
                outputs.add_child_ref(handle.root.clone());
                output_edges.push(handle);
            }
        }

        let inputs = connections.add_child(inputs.build()).upgrade().unwrap();
        /*connections.add_child(
            UIElement::new(UIContent::Rect(Color::RED))
                .sizing(Sizing::Grow(None))
                .build(),
        );*/

        let outputs = connections.add_child(outputs.build()).upgrade().unwrap();

        ui.add_child(connections.build());

        Self {
            root: Rc::new(RefCell::new(ui.build())),
            title,
            inputs,
            outputs,
            node: node_id,
            input_edges,
            output_edges,
        }
    }

    pub fn update_data(
        &mut self,
        block: &hir::Block,
        header_font: Arc<WeakFont>,
        proj: Arc<RefCell<hir::Project>>,
    ) {
        // TODO update title & edges
    }
}

pub fn make_edge_handle(
    swap: bool,
    ident: String,
    handle_color: Color,
    font: Arc<WeakFont>,
) -> GraphEdgeHandle {
    let mut root = UIElement::new(UIContent::None)
        .spacing(10f32)
        .align(LayoutAlign::Center)
        .sizing(Sizing::Fit(None));

    let handle = Rc::new(RefCell::new(
        UIElement::new(UIContent::Rounded {
            color: handle_color,
            corner_radius: 5f32,
        })
        .sizing(Sizing::Fixed(10f32))
        .build(),
    ));
    let ident = Rc::new(RefCell::new(
        UIElement::new_text(font, ident, 20f32, false, Color::WHITE).build(),
    ));

    let mut a = handle.clone();
    let mut b = ident.clone();
    if swap {
        std::mem::swap(&mut a, &mut b);
    }
    root.add_child_ref(a);
    root.add_child_ref(b);

    let root = Rc::new(RefCell::new(root.build()));
    GraphEdgeHandle {
        root,
        ident,
        handle,
        preview_pos: None,
    }
}

struct AddBlockNodePopupCtx {
    is_shown: Cell<bool>,
    root: RefCell<UIElement>,
    items: UIRef,
    project: Arc<RefCell<hir::Project>>,
    header_font: Arc<WeakFont>,
}

impl AddBlockNodePopupCtx {
    pub fn new(header_font: Arc<WeakFont>, project: Arc<RefCell<hir::Project>>) -> Rc<Self> {
        let mut root = UIElement::new(UIContent::Rounded {
            color: style::BUTTON,
            corner_radius: 20f32,
        })
        .uniform_padding(4f32);

        let mut content = UIElement::new(UIContent::Rounded {
            color: style::NAV_WINDOWING_DIM_BG,
            corner_radius: 20f32,
        })
        .uniform_padding(5f32)
        .direction(LayoutDir::Decending);

        content.add_child(
            UIElement::new_text(
                header_font.clone(),
                "Add Item".into(),
                30f32,
                false,
                Color::WHITE,
            )
            .build(),
        );
        // Seperator
        content.add_child(
            UIElement::new(UIContent::Rounded {
                color: style::BUTTON,
                corner_radius: 1f32,
            })
            .height(Sizing::Fixed(2f32))
            .width(Sizing::Grow(None))
            .build(),
        );

        let items = content
            .add_child(
                UIElement::new(UIContent::None)
                    .direction(LayoutDir::Decending)
                    .build(),
            )
            .upgrade()
            .unwrap();

        root.add_child(content.build());

        let root = RefCell::new(root.build());

        let ctx = Rc::new(Self {
            is_shown: Cell::new(false),
            items,
            root,
            project,
            header_font,
        });

        ctx.root.borrow_mut().on_event = Some({
            let ctx = Rc::downgrade(&ctx);
            Box::new(move |event, in_bounds| {
                let Some(ctx) = ctx.upgrade() else {
                    return false;
                };
                match event {
                    UIEvent::MouseDown(_, _) => {
                        let was_shown = ctx.is_shown.get();
                        ctx.is_shown.set(false);
                        println!("hiding popup");
                        was_shown && in_bounds
                    }
                    UIEvent::MouseUp(_, _) => false,
                    UIEvent::MouseDrag(_) => false,
                }
            })
        });

        ctx
    }

    pub fn show(&self, pos: Vector2) {
        self.is_shown.set(true);
        let mut root = self.root.borrow_mut();
        root.rect.x = pos.x;
        root.rect.y = pos.y;
        root.compute_layout();
        // TODO windows bounds checking
    }

    pub fn draw(&self, d: &mut RaylibDrawHandle) {
        if self.is_shown.get() {
            let mut root = self.root.borrow_mut();
            root.compute_layout();
            root.draw(d);
        }
    }

    pub fn send_event(&self, event: UIEvent, pos: Option<Vector2>) -> bool {
        if self.is_shown.get() {
            self.root.borrow_mut().send_event(event, pos)
        } else {
            false
        }
    }

    pub fn clear_menu(&self) {
        self.items.borrow_mut().children.clear();
    }

    pub fn add_menu_item(&self, label: String, mut on_click: impl FnMut() + 'static) {
        let mut item = UIElement::new(UIContent::Rounded {
            color: style::NAV_WINDOWING_DIM_BG.lerp(Color::WHITE, 0.1f32),
            corner_radius: 20f32,
        })
        .on_event(move |event, in_bounds| match event {
            UIEvent::MouseDown(_vector2, mouse_button) => {
                if in_bounds && mouse_button == MouseButton::MOUSE_BUTTON_LEFT {
                    on_click();
                }
                // Click event gets consumed by root node
                false
            }
            _ => false,
        });

        item.add_child(
            UIElement::new_text(self.header_font.clone(), label, 20f32, false, Color::WHITE)
                .build(),
        );

        self.items
            .borrow_mut()
            .children
            .push(Rc::new(RefCell::new(item.build())));
    }
}
