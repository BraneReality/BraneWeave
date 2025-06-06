use std::{cell::RefCell, rc::Rc, sync::Arc};

use raylib::prelude::*;
use ui::{BorderSizes, TextElementContent, UIElement};
mod ui;

fn create_ui(font: Arc<WeakFont>) -> UIElement {
    let mut root = UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::None,
        layout: ui::Layout {
            width: ui::Sizing::Fit(None),
            height: ui::Sizing::Fit(None),
            padding: BorderSizes::uniform(4f32),
            spacing: 5f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::Center,
        },
        children: vec![],
        on_event: None,
    };

    let mut content = UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rect(Color::BLUE),
        layout: ui::Layout {
            width: ui::Sizing::Grow(None),
            height: ui::Sizing::Grow(None),
            padding: BorderSizes::uniform(4f32),
            spacing: 10f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::Start,
        },
        children: vec![],
        on_event: None,
    };

    content.children.push(Rc::new(RefCell::new(UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rounded {
            color: Color::RED,
            corner_radius: 10f32,
        },
        layout: ui::Layout {
            width: ui::Sizing::Fixed(50f32),
            height: ui::Sizing::Fixed(50f32),
            padding: BorderSizes::uniform(4f32),
            spacing: 4f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::Center,
        },
        children: vec![],
        on_event: None,
    })));

    content.children.push(Rc::new(RefCell::new(UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rounded {
            color: Color::GREEN,
            corner_radius: 10f32,
        },
        layout: ui::Layout {
            width: ui::Sizing::Grow(Some(300f32)),
            height: ui::Sizing::Fixed(100f32),
            padding: BorderSizes::uniform(4f32),
            spacing: 4f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::Center,
        },
        children: vec![],
        on_event: None,
    })));

    let mut bar = UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rounded {
            color: Color::YELLOWGREEN,
            corner_radius: 10f32,
        },
        layout: ui::Layout {
            width: ui::Sizing::Grow(None),
            height: ui::Sizing::Fit(Some(10f32)),
            padding: BorderSizes::uniform(10f32),
            spacing: 4f32,
            direction: ui::LayoutDir::Ascending,
            align: ui::LayoutAlign::Center,
        },
        children: vec![],
        on_event: None,
    };

    let carmack_quote = TextElementContent {
                color: Color::BLACK,
                text: "Obviously, virtual reality is where I've placed my bet about the future and where the excitement is going. At this point, I could say it's almost a lock. It's going to be magical - it is magical - and great things are coming from that. Along the way, I was focused on the first-person shooters. I said we should go do something on mobile. -John Carmack".into(),
                font_size: 20f32,
                wrap: true,
                font: font.clone(),
            };

    bar.children.push(Rc::new(RefCell::new(UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rect(Color::MAGENTA),
        layout: ui::Layout {
            width: ui::Sizing::Prefer {
                target: 600f32,
                min: Some(150f32),
            },
            height: ui::Sizing::Fit(None),
            padding: BorderSizes::uniform(4f32),
            spacing: 0f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::End,
        },
        children: vec![Rc::new(RefCell::new(UIElement {
            rect: Default::default(),
            wrapped_text: None,
            layout: ui::Layout::text_element(&carmack_quote),
            content: ui::UIContent::Text(carmack_quote),
            children: vec![],
            on_event: None,
        }))],
        on_event: None,
    })));

    content.children.push(Rc::new(RefCell::new(bar)));

    content.children.push(Rc::new(RefCell::new(UIElement {
        rect: Default::default(),
        wrapped_text: None,
        content: ui::UIContent::Rounded {
            color: Color::RED,
            corner_radius: 10f32,
        },
        layout: ui::Layout {
            width: ui::Sizing::Fixed(50f32),
            height: ui::Sizing::Fixed(50f32),
            padding: BorderSizes::uniform(4f32),
            spacing: 4f32,
            direction: ui::LayoutDir::LeftRight,
            align: ui::LayoutAlign::Center,
        },
        children: vec![],
        on_event: None,
    })));

    root.children.push(Rc::new(RefCell::new(content)));
    root
}

fn main() {
    let (mut rl, thread) = raylib::init()
        .size(1900, 1000)
        .resizable()
        .title("Brane Weave")
        .build();
    // TODO find host refresh rate
    rl.set_target_fps(60);

    //let mut proj = ProjectCtx::default();

    /*let mut ctx = UIContext::new();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();*/

    let standard_font = rl
        .load_font_ex(
            &thread,
            "fonts/JetBrains/JetBrainsMonoNerdFont-Bold.ttf",
            20,
            None,
        )
        .expect("Couldn't find font");

    let font_handle = Arc::new(standard_font.make_weak());
    let mut root = create_ui(font_handle.clone());

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::BLACK);
        //ctx.draw(&mut d, &mut proj);
        root.layout.width = ui::Sizing::Fixed(d.get_render_width() as f32);
        root.layout.height = ui::Sizing::Fixed(d.get_render_height() as f32);
        root.compute_layout();
        root.draw(&mut d);
        d.draw_fps(12, 12);
    }
}
