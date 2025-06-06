use std::sync::Arc;

use raylib::prelude::*;
use ui::{BorderSizes, UIElement};
mod ui;

fn create_ui(font: Arc<WeakFont>) -> UIElement {
    let mut root = UIElement::new(ui::UIContent::None)
        .padding(BorderSizes::uniform(0f32))
        .spacing(5f32);

    let mut content = UIElement::new(ui::UIContent::Rect(Color::BLUE))
        .sizing(ui::Sizing::Grow(None))
        .uniform_padding(4f32)
        .spacing(10f32);

    content.add_child(
        UIElement::new(ui::UIContent::Rounded {
            color: Color::RED,
            corner_radius: 10f32,
        })
        .sizing(ui::Sizing::Fixed(50f32))
        .build(),
    );

    content.add_child(
        UIElement::new(ui::UIContent::Rounded {
            color: Color::GREEN,
            corner_radius: 10f32,
        })
        .width(ui::Sizing::Grow(Some(300f32)))
        .height(ui::Sizing::Fixed(100f32))
        .build(),
    );

    let mut bar = UIElement::new(ui::UIContent::Rounded {
        color: Color::YELLOWGREEN,
        corner_radius: 10f32,
    })
    .width(ui::Sizing::Grow(None))
    .height(ui::Sizing::Fit(Some(10f32)))
    .spacing(4f32)
    .uniform_padding(10f32)
    .direction(ui::LayoutDir::Ascending)
    .align(ui::LayoutAlign::Center);

    let mut text_container = UIElement::new(ui::UIContent::Rect(Color::MAGENTA))
        .width(ui::Sizing::Prefer {
            target: 600f32,
            min: Some(150f32),
        })
        .uniform_padding(4f32);
    text_container.add_child(UIElement::new_text(
                font.clone(),
                "Obviously, virtual reality is where I've placed my bet about the future and where the excitement is going. At this point, I could say it's almost a lock. It's going to be magical - it is magical - and great things are coming from that. Along the way, I was focused on the first-person shooters. I said we should go do something on mobile. -John Carmack".into(),
                20f32,
                true,
                Color::BLACK,
            ).build());
    bar.add_child(text_container.build());

    bar.add_child(
        UIElement::new_text(font.clone(), "element 2".into(), 20f32, true, Color::BLACK).build(),
    );

    bar.add_child(
        UIElement::new_text(font.clone(), "element 3".into(), 20f32, true, Color::BLACK).build(),
    );
    content.add_child(bar.build());

    content.add_child(
        UIElement::new(ui::UIContent::Rounded {
            color: Color::RED,
            corner_radius: 10f32,
        })
        .sizing(ui::Sizing::Fixed(50f32))
        .build(),
    );

    root.add_child(content.build());
    root.build()
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
