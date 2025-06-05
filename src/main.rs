use std::sync::Arc;

use branec::ProjectCtx;
use raylib::prelude::*;
use ui::{BorderSizes, UIElement};
mod ui;

fn main() {
    dioxus_devtools::connect_subsecond();
    subsecond::call(|| {
        let (mut rl, thread) = raylib::init()
            .size(1900, 1000)
            .resizable()
            .title("Brane Weave")
            .build();
        // TODO find host refresh rate
        rl.set_target_fps(60);

        subsecond::call(|| {
            //let mut proj = ProjectCtx::default();

            /*let mut ctx = UIContext::new();
            ctx.node_graph.new_node();
            ctx.node_graph.new_node();
            ctx.node_graph.new_node();
            ctx.node_graph.new_node();*/

            subsecond::call(|| {
                let standard_font = rl
                    .load_font_ex(
                        &thread,
                        "fonts/JetBrains/JetBrainsMonoNerdFont-Bold.ttf",
                        20,
                        None,
                    )
                    .expect("Couldn't find font");
                let mut root = UIElement {
                    rect: Default::default(),
                    content: ui::UIContent::None,
                    layout: ui::Layout {
                        width: ui::Sizing::Fixed(rl.get_render_width() as f32),
                        height: ui::Sizing::Fixed(rl.get_render_width() as f32),
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
                    content: ui::UIContent::Rect(Color::BLUE),
                    layout: ui::Layout {
                        width: ui::Sizing::Grow(None),
                        height: ui::Sizing::Grow(None),
                        padding: BorderSizes::uniform(4f32),
                        spacing: 10f32,
                        direction: ui::LayoutDir::LeftRight,
                        align: ui::LayoutAlign::Center,
                    },
                    children: vec![],
                    on_event: None,
                };

                content.children.push(UIElement {
                    rect: Default::default(),
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
                });

                content.children.push(UIElement {
                    rect: Default::default(),
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
                });

                let mut bar = UIElement {
                    rect: Default::default(),
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
                        align: ui::LayoutAlign::End,
                    },
                    children: vec![],
                    on_event: None,
                };

                bar.children.push(UIElement {
                    rect: Default::default(),
                    content: ui::UIContent::Rect(Color::MAGENTA),
                    layout: ui::Layout {
                        width: ui::Sizing::Fit(None),
                        height: ui::Sizing::Fit(None),
                        padding: BorderSizes::uniform(4f32),
                        spacing: 0f32,
                        direction: ui::LayoutDir::LeftRight,
                        align: ui::LayoutAlign::End,
                    },
                    children: vec![UIElement {
                        rect: Default::default(),
                        content: ui::UIContent::Text {
                            color: Color::BLACK,
                            text: "test text".into(),
                            align: ui::TextAlign::Center,
                            font_size: 20f32,
                            font: Arc::new(standard_font.make_weak()),
                        },
                        layout: ui::Layout {
                            width: ui::Sizing::Fit(None),
                            height: ui::Sizing::Fit(None),
                            padding: BorderSizes::uniform(4f32),
                            spacing: 4f32,
                            direction: ui::LayoutDir::RightLeft,
                            align: ui::LayoutAlign::Center,
                        },
                        children: vec![],
                        on_event: None,
                    }],
                    on_event: None,
                });

                bar.children.push(UIElement {
                    rect: Default::default(),
                    content: ui::UIContent::Text {
                        color: Color::BLACK,
                        text: "2".into(),
                        align: ui::TextAlign::Center,
                        font_size: 20f32,
                        font: Arc::new(rl.get_font_default()),
                    },
                    layout: ui::Layout {
                        width: ui::Sizing::Fit(None),
                        height: ui::Sizing::Fit(None),
                        padding: BorderSizes::uniform(4f32),
                        spacing: 4f32,
                        direction: ui::LayoutDir::LeftRight,
                        align: ui::LayoutAlign::Center,
                    },
                    children: vec![],
                    on_event: None,
                });

                bar.children.push(UIElement {
                    rect: Default::default(),
                    content: ui::UIContent::Text {
                        color: Color::BLACK,
                        text: "3".into(),
                        align: ui::TextAlign::Center,
                        font_size: 20f32,
                        font: Arc::new(rl.get_font_default()),
                    },
                    layout: ui::Layout {
                        width: ui::Sizing::Fit(None),
                        height: ui::Sizing::Fit(None),
                        padding: BorderSizes::uniform(4f32),
                        spacing: 4f32,
                        direction: ui::LayoutDir::LeftRight,
                        align: ui::LayoutAlign::Center,
                    },
                    children: vec![],
                    on_event: None,
                });

                content.children.push(bar);

                content.children.push(UIElement {
                    rect: Default::default(),
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
                });

                root.children.push(content);

                while !rl.window_should_close() {
                    subsecond::call(|| {
                        let mut d = rl.begin_drawing(&thread);
                        d.clear_background(Color::BLACK);
                        //ctx.draw(&mut d, &mut proj);
                        root.layout.width = ui::Sizing::Fixed(d.get_render_width() as f32);
                        root.layout.height = ui::Sizing::Fixed(d.get_render_height() as f32);
                        root.compute_layout();
                        root.draw(&mut d);
                        d.draw_fps(12, 12);
                    })
                }
            })
        });
    });
}
