use raylib::prelude::*;
use ui::UIContext;
mod ui;

fn main() {
    let (mut rl, thread) = raylib::init()
        .size(1900, 1000)
        .resizable()
        .title("Brane Weave")
        .build();
    let standard_font = rl
        .load_font(&thread, "fonts/JetBrains/JetBrainsMonoNerdFont-Bold.ttf")
        .expect("Couldn't find font");
    // TODO find host refresh rate
    rl.set_target_fps(60);

    let mut ctx = UIContext::new();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();
    ctx.node_graph.new_node();

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::BLACK);
        ctx.draw(&mut d);
        d.draw_fps(12, 12);
    }
}
