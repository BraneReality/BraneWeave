use std::{cell::RefCell, ops::DerefMut, sync::Arc};

use branec::hir::{self, Block, BlockSig, Item, Project};
use raylib::prelude::*;
use ui::UIContext;
//mod bindings;
mod ui;

fn main() {
    let (mut rl, thread) = raylib::init()
        .size(1900, 1000)
        .resizable()
        .title("Brane Weave")
        .build();
    // TODO find host refresh rate
    rl.set_target_fps(60);

    let proj = Arc::new(RefCell::new(Project::new()));
    let graph = {
        let mut proj = proj.borrow_mut();
        let graph_id = proj.new_graph();
        let graph = proj.graph_mut(graph_id).unwrap();
        let sig = hir::BlockSig {
            inputs: vec![(), ()],
            outputs: vec![()],
        };

        let body = graph.create_block();

        let main_fn = graph.add_fn("main".into(), sig, body);

        let block = graph.block_mut(body).unwrap();

        block
            .outputs
            .push(hir::LocalValue::NodeOutput { node: 2, index: 0 });

        block.nodes.insert(
            0,
            hir::Node {
                inputs: vec![hir::LocalValue::BlockInput { index: 0 }],
                expr: hir::DefId {
                    graph: 1,
                    item: main_fn,
                },
            },
        );

        block.nodes.insert(
            1,
            hir::Node {
                inputs: vec![hir::LocalValue::BlockInput { index: 1 }],
                expr: hir::DefId {
                    graph: 1,
                    item: main_fn,
                },
            },
        );

        block.nodes.insert(
            2,
            hir::Node {
                inputs: vec![
                    hir::LocalValue::NodeOutput { node: 0, index: 0 },
                    hir::LocalValue::NodeOutput { node: 1, index: 0 },
                ],
                expr: hir::DefId {
                    graph: 1,
                    item: main_fn,
                },
            },
        );

        graph_id
    };

    let mut ctx = UIContext::new(&mut rl, &thread, graph, proj);

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::BLACK);
        ctx.draw(&mut d);
        d.draw_fps(12, 12);
    }
}
