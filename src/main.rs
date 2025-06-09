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
    rl.set_window_monitor(1);
    // TODO find host refresh rate
    rl.set_target_fps(60);

    let proj = Arc::new(RefCell::new(Project::new()));

    // Intrinsics
    {
        let mut proj = proj.borrow_mut();
        let graph_id = proj.new_graph("std".into());
        let graph = proj.graph_mut(graph_id).unwrap();

        let add_sig = hir::BlockSig {
            inputs: vec![
                hir::BlockEdge {
                    ident: "a".into(),
                    ty: hir::Ty::F32,
                },
                hir::BlockEdge {
                    ident: "b".into(),
                    ty: hir::Ty::F32,
                },
            ],
            outputs: vec![hir::BlockEdge {
                ident: "value".into(),
                ty: hir::Ty::F32,
            }],
        };
        graph.add_fn("f32::add".into(), add_sig, 0);
    }

    let graph = {
        let mut proj = proj.borrow_mut();
        let graph_id = proj.new_graph("main".into());
        let graph = proj.graph_mut(graph_id).unwrap();
        let sig = hir::BlockSig {
            inputs: vec![
                hir::BlockEdge {
                    ident: "a".into(),
                    ty: hir::Ty::F32,
                },
                hir::BlockEdge {
                    ident: "b".into(),
                    ty: hir::Ty::F32,
                },
            ],
            outputs: vec![hir::BlockEdge {
                ident: "out".into(),
                ty: hir::Ty::F32,
            }],
        };

        let body = graph.create_block();

        let main_fn = graph.add_fn("main".into(), sig, body);

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
