// use crate::{
//     ast::{
//         ty::{Ty, TyKind},
//         visitor::AstVisitor,
//         ErrorNode, NodeId, Path, WithNodeId, AST, N, PR,
//     },
//     message::message::{Message, MessageHolder, MessageStorage},
//     session::{Session, Stage, StageOutput},
//     span::span::Ident,
// };

// use super::res::{NamePath, ResKind};

// pub struct Expander<'ast> {
//     sess: Session,
//     ast: &AST,
//     msg: MessageStorage,
// }

// impl<'ast> Expander<'ast> {
//     pub fn new(sess: Session, ast: &AST, msg: MessageStorage) -> Self {
//         Self { sess, ast, msg }
//     }

//     pub fn expand_ty_alias(&mut self, ty: Ty, node_id: NodeId) -> Ty {
//         match ty.kind() {
//             TyKind::Path(path) => {
//                 let res = self.sess.res.get(NamePath::new(path.id())).unwrap();
//                 match res.kind() {
//                     ResKind::Local(_) => unreachable!(),
//                     ResKind::Error => unreachable!(),
//                     ResKind::Def(def_id) => {},
//                 }
//                 todo!()
//             },
//             TyKind::Func(_, _) => todo!(),
//             TyKind::Paren(_) => todo!(),
//             TyKind::Unit => ty,
//         }
//     }
// }

// impl<'a> MessageHolder for Expander<'a> {
//     fn save(&mut self, msg: Message) {
//         self.msg.add_message(msg)
//     }
// }

// impl<'a> AstVisitor for Expander<'a> {
//     fn visit_err(&mut self, _: &ErrorNode) {
//         panic!("Error node on Expander stage");
//     }

//     fn visit_type_item(&mut self, _: &PR<Ident>, ty: &PR<N<Ty>>, node_id: NodeId) {
//         todo!()
//     }
// }

// impl<'ast> Stage<()> for Expander<'ast> {
//     fn run(mut self) -> StageOutput<()> {
//         self.sess.def_table.add_root_module();
//         self.visit_ast(self.ast);
//         StageOutput::new(self.sess, (), self.msg)
//     }
// }