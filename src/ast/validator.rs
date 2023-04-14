use convert_case::Case;
use lazy_static::lazy_static;
use regex::Regex;

use super::{
    item::{ExternItem, Field, Item, ItemKind, Variant},
    visitor::{walk_each_pr, walk_pr, AstVisitor},
    ErrorNode, NodeId, Path, WithNodeId, AST, PR,
};
use crate::{
    message::message::{
        Message, MessageBuilder, MessageHolder, MessageStorage, NameKind, Solution, SolutionKind,
    },
    session::{Session, Stage, StageOutput},
    span::sym::{Ident, Symbol},
};

lazy_static! {
    static ref TYPENAME_REGEX: Regex = Regex::new(r"^[A-Z][A-z\d]*$").unwrap();
    static ref VARNAME_REGEX: Regex =
        Regex::new(r"^([a-z][A-z\d]*|[!\$\+\-\*/%\?\^\|\&\~=]+)|\(\)$").unwrap();
    static ref SNAKE_CASE_REGEX: Regex = Regex::new(r"^[a-z]+(?:_[a-z\d]+)*$").unwrap();
    static ref KEBAB_CASE_REGEX: Regex = Regex::new(r"^[a-z]+(?:-[a-z\d]+)*$").unwrap();
    static ref SCREAMING_SNAKE_CASE_REGEX: Regex = Regex::new(r"^[A-Z]+(?:_[A-Z\d]+)*$").unwrap();
}

trait StrExtension {
    fn check_first(&self, pred: fn(char) -> bool) -> bool;
    fn is_uppercase(&self) -> bool;
    fn is_lowercase(&self) -> bool;
}

impl<'a> StrExtension for &'a str {
    fn check_first(&self, pred: fn(char) -> bool) -> bool {
        match self.chars().nth(0) {
            Some(ch) => pred(ch),
            None => false,
        }
    }

    fn is_uppercase(&self) -> bool {
        self.check_first(|ch| ch.is_uppercase())
    }

    fn is_lowercase(&self) -> bool {
        self.check_first(|ch| ch.is_lowercase())
    }
}

pub struct AstValidator<'ast> {
    ast: &'ast AST,
    sess: Session,
    msg: MessageStorage,
}

impl<'ast> MessageHolder for AstValidator<'ast> {
    fn save(&mut self, msg: Message) {
        self.msg.add_message(msg)
    }
}

impl<'ast> AstValidator<'ast> {
    pub fn new(sess: Session, ast: &'ast AST) -> Self {
        Self {
            ast,
            sess,
            msg: MessageStorage::default(),
        }
    }

    fn to_case(str: &str, _case: Case) -> Option<String> {
        if Symbol::intern(str).is_non_op() {
            Some(str.to_string())
        } else {
            None
        }
    }

    fn validate_name(&mut self, name: &Ident, kind: NameKind) {
        let solution = match kind {
            NameKind::Field | NameKind::Var | NameKind::Func | NameKind::TypeVar => {
                self.validate_varname(name, kind)
            },
            NameKind::Const => self.validate_const_name(name, kind),
            NameKind::File => self.validate_file_name(name, kind),
            NameKind::DataTy | NameKind::Variant | NameKind::Type => {
                self.validate_typename(name, kind)
            },
            NameKind::Mod => self.validate_mod_name(name, kind),
        };

        if let Some(solution) = solution {
            MessageBuilder::error()
                .span(name.span())
                .text(format!("Invalid format of {} name '{}'", kind, name.sym()))
                .solution(solution)
                .emit_single_label(self);
        }
    }

    /// Typename is PascalCase name, i.e.:
    /// - All words start with an uppercase letter, even words after digits
    /// - It does not contain underscore
    fn validate_typename(&self, name: &Ident, kind: NameKind) -> Option<Solution> {
        let name = *name;
        let sym = name.sym();
        let str = sym.as_str();

        if !TYPENAME_REGEX.is_match(str) {
            let to = Self::to_case(str, Case::Pascal);
            return Some(Solution::new(SolutionKind::Rename { kind, name, to }));
        }

        None
    }

    /// Varname is camelCase name, i.e.:
    /// - First word starts with a lowercase letter
    /// - All subsequent words start with an uppercase letter
    /// - It does not contain underscore
    fn validate_varname(&self, name: &Ident, kind: NameKind) -> Option<Solution> {
        let name = *name;
        let sym = name.sym();
        let str = sym.as_str();

        if !VARNAME_REGEX.is_match(str) {
            let to = Self::to_case(str, Case::Camel);
            return Some(Solution::new(SolutionKind::Rename { kind, name, to }));
        }

        None
    }

    /// Module name is snake_case (or one word) name i.e.:
    /// - All letters are lowercase
    /// - Words are delimited with underscores
    fn validate_mod_name(&self, name: &Ident, kind: NameKind) -> Option<Solution> {
        let name = *name;
        let sym = name.sym();
        let str = sym.as_str();

        if !SNAKE_CASE_REGEX.is_match(str) {
            let to = Self::to_case(str, Case::Snake);
            return Some(Solution::new(SolutionKind::Rename { kind, name, to }));
        }

        None
    }

    /// Constant name is SCREAMING_SNAKE_CASE (or one uppercase word) name i.e.:
    /// - All letters are uppercase
    /// - Words are delimited with underscores
    fn validate_const_name(&self, name: &Ident, kind: NameKind) -> Option<Solution> {
        let name = *name;
        let sym = name.sym();
        let str = sym.as_str();

        if !SCREAMING_SNAKE_CASE_REGEX.is_match(str) {
            let to = Self::to_case(str, Case::ScreamingSnake);
            return Some(Solution::new(SolutionKind::Rename { kind, name, to }));
        }

        None
    }

    /// File name is either snake_case or kebab-case
    fn validate_file_name(&self, name: &Ident, kind: NameKind) -> Option<Solution> {
        let name = *name;
        let sym = name.sym();
        let str = sym.as_str();

        if !SNAKE_CASE_REGEX.is_match(str) && !KEBAB_CASE_REGEX.is_match(str) {
            let to = Self::to_case(str, Case::Snake);
            return Some(Solution::new(SolutionKind::Rename { kind, name, to }));
        }

        None
    }
}

impl<'ast> AstVisitor<'ast> for AstValidator<'ast> {
    fn visit_err(&mut self, _: &'ast ErrorNode) {
        panic!("Error node in AstValidator")
    }

    fn visit_item(&mut self, item: &'ast Item) {
        match item.kind() {
            ItemKind::Type(name, ty) => {
                self.validate_typename(name.as_ref().unwrap(), NameKind::Type);
                self.visit_type_item(name, ty, item.id());
            },
            ItemKind::Mod(name, items) => {
                self.validate_name(name.as_ref().unwrap(), NameKind::Mod);
                self.visit_mod_item(name, items, item.id());
            },
            ItemKind::Decl(name, params, body) => {
                let name_kind = if params.is_empty() {
                    NameKind::Var
                } else {
                    NameKind::Func
                };
                self.validate_name(name.as_ref().unwrap(), name_kind);
                self.visit_decl_item(name, params, body, item.id());
            },
            ItemKind::Adt(name, variants) => {
                self.validate_name(name.as_ref().unwrap(), NameKind::Type);
                self.visit_data_item(name, variants, item.id());
            },
            ItemKind::Extern(items) => self.visit_extern_block(items),
        }
    }

    fn visit_extern_item(&mut self, item: &'ast ExternItem) {
        self.validate_name(item.name.as_ref().unwrap(), NameKind::Var);
        walk_pr!(self, &item.ty, visit_ty);
    }

    fn visit_data_item(&mut self, name: &'ast PR<Ident>, variants: &'ast [PR<Variant>], _: NodeId) {
        self.validate_name(name.as_ref().unwrap(), NameKind::DataTy);
        walk_each_pr!(self, variants, visit_variant);
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        self.validate_name(variant.name.as_ref().unwrap(), NameKind::Variant);
        walk_each_pr!(self, &variant.fields, visit_field);

        assert!(!variant.fields.is_empty());

        let mut fields = variant.fields.iter().map(|field| field.as_ref().unwrap());

        let with_name = fields.next().unwrap().name.is_some();
        if !fields.all(|field| field.name.is_some() == with_name) {
            MessageBuilder::error()
                .text("All fields in variant must either all be named or unnamed".to_string())
                .emit_single_label(self);
        }
    }

    fn visit_field(&mut self, field: &'ast Field) {
        field
            .name
            .as_ref()
            .map(|name| self.validate_name(name.as_ref().unwrap(), NameKind::Field));
        walk_pr!(self, &field.ty, visit_ty);
    }

    fn visit_path(&mut self, path: &'ast Path) {
        assert!(!path.segments().is_empty());

        path.segments().iter().enumerate().for_each(|(index, seg)| {
            let last = index == path.segments().len() - 1;
            let name = seg.name.as_ref().unwrap();

            // FIXME: Replace with multi-span when added
            //  Like so:
            //  (+)::blah::blah::(-)::something
            //  ^^^--------------^^^------ Error Message
            if !last && name.sym().is_op() {
                MessageBuilder::error()
                    .span(name.span())
                    .text(format!("Unexpected operator name in path prefix"))
                    .emit_single_label(self);
            }
        });
    }
}

impl<'ast> Stage<()> for AstValidator<'ast> {
    fn run(mut self) -> StageOutput<()> {
        self.visit_ast(self.ast);
        StageOutput::new(self.sess, (), self.msg)
    }
}
