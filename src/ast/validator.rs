use convert_case::Case;
use lazy_static::lazy_static;
use regex::{escape, Regex};

use super::{
    item::{ExternItem, Field, Item, ItemKind, TyParam, Variant},
    visitor::{walk_each_pr, walk_pr, AstVisitor},
    ErrorNode, NodeId, Path, WithNodeId, AST, PR,
};
use crate::{
    message::{
        human_lang::items_are,
        message::{
            impl_message_holder, MessageBuilder, MessageStorage, NameKind, Solution,
            SolutionKind,
        },
    },
    parser::lexer::CUSTOM_OP_CHARS,
    session::{impl_session_holder, stage_result, Session, Stage, StageResult},
    span::{
        sym::{Ident, Symbol},
        WithSpan,
    },
};

lazy_static! {
    static ref TYPENAME_REGEX: Regex = Regex::new(r"^[A-Z][A-z\d]*$").unwrap();
    // Saved regex for custom op chars: !\$\+\-\*/%\?\^\|\&\~=
    static ref VARNAME_REGEX: Regex = Regex::new(&format!(
        r"^([a-z][A-z\d]*|[{}]+)|\(\)$",
        CUSTOM_OP_CHARS.iter().map(|ch| escape(&ch.to_string())).collect::<String>()
    ))
    .unwrap();
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

impl_message_holder!(AstValidator<'ast>);
impl_session_holder!(AstValidator<'ast>);

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
            NameKind::Struct | NameKind::Adt | NameKind::Variant | NameKind::Type => {
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
            ItemKind::Type(name, generics, ty) => {
                self.validate_typename(name.as_ref().unwrap(), NameKind::Type);
                self.visit_ty_item(name, generics, ty, item.id());
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
            ItemKind::Adt(name, generics, variants) => {
                self.validate_name(name.as_ref().unwrap(), NameKind::Type);
                self.visit_adt_item(name, generics, variants, item.id());
            },
            ItemKind::Struct(name, generics, fields, ctor_id) => {
                self.validate_name(name.as_ref().unwrap(), NameKind::Adt);
                self.visit_struct_item(name, generics, fields, *ctor_id, item.id());
            },
            ItemKind::Extern(items) => self.visit_extern_block(items),
        }
    }

    fn visit_ty_param(&mut self, type_param: &'ast TyParam) {
        self.validate_name(type_param.name.as_ref().unwrap(), NameKind::TypeVar);
    }

    fn visit_extern_item(&mut self, item: &'ast ExternItem) {
        self.validate_name(item.name.as_ref().unwrap(), NameKind::Var);
        walk_pr!(self, &item.ty, visit_ty);
    }

    fn visit_adt_item(
        &mut self,
        name: &'ast PR<Ident>,
        generics: &'ast super::item::GenericParams,
        variants: &'ast [PR<Variant>],
        _: NodeId,
    ) {
        self.validate_name(name.as_ref().unwrap(), NameKind::Adt);

        self.visit_generic_params(generics);

        assert!(!variants.is_empty());

        walk_each_pr!(self, variants, visit_variant);
    }

    fn visit_variant(&mut self, variant: &'ast Variant) {
        self.validate_name(variant.name.as_ref().unwrap(), NameKind::Variant);
        walk_each_pr!(self, &variant.fields, visit_field);

        // Two groups of fields: (unnamed, named)
        let groups = variant.fields.iter().enumerate().fold(
            (vec![], vec![]),
            |mut groups, (index, field)| {
                let field = field.as_ref().unwrap();
                if let Some(name) = &field.name {
                    groups.1.push((name.as_ref().unwrap(), field.span()));
                } else {
                    groups.0.push((index, field.span()));
                }
                groups
            },
        );

        // FIXME: Replace with fields as multi-spans
        if !groups.0.is_empty() && !groups.1.is_empty() {
            MessageBuilder::error()
                .span(variant.span())
                .text("Fields in variant must either all be named or unnamed".to_string())
                .label(
                    variant.span(),
                    format!(
                        "{} unnamed",
                        items_are("field", groups.0.iter().map(|(index, _)| index))
                    ),
                )
                .label(
                    variant.span(),
                    format!(
                        "but {} named",
                        items_are("field", groups.1.iter().map(|(name, _)| name))
                    ),
                )
                .emit(self);
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
    fn run(mut self) -> StageResult<()> {
        self.visit_ast(self.ast);
        stage_result(self.sess, (), self.msg)
    }
}
