use crate::{
    message::message::{Solution, SolutionKind},
    session::Stage,
    span::span::Ident,
};

use super::visitor::AstVisitor;

trait StringSliceExtension {
    fn check_first(&self, pred: fn(char) -> bool) -> bool;
    fn is_uppercase(&self) -> bool;
    fn is_lowercase(&self) -> bool;
    fn is_alphabetic(&self) -> bool;

    fn transform_first<It: Iterator<Item = char>>(&self, transform: fn(char) -> It) -> String;
    fn to_uppercase(&self) -> String;
    fn to_lowercase(&self) -> String;
}

impl StringSliceExtension for &str {
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

    fn is_alphabetic(&self) -> bool {
        self.check_first(|ch| ch.is_alphabetic())
    }

    fn transform_first<It: Iterator<Item = char>>(&self, transform: fn(char) -> It) -> String {
        let mut chars = self.chars();
        match chars.next() {
            None => String::new(),
            Some(ch) => transform(ch).collect::<String>() + chars.as_str(),
        }
    }

    fn to_uppercase(&self) -> String {
        self.transform_first(|ch| ch.to_uppercase())
    }

    fn to_lowercase(&self) -> String {
        self.transform_first(|ch| ch.to_lowercase())
    }
}

struct Words<'a> {
    str: &'a str,
    words: Vec<&'a str>,
}

impl<'a> Words<'a> {
    fn new(str: &'a str) -> Self {
        Self {
            str,
            words: str.split(|ch: char| !ch.is_alphanumeric()).collect(),
        }
    }

    pub fn is_pascal_case(&self) -> bool {
        if self.str.contains("_")
            || self
                .words
                .iter()
                .any(|word| word.is_alphabetic() && !word.is_uppercase())
        {
            return false;
        }

        true
    }

    pub fn is_camel_case(&self) -> bool {
        if self.str.contains("_") {
            return false;
        }

    }
}

pub struct AstValidator {}

impl AstValidator {
    /**
     * Typename is PascalCase name, i.e.:
     * - All words start with an uppercase letter, even words after digits
     * - It does not contain underscore
     */
    fn validate_typename(&self, name: Ident) -> Option<Solution> {
        let sym = name.sym();
        let str = sym.as_str();

        assert!(!str.is_empty());

        let words = str
            .split(|ch: char| !ch.is_alphabetic())
            .collect::<Vec<_>>();

        if str.contains("_") || words.iter().any(|word| !word.is_uppercase()) {
            let to = words
                .iter()
                .map(|word| word.to_uppercase())
                .collect::<String>();

            return Some(Solution::new(SolutionKind::Rename { name, to }));
        }

        None
    }

    /**
     * Varname is camelCase name, i.e.:
     * - First word starts with a lowercase letter
     * - All subsequent words start with an uppercase letter
     * - It does not contain underscore
     */
    fn validate_varname(&self, name: Ident) -> Option<Solution> {
        let sym = name.sym();
        let str = sym.as_str();

        assert!(!str.is_empty());

        let words = str
            .split(|ch: char| !ch.is_alphabetic())
            .collect::<Vec<_>>();

        if str.contains("_") || words[1..].iter().any(|word| word.is_uppercase()) {
            let mut to = words[0].to_lowercase();
            to.push_str(
                &words[1..]
                    .iter()
                    .map(|word| word.to_uppercase())
                    .collect::<String>(),
            );

            return Some(Solution::new(SolutionKind::Rename { name, to }));
        }

        None
    }
}

impl AstVisitor for AstValidator {
    fn visit_err(&mut self, _: &super::ErrorNode) {
        panic!("Error node in AstValidator")
    }
}

impl Stage<()> for AstValidator {
    fn run(self) -> crate::session::StageOutput<()> {
        todo!()
    }
}
