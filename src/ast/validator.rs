use crate::{
    message::message::{Solution, SolutionKind},
    session::Stage,
    span::span::Ident,
};

use super::visitor::AstVisitor;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref PASCAL_CASE_REGEX: Regex = Regex::new(r"^[A-Z][A-z\d]*$").unwrap();
    static ref CAMEL_CASE_REGEX: Regex = Regex::new(r"^[a-z][A-z\d]*$").unwrap();
    static ref SNAKE_CASE_REGEX: Regex = Regex::new(r"^[a-z]+(?:_[a-z\d]+)*$").unwrap();
    static ref CONSTANT_CASE_REGEX: Regex = Regex::new(r"^[A-Z]+(?:_[A-Z\d]+)*$").unwrap();
    static ref WORDS_REGEX: Regex = Regex::new("re").unwrap();
}

trait NameFormat {
    fn transform_first<It: Iterator<Item = char>>(&self, transform: fn(char) -> It) -> String;
    fn lowercase(&self) -> String;
    fn capitalize(&self) -> String;

    fn to_pascal_case(&self) -> String;
    fn to_camel_case(&self) -> String;
    fn to_snake_case(&self) -> String;
}

impl<'a> NameFormat for &'a str {
    fn transform_first<It: Iterator<Item = char>>(&self, transform: fn(char) -> It) -> String {
        let mut chars = self.chars();
        match chars.next() {
            None => String::new(),
            Some(ch) => transform(ch).collect::<String>() + chars.as_str(),
        }
    }

    fn capitalize(&self) -> String {
        self.transform_first(|ch| ch.to_uppercase())
    }

    fn lowercase(&self) -> String {
        self.transform_first(|ch| ch.to_lowercase())
    }

    fn to_pascal_case(&self) -> String {
        self.split(|ch: char| !ch.is_alphanumeric())
            .collect::<Vec<_>>()
            .iter()
            .map(NameFormat::capitalize)
            .collect::<String>()
    }

    fn to_camel_case(&self) -> String {
        let words = self
            .split(|ch: char| !ch.is_alphanumeric())
            .collect::<Vec<_>>();

        format!(
            "{}{}",
            words[0].capitalize(),
            words[1..]
                .iter()
                .map(NameFormat::capitalize)
                .collect::<String>()
        )
    }

    fn to_snake_case(&self) -> String {
        

        words.join("_")
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

        if PASCAL_CASE_REGEX.is_match(str) {
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
                    .map(|word| word.capitalize())
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
