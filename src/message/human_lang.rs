use std::fmt::Display;

pub enum Verb {
    Be,
}

impl Verb {
    pub fn form_by_number(&self, n: usize) -> &str {
        if n > 1 {
            "are"
        } else {
            "is"
        }
    }
}

pub fn number_form(word: &str, n: usize) -> String {
    if n > 1 {
        format!("{word}s")
    } else {
        word.to_string()
    }
}

pub fn list_items<T>(items: impl Iterator<Item = T>) -> String
where
    T: Display,
{
    let items = items.map(|i| i.to_string()).collect::<Vec<_>>();

    if items.is_empty() {
        return "none".to_string();
    }

    let last = if items.len() > 1 { items.last() } else { None };

    format!(
        "{}{}",
        items[0..items.len()].join(", "),
        if let Some(last) = last {
            format!(" and {last}")
        } else {
            "".to_string()
        }
    )
}

pub fn items_are<T>(word: &str, items: impl Iterator<Item = T>) -> String
where
    T: Display,
{
    let items = items.collect::<Vec<_>>();

    if items.is_empty() {
        format!("none of {word}s are")
    } else {
        format!(
            "{} {} {}",
            number_form(word, items.len()),
            list_items(items.iter()),
            Verb::Be.form_by_number(items.len())
        )
    }
}
