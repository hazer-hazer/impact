mod parser_stage;

trait PP {
    pub fn pp(&self) -> &str;
}
