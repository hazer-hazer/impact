#[derive(Default)]
pub struct Writer {
    data: String,
}

impl Writer {
    pub fn write(&mut self, add: String) {
        self.data.push_str(&add)
    }

    pub fn writeln(&mut self, add: String) {
        self.data.push_str(&add);
        self.data.push('\n')
    }

    pub fn data(&self) -> &String {
        &self.data
    }
}

macro_rules! out {
    ($writer: expr, $($args: tt)*) => {
        $writer.write(std::format!($($args)*))
    };
}

macro_rules! outln {
    ($writer: expr, $($args: tt)*) => {
        $writer.writeln(std::format!($($args)*))
    };
}

pub(crate) use outln;
