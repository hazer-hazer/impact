use super::interface::InterruptResult;

pub trait Writer<R> {
    fn write(&mut self, data: String) -> R;
}

macro_rules! out {
    ($writer: expr, $($args: tt)*) => {
        $writer.write(std::format!($($args)*))
    };
}

macro_rules! outl {
    ($writer: expr, $($args: tt)*) => {
        $writer.write(std::format!($($args)*, "\n"))
    };
}

pub(crate) use out;
pub(crate) use outl;

pub struct ConsoleWriter;

impl Writer<InterruptResult> for ConsoleWriter {
    fn write(&mut self, data: String) -> InterruptResult {
        print!("{}", data);
        Ok(())
    }
}

#[derive(Default)]
pub struct StorageWriter {
    data: String,
}

impl StorageWriter {
    pub fn data(&self) -> &str {
        self.data.as_ref()
    }
}

impl Writer<InterruptResult> for StorageWriter {
    fn write(&mut self, data: String) -> InterruptResult {
        self.data.push_str(&data);
        Ok(())
    }
}
