use super::interface::UnitInterruptResult;

pub trait Writer<R, D> {
    fn write(&mut self, data: String) -> R;
    fn writeln(&mut self, data: String) -> R {
        self.write(data);
        self.write(String::from("\n"))
    }

    fn data(self) -> D;
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

pub(crate) use out;
pub(crate) use outln;

pub struct ConsoleWriter;

impl Writer<UnitInterruptResult, ()> for ConsoleWriter {
    fn write(&mut self, data: String) -> UnitInterruptResult {
        print!("{}", data);
        Ok(())
    }

    fn data(self) {}
}

#[derive(Default)]
pub struct StorageWriter {
    data: String,
}

impl Writer<UnitInterruptResult, String> for StorageWriter {
    fn write(&mut self, data: String) -> UnitInterruptResult {
        self.data.push_str(&data);
        Ok(())
    }

    fn data(self) -> String {
        self.data
    }
}
