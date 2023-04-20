macro_rules! match_expected {
    ($expr: expr, $($patterns: pat => $arms: expr),+ $(;$($panic_args: tt)+)?) => {
        match $expr {
            $($patterns => $arms,)+
            _ => panic!($($($panic_args)+)?)
        }
    };
    ($expr: expr, $($patterns: pat => $arms: expr),+ $(,)?) => {
        match $expr {
            $($patterns => $arms,)+
            _ => panic!("Expected expression to match {} pattern", stringify!($($patterns),+))
        }
    };
    (@pp $expr: expr, $($patterns: pat => $arms: expr),+) => {
        match_expected!($expr, $($patterns => $arms),+; "Expected {} to match one of {{{}}}", $expr, stringify!($($patterns),+))
    };
}

pub(crate) use match_expected;

macro_rules! concat_string {
    () => { String::with_capacity(0) };
    ($($s:expr),+) => {{
        use std::ops::AddAssign;
        let mut len = 0;
        $(len.add_assign(AsRef::<str>::as_ref(&$s).len());)+
        let mut buf = String::with_capacity(len);
        $(buf.push_str($s.as_ref());)+
        buf
    }};
}

pub(crate) use concat_string;
