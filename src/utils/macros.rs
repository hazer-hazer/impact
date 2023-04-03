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
