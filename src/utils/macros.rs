macro_rules! match_expected {
    ($expr: expr, $($patterns: pat => $arms: expr)+ $(,$($panic_args: tt)*)?) => {
        match $expr {
            $($patterns => $arms,)+
            _ => panic!($($panic_args)*)
        }
    };
}

pub(crate) use match_expected;
