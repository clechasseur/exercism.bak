#[macro_export]
macro_rules! hashmap {
    () => {
        ::std::collections::HashMap::new()
    };
    ( $( $k:expr => $v:expr ),+ $(,)? ) => {
        {
            let mut h = ::std::collections::HashMap::new();
            $(
                h.insert($k, $v);
            )+
            h
        }
    };
}
