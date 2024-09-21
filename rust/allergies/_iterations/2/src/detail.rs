macro_rules! define_allergens {
    (
        $vis:vis enum $typ:ident {
            $( $vari:ident = $val:expr ),* $(,)?
        }
        const $cons:ident;
    ) => {
        #[repr(u8)]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        $vis enum $typ {
            $( $vari = $val, )*
        }

        const $cons: &[$typ] = &[
            $( $typ::$vari, )*
        ];
    };
}
