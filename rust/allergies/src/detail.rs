// This recursive macro generates the enum with increasing bit values
// as well as a constant array of all enum values, in bit order.
// The macro being longer than the code it actually generates, it's
// definitely not something to emulate in a real program. But it's fun! ;)
macro_rules! define_allergens {
    (
        $vis:vis enum $typ:ident {
            $hvari:ident,
            $( $tvari:ident ),* $(,)?
        }
        const $cons:ident;
    ) => {
        define_allergens! {
            $vis enum $typ {
                [$hvari = 1],
                $( $tvari, )*
            }
            const $cons;
        }
    };
    (
        $vis:vis enum $typ:ident {
            $( ($vvari:ident = $vval:expr), )*
            [$hvari:ident = $hval:expr],
            $nvari:ident,
            $( $tvari:ident, )*
        }
        const $cons:ident;
    ) => {
        define_allergens! {
            $vis enum $typ {
                $( ($vvari = $vval), )*
                ($hvari = $hval),
                [$nvari = $hval << 1],
                $( $tvari, )*
            }
            const $cons;
        }
    };
    (
        $vis:vis enum $typ:ident {
            $( ($hvari:ident = $hval:expr), )*
            [$tvari:ident = $tval:expr],
        }
        const $cons:ident;
    ) => {
        #[repr(u8)]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        $vis enum $typ {
            $( $hvari = $hval, )+
            $tvari = $tval,
        }

        $vis const $cons: &[$typ] = &[
            $( $typ::$hvari, )+
            $typ::$tvari,
        ];
    };
}
