#[macro_export]
macro_rules! guid {
    (
        $a:expr,
        $b:expr,
        $c:expr,
        $d:expr
    ) => {
        winapi::shared::guiddef::GUID {
            Data1: $a,
            Data2: $b,
            Data3: $c,
            Data4: $d,
        }
    };

    (
        $a:expr,
        $b:expr,
        $c:expr,
        $d0:expr,
        $d1:expr,
        $d2:expr,
        $d3:expr,
        $d4:expr,
        $d5:expr,
        $d6:expr,
        $d7:expr
    ) => {
        winapi::shared::guiddef::GUID {
            Data1: $a,
            Data2: $b,
            Data3: $c,
            Data4: [$d0, $d1, $d2, $d3, $d4, $d5, $d6, $d7],
        }
    };
}
