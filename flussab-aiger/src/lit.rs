use std::hash::Hash;

macro_rules! prim_int_impl {
    ($t:ty) => {
        impl Lit for $t {
            const MAX_CODE: usize = <$t>::MAX as usize;

            fn from_code(code: usize) -> Self {
                code as $t
            }

            fn code(self) -> usize {
                self as usize
            }
        }
    };
}

prim_int_impl!(u8);
prim_int_impl!(u16);
prim_int_impl!(u32);
prim_int_impl!(u64);
prim_int_impl!(usize);

pub trait Lit: Copy + Eq + Hash + std::fmt::Debug {
    const MAX_CODE: usize;

    fn from_code(code: usize) -> Self;
    fn code(self) -> usize;
}
