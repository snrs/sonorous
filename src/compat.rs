// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! For the compatibility with the 0.6 release.

pub mod core {
    pub mod hashmap {
        pub use core::hashmap::*;
        #[cfg(legacy)] pub use HashMap = core::hashmap::linear::LinearMap;
        #[cfg(legacy)] pub use HashSet = core::hashmap::linear::LinearSet;
    }

    pub mod str {
        pub use core::str::*;
        #[cfg(not(legacy))] pub trait StrLegacyUtil {
            fn to_upper(&self) -> ~str;
            fn to_lower(&self) -> ~str;
        }
        #[cfg(not(legacy))] impl<'self> StrLegacyUtil for &'self str {
            fn to_upper(&self) -> ~str {
                self.to_ascii().to_upper().to_str_ascii()
            }
            fn to_lower(&self) -> ~str {
                self.to_ascii().to_lower().to_str_ascii()
            }
        }
    }

    pub mod to_bytes {
        pub use core::to_bytes::*;
        #[cfg(legacy)] pub type Ret = ();
        #[cfg(not(legacy))] pub type Ret = bool;
        #[cfg(not(legacy))] pub fn iter_bytes_2<A:IterBytes,B:IterBytes>(a: &A, b: &B, lsb0: bool,
                                                                         f: Cb) -> bool {
            a.iter_bytes(lsb0, f) && b.iter_bytes(lsb0, f)
        }
    }

    #[cfg(legacy)]
    pub mod iter {
        pub use core::iter::*;
        pub type Ret = ();
        pub static EarlyExit: Ret = ();
        pub static Finished: Ret = ();
    }

    #[cfg(not(legacy))]
    pub mod iter {
        pub use core::iter::*;
        pub type Ret = bool;
        pub static EarlyExit: Ret = false;
        pub static Finished: Ret = true;
    }

    pub mod rand {
        pub use core::rand::*;
        #[cfg(not(legacy))] pub fn Rng() -> IsaacRng { rng() }
    }
}

