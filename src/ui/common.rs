// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common UI patterns.

#[macro_escape];

/// Immediately terminates the program with given exit code.
pub fn exit(exitcode: int) -> ! {
    // Rust: `os::set_exit_status` doesn't immediately terminate the program.
    unsafe { libc::exit(exitcode as libc::c_int); }
}

/// Exits with an error message. Internally used in the `die!` macro below.
#[cfg(target_os = "win32")]
pub fn die(s: ~str) -> ! {
    use util::core::str::as_utf16_c_str;
    do as_utf16_c_str(::exename()) |caption| {
        do as_utf16_c_str(s) |text| {
            unsafe { ::ext::win32::ll::MessageBoxW(ptr::mut_null(), text, caption, 0); }
        }
    }
    exit(1)
}

/// Exits with an error message. Internally used in the `die!` macro below.
#[cfg(not(target_os = "win32"))]
pub fn die(s: ~str) -> ! {
    ::core::io::stderr().write_line(fmt!("%s: %s", ::exename(), s));
    exit(1)
}

/// Prints an warning message. Internally used in the `warn!` macro below.
pub fn warn(s: ~str) {
    ::core::io::stderr().write_line(fmt!("*** Warning: %s", s));
}

// Exits with a formatted error message. (C: `die`)
//
// Rust: this comment cannot be a doc comment (yet).
macro_rules! die(
    ($($e:expr),+) => (::ui::common::die(fmt!($($e),+)))
)

// Prints a formatted warning message. (C: `warn`)
macro_rules! warn(
    ($($e:expr),+) => (::ui::common::warn(fmt!($($e),+)))
)

/// Reads a path string from the user in the platform-dependent way. Returns `None` if the user
/// refused to do so or the platform is unsupported. (C: `filedialog`)
#[cfg(target_os = "win32")]
pub fn get_path_from_dialog() -> Option<~str> {
    use core::{str, vec};
    use util::core::str::as_utf16_c_str;
    use ext::win32;

    let filter =
        "All Be-Music Source File (*.bms;*.bme;*.bml;*.pms)\x00*.bms;*.bme;*.bml;*.pms\x00\
         Be-Music Source File (*.bms)\x00*.bms\x00\
         Extended Be-Music Source File (*.bme)\x00*.bme\x00\
         Longnote Be-Music Source File (*.bml)\x00*.bml\x00\
         Po-Mu Source File (*.pms)\x00*.pms\x00\
         All Files (*.*)\x00*.*\x00";
    do as_utf16_c_str(filter) |filter| {
        do as_utf16_c_str("Choose a file to play") |title| {
            let mut buf = [0u16, ..512];
            let ret = do vec::as_mut_buf(buf) |buf, bufsize| {
                let ofn = win32::ll::OPENFILENAMEW {
                    lStructSize: sys::size_of::<win32::ll::OPENFILENAMEW>() as libc::DWORD,
                    lpstrFilter: filter,
                    lpstrFile: buf,
                    nMaxFile: bufsize as libc::DWORD,
                    lpstrTitle: title,
                    Flags: win32::ll::OFN_HIDEREADONLY,

                    // zero-initialized fields
                    hwndOwner: ptr::mut_null(), hInstance: ptr::mut_null(),
                    lpstrCustomFilter: ptr::mut_null(), nMaxCustFilter: 0, nFilterIndex: 0,
                    lpstrFileTitle: ptr::mut_null(), nMaxFileTitle: 0,
                    lpstrInitialDir: ptr::null(), nFileOffset: 0, nFileExtension: 0,
                    lpstrDefExt: ptr::null(), lCustData: 0, lpfnHook: ptr::null(),
                    lpTemplateName: ptr::null(), pvReserved: ptr::null(),
                    dwReserved: 0, FlagsEx: 0,
                };
                unsafe {win32::ll::GetOpenFileNameW(cast::transmute(&ofn))}
            };
            if ret != 0 {
                let path: &[u16] = match buf.position_elem(&0) {
                    Some(idx) => buf.slice(0, idx),
                    // Rust: why can't we cast `&[u16, ..512]` to `&[u16]`?!
                    None => buf.slice(0, buf.len())
                };
                Some(str::from_utf16(path))
            } else {
                None
            }
        }
    }
}

/// Reads a path string from the user in the platform-dependent way. Returns `None` if the user
/// refused to do so or the platform is unsupported. (C: `filedialog`)
#[cfg(not(target_os = "win32"))]
pub fn get_path_from_dialog() -> Option<~str> {
    None
}

/// A periodic timer for thresholding the rate of information display.
pub struct Ticker {
    /// Minimal required milliseconds after the last display.
    interval: uint,
    /// The timestamp at the last display. It is a return value from `sdl::get_ticks` and
    /// measured in milliseconds. May be a `None` if the ticker is at the initial state or
    /// has been reset by `reset` method. (C: `lastinfo`)
    lastinfo: Option<uint>
}

/// Returns a new ticker with a default display interval.
pub fn Ticker() -> Ticker {
    /// A reasonable interval for the console and graphic display. Currently set to about 21fps.
    /// (C: `INFO_INTERVAL`)
    static INFO_INTERVAL: uint = 47;
    Ticker { interval: INFO_INTERVAL, lastinfo: None }
}

pub impl Ticker {
    /// Calls `f` only when required milliseconds have passed after the last display.
    /// `now` should be a return value from `sdl::get_ticks`.
    fn on_tick(&mut self, now: uint, f: &fn()) {
        if self.lastinfo.map_default(true, |&t| now - t >= self.interval) {
            self.lastinfo = Some(now);
            f();
        }
    }

    /// Lets the next call to `on_tick` always call the callback.
    fn reset(&mut self) {
        self.lastinfo = None;
    }
}

