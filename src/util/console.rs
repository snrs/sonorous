// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Encoding-aware console I/O.

use std::local_data;
use std::io::{stdout, stderr};
use encoding::{Encoding, Encoder, EncoderTrap, ByteWriter};

local_data_key!(console_encoding_key: &'static Encoding)

/// Returns an encoding usable for console I/O.
#[cfg(target_os = "win32")]
fn get_console_encoding() -> &'static Encoding {
    #[fixed_stack_segment]; #[inline(never)];
    use ext::win32::ll::GetACP;
    use encoding::all::ASCII;
    use encoding::label::encoding_from_windows_code_page;
    let cp = unsafe { GetACP() } as uint;
    encoding_from_windows_code_page(cp).unwrap_or(ASCII as &'static Encoding)
}

/// Returns an encoding usable for console I/O.
#[cfg(not(target_os = "win32"))]
fn get_console_encoding() -> &'static Encoding {
    use encoding::all::UTF_8;
    UTF_8 as &'static Encoding // TODO
}

/// Returns an encoding usable for console I/O.
/// The result is cached to the task-local storage.
pub fn console_encoding() -> &'static Encoding {
    match local_data::get(console_encoding_key, |e| e.map_move(|&e| e)) {
        Some(encoding) => encoding,
        None => {
            let encoding = get_console_encoding();
            local_data::set(console_encoding_key, encoding);
            encoding
        }
    }
}

/// An encoder trap function used for `to_console_encoding`.
fn hex_ncr_escape(_encoder: &Encoder, input: &str, output: &mut ByteWriter) -> bool {
    let mut escapes = ~"";
    for ch in input.iter() { escapes.push_str(format!("&\\#x{:X};", ch as int)); }
    output.write_bytes(escapes.as_bytes());
    true
}

/// Converts the string to the current console encoding.
pub fn to_console_encoding(s: &str) -> ~[u8] {
    console_encoding().encode(s, EncoderTrap(hex_ncr_escape)).unwrap()
}

/// Same as `std::io::print` but converts to the current console encoding if possible.
pub fn printout(s: &str) {
    stdout().write(to_console_encoding(s));
}

/// Same as `std::io::println` but converts to the current console encoding if possible.
pub fn printoutln(s: &str) {
    let out = stdout();
    out.write(to_console_encoding(s));
    out.write(['\n' as u8]);
}

/// Same as `std::io::stderr().write_str` but converts to the current console encoding if possible.
pub fn printerr(s: &str) {
    stderr().write(to_console_encoding(s));
}

/// Same as `std::io::stderr().write_line` but converts to the current console encoding if possible.
pub fn printerrln(s: &str) {
    let err = stderr();
    err.write(to_console_encoding(s));
    err.write(['\n' as u8]);
}
