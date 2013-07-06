// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Utilities for searching files.

/**
 * Resolves the specified resource path to the actual path if possible. May fail, but its
 * success doesn't guarantee that the resource should be read without a failure either.
 * (C: `resolve_relative_path`)
 *
 * The actual resolution is complicated by the fact that many BMSes assume the case-insensitive
 * matching on file names and the coexistence between WAV resources and MP3 resources while
 * keeping the same BMS file. Therefore Angolmois adopted the following resolution rules:
 *
 * 1. Both `/` and `\` are accepted as a directory separator.
 * 2. Path components including file names are matched case-insensitively. If there are multiple
 *    matches then any one can be used, even when a better match exists.
 * 3. If the initial match on the file name fails, and the file name does contain an extension,
 *    then a list of alternative extensions is applied with the same matching procedure.
 */
pub fn resolve_relative_path(basedir: &Path, path: &str, exts: &[&str]) -> Option<Path> {
    use std::os::{path_is_dir, list_dir};
    use util::std::str::StrUtil;

    let mut parts = ~[];
    for path.split_iter(|c: char| c == '/' || c == '\\').advance |part| {
        if part.is_empty() { loop; }
        parts.push(part);
    }
    if parts.is_empty() { return None; }

    let mut cur = basedir.clone();
    let lastpart = parts.pop();
    for parts.iter().advance |part| {
        // early exit if the intermediate path does not exist or is not a directory
        if !path_is_dir(&cur) { return None; }

        let part = part.to_ascii_upper();
        let mut found = false;
        let entries = list_dir(&cur); // XXX #3511
        for entries.iter().advance |&next| {
            if next == ~"." || next == ~".." { loop; }
            if next.to_ascii_upper() == part {
                cur = cur.push(next);
                found = true;
                break;
            }
        }
        if !found { return None; }
    }

    if !path_is_dir(&cur) { return None; }

    let lastpart = lastpart.to_ascii_upper();
    let entries = list_dir(&cur); // XXX #3511
    for entries.iter().advance |&next| {
        if next == ~"." || next == ~".." { loop; }
        let next_ = next.to_ascii_upper();
        let mut found = (next_ == lastpart);
        if !found {
            match next_.rfind('.') {
                Some(idx) => {
                    let nextnoext = next_.slice(0, idx).to_owned();
                    for exts.iter().advance |ext| {
                        if nextnoext + ext.to_owned() == lastpart {
                            found = true;
                            break;
                        }
                    }
                }
                None => {} // does not try alternative extensions if there was no extension
            }
        }
        if found {
            return Some(cur.push(next));
        }
    }

    None
}
