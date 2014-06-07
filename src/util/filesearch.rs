// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Utilities for searching files.

use std::{str, io};
use std::rc::Rc;
use std::collections::HashMap;

/// Context for searching files.
pub struct SearchContext {
    /// Cached return values of `get_entries`.
    pub get_entries_cache: HashMap<Path,(Rc<Vec<Path>>,Rc<Vec<Path>>)>,
}

impl SearchContext {
    /// Creates a fresh search context.
    pub fn new() -> SearchContext {
        SearchContext { get_entries_cache: HashMap::new() }
    }

    /// Returns a list of immediate subdirectories (i.e. without `.` and `..`) and files
    /// in given directory. Returns a pair of empty lists if `dir` is not a directory.
    /// The results may be cached by the context.
    pub fn get_entries<'r>(&'r mut self, dir: &Path) -> (&'r [Path], &'r [Path]) {
        // the original plan is to implement an LRU cache for `get_entries`, but for now we don't
        // invalidate any cache items since it turned out that `os::list_dir` is very, very slow.
        // for example, it is not rare for `list_dir` to take more than 100ms in Windows.
        let &(ref dirs, ref files) = self.get_entries_cache.find_or_insert_with(dir.clone(), |_| {
            let entries = io::fs::readdir(dir).ok().unwrap_or_else(|| Vec::new());
            let (dirs, files) = entries.partition(|path| path.is_dir());
            (Rc::new(dirs), Rc::new(files))
        });
        (dirs.as_slice(), files.as_slice())
    }

    /**
     * Resolves the specified resource path to the actual path if possible. May fail, but its
     * success doesn't guarantee that the resource should be read without a failure either.
     *
     * The actual resolution is complicated by the fact that many BMSes assume the case-insensitive
     * matching on file names and the coexistence between WAV resources and MP3 resources while
     * keeping the same BMS file. Therefore Sonorous adopted the following resolution rules:
     *
     * 1. Both `/` and `\` are accepted as a directory separator.
     * 2. Path components including file names are matched case-insensitively. If there are multiple
     *    matches then any one can be used, even when a better match exists.
     * 3. If the initial match on the file name fails, and the file name does contain an extension,
     *    then a list of alternative extensions is applied with the same matching procedure.
     */
    pub fn resolve_relative_path(&mut self, basedir: &Path, path: &str,
                                 exts: &[&str]) -> Option<Path> {
        use std::ascii::StrAsciiExt;

        let mut parts = Vec::new();
        for part in path.split(|c: char| c == '/' || c == '\\') {
            if part.is_empty() { continue; }
            parts.push(part);
        }
        if parts.is_empty() { return None; }

        let mut cur = basedir.clone();
        let lastpart = parts.pop().unwrap();
        for part in parts.iter() {
            let (dirs, _files) = self.get_entries(&cur);
            let part = part.to_ascii_upper();
            let mut found = false;
            for next in dirs.iter() {
                let name = next.filename().and_then(str::from_utf8).map(|v| v.to_ascii_upper());
                if name.as_ref().map_or(false, |name| *name == part) {
                    cur = next.clone();
                    found = true;
                    break;
                }
            }
            if !found { return None; }
        }

        let (_dirs, files) = self.get_entries(&cur);
        let lastpart = lastpart.to_ascii_upper();
        for next in files.iter() {
            let name = next.filename().and_then(str::from_utf8).map(|v| v.to_ascii_upper());
            let mut found = name.as_ref().map_or(false, |name| *name == lastpart);
            if !found && name.is_some() {
                let name = name.unwrap();
                match name.as_slice().rfind('.') {
                    Some(idx) => {
                        let nextnoext = name.as_slice().slice_to(idx);
                        for ext in exts.iter() {
                            if nextnoext.to_string().append(*ext) == lastpart {
                                found = true;
                                break;
                            }
                        }
                    }
                    None => {} // does not try alternative extensions if there was no extension
                }
            }
            if found {
                return Some(next.clone());
            }
        }

        None
    }
}

