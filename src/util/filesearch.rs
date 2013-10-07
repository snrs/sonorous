// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Utilities for searching files.

use std::os;

/// Context for searching files.
pub struct SearchContext {
    /// Last return value of `get_entries` if any.
    last_get_entries: Option<(Path, (~[~str], ~[~str]))>,
}

impl SearchContext {
    /// Creates a fresh search context.
    pub fn new() -> SearchContext {
        SearchContext { last_get_entries: None }
    }

    /// Returns a list of immediate subdirectories (i.e. without `.` and `..`) and files
    /// in given directory. Returns a pair of empty lists if `dir` is not a directory.
    /// The results may be cached by the context.
    pub fn get_entries(&mut self, dir: &Path) -> (~[~str], ~[~str]) {
        match self.last_get_entries {
            Some((ref prevdir, ref prevret)) if prevdir == dir => prevret.clone(),
            _ => {
                let mut entries = os::list_dir(dir);
                entries.retain(|e| !(".".equiv(e) || "..".equiv(e)));
                let ret = entries.partition(|e| os::path_is_dir(&dir.push(*e)));
                self.last_get_entries = Some((dir.clone(), ret.clone()));
                ret
            }
        }
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

        let mut parts = ~[];
        for part in path.split_iter(|c: char| c == '/' || c == '\\') {
            if part.is_empty() { loop; }
            parts.push(part);
        }
        if parts.is_empty() { return None; }

        let mut cur = basedir.clone();
        let lastpart = parts.pop();
        for part in parts.iter() {
            let (dirs, _files) = self.get_entries(&cur);
            let part = part.to_ascii_upper();
            let mut found = false;
            for next in dirs.iter() {
                if next.to_ascii_upper() == part {
                    cur = cur.push(*next);
                    found = true;
                    break;
                }
            }
            if !found { return None; }
        }

        let (_dirs, files) = self.get_entries(&cur);
        let lastpart = lastpart.to_ascii_upper();
        for next in files.iter() {
            let next_ = next.to_ascii_upper();
            let mut found = (next_ == lastpart);
            if !found {
                match next_.rfind('.') {
                    Some(idx) => {
                        let nextnoext = next_.slice_to(idx).to_owned();
                        for ext in exts.iter() {
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
                return Some(cur.push(*next));
            }
        }

        None
    }
}

