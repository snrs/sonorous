// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Caches backed by the external database.

use std::{io, result, path};
use std::io::{IoError, OtherIoError, IoResult, FileStat};
use std::io::fs::readdir;

use sqlite3;

/// Encodes a path (possibly) relative to the root path as a byte vector.
fn encode_path(root: &Path, path: &Path) -> Vec<u8> {
    let normalized0 = path.path_relative_from(root);
    let normalized = normalized0.as_ref().unwrap_or(path);
    let mut ret = normalized.root_path().unwrap_or(Path::new(".")).into_vec();
    for component in normalized.components() {
        ret.push(0);
        ret.extend(component.iter().map(|&s| s));
    }
    ret
}

#[test]
fn test_encode_path() {
    fn path(cs: &[&str]) -> Path {
        if cs.len() == 1 {
            Path::new(cs[0])
        } else {
            Path::new(cs[0]).join_many(cs.slice_from(1))
        }
    }

    let root = path([".", "a", "b"]);
    assert_eq!(encode_path(&root, &path([".", "c", "d"])).as_slice(),
               bytes!(".\0..\0..\0c\0d"));
    assert_eq!(encode_path(&root, &path([".", "c", "d", ""])).as_slice(),
               bytes!(".\0..\0..\0c\0d"));
    assert_eq!(encode_path(&root, &path([".", "a", "e"])).as_slice(),
               bytes!(".\0..\0e"));
    assert_eq!(encode_path(&root, &path([".", "a", "b", "f"])).as_slice(),
               bytes!(".\0f"));

    if cfg!(target_os = "win32") {
        assert_eq!(encode_path(&root, &path(["\\", "x", "y", "z"])).as_slice(),
                   bytes!("\\\0x\0y\0z"));
        assert_eq!(encode_path(&root, &path(["C:\\", "x", "y", "z"])).as_slice(),
                   bytes!("C:\\\0x\0y\0z"));
        assert_eq!(encode_path(&root, &path(["c:\\", "x", "y", "z"])).as_slice(),
                   bytes!("C:\\\0x\0y\0z"));
        assert_eq!(encode_path(&root, &path(["C:.", "x", "y", "z"])).as_slice(),
                   bytes!("C:\0x\0y\0z"));
        assert_eq!(encode_path(&root, &path(["c:.", "x", "y", "z"])).as_slice(),
                   bytes!("C:\0x\0y\0z"));

        let absroot = path(["C:\\", "a", "b"]);
        assert_eq!(encode_path(&absroot, &path(["C:\\", "c", "d"])).as_slice(),
                   bytes!(".\0..\0..\0c\0d"));
        assert_eq!(encode_path(&absroot, &path(["D:\\", "c", "d"])).as_slice(),
                   bytes!("D:\\\0c\0d"));
    } else {
        assert_eq!(encode_path(&root, &path(["/", "x", "y", "z"])).as_slice(),
                   bytes!("/\0x\0y\0z"));

        let absroot = path(["/", "a", "b"]);
        assert_eq!(encode_path(&absroot, &path(["/", "c", "d"])).as_slice(),
                   bytes!(".\0..\0..\0c\0d"));
        // this is why the caller should use the absolute path if possible
        assert_eq!(encode_path(&absroot, &path([".", "c", "d"])).as_slice(),
                   bytes!(".\0c\0d"));
    }
}

/// Converts an SQLite `ResultCode` into an `IoError`.
fn io_error_from_sqlite(db: Option<&sqlite3::Database>, code: sqlite3::ResultCode) -> IoError {
    let detail = match db {
        Some(db) => format!("{} - {}", code, db.get_errmsg()),
        None => format!("{}", code),
    };
    IoError { kind: OtherIoError, desc: "SQLite error", detail: Some(detail) }
}

/// Calls `fail!` with an SQLite `ResultCode`.
fn fail_from_sqlite(db: &sqlite3::Database, code: sqlite3::ResultCode) -> ! {
    fail!("SQLite error: {}", io_error_from_sqlite(Some(db), code));
}

/// `try!`-friendly version of `Cursor::step`.
fn step_cursor(db: &sqlite3::Database, c: &sqlite3::Cursor) -> IoResult<bool> {
    match c.step() {
        sqlite3::SQLITE_ROW => Ok(true),
        sqlite3::SQLITE_DONE => Ok(false),
        code => Err(io_error_from_sqlite(Some(db), code)),
    }
}

/// RAII-based transaction object. Without any further action, it will get rolled back.
struct Transaction<'a> {
    db: Option<&'a sqlite3::Database>,
}

impl<'a> Transaction<'a> {
    /// Starts the transaction.
    fn new(db: &'a sqlite3::Database) -> IoResult<Transaction<'a>> {
        match db.exec("BEGIN;") {
            Ok(..) => Ok(Transaction { db: Some(db) }),
            Err(err) => Err(io_error_from_sqlite(Some(db), err)),
        }
    }

    /// Consumes the transaction while commiting it.
    fn commit(mut self) {
        let db = self.db.take_unwrap();
        match db.exec("COMMIT;") {
            Ok(..) => {},
            Err(err) => fail_from_sqlite(db, err),
        }
    }
}

// Rust: ICE workarounds. unsure why #[unsafe_destructor] suppresses ICE. (#13853)
#[unsafe_destructor]
impl<'a> Drop for Transaction<'a> {
    fn drop(&mut self) {
        match self.db {
            Some(db) => match db.exec("ROLLBACK;") {
                Ok(..) => {},
                Err(err) => fail_from_sqlite(db, err),
            },
            None => {}
        }
    }
}

/// The metadata cache backed by SQLite database.
///
/// The cache is used for either retrieving the directory contents (`get_entries`)
/// or retrieving the cached timeline based on the file's MD5 hash if any (`get_metadata`, TODO).
/// The former touches two tables `directories` (for the cached directory)
/// and `files` (for the cached directory contents);
/// the latter touches `files` (for the cached file hash if any) and `timelines` (for metadata).
/// This means that invalidating the directory contents will invalidate any related metadata if any.
pub struct MetadataCache {
    /// The predefined "root" path.
    ///
    /// This is used to normalize the in-database path, so that the cached timeline won't get
    /// invalidated when the files have been moved but the relative paths from the root path
    /// to the files haven't been changed.
    root: Path,
    /// The SQLite database.
    db: sqlite3::Database,
}

/// A value for `files.size` when the "file" is actually a directory.
static SIZE_FOR_DIRECTORY: i64 = -1;
/// A value for `files.size` when the "file" is actually not a file nor a directory.
static SIZE_FOR_NON_REGULAR: i64 = -2;

/// Calculates the value for `files.size` from given `stat` result.
fn size_from_filestat(st: &FileStat) -> i64 {
    match st.kind {
        io::TypeFile => st.size as i64,
        io::TypeDirectory => SIZE_FOR_DIRECTORY,
        _ => SIZE_FOR_NON_REGULAR,
    }
}

impl MetadataCache {
    /// Opens a metadata cache.
    pub fn open(root: Path, dbpath: &Path) -> IoResult<MetadataCache> {
        // avoid the initial `:`
        let mut path = dbpath.as_str().unwrap().into_maybe_owned();
        if path.as_slice().starts_with(":") {
            path = format!(".{}{}", path::SEP, path).into_maybe_owned();
        }

        let db = match sqlite3::open(path.as_slice()) {
            Ok(db) => db,
            Err(err) => { return Err(io_error_from_sqlite(None, err)); }
        };
        let db = MetadataCache { root: root, db: db };
        try!(db.create_schema());
        Ok(db)
    }

    /// Opens an in-memory metadata cache.
    pub fn open_in_memory(root: Path) -> IoResult<MetadataCache> {
        let db = match sqlite3::open(":memory:") {
            Ok(db) => db,
            Err(err) => { return Err(io_error_from_sqlite(None, err)); }
        };
        let db = MetadataCache { root: root, db: db };
        try!(db.create_schema());
        Ok(db)
    }

    /// `try!`-friendly version of `Database::prepare`.
    fn prepare<'a>(&'a self, sql: &str) -> IoResult<sqlite3::Cursor<'a>> {
        self.db.prepare(sql, &None).map_err(|err| io_error_from_sqlite(Some(&self.db), err))
    }

    /// `try!`-friendly version of `Database::exec`.
    fn exec(&self, sql: &str) -> IoResult<bool> {
        self.db.exec(sql).map_err(|err| io_error_from_sqlite(Some(&self.db), err))
    }

    /// Creates a required database schema.
    pub fn create_schema(&self) -> IoResult<()> {
        // TODO schema upgrade
        try!(self.exec("
            BEGIN;
            CREATE TABLE IF NOT EXISTS directories(
                id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                path BLOB NOT NULL UNIQUE, -- root-relative null-separated components
                mtime INTEGER NOT NULL -- msecs
            );
            CREATE TABLE IF NOT EXISTS files(
                id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
                dir INTEGER NOT NULL REFERENCES directories(id),
                name BLOB NOT NULL,
                size INTEGER NOT NULL, -- negative for non-regular files or directories
                mtime INTEGER NOT NULL, -- msecs
                hash BLOB,
                UNIQUE (dir, name)
            );
            CREATE TABLE IF NOT EXISTS timelines(
                hash BLOB PRIMARY KEY NOT NULL,
                title TEXT,
                artist TEXT,
                level INTEGER,
                levelsystem INTEGER,
                difficulty INTEGER
            );
            COMMIT;
        "));
        Ok(())
    }

    /// Retrieves a list of directories and non-directories in the directory if any.
    pub fn get_entries(&self, path: &Path) -> IoResult<(Vec<Path>, Vec<Path>)> {
        debug!("get_entries: path = {}", path.display());

        let encoded = encode_path(&self.root, path);
        let mut dirs = Vec::new();
        let mut files = Vec::new();

        let tr = try!(Transaction::new(&self.db));

        #[deriving(Show)]
        enum CacheResult {
            CacheValid(i64 /* dirid */),
            CacheInvalid(i64 /* dirid */),
            NotCached,
        }

        let c = try!(self.prepare("
            SELECT id, mtime FROM directories WHERE path = ?;
        "));
        c.bind_param(1, &sqlite3::Blob(encoded.clone()));
        let mut res = NotCached;
        let mut dirstat = None;
        if try!(step_cursor(&self.db, &c)) {
            // check if the directory has the same mtime
            let dirid = c.get_i64(0);
            let dirmtime = c.get_i64(1);
            let st = path.stat(); // DO NOT skip the error, this may indicate the required eviction
            match st {
                Ok(ref st) if st.kind == io::TypeDirectory && st.modified as i64 == dirmtime => {
                    res = CacheValid(dirid);
                }
                _ => {
                    res = CacheInvalid(dirid);
                }
            };
            dirstat = Some(st);
        }
        drop(c);

        debug!("get_entries: cache result = {}", res);

        match res {
            CacheValid(dirid) => {
                // retrieve entries from the cache
                let c = try!(self.prepare("
                    SELECT name, size FROM files WHERE dir = ?;
                "));
                c.bind_param(1, &sqlite3::Integer64(dirid));
                while try!(step_cursor(&self.db, &c)) {
                    let name = c.get_blob(0);
                    let size = c.get_i64(1);
                    if size >= 0 {
                        files.push(path.join(name));
                    } else if size == SIZE_FOR_DIRECTORY {
                        dirs.push(path.join(name));
                    }
                }
                drop(c);
            }

            CacheInvalid(..) | NotCached => {
                let dirstat: IoResult<FileStat> = dirstat.unwrap_or_else(|| path.stat());
                debug!("get_entries: dirstat.modified = {}",
                       dirstat.as_ref().ok().map(|st| st.modified));

                // entries for the cached directory, if any, are now invalid.
                match res {
                    CacheInvalid(dirid) => {
                        let c = try!(self.prepare("
                            DELETE FROM files WHERE dir = ?;
                        "));
                        c.bind_param(1, &sqlite3::Integer64(dirid));
                        try!(step_cursor(&self.db, &c));
                        drop(c);
                    }
                    _ => {}
                }

                match (&dirstat, res) {
                    (&Ok(ref dirst), _) => {
                        // this *can* fail; the transaction would get rolled back then.
                        let entries: Vec<Path> = try!(readdir(path));
                        debug!("get_entries: entries = {}",
                               entries.iter().map(|p| p.display().to_str())
                                             .collect::<Vec<String>>());
                        let entrystats: Vec<FileStat> =
                            try!(result::collect(entries.iter().map(|p| p.stat())));
                        debug!("get_entries: entrystats.modified = {}",
                               entrystats.iter().map(|p| p.modified).collect::<Vec<u64>>());

                        // insert or replace the directory entry
                        let c = try!(self.prepare("
                            INSERT OR REPLACE INTO directories(path, mtime) VALUES(?, ?);
                        "));
                        c.bind_param(1, &sqlite3::Blob(encoded));
                        c.bind_param(2, &sqlite3::Integer64(dirst.modified as i64));
                        try!(step_cursor(&self.db, &c));
                        drop(c);
                        let dirid = self.db.get_last_insert_rowid();

                        // insert file entries
                        let c = try!(self.prepare("
                            INSERT INTO files(dir, name, size, mtime) VALUES(?, ?, ?, ?);
                        "));
                        c.bind_param(1, &sqlite3::Integer64(dirid));
                        for (path, st) in entries.move_iter().zip(entrystats.iter()) {
                            let filename = Vec::from_slice(path.filename().unwrap());
                            c.reset();
                            c.bind_param(2, &sqlite3::Blob(filename));
                            c.bind_param(3, &sqlite3::Integer64(size_from_filestat(st)));
                            c.bind_param(4, &sqlite3::Integer64(st.modified as i64));
                            try!(step_cursor(&self.db, &c));
                            match st.kind {
                                io::TypeFile => files.push(path),
                                io::TypeDirectory => dirs.push(path),
                                _ => {}
                            }
                        }
                        drop(c);
                    }

                    (&Err(..), CacheInvalid(dirid)) => {
                        // remove the directory entry if any
                        let c = try!(self.prepare("
                            DELETE FROM directories WHERE id = ?;
                        "));
                        c.bind_param(1, &sqlite3::Integer64(dirid));
                        try!(step_cursor(&self.db, &c));
                        drop(c);
                    }

                    (_, _) => {}
                }

                tr.commit();

                // if stat failed we should return an IoError.
                try!(dirstat);
            }
        }

        debug!("get_entries: dirs = {}",
               dirs.iter().map(|p| p.display().to_str()).collect::<Vec<String>>());
        debug!("get_entries: files = {}",
               files.iter().map(|p| p.display().to_str()).collect::<Vec<String>>());

        Ok((dirs, files))
    }
}

