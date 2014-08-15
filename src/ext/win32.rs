// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Win32 API wrappers.

#![cfg(target_os = "windows")]

pub mod ll {
    #![allow(non_camel_case_types, uppercase_variables)]

    use libc::{c_int, c_uint, c_void};
    use libc::types::os::arch::extra::{BOOL, CHAR, WORD, DWORD, HANDLE};
    use libc::types::os::arch::extra::{LPCSTR, LPWSTR, LPCWSTR};

    pub type HWND = HANDLE;
    pub type HDC = HANDLE;
    pub type HINSTANCE = HANDLE;

    pub static OFN_HIDEREADONLY: DWORD = 4;

    pub struct OPENFILENAMEW {
        pub lStructSize: DWORD,
        pub hwndOwner: HWND,
        pub hInstance: HINSTANCE,
        pub lpstrFilter: LPCWSTR,
        pub lpstrCustomFilter: LPWSTR,
        pub nMaxCustFilter: DWORD,
        pub nFilterIndex: DWORD,
        pub lpstrFile: LPWSTR,
        pub nMaxFile: DWORD,
        pub lpstrFileTitle: LPWSTR,
        pub nMaxFileTitle: DWORD,
        pub lpstrInitialDir: LPCWSTR,
        pub lpstrTitle: LPCWSTR,
        pub Flags: DWORD,
        pub nFileOffset: WORD,
        pub nFileExtension: WORD,
        pub lpstrDefExt: LPCWSTR,
        pub lCustData: DWORD,
        pub lpfnHook: *mut (), // XXX LPOFNHOOKPROC = fn(HWND,c_uint,WPARAM,LPARAM)->c_uint
        pub lpTemplateName: LPCWSTR,
        pub pvReserved: *mut c_void,
        pub dwReserved: DWORD,
        pub FlagsEx: DWORD,
    }

    pub struct FILETIME {
        pub dwLowDateTime: DWORD,
        pub dwHighDateTime: DWORD,
    }

    pub struct WIN32_FIND_DATAA {
        pub dwFileAttributes: DWORD,
        pub ftCreationTime: FILETIME,
        pub ftLastAccessTime: FILETIME,
        pub ftLastWriteTime: FILETIME,
        pub nFileSizeHigh: DWORD,
        pub nFileSizeLow: DWORD,
        pub dwReserved0: DWORD,
        pub dwReserved1: DWORD,
        pub cFileName: [CHAR, ..260],
    }

    #[link(name = "kernel32")]
    extern "stdcall" {
        pub fn FindFirstFileA(lpFileName: LPCSTR,
                              lpFindFileData: *mut WIN32_FIND_DATAA) -> HANDLE;
        pub fn FindNextFileA(hFindFile: HANDLE, lpFindFileData: *mut WIN32_FIND_DATAA) -> BOOL;
        pub fn FindClose(hFindFile: HANDLE) -> BOOL;
        pub fn LoadLibraryA(lpFileName: LPCSTR) -> HANDLE;
        pub fn GetACP() -> c_uint;
    }

    #[link(name = "user32")]
    extern "stdcall" {
        pub fn MessageBoxW(hWnd: HWND, lpText: LPCWSTR, lpCaption: LPCWSTR,
                           uType: c_uint) -> c_int;
        pub fn GetDC(hwnd: HWND) -> HDC;
    }

    #[link(name = "comdlg32")]
    extern "stdcall" {
        pub fn GetOpenFileNameW(lpofn: *mut OPENFILENAMEW) -> BOOL;
    }
}

