// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Win32 API wrappers.

#![cfg(target_os = "win32")]

pub mod ll {
    #![allow(non_camel_case_types, uppercase_variables)]

    use std::libc::{c_int, c_uint, c_void};
    use std::libc::{BOOL, CHAR, WORD, DWORD, HANDLE, LPCSTR, LPWSTR, LPCWSTR};

    pub type HWND = HANDLE;
    pub type HDC = HANDLE;
    pub type HINSTANCE = HANDLE;

    pub static OFN_HIDEREADONLY: DWORD = 4;

    pub struct OPENFILENAMEW {
        lStructSize: DWORD,
        hwndOwner: HWND,
        hInstance: HINSTANCE,
        lpstrFilter: LPCWSTR,
        lpstrCustomFilter: LPWSTR,
        nMaxCustFilter: DWORD,
        nFilterIndex: DWORD,
        lpstrFile: LPWSTR,
        nMaxFile: DWORD,
        lpstrFileTitle: LPWSTR,
        nMaxFileTitle: DWORD,
        lpstrInitialDir: LPCWSTR,
        lpstrTitle: LPCWSTR,
        Flags: DWORD,
        nFileOffset: WORD,
        nFileExtension: WORD,
        lpstrDefExt: LPCWSTR,
        lCustData: DWORD,
        lpfnHook: *(), // XXX LPOFNHOOKPROC = fn(HWND,c_uint,WPARAM,LPARAM)->c_uint
        lpTemplateName: LPCWSTR,
        pvReserved: *c_void,
        dwReserved: DWORD,
        FlagsEx: DWORD,
    }

    pub struct FILETIME {
        dwLowDateTime: DWORD,
        dwHighDateTime: DWORD,
    }

    pub struct WIN32_FIND_DATAA {
        dwFileAttributes: DWORD,
        ftCreationTime: FILETIME,
        ftLastAccessTime: FILETIME,
        ftLastWriteTime: FILETIME,
        nFileSizeHigh: DWORD,
        nFileSizeLow: DWORD,
        dwReserved0: DWORD,
        dwReserved1: DWORD,
        cFileName: [CHAR, ..260],
    }

    #[link(name = "kernel32")]
    extern "stdcall" {
        pub fn FindFirstFileA(lpFileName: LPCSTR,
                              lpFindFileData: *WIN32_FIND_DATAA) -> HANDLE;
        pub fn FindNextFileA(hFindFile: HANDLE, lpFindFileData: *WIN32_FIND_DATAA) -> BOOL;
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
        pub fn GetOpenFileNameW(lpofn: *OPENFILENAMEW) -> BOOL;
    }
}

