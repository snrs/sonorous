// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Win32 API wrappers.

#[cfg(target_os = "win32")];

pub mod ll {
    use core::libc::{c_int, c_uint, c_void};
    use core::libc::{BOOL, CHAR, WORD, DWORD, HANDLE, LPCSTR, LPWSTR, LPCWSTR};

    pub type HWND = HANDLE;
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

    #[link_args = "-lkernel32"]
    #[abi = "stdcall"]
    pub extern "stdcall" {
        fn FindFirstFileA(lpFileName: LPCSTR, lpFindFileData: *WIN32_FIND_DATAA) -> HANDLE;
        fn FindNextFileA(hFindFile: HANDLE, lpFindFileData: *WIN32_FIND_DATAA) -> BOOL;
        fn FindClose(hFindFile: HANDLE) -> BOOL;
    }

    #[link_args = "-luser32"]
    #[abi = "stdcall"]
    pub extern "stdcall" {
        fn MessageBoxW(hWnd: HWND, lpText: LPCWSTR, lpCaption: LPCWSTR,
                       uType: c_uint) -> c_int;
    }

    #[link_args = "-lcomdlg32"]
    #[abi = "stdcall"]
    pub extern "stdcall" {
        fn GetOpenFileNameW(lpofn: *OPENFILENAMEW) -> BOOL;
    }
}

