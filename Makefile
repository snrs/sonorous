SRC = $(wildcard src/*.rs src/*/*.rs src/*/*/*.rs src/*/*/*/*.rs)
CRATE = src/sonorous.rs
BIN = sonorous
RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUSTPKG ?= rustpkg
RUSTSDL ?= libs/rust-sdl
RUSTOPENGLES ?= libs/rust-opengles
RUSTENCODING ?= libs/rust-encoding
RUSTSQLITE ?= libs/rustsqlite
DIRECTX_SDK_INCLUDES ?= libs/w32api-directx-standalone/include
SQLITE3 ?= libs/sqlite3
RUSTFLAGS ?= -O
RUSTPKGFLAGS ?= -O
CFLAGS ?= -Os
CXXFLAGS ?= -Os

TRIPLE = $(shell rustc -v | grep host: | cut -b7-)
LIBSDL = $(RUSTSDL)/build/$(TRIPLE)/sdl
LIBSDLIMAGE = $(RUSTSDL)/build/$(TRIPLE)/sdl_image
LIBSDLMIXER = $(RUSTSDL)/build/$(TRIPLE)/sdl_mixer


.PHONY: all clean clean-sdl clean-opengles clean-encoding clean-sqlite

all: $(BIN)

$(BIN): $(SRC) $(LIBSDL) $(LIBSDLIMAGE) $(LIBSDLMIXER) $(RUSTOPENGLES)/libopengles.dummy $(RUSTENCODING)/libencoding.dummy $(RUSTSQLITE)/libsqlite3.dummy
	$(RUSTC) $(RUSTFLAGS) -L $(LIBSDL) -L $(LIBSDLIMAGE) -L $(LIBSDLMIXER) -L $(RUSTOPENGLES) -L $(RUSTENCODING) -L $(RUSTSQLITE) -L $(SQLITE3) $(CRATE) -o $(BIN)

$(LIBSDL):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl

$(LIBSDLIMAGE):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl_image

$(LIBSDLMIXER):
	cd $(RUSTSDL) && $(RUSTPKG) build $(RUSTPKGFLAGS) sdl_mixer

$(RUSTOPENGLES)/libopengles.dummy:
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS)" DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

$(RUSTENCODING)/libencoding.dummy:
	cd $(RUSTENCODING) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS)"

$(RUSTSQLITE)/libsqlite3.dummy: $(SQLITE3)/libsqlite3.a
	# yes, "rustpkg build sqlite3" does not work at all. *sigh*
	$(RUSTC) $(RUSTPKGFLAGS) -L $(SQLITE3) $(RUSTSQLITE)/src/sqlite3/lib.rs -o $@
	touch $@

$(SQLITE3)/libsqlite3.a:
	cd $(SQLITE3) && $(MAKE) all

clean: clean-sdl clean-opengles clean-encoding clean-sqlite
	rm -rf $(BIN)

clean-sdl:
	rm -rf $(RUSTSDL)/bin $(RUSTSDL)/lib $(RUSTSDL)/build $(RUSTSDL)/.rust

clean-opengles:
	rm -rf $(RUSTOPENGLES)/bin $(RUSTOPENGLES)/lib $(RUSTOPENGLES)/build $(RUSTOPENGLES)/.rust

clean-encoding:
	cd $(RUSTENCODING) && $(MAKE) clean

clean-sqlite:
	cd $(SQLITE3) && $(MAKE) clean
	cd $(RUSTSQLITE) && rm -f *.so *.dylib *.dll *.dummy

