# This is a part of Sonorous.
# Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
# See README.md and LICENSE.txt for details.

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
RUSTPKGFLAGS ?= -O --rlib
CFLAGS ?= -Os
CXXFLAGS ?= -Os

# intentionally ignores rustpkg for sdl, sdl_image, sdl_mixer and sqlite3.
LIBSDL = $(RUSTSDL)/libsdl.dummy
LIBSDL_IMAGE = $(RUSTSDL)/libsdl_image.dummy
LIBSDL_MIXER = $(RUSTSDL)/libsdl_mixer.dummy
LIBOPENGLES = $(RUSTOPENGLES)/libopengles.dummy
LIBENCODING = $(RUSTENCODING)/libencoding.dummy
LIBSQLITE3 = $(RUSTSQLITE)/libsqlite3.dummy
LIBS = $(LIBSDL) $(LIBSDL_IMAGE) $(LIBSDL_MIXER) $(LIBOPENGLES) $(LIBENCODING) $(LIBSQLITE3)


.PHONY: all clean clean-sdl clean-opengles clean-encoding clean-sqlite

all: $(BIN)

$(BIN): $(SRC) $(LIBS)
	$(RUSTC) $(RUSTFLAGS) $(patsubst %,-L %,$(dir $(LIBS))) -L $(SQLITE3) $(CRATE) -o $(BIN)

$(LIBSDL):
	$(RUSTC) $(RUSTPKGFLAGS) $(RUSTSDL)/src/sdl/lib.rs -o $@ && touch $@

$(LIBSDL_IMAGE): $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $(RUSTSDL)/src/sdl_image/lib.rs -o $@ && touch $@

$(LIBSDL_MIXER): $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $(RUSTSDL)/src/sdl_mixer/lib.rs -o $@ && touch $@

$(LIBOPENGLES):
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS)" DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

$(LIBENCODING):
	cd $(RUSTENCODING) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS)"

$(LIBSQLITE3): $(SQLITE3)/libsqlite3.a
	$(RUSTC) $(RUSTPKGFLAGS) -L $(SQLITE3) $(RUSTSQLITE)/src/sqlite3/lib.rs -o $@ && touch $@

$(SQLITE3)/libsqlite3.a:
	cd $(SQLITE3) && $(MAKE) all

clean: clean-sdl clean-opengles clean-encoding clean-sqlite
	rm -rf $(BIN) $(BIN).exe

clean-sdl:
	cd $(RUSTSDL) && rm -f *.so *.dylib *.dll *.rlib *.dummy

clean-opengles:
	cd $(RUSTOPENGLES) && $(MAKE) clean

clean-encoding:
	cd $(RUSTENCODING) && $(MAKE) clean

clean-sqlite:
	cd $(SQLITE3) && $(MAKE) clean
	cd $(RUSTSQLITE) && rm -f *.so *.dylib *.dll *.rlib *.dummy

