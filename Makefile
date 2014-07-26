# This is a part of Sonorous.
# Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
# See README.md and LICENSE.txt for details.

SRC = $(wildcard src/*.rs src/*/*.rs src/*/*/*.rs src/*/*/*/*.rs)
CRATE = src/sonorous.rs
BIN = sonorous
TESTBIN = sonorous-test
RUSTC ?= rustc
RUSTDOC ?= rustdoc
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

LIBSDL = $(RUSTSDL)/libsdl_rust.rlib
LIBSDL_IMAGE = $(RUSTSDL)/libsdl_image_rust.rlib
LIBSDL_MIXER = $(RUSTSDL)/libsdl_mixer_rust.rlib
LIBOPENGLES = $(RUSTOPENGLES)/libopengles_rust.rlib
LIBENCODING = $(RUSTENCODING)/libencoding.rlib
LIBSQLITE3 = $(RUSTSQLITE)/libsqlite3_rust.rlib
LIBS = $(LIBSDL) $(LIBSDL_IMAGE) $(LIBSDL_MIXER) $(LIBOPENGLES) $(LIBENCODING) $(LIBSQLITE3)


.PHONY: all check doc clean clean-sdl clean-opengles clean-encoding clean-sqlite

all: $(BIN)

$(BIN): $(SRC) $(LIBS)
	$(RUSTC) $(RUSTFLAGS) $(patsubst %,-L %,$(dir $(LIBS))) -L $(SQLITE3) $(CRATE) -o $(BIN)

$(LIBSDL): $(RUSTSDL)/src/sdl/lib.rs
	$(RUSTC) $(RUSTPKGFLAGS) $< -C extra-filename=_rust --out-dir $(dir $@)

$(LIBSDL_IMAGE): $(RUSTSDL)/src/sdl_image/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< -C extra-filename=_rust --out-dir $(dir $@)

$(LIBSDL_MIXER): $(RUSTSDL)/src/sdl_mixer/lib.rs $(LIBSDL)
	$(RUSTC) $(RUSTPKGFLAGS) -L $(RUSTSDL) $< -C extra-filename=_rust --out-dir $(dir $@)

$(LIBOPENGLES):
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS) -C extra-filename=_rust --crate-type rlib" DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

$(LIBENCODING):
	cd $(RUSTENCODING) && $(MAKE) RUSTFLAGS="$(RUSTPKGFLAGS)"

$(LIBSQLITE3): $(RUSTSQLITE)/src/sqlite3.rs $(SQLITE3)/libsqlite3.a
	$(RUSTC) $(RUSTPKGFLAGS) -L $(SQLITE3) $< -C extra-filename=_rust --out-dir $(dir $@)

$(SQLITE3)/libsqlite3.a:
	cd $(SQLITE3) && $(MAKE) all

#$(TESTBIN): $(SRC) $(LIBS)
#	$(RUSTC) $(RUSTFLAGS) $(patsubst %,-L %,$(dir $(LIBS))) -L $(SQLITE3) --test $(CRATE) -o $(TESTBIN)
#
#check: $(TESTBIN)
#	./$(TESTBIN)

doc:
	$(RUSTDOC) $(patsubst %,-L %,$(dir $(LIBS))) $(CRATE)

clean: clean-sdl clean-opengles clean-encoding clean-sqlite
	rm -rf $(BIN) $(BIN).exe $(TESTBIN) $(TESTBIN).exe

clean-sdl:
	cd $(RUSTSDL) && rm -f *.so *.dylib *.dll *.rlib *.dummy

clean-opengles:
	cd $(RUSTOPENGLES) && $(MAKE) clean

clean-encoding:
	cd $(RUSTENCODING) && $(MAKE) clean

clean-sqlite:
	cd $(SQLITE3) && $(MAKE) clean
	cd $(RUSTSQLITE) && rm -f *.so *.dylib *.dll *.rlib *.dummy

