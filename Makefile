SRC = $(wildcard src/*.rs src/*/*.rs src/*/*/*.rs src/*/*/*/*.rs)
CRATE = src/sonorous.rs
ifneq (,$(findstring MINGW,$(shell uname -s)))
EXE ?= .exe
else
EXE ?=
endif
BIN = sonorous$(EXE)
RUSTC ?= rustc$(EXE)
RUSTDOC ?= rustdoc$(EXE)
RUSTSDL ?= libs/rust-sdl
RUSTOPENGLES ?= libs/rust-opengles
RUSTENCODING ?= libs/rust-encoding
RUSTSQLITE ?= libs/rustsqlite
DIRECTX_SDK_INCLUDES ?= libs/w32api-directx-standalone/include
SQLITE3 ?= libs/sqlite3
RUSTFLAGS ?= -O
RUSTLIBFLAGS ?= -O
CFLAGS ?= -Os
CXXFLAGS ?= -Os


.PHONY: all clean clean-sdl clean-opengles clean-encoding clean-sqlite

all: $(BIN)

$(BIN): $(SRC) $(RUSTSDL)/libsdl.dummy $(RUSTOPENGLES)/librustopengles.dummy $(RUSTENCODING)/libencoding.dummy $(RUSTSQLITE)/libsqlite.dummy
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) -L $(RUSTOPENGLES) -L $(RUSTENCODING) -L $(RUSTSQLITE) -L $(SQLITE3) $(CRATE) -o $(BIN)

$(RUSTSDL)/libsdl.dummy:
	cd $(RUSTSDL) && ./configure && $(MAKE) RUSTFLAGS="--cfg image --cfg mixer $(RUSTLIBFLAGS)"

$(RUSTOPENGLES)/librustopengles.dummy:
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTLIBFLAGS)" DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

$(RUSTENCODING)/libencoding.dummy:
	cd $(RUSTENCODING) && ./configure && $(MAKE) RUSTFLAGS="$(RUSTLIBFLAGS)"

$(RUSTSQLITE)/libsqlite.dummy: $(SQLITE3)/libsqlite3.a
	$(RUSTC) $(RUSTLIBFLAGS) -L $(SQLITE3) $(RUSTSQLITE)/lib.rs -o $@
	touch $@

$(SQLITE3)/libsqlite3.a:
	cd $(SQLITE3) && $(MAKE) all

clean: clean-sdl clean-opengles clean-encoding clean-sqlite
	rm -rf $(BIN)

clean-sdl:
	cd $(RUSTSDL) && $(MAKE) clean

clean-opengles:
	cd $(RUSTOPENGLES) && $(MAKE) clean

clean-encoding:
	cd $(RUSTENCODING) && $(MAKE) clean

clean-sqlite:
	cd $(SQLITE3) && $(MAKE) clean
	cd $(RUSTSQLITE) && rm -f *.so *.dylib *.dll *.dummy

