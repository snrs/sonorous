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
DIRECTX_SDK_INCLUDES ?= libs/w32api-directx-standalone/include
RUSTFLAGS ?= -O


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(RUSTSDL)/libsdl.dummy $(RUSTOPENGLES)/libopengles.dummy
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) -L $(RUSTOPENGLES) $(CRATE) -o $(BIN)

$(RUSTSDL)/libsdl.dummy:
	cd $(RUSTSDL) && ./configure && $(MAKE) RUSTC=$(RUSTC)

$(RUSTOPENGLES)/libopengles.dummy:
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) RUSTC=$(RUSTC) DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

clean:
	rm -rf $(BIN)
