SRC = $(wildcard src/*.rs src/*/*.rs src/*/*/*.rs src/*/*/*/*.rs)
CRATE = src/sonorous.rs
BIN = sonorous
RUSTC ?= rustc
RUSTDOC ?= rustdoc
RUSTSDL ?= libs/rust-sdl
RUSTOPENGLES ?= libs/rust-opengles
DIRECTX_SDK_INCLUDES ?= libs/w32api-directx-standalone/include
RUSTFLAGS ?= -O


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(RUSTSDL)/libsdl.dummy $(RUSTOPENGLES)/libopengles.dummy
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) -L $(RUSTOPENGLES) $(CRATE) -o $(BIN)

$(RUSTSDL)/libsdl.dummy:
	cd $(RUSTSDL) && ./configure && $(MAKE)

$(RUSTOPENGLES)/libopengles.dummy:
	cd $(RUSTOPENGLES) && ./configure && $(MAKE) DIRECTX_SDK_INCLUDES=$(DIRECTX_SDK_INCLUDES)

clean:
	rm -rf $(BIN)

