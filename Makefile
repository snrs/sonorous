SRC = $(wildcard src/*.rs src/*/*.rs src/*/*/*.rs src/*/*/*/*.rs)
CRATE = src/sonorous.rs
BIN = sonorous
RUSTC = rustc
RUSTDOC = rustdoc
RUSTSDL = libs/rust-sdl
RUSTFLAGS = -O


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC) $(RUSTSDL)/libsdl.dummy
	$(RUSTC) $(RUSTFLAGS) -L $(RUSTSDL) $(CRATE) -o $(BIN)

$(RUSTSDL)/libsdl.dummy:
	cd $(RUSTSDL) && ./configure && $(MAKE)

clean:
	rm -rf $(BIN)

