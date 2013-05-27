SRC = src/sonorous.rs
BIN = sonorous
RUSTC = rustc
RUSTDOC = rustdoc
RUSTFLAGS = -O -L rust-sdl


.PHONY: all clean

all: $(BIN)

$(BIN): $(SRC)
	$(RUSTC) $(RUSTFLAGS) $(SRC) -o $(BIN)

clean:
	rm -rf $(BIN)

