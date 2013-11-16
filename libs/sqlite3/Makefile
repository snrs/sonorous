INC = sqlite3.h
SRC = sqlite3.c
OBJ = sqlite3.o
LIB = libsqlite3.a

AR ?= ar
CC ?= gcc
CFLAGS ?= -Os

.PHONY: all clean

all: $(LIB)

$(OBJ): $(SRC) $(INC)
	$(CC) $(CFLAGS) -c $< -o $@

$(LIB): $(OBJ)
	$(AR) ru $@ $(OBJ)

clean:
	rm -rf $(OBJ) $(LIB)

