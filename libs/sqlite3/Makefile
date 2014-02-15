INC = sqlite3.h
SRC = sqlite3.c
OBJ = sqlite3.o
LIB = libsqlite3.a

AR ?= ar
CC ?= gcc
# 20140215 temporary fix to avoid the travis build failure
#CFLAGS ?= -Os -fPIC
CFLAGS ?= -Os -fPIC -DSQLITE_DISABLE_LFS

.PHONY: all clean

all: $(LIB)

$(OBJ): $(SRC) $(INC)
	$(CC) $(CFLAGS) -c $< -o $@

$(LIB): $(OBJ)
	$(AR) ru $@ $(OBJ)

clean:
	rm -rf $(OBJ) $(LIB)

