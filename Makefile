# This is a part of Sonorous.
# Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
# See README.md and LICENSE.txt for details.

TARGET ?= target
TARGETREL ?= target/release

RUSTOPENGLESANGLE ?= libs/rust-opengles
DIRECTX_SDK_INCLUDES ?= libs/w32api-directx-standalone/include
SQLITE3 ?= libs/sqlite3

GIT ?= git
DLLTOOL ?= dlltool
CFLAGS ?= -Os
CXXFLAGS ?= -Os

.PHONY: all
all:
	@echo 'Use Cargo to build Sonorous.'
	@echo 'This Makefile is used for external dependencies. In particular, use `$(MAKE) deps` for copying non-system dependencies into `target/` and `target/release/`.'

.PHONY: update-git-submodule
update-git-submodule:
	$(GIT) submodule init
	$(GIT) submodule update

.PHONY: deps
deps: \
	update-git-submodule \
	$(TARGET)/libGLESv2.dll \
	$(TARGET)/libEGL.dll \
	$(TARGETREL)/libGLESv2.dll \
	$(TARGETREL)/libEGL.dll

.PHONY: angle
angle:
	cd $(RUSTOPENGLESANGLE) && $(MAKE) angle DIRECTX_SDK_INCLUDES=$(realpath $(DIRECTX_SDK_INCLUDES))

$(RUSTOPENGLESANGLE)/angle/src/libGLESv2.dll $(RUSTOPENGLESANGLE)/angle/src/libEGL.dll: angle
$(TARGET)/libGLESv2.dll: $(RUSTOPENGLESANGLE)/angle/src/libGLESv2.dll
	mkdir -p $(dir $@)
	cp $< $@
$(TARGET)/libEGL.dll: $(RUSTOPENGLESANGLE)/angle/src/libEGL.dll
	mkdir -p $(dir $@)
	cp $< $@
$(TARGETREL)/libGLESv2.dll: $(RUSTOPENGLESANGLE)/angle/src/libGLESv2.dll
	mkdir -p $(dir $@)
	cp $< $@
$(TARGETREL)/libEGL.dll: $(RUSTOPENGLESANGLE)/angle/src/libEGL.dll
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: rust-deps
ifneq (,$(findstring MINGW,$(shell uname -s)))
rust-deps: \
	update-git-submodule \
	$(OUT_DIR)/libsqlite3.a \
	$(OUT_DIR)/libGLESv2.dll \
	$(OUT_DIR)/libEGL.dll
else
rust-deps:
endif

$(SQLITE3)/libsqlite3.a:
	cd $(SQLITE3) && $(MAKE) all

$(OUT_DIR)/libsqlite3.a: $(SQLITE3)/libsqlite3.a
	cp $< $@
# XXX technically we may be able to use .def files instead of .dll, but it currently doesn't work
$(OUT_DIR)/libGLESv2.dll: $(RUSTOPENGLESANGLE)/angle/src/libGLESv2.dll
	cp $< $@
$(OUT_DIR)/libEGL.dll: $(RUSTOPENGLESANGLE)/angle/src/libEGL.dll
	cp $< $@

.PHONY: clean-all clean-angle clean-sqlite3
clean-all: clean-angle clean-sqlite3
clean-angle:
	cd $(RUSTOPENGLESANGLE) && $(MAKE) clean-all
clean-sqlite3:
	cd $(SQLITE3) && $(MAKE) clean

