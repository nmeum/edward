# This Makefile is a wrapper around chicken-install(1) which eases building
# edward without superuser rights and without requiring any configuration
# of chicken-install. Furthermore, it supports optional dependency vendoring
# to build edward without network access.
#
# By default, the Makefile builds a statically linked binary. For development
# purposes it can be desirable to do dynamic linked builds instead. To do so
# the PACKAGE environment variable can be set to zero.
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DOCDIR ?= $(PREFIX)/share/doc/edward

# By default, build a statically linked version for packaging.
# If package is zero, build a dynamically linked version instead.
PACKAGE ?= 1
ifneq ($(PACKAGE),0)
	INSTALL_FLAGS = -feature package
endif

# Install all CHICKEN files relative to ./output
#
# See: https://bugs.call-cc.org/ticket/1792

# Respect CHICKEN_REPOSITORY_PATH if set in the environment
CHICKEN_REPOSITORY_PATH ?= $(shell env -i chicken-install -repository)

export CHICKEN_INSTALL_PREFIX := $(CURDIR)/output
export CHICKEN_INSTALL_REPOSITORY := $(CURDIR)/output
export CHICKEN_REPOSITORY_PATH := $(CURDIR)/output:$(CHICKEN_REPOSITORY_PATH)

# Optional dependency vendoring to build without network access
#
# Release tarballs include vendored dependencies (see make dist).

VENDOR_DIRECTORY = $(CURDIR)/vendor
ifneq ($(wildcard $(VENDOR_DIRECTORY)/*),)
	VENDORED=1
endif

all:
ifdef VENDORED
	chicken-install -location $(VENDOR_DIRECTORY) $(shell ls $(VENDOR_DIRECTORY))
endif
	chicken-install $(INSTALL_FLAGS)

check: export EDWARD=$(CHICKEN_INSTALL_PREFIX)/bin/edward
check:
	chicken-install -test
	@./tests/integration/run.sh
	@./tests/interactive/run.sh

bench:
	csi -quiet -script benchmarks/run.scm

install:
	install -Dm755 $(CHICKEN_INSTALL_PREFIX)/bin/edward "$(DESTDIR)$(BINDIR)/edward"
	install -Dm644 README.md "$(DESTDIR)$(DOCDIR)/README.md"

vendor:
	env -i CHICKEN_EGG_CACHE=$(VENDOR_DIRECTORY) chicken-install -r -recursive -test
	find $(VENDOR_DIRECTORY) \( -name STATUS -a -type f \) -exec rm {} +
# XXX: Make sure to remove the vendor directory before running `make dist`.
# As libraries are, unfortunately, build within the vendor directory.
dist: VERSION = $(shell git describe --tags)
dist: vendor
	mkdir -p edward-$(VERSION)
	cp -R LICENSE.txt GNUmakefile README.md bin edward.egg lib tests vendor edward-$(VERSION)
	tar -czf edward-$(VERSION).tar.gz edward-$(VERSION)
	rm -rf edward-$(VERSION)

.PHONY: all check bench install vendor dist
