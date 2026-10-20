DUNE ?= dune
PROFILE ?= release
PREFIX ?= /usr/local
DESTDIR ?=
KDIR ?= /lib/modules/$(shell uname -r)/build

.PHONY: all controller module test install uninstall install-controller install-module uninstall-controller uninstall-module clean help

all: controller module

controller:
	$(DUNE) build --profile $(PROFILE) @all @install

module:
	$(MAKE) -C kernel KDIR="$(KDIR)" W=1

test: controller
	$(DUNE) runtest --profile $(PROFILE)

install: install-controller install-module

install-controller: controller
	$(DUNE) install --profile $(PROFILE) --prefix "$(PREFIX)" $(if $(DESTDIR),--destdir "$(DESTDIR)")

install-module: module
	$(MAKE) -C kernel KDIR="$(KDIR)" DESTDIR="$(DESTDIR)" install

uninstall: uninstall-controller uninstall-module

uninstall-controller:
	$(DUNE) uninstall --prefix "$(PREFIX)" $(if $(DESTDIR),--destdir "$(DESTDIR)")

uninstall-module:
	$(MAKE) -C kernel KDIR="$(KDIR)" DESTDIR="$(DESTDIR)" uninstall

clean:
	$(DUNE) clean
	$(MAKE) -C kernel KDIR="$(KDIR)" clean

help:
	@printf '%s\n' 'make                 build the controller and module' 'make test            run the portable tests' 'make install         install under PREFIX and the selected kernel' 'make uninstall       remove the installed controller and module' 'make install DESTDIR=/path/to/staging PREFIX=/usr' 'make KDIR=/path/to/kernel/build'
