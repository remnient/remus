DUNE ?= dune
PROFILE ?= release
PREFIX ?= /usr/local
DESTDIR ?=
KDIR ?= /lib/modules/$(shell uname -r)/build
DISTDIR ?= _build/dist
SOURCE_DATE_EPOCH ?= 0

.PHONY: all controller module test install uninstall install-controller install-module uninstall-controller uninstall-module clean help dist distcheck

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

dist:
	mkdir -p "$(DISTDIR)"
	tar --sort=name --format=gnu --mtime="@$(SOURCE_DATE_EPOCH)" --owner=0 --group=0 --numeric-owner --mode='u+rwX,go+rX,go-w' \
	  --exclude='*.o' --exclude='*.ko' --exclude='*.mod' --exclude='*.mod.c' --exclude='.*.cmd' --exclude='.tmp_versions' \
	  --exclude='Module.symvers' --exclude='modules.order' --transform='s,^,remus/,' -cf "$(DISTDIR)/remus.tar" \
	  .gitattributes .gitignore .github README.md Makefile dune dune-project remus.opam assets bin kernel lib test $(wildcard LICENSE NOTICE)
	gzip -n -c "$(DISTDIR)/remus.tar" > "$(DISTDIR)/remus.tar.gz.tmp"
	mv "$(DISTDIR)/remus.tar.gz.tmp" "$(DISTDIR)/remus.tar.gz"
	rm "$(DISTDIR)/remus.tar"
	cd "$(DISTDIR)" && sha256sum remus.tar.gz > SHA256

distcheck: dist
	@set -eu; \
	(cd "$(DISTDIR)" && sha256sum --check SHA256); \
	package_directory=$$(mktemp -d); \
	trap 'rm -rf "$$package_directory"' EXIT; \
	tar -xzf "$(DISTDIR)/remus.tar.gz" -C "$$package_directory"; \
	$(MAKE) -C "$$package_directory/remus" all test KDIR="$(KDIR)"; \
	$(MAKE) -C "$$package_directory/remus" install KDIR="$(KDIR)" DESTDIR="$$package_directory/stage" PREFIX=/usr/local; \
	"$$package_directory/stage/usr/local/bin/remus" --help; \
	$(MAKE) -C "$$package_directory/remus" uninstall KDIR="$(KDIR)" DESTDIR="$$package_directory/stage" PREFIX=/usr/local; \
	test -z "$$(find "$$package_directory/stage" -type f -o -type l)"

help:
	@printf '%s\n' 'make                 build the controller and module' 'make test            run the portable tests' 'make install         install under PREFIX and the selected kernel' 'make uninstall       remove the installed controller and module' 'make dist            create a reproducible source archive and checksum' 'make distcheck       build, test, install and uninstall the source package' 'make install DESTDIR=/path/to/staging PREFIX=/usr' 'make install SIGNING_KEY=/path/to/key SIGNING_CERT=/path/to/certificate' 'make KDIR=/path/to/kernel/build'
