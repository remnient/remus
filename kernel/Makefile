KDIR ?= /lib/modules/$(shell uname -r)/build

ifneq ($(KERNELRELEASE),)
obj-m += remus.o
remus-y := control.o vmx.o ept.o memory.o entry.o
CFLAGS_REMOVE_vmx.o += $(CC_FLAGS_FTRACE)
CFLAGS_REMOVE_ept.o += $(CC_FLAGS_FTRACE)
CFLAGS_REMOVE_memory.o += $(CC_FLAGS_FTRACE)
KASAN_SANITIZE_vmx.o := n
KASAN_SANITIZE_ept.o := n
KASAN_SANITIZE_memory.o := n
KCSAN_SANITIZE_vmx.o := n
KCSAN_SANITIZE_ept.o := n
KCSAN_SANITIZE_memory.o := n
KCOV_INSTRUMENT_vmx.o := n
KCOV_INSTRUMENT_ept.o := n
KCOV_INSTRUMENT_memory.o := n
else
.PHONY: all clean install uninstall
all:
	$(MAKE) -C "$(KDIR)" M="$(CURDIR)" modules
clean:
	$(MAKE) -C "$(KDIR)" M="$(CURDIR)" clean
install: all
	@set -eu; release="$$($(MAKE) --no-print-directory -s -C "$(KDIR)" kernelrelease)"; \
	destination="$(DESTDIR)/lib/modules/$$release/extra/remus.ko"; \
	install -D -m 644 remus.ko "$$destination"; \
	if test -n "$(SIGNING_KEY)$(SIGNING_CERT)"; then \
	  test -n "$(SIGNING_KEY)" && test -n "$(SIGNING_CERT)"; \
	  "$(KDIR)/scripts/sign-file" sha256 "$(SIGNING_KEY)" "$(SIGNING_CERT)" "$$destination"; \
	fi; \
	if test -z "$(DESTDIR)"; then depmod -a "$$release"; fi
uninstall:
	@release="$$($(MAKE) --no-print-directory -s -C "$(KDIR)" kernelrelease)"; \
	for suffix in ko ko.gz ko.xz ko.zst; do rm -f "$(DESTDIR)/lib/modules/$$release/extra/remus.$$suffix"; done; \
	if test -z "$(DESTDIR)"; then depmod -a "$$release"; fi
endif
