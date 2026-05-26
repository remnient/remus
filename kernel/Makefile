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
.PHONY: all clean
all:
	$(MAKE) -C $(KDIR) M=$(CURDIR) modules
clean:
	$(MAKE) -C $(KDIR) M=$(CURDIR) clean
endif
