#ifndef REMUS_PROTOCOL_H
#define REMUS_PROTOCOL_H

#include <linux/ioctl.h>
#include <linux/types.h>

#define REMUS_ABI_VERSION 1
#define REMUS_MAX_CPUS 64
#define REMUS_MAX_SHADOWS 128
#define REMUS_VMCALL_KEY 0xdeaddeadUL
#define REMUS_ACTION_STOP 0x100UL
#define REMUS_ACTION_INVALIDATE 0x101UL

struct remus_vmcall_command {
  __u32 action;
  __u32 reserved;
  __u64 buffer;
  __u64 size;
};

struct remus_root_parameters {
  __u64 callback;
  __u64 parameter;
};

struct remus_capability {
  __u32 cpu;
  __u32 flags;
  __u64 feature_control;
  __u64 basic;
  __u64 ept_vpid;
  __u64 pin;
  __u64 primary;
  __u64 secondary;
  __u64 exit;
  __u64 entry;
  __u64 cr0_fixed0;
  __u64 cr0_fixed1;
  __u64 cr4_fixed0;
  __u64 cr4_fixed1;
  __u64 cr0;
  __u64 cr4;
  __u64 cr3;
};

struct remus_probe {
  __u32 version;
  __u32 count;
  __u32 active;
  __u32 reserved;
  struct remus_capability cpus[REMUS_MAX_CPUS];
};

struct remus_control {
  __u32 cpu;
  __u32 reserved;
  __u32 pin;
  __u32 primary;
  __u32 secondary;
  __u32 exit;
  __u32 entry;
  __u32 reserved2;
};

struct remus_start {
  __u32 version;
  __u32 count;
  __u64 reserved;
  struct remus_control cpus[REMUS_MAX_CPUS];
};

struct remus_cpu_status {
  __u32 cpu;
  __s32 error;
  __u64 active;
  __u64 exits;
  __u64 cpuid;
  __u64 msr;
  __u64 cr3;
  __u64 ept;
  __u64 vmcall;
  __u64 failures;
  __u64 instruction_error;
  __u64 mtf;
};

struct remus_status {
  __u32 version;
  __u32 count;
  __u32 active;
  __u32 shadows;
  struct remus_cpu_status cpus[REMUS_MAX_CPUS];
};

struct remus_shadow_request {
  __u32 version;
  __u32 pid;
  __u64 address;
  __u32 id;
  __u32 reserved;
  __u64 physical_address;
  __u64 process_cr3;
  __u8 page[4096];
};

#define REMUS_IOCTL_PROBE _IOR('R', 0, struct remus_probe)
#define REMUS_IOCTL_START _IOW('R', 1, struct remus_start)
#define REMUS_IOCTL_STOP _IO('R', 2)
#define REMUS_IOCTL_STATUS _IOR('R', 3, struct remus_status)
#define REMUS_IOCTL_SHADOW _IOWR('R', 4, struct remus_shadow_request)
#define REMUS_IOCTL_UNSHADOW _IOW('R', 5, __u32)
#define REMUS_IOCTL_READ_PAGE _IOWR('R', 6, struct remus_shadow_request)

#endif
