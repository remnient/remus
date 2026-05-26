#include <linux/capability.h>
#include <linux/cpu.h>
#include <linux/fs.h>
#include <linux/miscdevice.h>
#include <linux/ioport.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/pid.h>
#include <linux/reboot.h>
#include <linux/sched/mm.h>
#include <linux/slab.h>
#include <linux/stop_machine.h>
#include <linux/suspend.h>
#include <linux/uaccess.h>
#include <linux/vmalloc.h>
#include "remus.h"

struct remus_shadow remus_shadows[REMUS_MAX_SHADOWS];
static DEFINE_MUTEX(control_lock);
static bool active;
static struct remus_cpu_status last_status[REMUS_MAX_CPUS];
static unsigned int last_count;
struct remus_ram_range remus_ram_ranges[256];
unsigned int remus_ram_count;

static int ram_above_limit(struct resource *resource, void *argument) {
  return resource->end >= REMUS_EPT_LIMIT ? -ERANGE : 0;
}

static int record_ram(struct resource *resource, void *argument) {
  if (remus_ram_count == ARRAY_SIZE(remus_ram_ranges)) return -E2BIG;
  remus_ram_ranges[remus_ram_count++] = (struct remus_ram_range) { resource->start, resource->end };
  return 0;
}

static void release_shadow(struct remus_shadow *shadow) {
  if (shadow->page) {
    if (shadow->mm) unpin_user_page(shadow->page);
    else put_page(shadow->page);
  }
  if (shadow->mm) mmput(shadow->mm);
  if (shadow->payload) free_page((unsigned long)shadow->payload);
  memset(shadow, 0, sizeof(*shadow));
}

static int refresh_cpu(void *argument) {
  remus_refresh_shadows(NULL);
  return 0;
}

static bool any_active(void) {
  unsigned int id;
  for_each_online_cpu(id) {
    struct remus_cpu *cpu = per_cpu(remus_cpu_data, id);
    if (cpu && READ_ONCE(cpu->status.active)) return true;
  }
  return false;
}

static void release_cpus(void) {
  unsigned int id, i;
  last_count = 0;
  for_each_online_cpu(id) {
    struct remus_cpu *cpu = per_cpu(remus_cpu_data, id);
    if (cpu && last_count < REMUS_MAX_CPUS) last_status[last_count++] = cpu->status;
    per_cpu(remus_cpu_data, id) = NULL;
    remus_free_cpu(cpu);
  }
  for (i = 0; i < REMUS_MAX_SHADOWS; i++) release_shadow(&remus_shadows[i]);
}

static int stop_locked(void) {
  int error;
  if (!active) return 0;
  error = stop_machine(remus_stop_cpu, NULL, cpu_online_mask);
  if (any_active()) return error ?: -EIO;
  release_cpus();
  active = false;
  cpu_hotplug_enable();
  module_put(THIS_MODULE);
  return error;
}

static int start_locked(struct remus_start *request) {
  unsigned int id, count = 0;
  int error;
  if (active) return -EALREADY;
  if (request->version != REMUS_ABI_VERSION || request->reserved) return -EINVAL;
  cpu_hotplug_disable();
  error = walk_iomem_res_desc(IORES_DESC_NONE, IORESOURCE_SYSTEM_RAM, REMUS_EPT_LIMIT, U64_MAX, NULL, ram_above_limit);
  if (error && error != -EINVAL) goto release;
  if (num_online_cpus() > REMUS_MAX_CPUS || request->count != num_online_cpus()) { error = -EINVAL; goto release; }
  remus_ram_count = 0;
  error = walk_iomem_res_desc(IORES_DESC_NONE, IORESOURCE_SYSTEM_RAM, 0, U64_MAX, NULL, record_ram);
  if (error || !remus_ram_count) { error = error ?: -ENXIO; goto release; }
  for_each_online_cpu(id) {
    struct remus_cpu *cpu;
    if (request->cpus[count].cpu != id || request->cpus[count].reserved || request->cpus[count].reserved2) {
      error = -EINVAL; goto release;
    }
    cpu = kzalloc(sizeof(*cpu), GFP_KERNEL);
    if (!cpu) { error = -ENOMEM; goto release; }
    INIT_LIST_HEAD(&cpu->splits);
    cpu->status.cpu = id;
    cpu->control = request->cpus[count++];
    per_cpu(remus_cpu_data, id) = cpu;
    error = smp_call_function_single(id, remus_capture, cpu, 1);
    if (error) goto release;
    error = remus_allocate_cpu(cpu);
    if (error) { cpu->status.error = error; goto release; }
  }
  __module_get(THIS_MODULE);
  active = true;
  error = stop_machine(remus_start_cpu, NULL, cpu_online_mask);
  if (error) {
    int rollback = stop_locked();
    if (rollback) pr_err("remus: rollback failed: %d\n", rollback);
  }
  return error;
release:
  release_cpus();
  cpu_hotplug_enable();
  return error;
}

static int fill_probe(struct remus_probe *probe) {
  struct remus_cpu *sample;
  unsigned int id;
  int error = 0;
  sample = kzalloc(sizeof(*sample), GFP_KERNEL);
  if (!sample) return -ENOMEM;
  probe->version = REMUS_ABI_VERSION;
  probe->active = active;
  cpus_read_lock();
  if (num_online_cpus() > REMUS_MAX_CPUS) { error = -E2BIG; goto out; }
  for_each_online_cpu(id) {
    memset(&sample->capability, 0, sizeof(sample->capability));
    error = smp_call_function_single(id, remus_capture, sample, 1);
    if (error) break;
    probe->cpus[probe->count++] = sample->capability;
  }
out:
  cpus_read_unlock();
  kfree(sample);
  return error;
}

static int pin_target(struct remus_shadow *shadow, struct remus_shadow_request *request, bool read_only) {
  if (request->version != REMUS_ABI_VERSION || request->reserved) return -EINVAL;
  if (request->pid) {
    struct pid *pid = find_get_pid(request->pid);
    struct task_struct *task = pid ? get_pid_task(pid, PIDTYPE_PID) : NULL;
    int locked = 1;
    long pinned;
    if (pid) put_pid(pid);
    if (!task) return -ESRCH;
    shadow->mm = get_task_mm(task);
    put_task_struct(task);
    if (!shadow->mm) return -ESRCH;
    mmap_read_lock(shadow->mm);
    pinned = pin_user_pages_remote(shadow->mm, request->address & PAGE_MASK, 1,
                                   FOLL_LONGTERM, &shadow->page, &locked);
    if (locked) mmap_read_unlock(shadow->mm);
    if (pinned != 1) return pinned < 0 ? pinned : -EFAULT;
    shadow->process_cr3 = __pa(shadow->mm->pgd);
  } else {
    void *address = (void *)(request->address & PAGE_MASK);
    if (request->address < TASK_SIZE_MAX || (!read_only && within_module(request->address, THIS_MODULE))) return -EINVAL;
    shadow->page = is_vmalloc_addr(address) ? vmalloc_to_page(address) :
                   (virt_addr_valid(address) ? virt_to_page(address) : NULL);
    if (!shadow->page) return -EFAULT;
    get_page(shadow->page);
  }
  shadow->address = page_to_phys(shadow->page);
  if (shadow->address >= REMUS_EPT_LIMIT) return -ERANGE;
  return 0;
}

static int read_target(struct remus_shadow_request *request) {
  struct remus_shadow target = { 0 };
  int error = pin_target(&target, request, true);
  if (!error) {
    request->physical_address = target.address;
    request->process_cr3 = target.process_cr3;
    memcpy(request->page, page_address(target.page), PAGE_SIZE);
  }
  release_shadow(&target);
  return error;
}

static int install_shadow(struct remus_shadow_request *request) {
  struct remus_shadow *shadow;
  unsigned int slot, id;
  int error;
  if (!active) return -ENXIO;
  for (slot = 0; slot < REMUS_MAX_SHADOWS; slot++) if (!remus_shadows[slot].page) break;
  if (slot == REMUS_MAX_SHADOWS) return -ENOSPC;
  shadow = &remus_shadows[slot];
  shadow->payload = (void *)get_zeroed_page(GFP_KERNEL);
  if (!shadow->payload) return -ENOMEM;
  memcpy(shadow->payload, request->page, PAGE_SIZE);
  error = pin_target(shadow, request, false);
  if (error) goto release;
  for (id = 0; id < REMUS_MAX_SHADOWS; id++)
    if (id != slot && remus_shadows[id].page && remus_shadows[id].address == shadow->address) { error = -EEXIST; goto release; }
  for_each_online_cpu(id) {
    error = remus_split_page(per_cpu(remus_cpu_data, id), shadow->address);
    if (error) goto release;
  }
  request->id = slot;
  request->physical_address = shadow->address;
  request->process_cr3 = shadow->process_cr3;
  memcpy(request->page, page_address(shadow->page), PAGE_SIZE);
  smp_wmb();
  shadow->enabled = true;
  error = stop_machine(refresh_cpu, NULL, cpu_online_mask);
  if (!error) return 0;
  shadow->enabled = false;
  stop_machine(refresh_cpu, NULL, cpu_online_mask);
release:
  release_shadow(shadow);
  return error;
}

static int remove_shadow(u32 slot) {
  int error;
  if (!active) return -ENXIO;
  if (slot >= REMUS_MAX_SHADOWS || !remus_shadows[slot].page) return -ENOENT;
  remus_shadows[slot].enabled = false;
  error = stop_machine(refresh_cpu, NULL, cpu_online_mask);
  if (error) { remus_shadows[slot].enabled = true; return error; }
  release_shadow(&remus_shadows[slot]);
  return 0;
}

static long remus_ioctl(struct file *file, unsigned int command, unsigned long user) {
  void __user *pointer = (void __user *)user;
  void *buffer = NULL;
  unsigned int size = _IOC_SIZE(command), id;
  int error = 0;
  if (!capable(CAP_SYS_RAWIO)) return -EPERM;
  if (command != REMUS_IOCTL_PROBE && command != REMUS_IOCTL_START && command != REMUS_IOCTL_STOP
      && command != REMUS_IOCTL_STATUS && command != REMUS_IOCTL_SHADOW && command != REMUS_IOCTL_UNSHADOW
      && command != REMUS_IOCTL_READ_PAGE) return -ENOTTY;
  if (size) {
    buffer = kvzalloc(size, GFP_KERNEL);
    if (!buffer) return -ENOMEM;
    if ((_IOC_DIR(command) & _IOC_WRITE) && copy_from_user(buffer, pointer, size)) { error = -EFAULT; goto free; }
  }
  if (mutex_lock_interruptible(&control_lock)) { error = -ERESTARTSYS; goto free; }
  switch (command) {
    case REMUS_IOCTL_PROBE: error = fill_probe(buffer); break;
    case REMUS_IOCTL_START: error = start_locked(buffer); break;
    case REMUS_IOCTL_STOP: error = stop_locked(); break;
    case REMUS_IOCTL_SHADOW: error = install_shadow(buffer); break;
    case REMUS_IOCTL_READ_PAGE: error = read_target(buffer); break;
    case REMUS_IOCTL_UNSHADOW: error = remove_shadow(*(u32 *)buffer); break;
    case REMUS_IOCTL_STATUS: {
      struct remus_status *status = buffer;
      status->version = REMUS_ABI_VERSION;
      status->active = active;
      for (id = 0; id < REMUS_MAX_SHADOWS; id++) if (remus_shadows[id].enabled) status->shadows++;
      cpus_read_lock();
      for_each_online_cpu(id) {
        struct remus_cpu *cpu = per_cpu(remus_cpu_data, id);
        if (status->count >= REMUS_MAX_CPUS) { error = -E2BIG; break; }
        if (cpu) status->cpus[status->count++] = cpu->status;
        else {
          unsigned int old;
          status->cpus[status->count].cpu = id;
          for (old = 0; old < last_count; old++)
            if (last_status[old].cpu == id) status->cpus[status->count] = last_status[old];
          status->count++;
        }
      }
      cpus_read_unlock();
      break;
    }
  }
  if (!error && (_IOC_DIR(command) & _IOC_READ) && copy_to_user(pointer, buffer, size)) {
    if (command == REMUS_IOCTL_SHADOW) remove_shadow(((struct remus_shadow_request *)buffer)->id);
    error = -EFAULT;
  }
  mutex_unlock(&control_lock);
free:
  kvfree(buffer);
  return error;
}

static const struct file_operations remus_operations = {
  .owner = THIS_MODULE,
  .unlocked_ioctl = remus_ioctl,
  .compat_ioctl = remus_ioctl,
  .llseek = noop_llseek
};

static struct miscdevice remus_device = {
  .minor = MISC_DYNAMIC_MINOR,
  .name = "remus",
  .fops = &remus_operations,
  .mode = 0600
};

static int reboot_callback(struct notifier_block *block, unsigned long event, void *data) {
  int error;
  mutex_lock(&control_lock);
  error = stop_locked();
  mutex_unlock(&control_lock);
  return error ? NOTIFY_BAD : NOTIFY_DONE;
}

static int power_callback(struct notifier_block *block, unsigned long event, void *data) {
  int result = NOTIFY_DONE;
  mutex_lock(&control_lock);
  if ((event == PM_SUSPEND_PREPARE || event == PM_HIBERNATION_PREPARE) && active) result = NOTIFY_BAD;
  mutex_unlock(&control_lock);
  return result;
}

static struct notifier_block reboot_notifier = { .notifier_call = reboot_callback };
static struct notifier_block power_notifier = { .notifier_call = power_callback };

static int __init remus_init(void) {
  int error;
  BUILD_BUG_ON(offsetof(struct remus_cpu, return_rip) != 128);
  BUILD_BUG_ON(offsetof(struct remus_cpu, return_ss) != 160);
  BUILD_BUG_ON(sizeof(struct remus_capability) != 128);
  BUILD_BUG_ON(sizeof(struct remus_cpu_status) != 88);
  error = misc_register(&remus_device);
  if (error) return error;
  error = register_reboot_notifier(&reboot_notifier);
  if (error) goto misc;
  error = register_pm_notifier(&power_notifier);
  if (error) goto reboot;
  return 0;
reboot:
  unregister_reboot_notifier(&reboot_notifier);
misc:
  misc_deregister(&remus_device);
  return error;
}

static void __exit remus_exit(void) {
  unregister_pm_notifier(&power_notifier);
  unregister_reboot_notifier(&reboot_notifier);
  misc_deregister(&remus_device);
}

module_init(remus_init);
module_exit(remus_exit);
MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("Remus native Intel VT-x backend");
