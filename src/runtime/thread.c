/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include "sbcl.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "dynbind.h"
#include "genesis/cons.h"
#include "genesis/fdefn.h"
#include "genesis/vector.h"
#include "interr.h"             /* for lose() */
#include "alloc.h"
#include "gc-internal.h"
#include "getallocptr.h"
#include "interrupt.h"
#include "lispregs.h"

#ifdef LISP_FEATURE_SB_THREAD

#if defined LISP_FEATURE_OPENBSD || defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_DRAGONFLY
#include <pthread_np.h>
#endif

#ifdef LISP_FEATURE_SUNOS
#include <thread.h>
#endif
#endif

int dynamic_values_bytes = 4096 * sizeof(lispobj);  // same for all threads
// exposed to lisp for pthread_create if not C_STACK_IS_CONTROL_STACK
os_vm_size_t thread_alien_stack_size = ALIEN_STACK_SIZE;
struct thread *all_threads;

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;

static __attribute__((unused)) pthread_mutex_t create_thread_lock = PTHREAD_MUTEX_INITIALIZER;

#ifdef LISP_FEATURE_GCC_TLS
__thread struct thread *current_thread;
#elif !defined LISP_FEATURE_WIN32
pthread_key_t specials = 0;
#endif
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs)
# ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
# endif
    ;
#endif

static void
link_thread(struct thread *th)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    all_threads=th;
}

#ifdef LISP_FEATURE_SB_THREAD
static void
unlink_thread(struct thread *th)
{
    if (th->prev)
        th->prev->next = th->next;
    else
        all_threads = th->next;
    if (th->next)
        th->next->prev = th->prev;
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
/* Only access thread state with blockables blocked. */
lispobj
thread_state(struct thread *thread)
{
    lispobj state;
    sigset_t old;
    block_blockable_signals(&old);
    os_sem_wait(thread->state_sem, "thread_state");
    state = thread->state;
    os_sem_post(thread->state_sem, "thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return state;
}

void
set_thread_state(struct thread *thread, lispobj state,
                 boolean signals_already_blocked) // for foreign thread
{
    int i, waitcount = 0;
    sigset_t old;
    // If we've already masked the blockable signals we can avoid two syscalls here.
    if (!signals_already_blocked)
        block_blockable_signals(&old);
    os_sem_wait(thread->state_sem, "set_thread_state");
    if (thread->state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_running_waitcount;
            thread->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_running_sem, "set_thread_state (not running)");
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_stopped_waitcount;
            thread->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_stopped_sem, "set_thread_state (not stopped)");
        }
        thread->state = state;
    }
    os_sem_post(thread->state_sem, "set_thread_state");
    if (!signals_already_blocked)
        thread_sigmask(SIG_SETMASK, &old, NULL);
}

void
wait_for_thread_state_change(struct thread *thread, lispobj state)
{
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(&old);
  start:
    os_sem_wait(thread->state_sem, "wait_for_thread_state_change");
    if (thread->state == state) {
        switch (state) {
        case STATE_RUNNING:
            wait_sem = thread->state_not_running_sem;
            thread->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = thread->state_not_stopped_sem;
            thread->state_not_stopped_waitcount++;
            break;
        default:
            lose("Invalid state in wait_for_thread_state_change: %"OBJ_FMTX, state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(thread->state_sem, "wait_for_thread_state_change");
    if (wait_sem) {
        os_sem_wait(wait_sem, "wait_for_thread_state_change");
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
}
#endif /* sb-safepoint */
#endif /* sb-thread */

#ifdef LISP_FEATURE_WIN32
#define sb_GetTID() GetCurrentThreadId()
#elif defined __linux__
// gettid() was added in glibc 2.30 but we support older glibc
int sb_GetTID() { return syscall(SYS_gettid); }
#else
#define sb_GetTID() 0
#endif

// Only a single 'attributes' object is used if #+pauseless-threadstart.
// This is ok because creation is synchronized by *MAKE-THREAD-LOCK*.
#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_WIN32
pthread_attr_t new_lisp_thread_attr;
#define init_shared_attr_object() (pthread_attr_init(&new_lisp_thread_attr)==0)
#else
#define init_shared_attr_object() (1)
#endif
struct thread *alloc_thread_struct(void*,lispobj);

#ifndef LISP_FEATURE_SB_THREAD
# define ASSIGN_CURRENT_THREAD(dummy)
#elif defined LISP_FEATURE_GCC_TLS
# define ASSIGN_CURRENT_THREAD(x) current_thread = x
#elif !defined LISP_FEATURE_WIN32
# define ASSIGN_CURRENT_THREAD(x) pthread_setspecific(specials, x)
#elif defined LISP_FEATURE_64_BIT
# define ASSIGN_CURRENT_THREAD(x) \
  ((struct pthread_thread*)TlsGetValue(thread_self_tls_index))->vm_thread = x
#else
# define ASSIGN_CURRENT_THREAD(x) TlsSetValue(OUR_TLS_INDEX, x)
#endif

#ifdef LISP_FEATURE_WIN32
// Need a function callable from assembly code. The inline one won't do.
// This is basically pthread_getspecific without an argument.
void* get_current_vm_thread() {
  return arch_os_get_current_thread();
}
#endif

#ifdef LISP_FEATURE_DARWIN
    extern pthread_key_t sigwait_bug_mitigation;
#endif

void create_main_lisp_thread(lispobj function) {
    struct thread *th = alloc_thread_struct(0, NO_TLS_VALUE_MARKER_WIDETAG);
    if (!th || arch_os_thread_init(th)==0 || !init_shared_attr_object())
        lose("can't create initial thread");
#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_GCC_TLS && !defined LISP_FEATURE_WIN32
    pthread_key_create(&specials, 0);
#endif
#ifdef LISP_FEATURE_DARWIN
    pthread_key_create(&sigwait_bug_mitigation, 0);
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    lispobj *args = NULL;
#endif
    ASSIGN_CURRENT_THREAD(th);
#if defined THREADS_USING_GCSIGNAL && \
    (defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV)
    /* SIG_STOP_FOR_GC defaults to blocked on PPC? */
    unblock_gc_signals();
#endif
    link_thread(th);
    th->os_kernel_tid = sb_GetTID();
    th->os_thread = thread_self();
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_hard_guard_page(1, NULL);
#endif
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_guard_page(1, NULL);
#endif
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);

#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_X86_64)
    set_thread_stack(th->control_stack_end);
#endif

    /* WIN32 has a special stack arrangement, calling
     * call_into_lisp_first_time will put the new stack in the middle
     * of the current stack */
#if !(defined(LISP_FEATURE_WIN32) && !defined(OS_THREAD_STACK)) \
    && (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    call_into_lisp_first_time(function,args,0);
#else
    funcall0(function);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD

void free_thread_struct(struct thread *th)
{
#if defined(LISP_FEATURE_WIN32)
    os_invalidate_free((os_vm_address_t) th->os_address, THREAD_STRUCT_SIZE);
#else
    os_invalidate((os_vm_address_t) th->os_address, THREAD_STRUCT_SIZE);
#endif
}

/* Note: scribble must be stack-allocated */
static void
init_new_thread(struct thread *th,
                init_thread_data __attribute__((unused)) *scribble,
                int guardp,
                int retain_all_threads_lock)
{
    int lock_ret;

    ASSIGN_CURRENT_THREAD(th);
    if(arch_os_thread_init(th)==0) {
        /* FIXME: handle error */
        lose("arch_os_thread_init failed");
    }

    th->os_thread=thread_self();

#define GUARD_CONTROL_STACK 1
#define GUARD_BINDING_STACK 2
#define GUARD_ALIEN_STACK   4

    if (guardp & GUARD_CONTROL_STACK)
        protect_control_stack_guard_page(1, NULL);
    if (guardp & GUARD_BINDING_STACK)
        protect_binding_stack_guard_page(1, NULL);
    if (guardp & GUARD_ALIEN_STACK)
        protect_alien_stack_guard_page(1, NULL);

    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it, there is no
     * danger of deadlocking even with SIG_STOP_FOR_GC blocked (which
     * it is not). */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    *th->csp_around_foreign_call = (lispobj)scribble;
#endif
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    link_thread(th);
    if (!retain_all_threads_lock) {
        lock_ret = pthread_mutex_unlock(&all_threads_lock);
        gc_assert(lock_ret == 0);
    }

    /* Kludge: Changed the order of some steps between the safepoint/
     * non-safepoint versions of this code.  Can we unify this more?
     */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    WITH_GC_STATE_LOCK {
        gc_state_wait(GC_NONE);
    }
    push_gcing_safety(&scribble->safety);
#endif
}

static void
unregister_thread(struct thread *th,
                  init_thread_data __attribute__((unused)) *scribble)
{
    int lock_ret;

    /* Kludge: Changed the order of some steps between the safepoint/
     * non-safepoint versions of this code.  Can we unify this more?
     */
#ifdef LISP_FEATURE_SB_SAFEPOINT

    block_blockable_signals(0);
    ensure_region_closed(&th->alloc_region, BOXED_PAGE_FLAG);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    ensure_region_closed(&th->sprof_alloc_region, BOXED_PAGE_FLAG);
#endif
    pop_gcing_safety(&scribble->safety);
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    unlink_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

#else

    /* Block GC */
    block_blockable_signals(0);
    /* This state change serves to "acknowledge" any stop-the-world
     * signal received while the STOP_FOR_GC signal is blocked */
    set_thread_state(th, STATE_DEAD, 1);

    /* SIG_STOP_FOR_GC is blocked and GC might be waiting for this
     * thread, but since we are either exiting lisp code as a lisp
     * thread that is dying, or exiting lisp code to return to
     * former status as a C thread, it won't wait long. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    /* FIXME: this nests the free_pages_lock inside the all_threads_lock.
     * There's no reason for that, so closing of regions should be done
     * sooner to eliminate an ordering constraint. */
    ensure_region_closed(&th->alloc_region, BOXED_PAGE_FLAG);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    ensure_region_closed(&th->sprof_alloc_region, BOXED_PAGE_FLAG);
#endif
    unlink_thread(th);
    pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

#endif

    arch_os_thread_cleanup(th);

#ifndef LISP_FEATURE_SB_SAFEPOINT
    os_sem_destroy(th->state_sem);
    os_sem_destroy(th->state_not_running_sem);
    os_sem_destroy(th->state_not_stopped_sem);
#endif


#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    mach_lisp_thread_destroy(th);
#endif

#if defined(LISP_FEATURE_WIN32)
    int i;
    for (i = 0; i<
             (int) (sizeof(th->private_events.events)/
                    sizeof(th->private_events.events[0])); ++i) {
      CloseHandle(th->private_events.events[i]);
    }
#ifndef LISP_FEATURE_64_BIT
    TlsSetValue(OUR_TLS_INDEX,NULL);
#endif
#endif

    /* Undo the association of the current pthread to its `struct thread',
     * such that we can call arch_os_get_current_thread() later in this
     * thread and cleanly get back NULL. */
    /* FIXME: what if, after we blocked signals, someone uses INTERRUPT-THREAD
     * on this thread? It's no longer a lisp thread; I suspect the signal
     * will be redirected to a lisp thread.
     * Can anything else go wrong with other signals? Nothing else should
     * direct signals specifically to this thread. Per-process signals are ok
     * because the kernel picks a thread in which a signal isn't blocked */
    ASSIGN_CURRENT_THREAD(NULL);
}

#ifdef LISP_FEATURE_WIN32
#define THREAD_TRAMPOLINE_PROLOGUE \
    extern void set_thread_self(pthread_t thread); \
    pthread_t self = (pthread_t) arg; \
    self->teb = NtCurrentTeb(); \
    set_thread_self(self); \
    struct thread* th = self->vm_thread
#define THREAD_TRAMPOLINE_EPILOGUE \
    if (self->cv_event) CloseHandle(self->cv_event); \
    free_thread_struct(th); \
    HANDLE h = self->handle; \
    free(self); \
    CloseHandle(h)
#else
#define THREAD_TRAMPOLINE_PROLOGUE \
    struct thread* th = arg
#define THREAD_TRAMPOLINE_EPILOGUE
#endif

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
#ifdef LISP_FEATURE_WIN32
DWORD WINAPI new_thread_trampoline(LPVOID arg)
#else
void* new_thread_trampoline(void* arg)
#endif
{
    THREAD_TRAMPOLINE_PROLOGUE;
#ifdef LISP_FEATURE_PAUSELESS_THREADSTART
#ifdef LISP_FEATURE_SB_SAFEPOINT
    init_thread_data scribble;
    // This "scribble" thing is really quite pointless because the original sigset_t
    // was passed in the thread's startup info (unless no signals at all were blocked).
    // And when terminating, why does anyone care what the signal mask was???
    // Well, there's a big "however": '&scribble' is no mere pass-by-reference arg-
    // it is actually used as an approximation of the C stack pointer.
#define SCRIBBLE &scribble
#else
#define SCRIBBLE 0
#endif
    // 'th->lisp_thread' remains valid despite not being in all_threads
    // due to the pinning via *STARTING-THREADS*.
    struct thread_instance *lispthread = (void*)native_pointer(th->lisp_thread);
    struct vector* startup_info = VECTOR(lispthread->startup_info); // 'lispthread' is pinned
    gc_assert(header_widetag(startup_info->header) == SIMPLE_VECTOR_WIDETAG);
    lispobj startfun = startup_info->data[0]; // 'startup_info' is pinned
    gc_assert(functionp(startfun));
    // GC can benefit from knowing the _effective_ end of the ambiguous root range.
    // Nothing at a higher address than &arg needs to be scanned for ambiguous roots.
    // For x86 + linux this optimization skips over about 800 words in the stack scan,
    // and for x86-64 it skip about 550 words as observed via:
    // fprintf(stderr, "%d non-lisp stack words\n",
    //                 (int)((lispobj*)th->control_stack_end - (lispobj*)&arg));
    // ADDRESS_SANITIZER doesn't allow this optimization.
    // Both of these assertions fail with the sanitizer enabled:
    //    gc_assert(th->control_stack_start <= (lispobj*)&arg
    //              && (lispobj*)&arg <= th->control_stack_end);
    //    gc_assert(th->control_stack_start <= (lispobj*)&startup_info
    //              && (lispobj*)&startup_info <= th->control_stack_end);
    // It seems to subvert the "&" and "*" operators in a way that only it understands,
    // while the stack pointer register is unperturbed.
    // (gencgc takes '&raise' for the current thread, but it disables the sanitizers)
    //
    // A stop-for-GC signal that hits after init_new_thread() releases the all_threads lock
    // and returns control here needs to see in the interrupt context a stack pointer
    // strictly below the computed th->control_stack_end. So make sure the value we pick
    // is strictly above any value of SP that the interrupt context could have.
#if defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK && !defined ADDRESS_SANITIZER \
    && !defined LISP_FEATURE_SB_SAFEPOINT
    th->control_stack_end = (lispobj*)&arg + 1;
#endif
    th->os_kernel_tid = sb_GetTID();
    init_new_thread(th, SCRIBBLE, 0, 0);
    // Passing the untagged pointer ensures 2 things:
    // - that the pinning mechanism works as designed, and not just by accident.
    // - that the initial stack does not contain a lisp pointer after it is not needed.
    //   (a regression test asserts that not even a THREAD instance is on the stack)
    funcall1(startfun, (lispobj)lispthread); // both pinned
    // Close the GC region and unlink from all_threads
    unregister_thread(th, SCRIBBLE);

#else // !PAUSELESS_THREADSTART

    th->os_kernel_tid = sb_GetTID();
    init_thread_data scribble;

    FSHOW((stderr,"/creating thread %p\n", thread_self()));
    check_deferrables_blocked_or_lose(0);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    check_gc_signals_unblocked_or_lose(0);
#endif

    lispobj function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    init_new_thread(th, &scribble,
                    GUARD_CONTROL_STACK|GUARD_BINDING_STACK|GUARD_ALIEN_STACK,
                    0);
    funcall0(function);
    unregister_thread(th, &scribble);

    // OS_THREAD_STACK means we start on the OS-provided stack
    // but switch to our chosen stack.
    // win32 doesn't do that - it stays on the OS-provided stack
    // and all cleanup is automatic
#if !defined LISP_FEATURE_OS_THREAD_STACK && !defined LISP_FEATURE_WIN32
    schedule_thread_post_mortem(th);
#endif
#endif
    THREAD_TRAMPOLINE_EPILOGUE;
    return 0;
}

#ifdef LISP_FEATURE_OS_THREAD_STACK
extern void* funcall1_switching_stack(void*, void *(*fun)(void *));

void* new_thread_trampoline_switch_stack(void* th) {
    return funcall1_switching_stack(th, new_thread_trampoline);
}
#endif

static struct thread* recyclebin_threads;
static pthread_mutex_t recyclebin_lock = PTHREAD_MUTEX_INITIALIZER;
static struct thread* get_recyclebin_item()
{
    struct thread* result = 0;
    int rc;
    rc = pthread_mutex_lock(&recyclebin_lock);
    gc_assert(!rc);
    if (recyclebin_threads) {
        result = recyclebin_threads;
        recyclebin_threads = result->next;
    }
    pthread_mutex_unlock(&recyclebin_lock);
    return result ? result->os_address : 0;
}
static void put_recyclebin_item(struct thread* th)
{
    int rc;
    rc = pthread_mutex_lock(&recyclebin_lock);
    gc_assert(!rc);
    th->next = recyclebin_threads;
    recyclebin_threads = th;
    pthread_mutex_unlock(&recyclebin_lock);
}
void empty_thread_recyclebin()
{
    if (!recyclebin_threads) return;
    sigset_t old;
    block_deferrable_signals(&old);
    int rc = pthread_mutex_trylock(&recyclebin_lock);
    if (!rc) { // no big deal if already locked (recursive GC?)
        struct thread* this = recyclebin_threads;
        while (this) {
            struct thread* next = this->next;
            free_thread_struct(this);
            this = next;
        }
        recyclebin_threads = 0;
        pthread_mutex_unlock(&recyclebin_lock);
    }
    thread_sigmask(SIG_SETMASK, &old, 0);
}

void
attach_os_thread(init_thread_data *scribble)
{
    block_deferrable_signals(&scribble->oldset);
    void* recycled_memory = get_recyclebin_item();
    struct thread *th = alloc_thread_struct(recycled_memory,
                                            NO_TLS_VALUE_MARKER_WIDETAG);

#ifndef LISP_FEATURE_SB_SAFEPOINT
    /* new-lisp-thread-trampoline doesn't like when the GC signal is blocked */
    /* FIXME: could be done using a single call to pthread_sigmask
       together with locking the deferrable signals above. */
    unblock_gc_signals();
#endif

    th->os_kernel_tid = sb_GetTID();
    th->os_thread = pthread_self();

#if !defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    /* On windows, arch_os_thread_init will take care of finding the
     * stack. */
    void *stack_addr;
    size_t stack_size;
# ifdef LISP_FEATURE_OPENBSD
    stack_t stack;
    pthread_stackseg_np(th->os_thread, &stack);
    stack_size = stack.ss_size;
    stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
# elif defined LISP_FEATURE_SUNOS
    stack_t stack;
    thr_stksegment(&stack);
    stack_size = stack.ss_size;
    stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
# elif defined(LISP_FEATURE_DARWIN)
    stack_size = pthread_get_stacksize_np(th->os_thread);
    stack_addr = (char*)pthread_get_stackaddr_np(th->os_thread) - stack_size;
# else
    pthread_attr_t attr;
#   if defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_DRAGONFLY
    pthread_attr_get_np(th->os_thread, &attr);
#   else
    int pthread_getattr_np(pthread_t, pthread_attr_t *);
    pthread_getattr_np(th->os_thread, &attr);
#   endif
    pthread_attr_getstack(&attr, &stack_addr, &stack_size);
    pthread_attr_destroy(&attr);
# endif
    th->control_stack_start = stack_addr;
    th->control_stack_end = (void *) (((uintptr_t) stack_addr) + stack_size);
#endif

#ifdef LISP_FEATURE_SB_SAFEPOINT
    const int retain_lock = 0;
#else
    const int retain_lock = 1;
#endif
    init_new_thread(th, scribble,
                    /* recycled memory already had mprotect() done,
                     * so avoid 2 syscalls when possible */
                    recycled_memory ? 0 : GUARD_BINDING_STACK|GUARD_ALIEN_STACK,
                    retain_lock);
}

void
detach_os_thread(init_thread_data *scribble)
{
    struct thread *th = arch_os_get_current_thread();
    odxprint(misc, "detach_os_thread: detaching");

    unregister_thread(th, scribble);

    /* We have to clear a STOP_FOR_GC signal if pending. Consider:
     *  - on entry to unregister_thread, we block all signals
     *  - simultaneously some other thread decides that it needs to initiate a GC
     *  - that thread observes that this thread exists in all_threads and sends
     *    STOP_FOR_GC, so it becomes pending but undeliverable in this thread
     *  - immediately after blocking signals, we change state to DEAD,
     *    which allows the GCing thread to ignore this thread
     *    (it sees the state change criterion as having been satisfied)
     *  - the GCing thread releases the all_threads lock
     *  - this thread acquires the lock and removes itself from all_threads,
     *    and indicates that it is no longer a lisp thread
     *  - but STOP_FOR_GC is pending because it was in the blocked set.
     * Bad things happen unless we clear the pending GC signal.
     */
#ifndef LISP_FEATURE_SB_SAFEPOINT
    sigset_t pending;
    sigpending(&pending);
    if (sigismember(&pending, SIG_STOP_FOR_GC)) {
        int sig, rc;
        rc = sigwait(&gc_sigset, &sig);
        gc_assert(rc == 0 && sig == SIG_STOP_FOR_GC);
#ifdef LISP_FEATURE_DARWIN
        sigpending(&pending);
        if (sigismember(&pending, SIG_STOP_FOR_GC)) {
            // fprintf(stderr, "Trying sigwait bug mitigation\n");
            pthread_setspecific(sigwait_bug_mitigation, (void*)1);
            sigfillset(&pending);
            sigdelset(&pending, SIG_STOP_FOR_GC);
            sigsuspend(&pending);
            // fprintf(stderr, "Back from sigsuspend\n");
        }
#endif
    }
#endif
    put_recyclebin_item(th);
    thread_sigmask(SIG_SETMASK, &scribble->oldset, 0);
}

#if defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_WIN32)
extern void funcall_alien_callback(lispobj arg1, lispobj arg2, lispobj arg0,
                                   struct thread* thread)
  __attribute__((sysv_abi));
#endif

void
callback_wrapper_trampoline(
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    /* On the x86oid backends, the assembly wrapper happens to not pass
     * in ENTER_ALIEN_CALLBACK explicitly for safepoints.  However, the
     * platforms with precise GC are tricky enough already, and I want
     * to minimize the read-time conditionals.  For those platforms, I'm
     * only replacing funcall3 with callback_wrapper_trampoline while
     * keeping the arguments unchanged. --DFL */
    lispobj __attribute__((__unused__)) fun,
#endif
    lispobj arg0, lispobj arg1, lispobj arg2)
{
#if defined(LISP_FEATURE_WIN32)
    pthread_np_notice_thread();
#endif
    struct thread* th = arch_os_get_current_thread();
    if (!th) {                  /* callback invoked in non-lisp thread */
        init_thread_data scribble;
        attach_os_thread(&scribble);

        WITH_GC_AT_SAFEPOINTS_ONLY()
        {
            funcall3(StaticSymbolFunction(ENTER_FOREIGN_CALLBACK), arg0,arg1,arg2);
        }
        detach_os_thread(&scribble);
        return;
    }

#ifdef LISP_FEATURE_WIN32
    /* arg2 is the pointer to a return value, which sits on the stack */
    th->carried_base_pointer = (os_context_register_t) *(((void**)arg2)-1);
#endif

    WITH_GC_AT_SAFEPOINTS_ONLY()
    {
#if defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_WIN32)
        funcall_alien_callback(arg1, arg2, arg0, th);
#else
        funcall3(StaticSymbolFunction(ENTER_ALIEN_CALLBACK), arg0,arg1,arg2);
#endif
    }
}

// Balance out the mutex_lock in attach_os_thread()
void release_all_threads_lock()
{
    if (pthread_mutex_unlock(&all_threads_lock))
        lose("ENTER-FOREIGN-CALLBACK bug");
}

#endif /* LISP_FEATURE_SB_THREAD */

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 *
 * The allocated memory will be laid out as depicted below.
 * Left-to-right is in order of lowest to highest address:
 *
 *      ______ spaces as obtained from OS
 *     /   ___ aligned_spaces
 *    /   /
 *  (0) (1)       (2)       (3)       (4)    (5)          (6)
 *   |   | CONTROL | BINDING |  ALIEN  |  CSP | thread     |          |
 *   |   |  STACK  |  STACK  |  STACK  | PAGE | structure  | altstack |
 *   |...|------------------------------------------------------------|
 *          2MiB       1MiB     1MiB               (*)         (**)
 *
 *  |  (*) interrupt contexts and Lisp TLS |   (**) altstack           |
 *  |-----------|--------------------------|------------|--------------|
 *  | interrupt | struct + dynamically     | nonpointer |   sigstack   |
 *  | contexts  | thread   assigned TLS    |     data   |              |
 *  +-----------+--------------------------|------------+--------------|
 *  | 1K words  | <--- TLS_SIZE words ---> | ~200 bytes | 32*SIGSTKSZ  |
 *              ^ thread base
 *
 *   (1) = control stack start. default size shown
 *   (2) = binding stack start. size = BINDING_STACK_SIZE
 *   (3) = alien stack start.   size = ALIEN_STACK_SIZE
 *   (4) = C safepoint page.    size = BACKEND_PAGE_BYTES or 0
 *   (5) = per_thread_data.     size = (MAX_INTERRUPTS + TLS_SIZE) words
 *   (6) = nonpointer_thread_data and signal stack.
 *
 *   (0) and (1) may coincide; (4) and (5) may coincide
 *
 *   - Lisp TLS overlaps 'struct thread' so that the first N (~30) words
 *     have preassigned TLS indices.
 *
 *   - nonpointer data are not in 'struct thread' because placing them there
 *     makes it tough to calculate addresses in 'struct thread' from Lisp.
 *     (Every 'struct thread' slot has a known size)
 *
 * On sb-safepoint builds one page before the thread base is used for the foreign calls safepoint.
 */

struct thread *
alloc_thread_struct(void* spaces, lispobj start_routine) {
#if defined(LISP_FEATURE_SB_THREAD) || defined(LISP_FEATURE_WIN32)
    unsigned int i;
#endif

    /* May as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed. SPACES must be appropriately aligned, since the GC
     * expects the control stack to start at a page boundary -- and
     * the OS may have even more rigorous requirements. We can't rely
     * on the alignment passed from os_validate, since that might
     * assume the current (e.g. 4k) pagesize, while we calculate with
     * the biggest (e.g. 64k) pagesize allowed by the ABI. */
    boolean zeroize_stack = 0;
    if (spaces) {
        // If reusing memory from a previously exited thread, start by removing
        // some old junk from the stack. This is imperfect since we only clear a little
        // at the top, but doing so enables diagnosing some garbage-retention issues
        // using a fine-toothed comb. It would not be possible at all to diagnose
        // if any newly started thread could refer a dead thread's heap objects.
        zeroize_stack = 1;
    } else {
        spaces = os_validate(MOVABLE|IS_THREAD_STRUCT, NULL, THREAD_STRUCT_SIZE);
        if (!spaces) return NULL;
    }
    /* Aligning up is safe as THREAD_STRUCT_SIZE has
     * THREAD_ALIGNMENT_BYTES padding. */
    char *aligned_spaces = PTR_ALIGN_UP(spaces, THREAD_ALIGNMENT_BYTES);
    char* csp_page=
        (aligned_spaces+
         thread_control_stack_size+
         BINDING_STACK_SIZE+
         ALIEN_STACK_SIZE +
         (MAX_INTERRUPTS*sizeof(os_context_t*)));

    // Refer to the ASCII art in the block comment above
    struct thread *th = (void*)(csp_page + THREAD_CSP_PAGE_SIZE);
    lispobj* tls = (lispobj*)th;

#ifdef LISP_FEATURE_SB_THREAD
    for(i = 0; i < (unsigned int)(dynamic_values_bytes/N_WORD_BYTES); i++)
        tls[i] = NO_TLS_VALUE_MARKER_WIDETAG;
    th->lisp_thread = 0; // force it to be always-thread-local, of course
    th->tls_size = dynamic_values_bytes;
#endif
#if defined LISP_FEATURE_X86_64 && defined LISP_FEATURE_LINUX
    tls[THREAD_MSAN_XOR_CONSTANT_SLOT] = 0x500000000000;
#endif
#ifdef LAYOUT_OF_FUNCTION
    tls[THREAD_FUNCTION_LAYOUT_SLOT] = LAYOUT_OF_FUNCTION << 32;
#endif
#ifdef LISP_FEATURE_GENCGC
#ifdef THREAD_VARYOBJ_CARD_MARKS_SLOT
    extern unsigned int* varyobj_page_touched_bits;
    tls[THREAD_VARYOBJ_SPACE_ADDR_SLOT] = VARYOBJ_SPACE_START;
    tls[THREAD_VARYOBJ_CARD_COUNT_SLOT] = varyobj_space_size / IMMOBILE_CARD_BYTES;
    tls[THREAD_VARYOBJ_CARD_MARKS_SLOT] = (lispobj)varyobj_page_touched_bits;
#endif
    th->dynspace_addr       = DYNAMIC_SPACE_START;
    th->dynspace_card_count = page_table_pages;
    th->dynspace_pte_base   = (lispobj)page_table;
#endif

    th->os_address = spaces;
    th->control_stack_start = (lispobj*)aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((char*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;

    if (zeroize_stack) {
#if GENCGC_IS_PRECISE
    /* Clear the entire control stack. Without this I was able to induce a GC failure
     * in a test which hammered on thread creation for hours. The control stack is
     * scavenged before the heap, so a stale word could point to the start (or middle)
     * of an object using a bad lowtag, for whatever object formerly was there.
     * Then a wrong transport function would be called and (if it worked at all) would
     * place a wrongly tagged FP into a word that might not be the base of an object.
     * Assume for simplicity (as is true) that stacks grow upward if GENCGC_IS_PRECISE.
     * This could just call scrub_thread_control_stack but the comment there says that
     * it's a lame algorithm and only mostly right - it stops after (1<<12) words
     * and checks if the next is nonzero, looping again if it isn't.
     * There's no reason not to be exactly right here instead of probably right */
        memset((char*)th->control_stack_start, 0,
               // take off 2 pages because of the soft and hard guard pages
               thread_control_stack_size - 2*os_vm_page_size);
#else
    /* This is a little wasteful of cycles to pre-zero the pthread overhead (which in glibc
     * resides at the highest stack addresses) comprising about 5kb, below which is the lisp
     * stack. We don't need to zeroize above the lisp stack end, but we don't know exactly
     * where that will be.  Zeroizing more than necessary is conservative, and helps ensure
     * that garbage retention from reused stacks does not pose a huge problem. */
        memset((char*)th->control_stack_end - 16384, 0, 16384);
#endif
    }

    th->control_stack_guard_page_protected = T;
    th->alien_stack_start=
        (lispobj*)((char*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this=th;
    th->os_thread=0;
    // Once allocated, the allocation profiling buffer sticks around.
    // If present and enabled, assign into the new thread.
    th->profile_data = (uword_t*)(alloc_profiling ? alloc_profile_buffer : 0);

#ifdef LISP_FEATURE_SB_SAFEPOINT
# ifdef LISP_FEATURE_WIN32
    th->carried_base_pointer = 0;
# endif
    // FIXME: this looks extremely bogus if the threads run on an OS-provided stack.
    // Wouldn't it make more sense to set it to 0 and leave it to init_new_thread
    // to set the value? (where would the main thread get it from?)
    th->csp_around_foreign_call = (lispobj *)th - 1;
#endif

    struct nonpointer_thread_data *nonpointer_data
      = (void *)((char*)th + dynamic_values_bytes);
    th->interrupt_data = &nonpointer_data->interrupt_data;

#ifdef LISP_FEATURE_SB_THREAD
# ifndef LISP_FEATURE_SB_SAFEPOINT
    th->state_sem = &nonpointer_data->state_sem;
    th->state_not_running_sem = &nonpointer_data->state_not_running_sem;
    th->state_not_stopped_sem = &nonpointer_data->state_not_stopped_sem;
    os_sem_init(th->state_sem, 1);
    os_sem_init(th->state_not_running_sem, 0);
    os_sem_init(th->state_not_stopped_sem, 0);
    th->state_not_running_waitcount = 0;
    th->state_not_stopped_waitcount = 0;
# endif

#endif
    th->state=STATE_RUNNING;
#ifdef ALIEN_STACK_GROWS_DOWNWARD
    th->alien_stack_pointer=(lispobj*)((char*)th->alien_stack_start
                                       + ALIEN_STACK_SIZE-N_WORD_BYTES);
#else
    th->alien_stack_pointer=(lispobj*)((char*)th->alien_stack_start);
#endif

#ifdef LISP_FEATURE_SB_THREAD
    th->pseudo_atomic_bits=0;
#elif defined LISP_FEATURE_GENCGC
    clear_pseudo_atomic_atomic(th);
    clear_pseudo_atomic_interrupted(th);
#endif

#ifdef LISP_FEATURE_GENCGC
    gc_init_region(&th->alloc_region);
# if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    gc_init_region(&th->sprof_alloc_region);
# endif
#endif
#ifdef LISP_FEATURE_SB_THREAD
    /* This parallels the same logic in globals.c for the
     * single-threaded foreign_function_call_active, KLUDGE and
     * all. */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    th->foreign_function_call_active = 0;
#else
    th->foreign_function_call_active = 1;
#endif
#endif

#ifndef LISP_FEATURE_SB_THREAD
    /* the tls-points-into-struct-thread trick is only good for threaded
     * sbcl, because unithread sbcl doesn't have tls.  So, we copy the
     * appropriate values from struct thread here, and make sure that
     * we use the appropriate SymbolValue macros to access any of the
     * variable quantities from the C runtime.  It's not quite OAOOM,
     * it just feels like it */
    SetSymbolValue(BINDING_STACK_START,(lispobj)th->binding_stack_start,th);
    SetSymbolValue(CONTROL_STACK_START,(lispobj)th->control_stack_start,th);
    SetSymbolValue(CONTROL_STACK_END,(lispobj)th->control_stack_end,th);
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    SetSymbolValue(ALIEN_STACK_POINTER,(lispobj)th->alien_stack_pointer,th);
#endif
#endif
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    access_control_stack_pointer(th)=th->control_stack_start;
    access_control_frame_pointer(th)=0;
#endif

    th->interrupt_data->pending_handler = 0;
    th->interrupt_data->gc_blocked_deferrables = 0;
#if GENCGC_IS_PRECISE
    th->interrupt_data->allocation_trap_context = 0;
#endif

#ifdef LISP_FEATURE_SB_THREAD
// This macro is the same as "write_TLS(sym,val,th)" but can't be spelled thus.
// 'sym' would get substituted prior to token pasting, so you end up with a bad
// token "(*)_tlsindex" because all symbols are #defined to "(*)" so that #ifdef
// remains meaningful to the preprocessor, while use of 'sym' itself yields
// a deliberate syntax error if you try to compile an expression involving it.
#  define INITIALIZE_TLS(sym,val) write_TLS_index(sym##_tlsindex, val, th, _ignored_)
#else
#  define INITIALIZE_TLS(sym,val) SYMBOL(sym)->value = val
#endif
#include "genesis/thread-init.inc"
    th->no_tls_value_marker = start_routine;

#if defined(LISP_FEATURE_WIN32)
    for (i = 0; i<sizeof(th->private_events.events)/
           sizeof(th->private_events.events[0]); ++i) {
      th->private_events.events[i] = CreateEvent(NULL,FALSE,FALSE,NULL);
    }
    th->synchronous_io_handle_and_flag = 0;
#endif
    th->stepping = 0;
    return th;
}
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_WIN32
/* Allocate a thread structure, call CreateThread(),
 * and return 1 for success, 0 for failure */
uword_t create_thread(lispobj start_routine)
{
    struct thread *th, *thread = arch_os_get_current_thread();

    /* Must defend against async unwinds. */
    if (read_TLS(INTERRUPTS_ENABLED, thread) != NIL)
        lose("create_thread is not safe when interrupts are enabled.");

    /* Assuming that a fresh thread struct has no lisp objects in it,
     * linking it to all_threads can be left to the thread itself
     * without fear of gc lossage. 'start_routine' violates this
     * assumption and must stay pinned until the child starts up. */
    th = alloc_thread_struct(0, start_routine);
    if (!th) return 0;

    /* The new thread inherits the restrictive signal mask set here,
     * and enables signals again when it is set up properly. */
    sigset_t oldset;
    boolean success = 0;

    /* Blocking deferrable signals is enough, no need to block
     * SIG_STOP_FOR_GC because the child process is not linked onto
     * all_threads until it's ready. */
    block_deferrable_signals(&oldset);
    DWORD tid;
    pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
    // Theoretically you should tell the new thread a signal mask to restore
    // after it finishes any uninterruptable setup code, but the way this worked
    // on windows is that we passed the mask of blocked signals in the parent
    // *after* blocking deferrables. It's immaterial what mask is passed
    // because the thread will unblock all deferrables,
    // and we don't really have posix signals anyway.
    pth->blocked_signal_set = deferrable_sigset;
    pth->vm_thread = th;
    pth->handle = CreateThread(NULL, thread_control_stack_size,
                               new_thread_trampoline, pth, 0, &tid);
    success = pth->handle != NULL;
    if (success)
        th->os_thread = pth, th->os_kernel_tid = tid;
    else
        free(pth);
    thread_sigmask(SIG_SETMASK,&oldset,0);
    if (!success) free_thread_struct(th);
    return success;
}
#endif

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */
/*
 * (With SB-SAFEPOINT, see the definitions in safepoint.c instead.)
 */
#ifndef LISP_FEATURE_SB_SAFEPOINT

/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which. */
void gc_stop_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int status, lock_ret;
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock\n"));

    /* Keep threads from registering with GC while the world is stopped. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got lock\n"));
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for(p=all_threads; p; p=p->next) {
        gc_assert(p->os_thread != 0);
        FSHOW_SIGNAL((stderr,"/gc_stop_the_world: thread=%lu, state=%x\n",
                      p->os_thread, thread_state(p)));
        if((p!=th) && ((thread_state(p)==STATE_RUNNING))) {
            FSHOW_SIGNAL((stderr,"/gc_stop_the_world: suspending thread %lu\n",
                          p->os_thread));
            /* We already hold all_thread_lock, P can become DEAD but
             * cannot exit, ergo it's safe to use pthread_kill. */
            status=pthread_kill(p->os_thread,SIG_STOP_FOR_GC);
            if (status==ESRCH) {
                /* This thread has exited. */
                gc_assert(thread_state(p)==STATE_DEAD);
            } else if (status) {
                lose("cannot send suspend thread=%lx: %d, %s",
                     // KLUDGE: assume that os_thread can be cast as long.
                     // See comment in 'interr.h' about that.
                     (long)p->os_thread,status,strerror(status));
            }
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:signals sent\n"));
    for(p=all_threads;p;p=p->next) {
        if (p!=th) {
            FSHOW_SIGNAL
                ((stderr,
                  "/gc_stop_the_world: waiting for thread=%lu: state=%x\n",
                  p->os_thread, thread_state(p)));
            wait_for_thread_state_change(p, STATE_RUNNING);
            if (p->state == STATE_RUNNING)
                lose("/gc_stop_the_world: unexpected state");
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:end\n"));
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int lock_ret;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will be suspended waiting to acquire
     * the all_threads lock */
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:begin\n"));
    for(p=all_threads;p;p=p->next) {
        gc_assert(p->os_thread!=0);
        if (p!=th) {
            lispobj state = thread_state(p);
            if (state != STATE_DEAD) {
                if(state != STATE_STOPPED) {
                    lose("gc_start_the_world: wrong thread state is %"OBJ_FMTX,
                         (lispobj)fixnum_value(state));
                }
                FSHOW_SIGNAL((stderr, "/gc_start_the_world: resuming %lu\n",
                              p->os_thread));
                set_thread_state(p, STATE_RUNNING, 0);
            }
        }
    }

    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}

#endif /* !LISP_FEATURE_SB_SAFEPOINT */
#endif /* !LISP_FEATURE_SB_THREAD */

int
thread_yield()
{
#ifdef LISP_FEATURE_SB_THREAD
    return sched_yield();
#else
    return 0;
#endif
}

// The code is extremely trashy and a good candidate for removal/cleanup
// but I don't understand this feature combination well enough to do so.
// The part of it we need to keep is the GC state interaction, though
// as can plainly be seen from the "good" case of SB-THREAD::INTERRUPT-THREAD
// there is a perfectly fine way of detecting validity of a pthread ID,
// and even the link below to Ulrich Drepper's blog post says exactly what
// to do. So why does this have anything to do with the all_threads_lock???
#if defined LISP_FEATURE_SB_THRUPTION || defined LISP_FEATURE_SB_SAFEPOINT
int
wake_thread(os_thread_t os_thread)
{
#if defined(LISP_FEATURE_WIN32)
    return kill_safely(os_thread, 1);
#elif !defined(LISP_FEATURE_SB_THRUPTION)
    return kill_safely(os_thread, SIGPIPE);
#else
    return wake_thread_posix(os_thread);
#endif
}

/* If the thread id given does not belong to a running thread (it has
 * exited or never even existed) pthread_kill _may_ fail with ESRCH,
 * but it is also allowed to just segfault, see
 * <http://udrepper.livejournal.com/16844.html>.
 *
 * Relying on thread ids can easily backfire since ids are recycled
 * (NPTL recycles them extremely fast) so a signal can be sent to
 * another process if the one it was sent to exited.
 *
 * For these reasons, we must make sure that the thread is still alive
 * when the pthread_kill is called and return if the thread is
 * exiting.
 *
 * Note (DFL, 2011-06-22): At the time of writing, this function is only
 * used for INTERRUPT-THREAD, hence the wake_thread special-case for
 * Windows is OK. */
int
kill_safely(os_thread_t os_thread, int signal)
{
    FSHOW_SIGNAL((stderr,"/kill_safely: %lu, %d\n", os_thread, signal));
    {
#ifdef LISP_FEATURE_SB_THREAD
        sigset_t oldset;
        struct thread *thread;
        /* Frequent special case: resignalling to self.  The idea is
         * that leave_region safepoint will acknowledge the signal, so
         * there is no need to take locks, roll thread to safepoint
         * etc. */
        /* Kludge (on safepoint builds): At the moment, this isn't just
         * an optimization; rather it masks the fact that
         * gc_stop_the_world() grabs the all_threads mutex without
         * releasing it, and since we're not using recursive pthread
         * mutexes, the pthread_mutex_lock() around the all_threads loop
         * would go wrong.  Why are we running interruptions while
         * stopping the world though?  Test case is (:ASYNC-UNWIND
         * :SPECIALS), especially with s/10/100/ in both loops. */
        /* From the linux man page on pthread_self() -
         * "variables  of  type  pthread_t  can't  portably be compared using
         *  the C equality operator (==); use pthread_equal(3) instead." */
        if (thread_equal(os_thread, pthread_self())) {
            pthread_kill(os_thread, signal);
#ifdef LISP_FEATURE_WIN32
            check_pending_thruptions(NULL);
#endif
            return 0;
        }

        /* pthread_kill is not async signal safe and we don't want to be
         * interrupted while holding the lock. */
        block_deferrable_signals(&oldset);
        pthread_mutex_lock(&all_threads_lock);
        for (thread = all_threads; thread; thread = thread->next) {
            if (thread->os_thread == os_thread) {
                int status = pthread_kill(os_thread, signal);
                if (status)
                    lose("kill_safely: pthread_kill failed with %d", status);
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THRUPTION)
                wake_thread_win32(thread);
#endif
                break;
            }
        }
        pthread_mutex_unlock(&all_threads_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
        if (thread)
            return 0;
        else
            return -1;
#elif defined(LISP_FEATURE_WIN32)
        return 0;
#else
        int status;
        if (os_thread != 0)
            lose("kill_safely: who do you want to kill? %d?", os_thread);
        /* Dubious (as in don't know why it works) workaround for the
         * signal sometimes not being generated on darwin. */
#ifdef LISP_FEATURE_DARWIN
        {
            sigset_t oldset;
            sigprocmask(SIG_BLOCK, &deferrable_sigset, &oldset);
            status = raise(signal);
            sigprocmask(SIG_SETMASK,&oldset,0);
        }
#else
        status = raise(signal);
#endif
        if (status == 0) {
            return 0;
        } else {
            lose("cannot raise signal %d, %d %s",
                 signal, status, strerror(errno));
        }
#endif
    }
}
#endif
