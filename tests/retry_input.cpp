
/*  Z80 CPU Emulator.
    https://github.com/kosarev/z80

    Copyright (c) 2026 Ivan Kosarev <mail@ivankosarev.com>
    Published under the MIT license.
*/

// Test aborting and retrying an instruction whose port read is not yet
// available (see z80::retry_input).

#include "z80.h"

#include "check.h"

using z80::fast_u8;
using z80::fast_u16;

class my_emulator : public z80::z80_machine<my_emulator> {
public:
    my_emulator() {}

    // The port value only becomes available on the second attempt.
    fast_u8 on_input(fast_u16 port) {
        z80::unused(port);
        return input_attempts++ == 0 ? z80::retry_input : 0x5a;
    }

    // Snapshot and restore the state a not-yet-available port read may
    // have already changed by the time it aborts: PC, R and the ticks.
    void on_save_retry_state() {
        saved_pc = get_pc();
        saved_r = get_r();
        saved_ticks = get_frame_tick();
    }
    void on_restore_retry_state() {
        set_pc(saved_pc);
        set_r(saved_r);
        set_frame_tick(saved_ticks);
    }

    unsigned input_attempts = 0;

private:
    fast_u16 saved_pc = 0;
    fast_u8 saved_r = 0;
    unsigned saved_ticks = 0;
};

static void test_retry_input() {
    my_emulator e;
    e.set_af(0x0000);  // Read the port into a known A.

    // IN A, (0xfe) at 0x0000, followed by a breakpoint.
    e.on_write(0x0000, 0xdb);
    e.on_write(0x0001, 0xfe);
    e.set_breakpoint(0x0002);

    // The port isn't available yet, so the instruction is aborted and all
    // its state changes are rolled back -- including the PC having reached
    // the breakpoint while fetching the operand, so only retry_input gets
    // reported, not breakpoint_hit.
    z80::events_mask::type events = e.on_run();
    CHECK(events == z80::events_mask::retry_input);
    CHECK(e.get_pc() == 0x0000);
    CHECK(e.get_r() == 0x00);
    CHECK(e.get_frame_tick() == 0);
    CHECK(e.get_a() == 0x00);
    CHECK(e.input_attempts == 1);

    // On retry the port is available, so the instruction completes and
    // execution stops at the following breakpoint.
    events = e.on_run();
    CHECK(events == z80::events_mask::breakpoint_hit);
    CHECK(e.get_pc() == 0x0002);
    CHECK(e.get_a() == 0x5a);
    CHECK(e.input_attempts == 2);
}

int main() {
    test_retry_input();
}
