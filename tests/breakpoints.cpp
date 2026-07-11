
/*  Z80 CPU Emulator.
    https://github.com/kosarev/z80

    Copyright (c) 2026 Ivan Kosarev <mail@ivankosarev.com>
    Published under the MIT license.
*/

// Test tripping a breakpoint and resuming past it.

#include "z80.h"

#include "check.h"

class my_emulator : public z80::z80_machine<my_emulator> {
public:
    my_emulator() {}
};

using events_mask = my_emulator::events_mask;

static void test_trip_and_resume() {
    my_emulator e;
    e.set_af(0x0000);

    // A loop counting its iterations in A, with a breakpoint on the
    // counting instruction.
    e.write(0x0000, 0x00);  // nop
    e.write(0x0001, 0x3c);  // L: inc a
    e.write(0x0002, 0xc3);  // jp L
    e.write(0x0003, 0x01);
    e.write(0x0004, 0x00);
    e.set_breakpoint(0x0001);

    // The breakpoint fires on the attempt to execute the marked
    // instruction: the nop before it has been executed, the inc hasn't,
    // and the trap itself takes no time.
    events_mask::type events = e.on_run();
    CHECK(events == events_mask::breakpoint_hit);
    CHECK(e.get_pc() == 0x0001);
    CHECK(e.get_a() == 0x00);
    CHECK(e.get_frame_tick() == 4);

    // Running again is just another attempt to execute the marked
    // instruction, so it re-traps immediately with no progress made.
    // The same goes for direct steps, which report their events as the
    // return value.
    events = e.on_run();
    CHECK(events == events_mask::breakpoint_hit);
    CHECK(e.on_step() == events_mask::breakpoint_hit);
    CHECK(e.get_pc() == 0x0001);
    CHECK(e.get_a() == 0x00);
    CHECK(e.get_frame_tick() == 4);

    // Resuming means explicitly stepping over the marked instruction
    // with breakpoints not consulted.
    events = e.on_step_over_breakpoint();
    CHECK(events == 0);
    CHECK(e.get_pc() == 0x0002);
    CHECK(e.get_a() == 0x01);

    // The jp then loops back and the breakpoint fires again on the
    // next attempt to execute the inc.
    events = e.on_run();
    CHECK(events == events_mask::breakpoint_hit);
    CHECK(e.get_pc() == 0x0001);
    CHECK(e.get_a() == 0x01);
    CHECK(e.get_frame_tick() == 4 + 14);

    // Every resume advances the loop by exactly one iteration.
    events = e.on_step_over_breakpoint();
    CHECK(events == 0);
    events = e.on_run();
    CHECK(events == events_mask::breakpoint_hit);
    CHECK(e.get_pc() == 0x0001);
    CHECK(e.get_a() == 0x02);
    CHECK(e.get_frame_tick() == 4 + 14 + 14);
}

static void test_trap_events() {
    my_emulator e;

    e.write(0x0000, 0x00);  // L: nop
    e.write(0x0001, 0xc3);  // jp L
    e.write(0x0002, 0x00);
    e.write(0x0003, 0x00);
    e.set_breakpoint(0x0000);

    CHECK(e.on_run() == events_mask::breakpoint_hit);

    // Step over the nop with the tick counter about to wrap the
    // 100000-tick frame, so the step raises end_of_frame.
    e.set_frame_tick(100 * 1000 - 1);
    CHECK(e.on_step_over_breakpoint() == events_mask::end_of_frame);

    // The jp loops back to the marked nop, and the trap reports exactly
    // breakpoint_hit, not re-reporting the end_of_frame left from the
    // explicitly stepped-over instruction.
    CHECK(e.on_run() == events_mask::breakpoint_hit);
}

int main() {
    test_trip_and_resume();
    test_trap_events();
}
