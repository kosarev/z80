
/*  Z80 CPU Emulator.
    https://github.com/kosarev/z80

    Copyright (c) 2026 Ivan Kosarev <mail@ivankosarev.com>
    Published under the MIT license.
*/

// Test that data accesses are timed as on hardware. The tick counter
// counts completed ticks, so an access the hardware makes during tick
// N of its machine cycle sees the counter reading N-1: memory reads
// and writes happen entering the t3 of their cycles, and port inputs
// and outputs during the t4 of theirs.

#include <cstring>

#include "z80.h"

#include "check.h"

using z80::fast_u8;
using z80::fast_u16;

struct access {
    const char *kind;
    fast_u16 addr;
    unsigned tick;
};

static const access expected_accesses[] = {
    // ld a, 0x42
    {"read", 0x0000, 2},    // the opcode, entering the t3 of the fetch
    {"read", 0x0001, 6},    // the operand, entering the t3 of the read
    // ld (0x8000), a
    {"read", 0x0002, 9},
    {"read", 0x0003, 13},
    {"read", 0x0004, 16},
    {"write", 0x8000, 19},  // committed entering the t3 of the write
    // in a, (0xfe)
    {"read", 0x0005, 22},
    {"read", 0x0006, 26},
    {"input", 0x42fe, 30},  // sampled during the t4 of the input cycle
    // out (0xfe), a
    {"read", 0x0007, 33},
    {"read", 0x0008, 37},
    {"output", 0x5afe, 41},  // driven during the t4 of the output cycle
};

static const unsigned num_of_expected_accesses =
    sizeof expected_accesses / sizeof expected_accesses[0];

class my_emulator : public z80::z80_machine<my_emulator> {
public:
    typedef z80::z80_machine<my_emulator> base;

    my_emulator() {}

    fast_u8 on_read(fast_u16 addr) {
        record("read", addr);
        return base::on_read(addr);
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        record("write", addr);
        base::on_write(addr, n);
    }

    fast_u8 on_input(fast_u16 port) {
        record("input", port);
        // A definite value: the default 0xff is the reserved
        // retry_input sentinel and would abort the instruction.
        return 0x5a;
    }

    void on_output(fast_u16 port, fast_u8 n) {
        record("output", port);
        base::on_output(port, n);
    }

    unsigned num_of_accesses = 0;

private:
    void record(const char *kind, fast_u16 addr) {
        CHECK(num_of_accesses < num_of_expected_accesses);
        const access &a = expected_accesses[num_of_accesses];
        CHECK(std::strcmp(kind, a.kind) == 0);
        CHECK(addr == a.addr);
        CHECK(get_frame_tick() == a.tick);
        ++num_of_accesses;
    }
};

static void test_access_timing() {
    my_emulator e;

    // ld a, 0x42; ld (0x8000), a; in a, (0xfe); out (0xfe), a
    static const fast_u8 code[] = {
        0x3e, 0x42, 0x32, 0x00, 0x80, 0xdb, 0xfe, 0xd3, 0xfe};
    for(fast_u16 i = 0; i != sizeof code; ++i)
        e.write(i, code[i]);

    for(unsigned i = 0; i != 4; ++i)
        CHECK(e.on_step() == 0);

    CHECK(e.num_of_accesses == num_of_expected_accesses);
    CHECK(e.get_frame_tick() == 42);
}

int main() {
    test_access_timing();
}
