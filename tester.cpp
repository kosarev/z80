
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstring>

#include "z80.h"

namespace {

class disassembler : public z80::memory_interface<disassembler>,
                     public z80::instructions_decoder<disassembler>,
                     public z80::disassembler<disassembler> {
public:
    disassembler() {}

    z80::least_u8 &at(z80::fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

private:
    static const z80::size_type image_size = 0x10000;  // 64K bytes.
    z80::least_u8 image[image_size];
};

class machine : public z80::memory_interface<machine>,
                public z80::instructions_decoder<machine>,
                public z80::processor<machine> {
public:
    typedef uint_fast32_t ticks_type;

    machine() {}

    void tick(unsigned t) { ticks.tick(t); }

    ticks_type get_ticks() const { return ticks.get_ticks(); }

    z80::least_u8 &at(z80::fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

private:
    z80::trivial_ticks_counter<ticks_type> ticks;

    static const z80::size_type image_size = 0x10000;  // 64K bytes.
    z80::least_u8 image[image_size];
};

}  // anonymous namespace

static void test_disassembling() {
    disassembler disasm;
    disasm.disassemble();
    assert(std::strcmp(disasm.get_output(), "nop") == 0);
}

static void test_execution() {
    machine mach;
    assert(mach.get_pc() == 0);
    assert(mach.get_ticks() == 0);
    mach.step();
    assert(mach.get_pc() == 1);
    assert(mach.get_ticks() == 4);
}

int main() {
    test_disassembling();
    test_execution();
}
