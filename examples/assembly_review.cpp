
// A fake emulator implementation used to review resulting
// assembly code for various instructions.

#include "z80.h"

using z80::fast_u8;
using z80::fast_u16;

using z80::least_u8;

using z80::reg;

template<typename B>
class emulator : public B {
public:
    typedef B base;

    emulator() {}

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        return memory[addr];
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        memory[addr] = static_cast<least_u8>(n);
    }

private:
    least_u8 memory[z80::address_space_size] = {};
};

class i8080_emulator : public emulator<z80::i8080_cpu<i8080_emulator>>
{};

class z80_emulator : public emulator<z80::z80_cpu<z80_emulator>>
{};

extern reg r1;
reg r1 = reg::a;

extern reg r2;
reg r2 = reg::a;

void foo(i8080_emulator &e);
void foo(i8080_emulator &e) {
    // e.on_decode(0x01);
    // e.on_nop();
    e.on_ld_r_r(r1, r2);
}

int main() {
    i8080_emulator e;
    foo(e);
}
