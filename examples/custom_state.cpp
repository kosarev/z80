
#include "z80.h"

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;

// A simple custom processor state that stores only some of the
// registers. Others are not stored anywhere and on reading give
// their default values.
template<typename B>
struct my_state : public B {
    fast_u16 pc;
    fast_u8 l, h;
    fast_u8 a;

    fast_u16 get_hl() const { return z80::make16(h, l); }

    fast_u16 on_get_pc() const { return pc; }
    void on_set_pc(fast_u16 n) { pc = n; }

    fast_u8 on_get_l() const { return l; }
    void on_set_l(fast_u8 n) { l = n; }

    fast_u8 on_get_h() const { return h; }
    void on_set_h(fast_u8 n) { h = n; }

    fast_u8 on_get_a() const { return a; }
    void on_set_a(fast_u8 n) { a = n; }

    // These always have to be explicitly defined.
    void on_ex_de_hl_regs() {}
    void on_ex_af_alt_af_regs() {}
    void on_exx_regs() {}
};

// A Z80 emulator that uses the custom state.
class my_emulator : public z80::z80_executor<z80::z80_decoder<
                               my_state<z80::root<my_emulator>>>> {
public:
    typedef z80::z80_cpu<my_emulator> base;

    my_emulator() {}

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        return memory[addr];
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        memory[addr] = static_cast<least_u8>(n);
    }

private:
    least_u8 memory[z80::address_space_size] = {
        0x21, 0x34, 0x12,  // ld hl, 0x1234
        0x3e, 0x07,        // ld a, 7
        0x77,              // ld (hl), a
    };
};

int main() {
    my_emulator e;
    e.on_step();
    e.on_step();
    e.on_step();

    std::printf("pc = 0x%04x, hl = 0x%04x, a = 0x%02x\n",
                static_cast<unsigned>(e.pc),
                static_cast<unsigned>(e.get_hl()),
                static_cast<unsigned>(e.a));
}
