
#include "z80.h"

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;

class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    typedef z80::z80_cpu<my_emulator> base;

    my_emulator() {}

    void on_set_pc(fast_u16 pc) {
        std::printf("pc = 0x%04x\n", static_cast<unsigned>(pc));
        base::on_set_pc(pc);
    }

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        fast_u8 n = memory[addr];
        std::printf("read 0x%02x at 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(addr));
        return n;
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        std::printf("write 0x%02x at 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(addr));
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
}
