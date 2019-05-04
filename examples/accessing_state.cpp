
#include "z80.h"

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;

class my_emulator : public z80::z80_cpu<my_emulator> {
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

    void on_step() {
        std::printf("hl = %04x\n", static_cast<unsigned>(get_hl()));
        base::on_step();

        // Start over on every new instruction.
        set_pc(0x0000);
    }

private:
    least_u8 memory[z80::address_space_size] = {
        0x2b,  // dec hl
    };
};

int main() {
    my_emulator e;
    e.on_step();
    e.on_step();
    e.on_step();
}
