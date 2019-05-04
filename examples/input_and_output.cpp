
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

    fast_u8 on_input(fast_u16 port) {
        fast_u8 n = 0xfe;
        std::printf("input 0x%02x from 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(port));
        return n;
    }

    void on_output(fast_u16 port, fast_u8 n) {
        std::printf("output 0x%02x to 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(port));
    }

private:
    least_u8 memory[z80::address_space_size] = {
        0xdb,        // in a, (0xfe)
        0xee, 0x07,  // xor 7
        0xd3,        // out (0xfe), a
    };
};

int main() {
    my_emulator e;
    e.on_step();
    e.on_step();
    e.on_step();
}
