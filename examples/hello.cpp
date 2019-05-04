
#include "z80.h"

class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    typedef z80::z80_cpu<my_emulator> base;

    my_emulator() {}

    void on_step() {
        std::printf("pc = 0x%04x\n", static_cast<unsigned>(get_pc()));
        base::on_step();
    }
};

int main() {
    my_emulator e;
    e.on_step();
    e.on_step();
    e.on_step();
}
