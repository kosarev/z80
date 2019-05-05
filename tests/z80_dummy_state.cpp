
// Test that all z80 state handlers have their dummy
// implementations.

#include "z80.h"

using z80::fast_u8;
using z80::fast_u16;
using z80::least_u8;

template<typename B>
struct my_state : public B {
    void on_ex_de_hl_regs() {}
    void on_ex_af_alt_af_regs() {}
    void on_exx_regs() {}
};

class my_emulator : public z80::z80_executor<z80::z80_decoder<
                               my_state<z80::root<my_emulator>>>> {
public:
    my_emulator() {}
};

int main() {
    my_emulator e;
    e.on_step();
}
