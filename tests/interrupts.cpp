
#include "z80.h"

class my_emulator : public z80::z80_machine<my_emulator>
{};

int main() {
    my_emulator e;
    e.on_handle_active_int();
}
