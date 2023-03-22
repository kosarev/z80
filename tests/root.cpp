
#include "z80.h"

#include "check.h"

using z80::fast_u8;

int main() {
    class my_root : public z80::root<my_root> {
    public:
        fast_u8 on_get_i() const { return 0x12; }
        fast_u8 on_get_r() const { return 0x34; }
    };

    my_root r;
    CHECK(r.on_get_ir() == 0x1234);
}
