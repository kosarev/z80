
#include "z80.h"

#define CHECK(c) (check((c), __LINE__))

using z80::fast_u16;

static void check(bool c, unsigned line_no) {
    if(c)
        return;

    std::fprintf(stderr, "%s:%u: check failed\n", __FILE__,
                 static_cast<unsigned>(line_no));
    std::abort();
}

class my_emulator : public z80::z80_cpu<my_emulator>
{};

static void test_reset_state(const my_emulator &e) {
    CHECK(e.get_pc() == 0);
    CHECK(e.get_ir() == 0);
    CHECK(e.get_int_mode() == 0);
    CHECK(e.get_iff1() == 0);
    CHECK(e.get_iff2() == 0);
    CHECK(e.get_sp() == 0xffff);
    CHECK(e.get_af() == 0xffff);
}

static void test_initial_state() {
    my_emulator e;
    test_reset_state(e);
}

static void clobber(my_emulator &e) {
    e.set_pc(0x1111);
    e.set_ir(0x2222);
    e.set_int_mode(1);
    e.set_iff1(1);
    e.set_iff2(1);
    e.set_sp(0x3333);
    e.set_af(0x4444);
    e.set_bc(z80::inc16(e.get_bc()));
}

static void test_hard_reset() {
    my_emulator e;
    clobber(e);
    e.on_reset();
    test_reset_state(e);
}

static void test_soft_reset() {
    my_emulator e;
    clobber(e);

    fast_u16 bc = e.get_bc();

    e.on_reset(/* soft= */ true);
    test_reset_state(e);

    CHECK(e.get_bc() == bc);
}

int main() {
    test_initial_state();
    test_hard_reset();
    test_soft_reset();
}
