
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#ifndef Z80_H
#define Z80_H

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>

namespace z80 {

typedef uint_fast8_t fast_u8;
typedef uint_least8_t least_u8;

typedef uint_fast16_t fast_u16;
typedef uint_least16_t least_u16;

typedef uint_fast32_t size_type;

static const fast_u8 mask8 = 0xff;
static const fast_u16 mask16 = 0xffff;

static inline void unused(...) {}

static inline fast_u8 get_low8(fast_u16 n) {
    return static_cast<fast_u8>(n & mask8);
}

static inline fast_u8 get_high8(fast_u16 n) {
    return static_cast<fast_u8>((n >> 8) & mask8);
}

static inline fast_u16 make16(fast_u8 hi, fast_u8 lo) {
    return (static_cast<fast_u16>(hi) << 8) | lo;
}

static inline fast_u16 add16(fast_u16 a, fast_u16 b) {
    return (a + b) & mask16;
}

static inline fast_u16 inc16(fast_u16 n) {
    return add16(n, 1);
}

template<typename D>
class instructions_decoder {
public:
    instructions_decoder() {}

    void decode() {
        fast_u8 op = (*this)->fetch_next_opcode();
        switch(op) {
        case 0x00: return (*this)->handle_nop();
        case 0xf3: return (*this)->handle_di();
        }

        // TODO
        std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>((*this)->get_last_fetch_addr()));
        std::abort();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

template<typename D>
class disassembler {
public:
    disassembler() {}

    void handle_nop() { (*this)->output("nop"); }
    void handle_di() { (*this)->output("di"); }

    void disassemble() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

template<typename D>
class processor {
public:
    processor() {}

    fast_u16 get_pc() const { return state.pc; }
    void set_pc(fast_u16 pc) { state.pc = pc; }

    fast_u16 get_pc_on_fetch() const { return (*this)->get_pc(); }
    void set_pc_on_fetch(fast_u16 pc) { (*this)->set_pc(pc); }

    bool get_iff1() const { return state.iff1; }
    void set_iff1(bool iff1) { state.iff1 = iff1; }

    bool get_iff1_on_di() const { return get_iff1(); }
    void set_iff1_on_di(bool iff1) { set_iff1(iff1); }

    bool get_iff2() const { return state.iff2; }
    void set_iff2(bool iff2) { state.iff2 = iff2; }

    bool get_iff2_on_di() const { return get_iff2(); }
    void set_iff2_on_di(bool iff2) { set_iff2(iff2); }

    fast_u16 get_last_fetch_addr() const { return state.last_fetch_addr; }

    fast_u8 fetch_opcode(fast_u16 addr) {
        (*this)->tick(4);
        return (*this)->at(addr);
    }

    void handle_nop() {}
    void handle_di() { (*this)->set_iff1_on_di(false);
                       (*this)->set_iff2_on_di(false); }

    fast_u8 fetch_next_opcode() {
        fast_u16 pc = (*this)->get_pc_on_fetch();
        fast_u8 op = (*this)->fetch_opcode(pc);
        state.last_fetch_addr = pc;
        (*this)->set_pc_on_fetch(inc16(pc));
        return op;
    }

    void step() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
    const D *operator -> () const { return static_cast<const D*>(this); }

    struct processor_state {
        processor_state()
            : last_fetch_addr(0), pc(0), iff1(false), iff2(false)
        {}

        fast_u16 last_fetch_addr;
        fast_u16 pc;
        bool iff1, iff2;
    } state;
};

}  // namespace z80

#endif  // Z80_H
