
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

class dummy_ticks_handler {
public:
    dummy_ticks_handler() {}

    void tick(unsigned t) { unused(t); }
};

template<typename T>
class trivial_ticks_counter {
public:
    trivial_ticks_counter() : ticks(0) {}

    T get_ticks() const { return ticks; }

    void tick(unsigned t) { ticks += t; }

private:
    T ticks;
};

template<typename D>
class memory_interface {
public:
    memory_interface() {}

    fast_u8 fetch_opcode(fast_u16 addr) {
        (*this)->tick(4);
        return (*this)->at(addr);
    }

    fast_u8 read8(fast_u16 addr) {
        (*this)->tick(3);
        return (*this)->at(addr);
    }

    void write8(fast_u16 addr, fast_u8 value) {
        (*this)->tick(3);
        (*this)->at(addr) = static_cast<least_u8>(value);
    }

    fast_u16 read16(fast_u16 addr) {
        fast_u8 lo = (*this)->read8(addr);
        fast_u8 hi = (*this)->read8(inc16(addr));
        return make16(hi, lo);
    }

    void write16(fast_u16 addr, fast_u16 value) {
        (*this)->write8(addr, get_low8(value));
        (*this)->write8(inc16(addr), get_high8(value));
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

template<typename D>
class instructions_decoder {
public:
    instructions_decoder() {}

    void decode() {
        fast_u8 op = (*this)->fetch_next_opcode();
        if(op == 0)
            return (*this)->handle_nop();

        // TODO
        std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>((*this)->get_instr_addr()));
        std::abort();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

template<typename D>
class disassembler {
public:
    disassembler() {}

    void handle_nop() {
        (*this)->output("nop");
    }

    void disassemble() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

template<typename D>
class processor {
public:
    processor()
        : instr_addr(0)
    {}

    fast_u16 get_instr_addr() const { return instr_addr; }
    void set_instr_addr(fast_u16 addr) { instr_addr = addr; }

    fast_u16 get_pc() const { return regs.pc; }
    void set_pc(fast_u16 pc) { regs.pc = pc; }

    fast_u16 get_pc_on_fetch() const { return (*this)->get_pc(); }
    void set_pc_on_fetch(fast_u16 pc) { (*this)->set_pc(pc); }

    void handle_nop() {}

    fast_u8 fetch_next_opcode() {
        fast_u16 pc = (*this)->get_pc_on_fetch();
        fast_u8 op = (*this)->fetch_opcode(pc);
        (*this)->set_pc_on_fetch(inc16(pc));
        return op;
    }

    void step() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
    const D *operator -> () const { return static_cast<const D*>(this); }

    fast_u16 instr_addr;

    struct register_file {
        register_file() : pc(0) {}

        fast_u16 pc;
    } regs;
};

}  // namespace z80

#endif  // Z80_H
