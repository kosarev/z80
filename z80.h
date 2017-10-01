
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

class instructions_decoder_base {
public:
    instructions_decoder_base()
        : current_addr(0)
    {}

protected:
    fast_u16 current_addr;
};

template<typename D>
class instructions_decoder : public instructions_decoder_base {
public:
    instructions_decoder() {}

    void decode() {
        fast_u16 instr_addr = current_addr;
        (*this)->set_instr_addr(instr_addr);

        fast_u8 op = (*this)->fetch_next_opcode();
        (*this)->set_pc(current_addr);

        if(op == 0)
            return (*this)->handle_nop();

        // TODO
        std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>(instr_addr));
        std::abort();
    }

protected:
    fast_u8 fetch_next_opcode() {
        fast_u8 op = (*this)->fetch_opcode(current_addr);
        current_addr = inc16(current_addr);
        return op;
    }

    D *operator -> () { return static_cast<D*>(this); }
};

class disassembler_base {
public:
    disassembler_base()
        : instr_addr(0), output()
    {}

    const char *get_output() const { return output; }

    fast_u16 get_instr_addr() const { return instr_addr; }
    void set_instr_addr(fast_u16 addr) { instr_addr = addr; }

    void set_pc(fast_u16 new_pc) { unused(new_pc); }

    void tick(unsigned t) { unused(t); }

    void handle_nop();

private:
    fast_u16 instr_addr;

    static const std::size_t max_output_size = 32;
    char output[max_output_size];
};

template<typename D>
class disassembler : public disassembler_base {
public:
    disassembler() {}

    void disassemble() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

class processor_base {
public:
    processor_base()
        : instr_addr(0), pc(0)
    {}

    fast_u16 get_instr_addr() const { return instr_addr; }
    void set_instr_addr(fast_u16 addr) { instr_addr = addr; }

    fast_u16 get_pc() const { return pc; }
    void set_pc(fast_u16 new_pc) { pc = new_pc; }

    void handle_nop() {}

private:
    fast_u16 instr_addr;
    fast_u16 pc;
};

template<typename D>
class processor : public processor_base {
public:
    processor() {}

    void step() {
        (*this)->decode();
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
};

}  // namespace z80

#endif  // Z80_H
