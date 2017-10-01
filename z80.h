
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

    void tick(unsigned t) {
        unused(t);
    }
};

template<typename ticks_handler>
class trivial_memory_handler {
public:
    trivial_memory_handler(ticks_handler &ticks)
        : ticks(ticks), image()
    {}

    least_u8 &operator [] (fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

    fast_u8 fetch_opcode(fast_u16 addr) {
        ticks.tick(4);
        return (*this)[addr];
    }

    fast_u8 read8(fast_u16 addr) {
        ticks.tick(3);
        return (*this)[addr];
    }

    void write8(fast_u16 addr, fast_u8 value) {
        ticks.tick(3);
        (*this)[addr] = value;
    }

    fast_u16 read16(fast_u16 addr) {
        fast_u8 lo = read8(addr);
        fast_u8 hi = read8(inc16(addr));
        return make16(hi, lo);
    }

    void write16(fast_u16 addr, fast_u16 value) {
        write8(addr, get_low8(value));
        write8(inc16(addr), get_high8(value));
    }

private:
    ticks_handler &ticks;

    static const size_type image_size = 0x10000;  // 64K bytes.
    least_u8 image[image_size];
};

class disassembling_handler {
public:
    disassembling_handler() {}

    const char *get_output() const {
        return output;
    }

    void nop(fast_u16 addr);

private:
    static const std::size_t max_output_size = 32;
    char output[max_output_size];
};

template<typename memory_handler, typename instructions_handler>
class instructions_decoder {
public:
    instructions_decoder(memory_handler &memory, instructions_handler &instrs)
        : current_addr(0), memory(memory), instrs(instrs)
    {}

    void decode() {
        fast_u16 addr = current_addr;
        fast_u8 op = fetch_opcode();

        if(op == 0)
            return instrs.nop(addr);

        // TODO
        std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op), static_cast<unsigned>(addr));
        std::abort();
    }

protected:
    fast_u8 fetch_opcode() {
        fast_u8 op = memory.fetch_opcode(current_addr);
        current_addr = inc16(current_addr);
        return op;
    }

private:
    fast_u16 current_addr;
    memory_handler &memory;
    instructions_handler &instrs;
};

#if 0  // TODO
class cpu_instance {
public:
    cpu_instance(memory_handler &memory);

private:
    instructions_decoder decoder;
};
#endif

}  // namespace z80

#endif  // Z80_H
