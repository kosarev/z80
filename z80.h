
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#ifndef Z80_H
#define Z80_H

#include <cassert>
#include <cstdint>

namespace z80 {

typedef uint_fast8_t fast_u8;
typedef uint_least8_t least_u8;

typedef uint_fast16_t fast_u16;
typedef uint_least16_t least_u16;

typedef uint_fast32_t size_type;
typedef uint_fast32_t ticks_type;

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

class trivial_memory_implementation {
public:
    trivial_memory_implementation()
        : image()
    {}

    least_u8 &operator [] (fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

    fast_u8 fetch_opcode(fast_u16 addr) {
        return (*this)[addr];
    }

    fast_u8 read8(fast_u16 addr) {
        return (*this)[addr];
    }

    void write8(fast_u16 addr, fast_u8 value) {
        (*this)[addr] = value;
    }

    fast_u16 read16(fast_u16 addr) {
        // It is important that we read the low byte first.
        fast_u8 lo = read8(addr);
        fast_u8 hi = read8(inc16(addr));
        return make16(hi, lo);
    }

    void write16(fast_u16 addr, fast_u16 value) {
        // It is important that we write the low byte first.
        write8(addr, get_low8(value));
        write8(inc16(addr), get_high8(value));
    }

private:
    static const size_type image_size = 0x10000;  // 64K bytes.
    least_u8 image[image_size];
};

enum class opcode_kind {
    nop,
};

class instructions_decoder_base {
protected:
    static opcode_kind decode(fast_u8 op, fast_u16 addr);
};

template<typename memory_interface>
class instructions_decoder : public instructions_decoder_base {
public:
    typedef memory_interface memory_interface_type;

    instructions_decoder(memory_interface_type &memory)
        : memory(memory)
    {}

    opcode_kind decode_opcode() {
        fast_u16 addr = current_addr;
        fast_u8 op = fetch_opcode();
        return decode(op, addr);
    }

protected:
    fast_u8 fetch_opcode() {
        fast_u8 op = memory.fetch_opcode(current_addr);
        current_addr = inc16(current_addr);
        return op;
    }

private:
    fast_u16 current_addr;
    memory_interface_type &memory;
};

#if 0  // TODO
class cpu_instance {
public:
    cpu_instance(memory_interface &memory);

private:
    instructions_decoder decoder;
};
#endif

}  // namespace z80

#endif  // Z80_H
