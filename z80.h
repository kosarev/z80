
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

static inline fast_u16 add16(fast_u16 a, fast_u16 b) {
    return (a + b) & mask16;
}

static inline fast_u16 inc16(fast_u16 n) {
    return add16(n, 1);
}

enum class memory_access_kind {
    transparent,
    fetch_opcode,
    access_data,
};

class memory_interface {
public:
    memory_interface(bool use_custom_read_handler = false,
                     bool use_custom_write_handler = false);

    least_u8 &operator [] (fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

    least_u8 *begin() { return image; }
    least_u8 *end() { return begin() + image_size; }

    fast_u8 read8(fast_u16 addr, memory_access_kind kind) {
        if(!use_custom_read_handler)
            return (*this)[addr];
        return on_read(addr, kind);
    }

    void write8(fast_u16 addr, fast_u8 value, memory_access_kind kind) {
        if(!use_custom_write_handler) {
            (*this)[addr] = value;
            return;
        }
        on_write(addr, value, kind);
    }

    fast_u16 read16(fast_u16 addr, memory_access_kind kind) {
        fast_u8 lo = read8(addr, kind);
        fast_u8 hi = read8(inc16(addr), kind);
        return (static_cast<fast_u16>(hi) << 8) | lo;
    }

    void write16(fast_u16 addr, fast_u16 value, memory_access_kind kind) {
        fast_u8 lo = static_cast<fast_u8>(value & mask8);
        write8(addr, lo, kind);

        fast_u8 hi = static_cast<fast_u8>((value >> 8) & mask8);
        write8(inc16(addr), hi, kind);
    }

    void clear();

protected:
    virtual fast_u8 on_read(fast_u16 addr, memory_access_kind kind);

    virtual void on_write(fast_u16 addr, fast_u8 value,
                          memory_access_kind kind);

private:
    // The purpose of these flags is to avoid unnecessary calls to custom
    // access handler.
    bool use_custom_read_handler;
    bool use_custom_write_handler;

    static const size_type image_size = 0x10000;  // 64K bytes.
    least_u8 image[image_size];
};

class instructions_decoder {
public:
    instructions_decoder(memory_interface &memory);

private:
    memory_interface &memory;
    fast_u16 current_addr;
};

class cpu_instance {
public:
    cpu_instance(memory_interface &memory);

private:
    memory_interface &memory;
    instructions_decoder decoder;
};

}  // namespace z80

#endif  // Z80_H
