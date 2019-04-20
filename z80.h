
/*  Z80 CPU Simulator.
    https://github.com/kosarev/z80

    Copyright (C) 2017-2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#ifndef Z80_H
#define Z80_H

#include <cassert>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <utility>

namespace z80 {

#if UINT_FAST8_MAX < UINT_MAX
typedef unsigned fast_u8;
#else
typedef uint_fast8_t fast_u8;
#endif

#if UINT_FAST16_MAX < UINT_MAX
typedef unsigned fast_u16;
#else
typedef uint_fast16_t fast_u16;
#endif

typedef uint_fast32_t fast_u32;

typedef uint_least8_t least_u8;
typedef uint_least16_t least_u16;

typedef fast_u32 size_type;

static inline void unused(...) {}

[[noreturn]] static inline void unreachable(const char *msg) {
    std::fprintf(stderr, "%s\n", msg);
    std::abort();
}

static inline constexpr fast_u8 mask8(fast_u8 n) {
    return n & 0xff;
}

static inline constexpr fast_u16 mask16(fast_u16 n) {
    return n & 0xffff;
}

static inline constexpr bool get_sign8(fast_u8 n) {
    return (n & 0x80) != 0;
}

static inline constexpr fast_u8 add8(fast_u8 a, fast_u8 b) {
    return mask8(a + b);
}

static inline constexpr fast_u8 sub8(fast_u8 a, fast_u8 b) {
    return mask8(a - b);
}

static inline constexpr fast_u8 inc8(fast_u8 n) {
    return add8(n, 1);
}

static inline constexpr fast_u8 dec8(fast_u8 n) {
    return sub8(n, 1);
}

static inline constexpr fast_u8 rol8(fast_u8 n) {
    return mask8((n << 1) | (n >> 7));
}

static inline constexpr fast_u8 ror8(fast_u8 n) {
    return mask8((n >> 1) | (n << 7));
}

static inline constexpr fast_u8 neg8(fast_u8 n) {
    return mask8(~n + 1);
}

static inline constexpr fast_u8 abs8(fast_u8 n) {
    return !get_sign8(n) ? n : neg8(n);
}

static inline constexpr int sign_extend8(fast_u8 n) {
    return !get_sign8(n) ? static_cast<int>(n) :
                           -static_cast<int>(neg8(n));
}

static inline constexpr fast_u8 get_low8(fast_u16 n) {
    return mask8(static_cast<fast_u8>(n));
}

static inline constexpr fast_u8 get_high8(fast_u16 n) {
    return mask8(static_cast<fast_u8>(n >> 8));
}

static inline constexpr fast_u16 make16(fast_u8 hi, fast_u8 lo) {
    return (static_cast<fast_u16>(hi) << 8) | lo;
}

static inline constexpr fast_u16 add16(fast_u16 a, fast_u16 b) {
    return mask16(a + b);
}

static inline constexpr fast_u16 sub16(fast_u16 a, fast_u16 b) {
    return mask16(a - b);
}

static inline constexpr fast_u16 inc16(fast_u16 n) {
    return add16(n, 1);
}

static inline constexpr fast_u16 dec16(fast_u16 n) {
    return sub16(n, 1);
}

enum class reg { b, c, d, e, h, l, at_hl, a };

enum class regp { bc, de, hl, sp };
enum class regp2 { bc, de, hl, af };
enum class index_regp { hl, ix, iy };

enum class alu { add, adc, sub, sbc, and_a, xor_a, or_a, cp };
enum class rot { rlc, rrc, rl, rr, sla, sra, sll, srl };
enum class block_ld { ldi, ldd, ldir, lddr };
enum class block_cp { cpi, cpd, cpir, cpdr };

enum condition { nz, z, nc, c, po, pe, p, m };

class i8080_decoder_state {};

class z80_decoder_state {
public:
    z80_decoder_state() {}

    index_regp get_index_rp_kind() const { return index_rp; }
    void set_index_rp_kind(index_regp irp) { index_rp = irp; }

    bool is_index_rp_hl() const {
        return get_index_rp_kind() == index_regp::hl;
    }

private:
    index_regp index_rp = index_regp::hl;
};

template<typename D, typename S>
class decoder_base : public S {
public:
    typedef S state;

    decoder_base() {}

    // TODO: Rename to 'decode()'.
    void decode_in_base(bool &handled, fast_u8 op) {
        handled = true;

        fast_u8 y = get_y_part(op);
        fast_u8 z = get_z_part(op);
        fast_u8 p = get_p_part(op);

        switch(op & x_mask) {
        case 0100: {
            // LD/MOV r[y], r[z] or
            // HALT/HLT (in place of LD (HL), (HL)/MOV M, M)
            // MOV r, r             f(5)
            // LD r, r              f(4)
            // LD r, (HL)           f(4)           r(3)
            // LD r, (i+d)     f(4) f(4) r(3) e(5) r(3)
            // LD (HL), r           f(4)           w(3)
            // LD (i+d), r     f(4) f(4) r(3) e(5) w(3)
            // HLT                  f(7)
            // HALT                 f(4)
            auto rd = static_cast<reg>(y);
            auto rs = static_cast<reg>(z);
            if(rd == reg::at_hl && rs == reg::at_hl)
                return (*this)->decode_halt();
            return (*this)->decode_ld_r_r(rd, rs); }
        case 0200: {
            // alu[y] r[z]
            // alu r            f(4)                 (both i8080 and z80)
            // alu M            f(4)           r(3)
            // alu (HL)         f(4)           r(3)
            // alu (i+d)   f(4) f(4) r(3) e(5) r(3)
            auto k = static_cast<alu>(y);
            auto r = static_cast<reg>(z);
            return (*this)->decode_alu_r(k, r); }
        }
        switch(op & (x_mask | z_mask)) {
        case 0004: {
            // INR/INC r[y]
            // INR r            f(5)
            // INR M            f(4)           r(3) r(3)
            // INC r            f(4)
            // INC (HL)         f(4)           r(4) w(3)
            // INC (i+d)   f(4) f(4) r(3) e(5) r(4) w(3)
            auto r = static_cast<reg>(y);
            return (*this)->decode_inc_r(r); }
        case 0005: {
            // DCR/DEC r[y]
            // DCR r            f(5)
            // DCR M            f(4)           r(3) w(3)
            // DEC r            f(4)
            // DEC (HL)         f(4)           r(4) w(3)
            // DEC (i+d)   f(4) f(4) r(3) e(5) r(4) w(3)
            auto r = static_cast<reg>(y);
            return (*this)->decode_dec_r(r); }
        case 0006: {
            // LD/MVI r[y], n
            // MVI r, n             f(4)      r(3)
            // LD r, n              f(4)      r(3)
            // LD (HL), n           f(4)      r(3) w(3)
            // LD (i+d), n     f(4) f(4) r(3) r(5) w(3)
            auto r = static_cast<reg>(y);
            return (*this)->decode_ld_r_n(r); }
        case 0300: {
            // RET cc[y]/Rcc[y]  f(5) + r(3) r(3)
            (*this)->on_5t_fetch_cycle();
            auto cc = static_cast<condition>(y);
            return (*this)->on_ret_cc(cc); }
        case 0302: {
            // Jcc[y] nn     f(4) r(3) r(3)
            // JP cc[y], nn  f(4) r(3) r(3)
            auto cc = static_cast<condition>(y);
            return (*this)->on_jp_cc_nn(cc, (*this)->on_3t_3t_imm16_read()); }
        case 0304: {
            // Ccc[y], nn
            // cc met:      f(5) r(3) r(3) w(3) w(3)
            // cc not met:  f(5) r(3) r(3)
            //
            // CALL cc[y], nn
            // cc met:      f(4) r(3) r(4) w(3) w(3)
            // cc not met:  f(4) r(3) r(3)
            auto cc = static_cast<condition>(y);
            return (*this)->decode_call_cc_nn(cc); }
        case 0307:
            // RST y*8  f(5) w(3) w(3)
            (*this)->on_5t_fetch_cycle();
            return (*this)->on_rst(y * 8);
        }
        switch(op & (x_mask | z_mask | q_mask)) {
        case 0001: {
            // LD/LXI rp[p], nn
            // LXI rp, nn       f(4) r(3) r(3)
            // LD rp, nn        f(4) r(3) r(3)
            // LD i, nn    f(4) f(4) r(3) r(3)
            auto rp = static_cast<regp>(p);
            return (*this)->on_ld_rp_nn(rp, (*this)->on_3t_3t_imm16_read()); }
        case 0011: {
            // ADD HL, rp[p] / DAD rp
            // DAD rp               f(4) e(3) e(3)
            // ADD HL, rp           f(4) e(4) e(3)
            // ADD i, rp       f(4) f(4) e(4) e(3)
            auto rp = static_cast<regp>(p);
            return (*this)->on_add_irp_rp(rp); }
        case 0013: {
            // DEC/DCX rp[p]
            // DCX rp           f(5)
            // DEC rp           f(6)
            // DEC i       f(4) f(6)
            auto rp = static_cast<regp>(p);
            return (*this)->decode_dec_rp(rp); }
        case 0003: {
            // INC/INX rp[p]
            // INX rp           f(5)
            // INC rp           f(6)
            // INC i       f(4) f(6)
            auto rp = static_cast<regp>(p);
            return (*this)->decode_inc_rp(rp); }
        case 0301: {
            // POP rp2[p]
            // POP rr           f(4) r(3) r(3)
            // POP i       f(4) f(4) r(3) r(3)
            auto rp = static_cast<regp2>(p);
            return (*this)->on_pop_rp(rp); }
        case 0305: {
            // PUSH rp2[p]
            // PUSH rr          f(5) w(3) w(3)
            // PUSH i      f(4) f(5) w(3) w(3)
            (*this)->on_5t_fetch_cycle();
            auto rp = static_cast<regp2>(p);
            return (*this)->on_push_rp(rp); }
        }
        switch(op & (x_mask | z_mask | q_mask | (p_mask - 1))) {
        case 0002: {
            // STAX rp[p]     f(4) w(3)
            // LD (rp[p]), A  f(4) w(3)
            auto rp = static_cast<regp>(p);
            return (*this)->on_ld_at_rp_a(rp); }
        case 0012: {
            // LDAX rp[p]     f(4) r(3)
            // LD A, (rp[p])  f(4) r(3)
            auto rp = static_cast<regp>(p);
            return (*this)->on_ld_a_at_rp(rp); }
        }
        switch(op) {
        case 0x00:
            // NOP  f(4)
            return (*this)->on_nop();
        case 0x07:
            // RLC   f(4)
            // RLCA  f(4)
            return (*this)->on_rlca();
        case 0x0f:
            // RRC   f(4)
            // RRCA  f(4)
            return (*this)->on_rrca();
        case 0x17:
            // RAL  f(4)
            // RLA  f(4)
            return (*this)->on_rla();
        case 0x1f:
            // RAR  f(4)
            // RRA  f(4)
            return (*this)->on_rra();
        case 0x22:
            // SHLD nn              f(4) r(3) r(3) w(3) w(3)
            // LD (nn), HL          f(4) r(3) r(3) w(3) w(3)
            // LD (nn), i      f(4) f(4) r(3) r(3) w(3) w(3)
            return (*this)->on_ld_at_nn_irp((*this)->on_3t_3t_imm16_read());
        case 0x2a:
            // LHLD nn              f(4) r(3) r(3) r(3) r(3)
            // LD HL, (nn)          f(4) r(3) r(3) r(3) r(3)
            // LD i, (nn)      f(4) f(4) r(3) r(3) r(3) r(3)
            return (*this)->on_ld_irp_at_nn((*this)->on_3t_3t_imm16_read());
        case 0x2f:
            // CMA  f(4)
            // CPL  f(4)
            return (*this)->on_cpl();
        case 0x32:
            // STA nn      f(4) r(3) r(3) w(3)
            // LD (nn), A  f(4) r(3) r(3) w(3)
            return (*this)->on_ld_at_nn_a((*this)->on_3t_3t_imm16_read());
        case 0x37:
            // STC  f(4)
            // SCF  f(4)
            return (*this)->on_scf();
        case 0x3f:
            // CMC  f(4)
            // CCF  f(4)
            return (*this)->on_ccf();
        case 0x3a:
            // LDA nn      f(4) r(3) r(3) r(3)
            // LD A, (nn)  f(4) r(3) r(3) r(3)
            return (*this)->on_ld_a_at_nn((*this)->on_3t_3t_imm16_read());
        case 0xc3:
            // JMP nn  f(4) r(3) r(3)
            // JP nn   f(4) r(3) r(3)
            return (*this)->on_jp_nn((*this)->on_3t_3t_imm16_read());
        case 0xc9:
            // RET  f(4) r(3) r(3)
            return (*this)->on_ret();
        case 0xcd:
            // CALL nn  f(4) r(3) r(4) w(3) w(3)
            return (*this)->on_call_nn((*this)->on_3t_4t_imm16_read());
        case 0xe3:
            // EX (SP), irp / XHTL
            // XTHL                 f(4) r(3) r(3) w(3) w(5)
            // EX (SP), HL          f(4) r(3) r(4) w(3) w(5)
            // EX (SP), i      f(4) f(4) r(3) r(4) w(3) w(5)
            return (*this)->on_ex_at_sp_irp();
        case 0xe9:
            // PCHL        f(5)
            // JP HL       f(4)
            // JP i   f(4) f(4)
            return (*this)->decode_jp_irp();
        case 0xeb:
            // XCHG       f(5)
            // EX DE, HL  f(4)
            return (*this)->decode_ex_de_hl();
        case 0xf3:
            // DI  f(4)
            return (*this)->on_di();
        case 0xf9:
            // SPHL            f(5)
            // LD SP, HL       f(6)
            // LD SP, i   f(4) f(6)
            return (*this)->decode_ld_sp_irp();
        case 0xfb:
            // EI  f(4)
            return (*this)->on_ei();
        }

        handled = false;
    }

    void decode() { (*this)->on_decode(); }

protected:
    D *operator -> () { return static_cast<D*>(this); }

    static const fast_u8 x_mask = 0300;

    static const fast_u8 y_mask = 0070;
    fast_u8 get_y_part(fast_u8 op) { return (op & y_mask) >> 3; }

    static const fast_u8 z_mask = 0007;
    fast_u8 get_z_part(fast_u8 op) { return op & z_mask; }

    static const fast_u8 p_mask = 0060;
    fast_u8 get_p_part(fast_u8 op) { return (op & p_mask) >> 4; }

    static const fast_u8 q_mask = 0010;
};

template<typename D, typename S = i8080_decoder_state>
class i8080_decoder : public decoder_base<D, S> {
public:
    typedef S state;
    typedef D derived;
    typedef decoder_base<derived, state> base;

    void decode_alu_r(alu k, reg r) {
        (*this)->on_alu_r(k, r); }
    void decode_call_cc_nn(condition cc) {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_call_cc_nn(cc, (*this)->on_3t_3t_imm16_read()); }
    void decode_dec_r(reg r) {
        if(r != reg::at_hl)
            (*this)->on_5t_fetch_cycle();
        (*this)->on_dec_r(r); }
    void decode_dec_rp(regp rp) {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_dec_rp(rp); }
    void decode_ex_de_hl() {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_ex_de_hl(); }
    void decode_halt() {
        (*this)->on_7t_fetch_cycle();
        (*this)->on_halt(); }
    void decode_inc_r(reg r) {
        if(r != reg::at_hl)
            (*this)->on_5t_fetch_cycle();
        (*this)->on_inc_r(r); }
    void decode_inc_rp(regp rp) {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_inc_rp(rp); }
    void decode_jp_irp() {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_jp_irp(); }
    void decode_ld_r_n(reg r) {
        fast_u8 n = (*this)->on_3t_imm8_read();
        (*this)->on_ld_r_n(r, n); }
    void decode_ld_r_r(reg rd, reg rs) {
        (*this)->on_ld_r_r(rd, rs); }
    void decode_ld_sp_irp() {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_ld_sp_irp(); }

    void on_decode() {
        fast_u8 op = (*this)->on_fetch();

        // TODO
        bool handled = false;
        base::decode_in_base(handled, op);
        assert(handled);
    }
};

template<typename D, typename S = z80_decoder_state>
class z80_decoder : public decoder_base<D, S> {
public:
    typedef S state;
    typedef D derived;
    typedef decoder_base<derived, state> base;

    z80_decoder() {}

    using state::get_index_rp_kind;
    using state::set_index_rp_kind;
    using state::is_index_rp_hl;

    fast_u8 read_disp_or_null(bool may_need_disp = true) {
        if(is_index_rp_hl() || !may_need_disp)
            return 0;
        fast_u8 d = (*this)->on_disp_read();
        (*this)->on_5t_exec_cycle();
        return d;
    }

    fast_u8 read_disp_or_null(reg r) {
        return read_disp_or_null(r == reg::at_hl);
    }

    fast_u8 read_disp_or_null(reg r1, reg r2) {
        return read_disp_or_null(r1 == reg::at_hl || r2 == reg::at_hl);
    }

    void on_disable_int() {}
    void disable_int_on_index_prefix() { (*this)->on_disable_int(); }

    void on_set_index_rp_kind(index_regp irp) {
        set_index_rp_kind(irp);
        (*this)->disable_int_on_index_prefix();
    }

    unsigned decode_int_mode(fast_u8 y) {
        y &= 3;
        return y < 2 ? 0 : y - 1;
    }

    void decode_alu_r(alu k, reg r) {
        (*this)->on_alu_r(k, r, read_disp_or_null(r)); }
    void decode_call_cc_nn(condition cc) {
        // TODO: Read imm16 as r(3) r(3) and do extra tick on
        // execution if the condition is met. Do not check the
        // condition on decoding.
        bool cc_met = (*this)->check_condition(cc);
        fast_u16 nn = cc_met ? (*this)->on_3t_4t_imm16_read() :
                               (*this)->on_3t_3t_imm16_read();
        (*this)->on_call_cc_nn(cc, nn); }
    void decode_dec_r(reg r) {
        (*this)->on_dec_r(r, read_disp_or_null(r)); }
    void decode_dec_rp(regp rp) {
        (*this)->on_6t_fetch_cycle();
        (*this)->on_dec_rp(rp); }
    void decode_ex_de_hl() {
        (*this)->on_ex_de_hl(); }
    void decode_halt() {
        (*this)->on_halt(); }
    void decode_jp_irp() {
        (*this)->on_jp_irp(); }
    void decode_inc_r(reg r) {
        (*this)->on_inc_r(r, read_disp_or_null(r)); }
    void decode_inc_rp(regp rp) {
        (*this)->on_6t_fetch_cycle();
        (*this)->on_inc_rp(rp); }
    void decode_ld_r_n(reg r) {
        fast_u8 d, n;
        if(r != reg::at_hl || is_index_rp_hl()) {
            d = 0;
            n = (*this)->on_3t_imm8_read();
        } else {
            d = (*this)->on_disp_read();
            n = (*this)->on_5t_imm8_read();
        }
        (*this)->on_ld_r_n(r, d, n); }
    void decode_ld_r_r(reg rd, reg rs) {
        (*this)->on_ld_r_r(rd, rs, read_disp_or_null(rd, rs)); }
    void decode_ld_sp_irp() {
        (*this)->on_6t_fetch_cycle();
        (*this)->on_ld_sp_irp(); }

    void decode_unprefixed(bool &reset_index_rp) {
        fast_u8 op = (*this)->on_fetch();
        fast_u8 y = get_y_part(op);

        // TODO
        {
            bool handled = false;
            base::decode_in_base(handled, op);
            if(handled)
                return;
        }

        switch(op & (x_mask | z_mask)) {
        case 0306: {
            // alu[y] n  f(4) r(3)
            auto k = static_cast<alu>(y);
            return (*this)->on_alu_n(k, (*this)->on_3t_imm8_read()); }
        }
        if((op & (x_mask | z_mask | (y_mask - 0030))) == 0040) {
            // JR cc[y-4], d  f(4) r(3) + e(5)
            auto cc = static_cast<condition>((op & (y_mask - 0040)) >> 3);
            return (*this)->on_jr_cc(cc, (*this)->on_disp_read());
        }
        switch(op) {
        case 0x08:
            // EX AF, AF'  f(4)
            return (*this)->on_ex_af_alt_af();
        case 0x10:
            // DJNZ  f(5) r(3) + e(5)
            (*this)->on_5t_fetch_cycle();
            return (*this)->on_djnz((*this)->on_disp_read());
        case 0x18:
            // JR d  f(4) r(3) e(5)
            return (*this)->on_jr((*this)->on_disp_read());
        case 0x27:
            // DAA  f(4)
            return (*this)->on_daa();
        case 0xcb:
            // CB prefix.
            return decode_cb_prefixed();
        case 0xd3:
            // OUT (n), A  f(4) r(3) o(4)
            return (*this)->on_out_n_a((*this)->on_3t_imm8_read());
        case 0xd9:
            // EXX  f(4)
            return (*this)->on_exx();
        case 0xdb:
            // IN A, (n)  f(4) r(3) i(4)
            return (*this)->on_in_a_n((*this)->on_3t_imm8_read());
        case 0xdd:
            // DD prefix (IX-indexed instructions).
            reset_index_rp = false;
            return (*this)->on_set_index_rp_kind(index_regp::ix);
        case 0xed:
            // ED prefix.
            return decode_ed_prefixed();
        case 0xfd:
            // FD prefix (IY-indexed instructions).
            reset_index_rp = false;
            return (*this)->on_set_index_rp_kind(index_regp::iy);
        }

        // TODO
        std::fprintf(stderr, "Unknown opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>((*this)->get_last_read_addr()));
        std::abort();
    }

    void decode_cb_prefixed() {
        fast_u8 d = 0;
        index_regp irp = get_index_rp_kind();
        if(irp != index_regp::hl)
            d = (*this)->on_disp_read();

        fast_u8 op;
        if(irp == index_regp::hl) {
            op = (*this)->on_fetch(/* m1= */ true);
        } else {
            // In ddcb- and fdcb-prefixed instructions the
            // reading of the 3rd opcode is not an M1 cycle.
            op = (*this)->on_fetch(/* m1= */ false);
            (*this)->on_5t_fetch_cycle();
        }

        fast_u8 y = get_y_part(op);
        fast_u8 z = get_z_part(op);

        auto r = static_cast<reg>(z);

        switch(op & x_mask) {
        case 0000: {
            // rot[y] r[z]
            // rot r                f(4)      f(4)
            // rot (HL)             f(4)      f(4) r(4) w(3)
            // rot (i+d)       f(4) f(4) r(3) f(5) r(4) w(3)
            auto k = static_cast<rot>(y);
            return (*this)->on_rot(k, r, d); }
        case 0100: {
            // BIT y, r[z]
            // BIT b, r             f(4)      f(4)
            // BIT b, (HL)          f(4)      f(4) r(4)
            // BIT b, (i+d)    f(4) f(4) r(3) f(5) r(4)
            auto b = static_cast<unsigned>(y);
            return (*this)->on_bit(b, r, d); }
        case 0200: {
            // RES y, r[z]
            // RES b, r             f(4)      f(4)
            // RES b, (HL)          f(4)      f(4) r(4) w(3)
            // RES b, (i+d)    f(4) f(4) r(3) f(5) r(4) w(3)
            auto b = static_cast<unsigned>(y);
            return (*this)->on_res(b, r, d); }
        case 0300: {
            // SET y, r[z]
            // SET b, r             f(4)      f(4)
            // SET b, (HL)          f(4)      f(4) r(4) w(3)
            // SET b, (i+d)    f(4) f(4) r(3) f(5) r(4) w(3)
            auto b = static_cast<unsigned>(y);
            return (*this)->on_set(b, r, d); }
        }

        std::fprintf(stderr, "Unknown CB-prefixed opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>((*this)->get_last_read_addr()));
        std::abort();
    }

    void decode_ed_prefixed() {
        fast_u8 op = (*this)->on_fetch();
        fast_u8 y = get_y_part(op);
        fast_u8 p = get_p_part(op);

        switch(op & (x_mask | z_mask)) {
        case 0100: {
            // IN r[y], (C)  f(4) f(4) i(4)
            // IN (C)        f(4) f(4) i(4)
            auto r = static_cast<reg>(y);
            return (*this)->on_in_r_c(r); }
        case 0101: {
            // OUT (C), r[y]  f(4) f(4) o(4)
            // OUT (C), 0     f(4) f(4) o(4)
            auto r = static_cast<reg>(y);
            return (*this)->on_out_c_r(r); }
        case 0102: {
            // ADC HL, rp[p]  f(4) f(4) e(4) e(3)
            // SBC HL, rp[p]  f(4) f(4) e(4) e(3)
            auto rp = static_cast<regp>(p);
            return op & q_mask ? (*this)->on_adc_hl_rp(rp) :
                                 (*this)->on_sbc_hl_rp(rp); }
        case 0103: {
            // LD rp[p], (nn)  f(4) f(4) r(3) r(3) r(3) r(3)
            // LD (nn), rp[p]  f(4) f(4) r(3) r(3) w(3) w(3)
            auto rp = static_cast<regp>(p);
            fast_u16 nn = (*this)->on_3t_3t_imm16_read();
            return op & q_mask ? (*this)->on_ld_rp_at_nn(rp, nn) :
                                 (*this)->on_ld_at_nn_rp(nn, rp); }
        case 0104:
            // NEG  f(4) f(4)
            return (*this)->on_neg();
        case 0105:
            // RETI  f(4) f(4) r(3) r(3)
            // RETN  f(4) f(4) r(3) r(3)
            if(y == 1)
                return (*this)->on_reti();
            return (*this)->on_retn();
        case 0106: {
            // IM im[y]  f(4) f(4)
            return (*this)->on_im(decode_int_mode(y)); }
        case 0200: {
            // LDI, LDD, LDIR, LDDR  f(4) f(4) r(3) w(5) + e(5)
            if(y < 4)
                return (*this)->on_noni_ed(op);
            auto k = static_cast<block_ld>(y - 4);
            return (*this)->on_block_ld(k); }
        case 0201: {
            // CPI, CPD, CPIR, CPDR  f(4) f(4) r(3) e(5) + e(5)
            if(y < 4)
                return (*this)->on_noni_ed(op);
            auto k = static_cast<block_cp>(y - 4);
            return (*this)->on_block_cp(k); }
        }
        switch(op) {
        case 0x47: {
            // LD I, A  f(4) f(5)
            (*this)->on_5t_fetch_cycle();
            return (*this)->on_ld_i_a(); }
        case 0x4f: {
            // LD R, A  f(4) f(5)
            (*this)->on_5t_fetch_cycle();
            return (*this)->on_ld_r_a(); }
        case 0x5f: {
            // LD A, R  f(4) f(5)
            (*this)->on_5t_fetch_cycle();
            return (*this)->on_ld_a_r(); }
        case 0x67:
            // RRD  f(4) f(4) r(3) e(4) w(3)
            return (*this)->on_rrd();
        case 0x6f:
            // RLD  f(4) f(4) r(3) e(4) w(3)
            return (*this)->on_rld();
        }

        std::fprintf(stderr, "Unknown ED-prefixed opcode 0x%02x at 0x%04x.\n",
                     static_cast<unsigned>(op),
                     static_cast<unsigned>((*this)->get_last_read_addr()));
        std::abort();
    }

    void on_decode() {
        bool reset_index_rp = true;
        decode_unprefixed(reset_index_rp);
        if(reset_index_rp)
            set_index_rp_kind(index_regp::hl);
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }

    using base::x_mask;
    using base::y_mask;
    using base::z_mask;
    using base::p_mask;
    using base::q_mask;

    using base::get_y_part;
    using base::get_z_part;
    using base::get_p_part;
};

const char *get_reg_name(index_regp irp);
const char *get_mnemonic(rot k);
const char *get_mnemonic(block_ld k);
const char *get_mnemonic(block_cp k);
bool is_two_operand_alu_instr(alu k);
const char *get_index_reg_name(index_regp irp);
const char *get_condition_name(condition cc);

template<typename E>
class disassembler_base : public E {
protected:
    class output_buff;

public:
    typedef E decoder;
    typedef typename decoder::derived derived;
    typedef typename decoder::state state;

    disassembler_base() {}

    void on_5t_fetch_cycle() {}

    fast_u8 on_3t_imm8_read() { return (*this)->on_read_next_byte(); }

    fast_u16 on_3t_3t_imm16_read() {
        fast_u8 lo = (*this)->on_read_next_byte();
        fast_u8 hi = (*this)->on_read_next_byte();
        return make16(hi, lo); }
    fast_u16 on_3t_4t_imm16_read() {
        fast_u8 lo = (*this)->on_read_next_byte();
        fast_u8 hi = (*this)->on_read_next_byte();
        return make16(hi, lo); }

    void on_format(const char *fmt) {
        (*this)->on_format_impl(fmt, /* args= */ nullptr);
    }

    template<typename... types>
    void on_format(const char *fmt, const types &... args) {
        const void *ptrs[] = { static_cast<const void*>(&args)... };
        (*this)->on_format_impl(fmt, ptrs);
    }

    void on_format_char(char c, const void **&args, output_buff &out) {
        switch(c) {
        case 'C': {  // A condition operand.
            auto cc = get_arg<condition>(args);
            out.append(get_condition_name(cc));
            break; }
        case 'N': {  // An 8-bit immediate operand.
            auto n = get_arg<fast_u8>(args);
            out.append_u8(n);
            break; }
        case 'W': {  // A 16-bit immediate operand.
            auto nn = get_arg<fast_u16>(args);
            out.append_u16(nn);
            break; }
        default:
            out.append(c);
        }
    }

    void on_format_impl(const char *fmt, const void *args[]) {
        output_buff out;
        for(const char *p = fmt; *p != '\0'; ++p)
            (*this)->on_format_char(*p, args, out);
        out.append('\0');
        (*this)->on_output(out.get_buff());
    }

    void on_call_nn(fast_u16 nn) {
        (*this)->on_format("call W", nn); }
    void on_di() {
        (*this)->on_format("di"); }
    void on_ei() {
        (*this)->on_format("ei"); }
    void on_nop() {
        (*this)->on_format("nop"); }
    void on_ret() {
        (*this)->on_format("ret"); }
    void on_rst(fast_u16 nn) {
        (*this)->on_format("rst W", nn); }

protected:
    derived *operator -> () { return static_cast<derived*>(this); }

    class output_buff {
    public:
        output_buff() {}

        const char *get_buff() const {
            return buff;
        }

        void append(char c) {
            assert(size < max_size);
            buff[size++] = c;
        }

        void append(const char *str) {
            for(const char *p = str; *p != '\0'; ++p)
                append(*p);
        }

        void append_u8(fast_u8 n) {
            char pad[32];
            std::snprintf(pad, sizeof(pad), "0x%02x",
                          static_cast<unsigned>(n));
            append(pad);
        }

        void append_u(unsigned n) {
            char pad[32];
            std::snprintf(pad, sizeof(pad), "%u",
                          static_cast<unsigned>(n));
            append(pad);
        }

        void append_u16(fast_u16 n) {
            char pad[32];
            std::snprintf(pad, sizeof(pad), "0x%04x",
                          static_cast<unsigned>(n));
            append(pad);
        }

        void append_disp(int d) {
            char pad[32];
            std::snprintf(pad, sizeof(pad), "%c %d",
                          static_cast<int>(d < 0 ? '-' : '+'),
                          static_cast<int>(std::abs(d)));
            append(pad);
        }

    private:
        static const unsigned max_size = 32;
        unsigned size = 0;
        char buff[max_size];
    };

    template<typename T>
    static T get_arg(const void **&args) {
        const T &value = *static_cast<const T*>(*args);
        ++args;
        return value;
    }
};

template<typename D>
class i8080_disassembler : public disassembler_base<i8080_decoder<D>> {
public:
    typedef D derived;
    typedef disassembler_base<i8080_decoder<derived>> base;
    typedef typename base::state state;

    i8080_disassembler() {}

    void on_7t_fetch_cycle() {}

    fast_u8 on_fetch() {
        return (*this)->on_read_next_byte(); }

    void on_format_char(char c, const void **&args,
                        typename base::output_buff &out) {
        switch(c) {
        case 'A': {  // ALU mnemonic.
            auto k = get_arg<alu>(args);
            out.append(get_mnemonic_r(k));
            break; }
        case 'G': {  // An alternative register pair.
            auto rp = get_arg<regp2>(args);
            out.append(get_reg_name(rp));
            break; }
        case 'R': {  // A register.
            auto r = get_arg<reg>(args);
            out.append(get_reg_name(r));
            break; }
        case 'P': {  // A register pair.
            auto rp = get_arg<regp>(args);
            out.append(get_reg_name(rp));
            break; }
        default:
            base::on_format_char(c, args, out);
        }
    }

    void on_add_irp_rp(regp rp) {
        (*this)->on_format("dad P", rp); }
    void on_alu_r(alu k, reg r) {
        (*this)->on_format("A R", k, r); }
    void on_call_cc_nn(condition cc, fast_u16 nn) {
        (*this)->on_format("cC W", cc, nn); }
    void on_ccf() {
        (*this)->on_format("cmc"); }
    void on_cpl() {
        (*this)->on_format("cma"); }
    void on_dec_r(reg r) {
        (*this)->on_format("dcr R", r); }
    void on_dec_rp(regp rp) {
        (*this)->on_format("dcx P", rp); }
    void on_ex_de_hl() {
        (*this)->on_format("xchg"); }
    void on_ex_at_sp_irp() {
        (*this)->on_format("xthl"); }
    void on_halt() {
        (*this)->on_format("hlt"); }
    void on_jp_irp() {
        (*this)->on_format("pchl"); }
    void on_jp_nn(fast_u16 nn) {
        (*this)->on_format("jmp W", nn); }
    void on_jp_cc_nn(condition cc, fast_u16 nn) {
        (*this)->on_format("jC W", cc, nn); }
    void on_inc_r(reg r) {
        (*this)->on_format("inr R", r); }
    void on_inc_rp(regp rp) {
        (*this)->on_format("inx P", rp); }
    void on_ld_a_at_nn(fast_u16 nn) {
        (*this)->on_format("lda W", nn); }
    void on_ld_at_nn_a(fast_u16 nn) {
        (*this)->on_format("sta W", nn); }
    void on_ld_a_at_rp(regp rp) {
        (*this)->on_format("ldax P", rp); }
    void on_ld_at_rp_a(regp rp) {
        (*this)->on_format("stax P", rp); }
    void on_ld_r_n(reg r, fast_u8 n) {
        (*this)->on_format("mvi R, N", r, n); }
    void on_ld_r_r(reg rd, reg rs) {
        (*this)->on_format("mov R, R", rd, rs); }
    void on_ld_rp_nn(regp rp, fast_u16 nn) {
        (*this)->on_format("lxi P, W", rp, nn); }
    void on_ld_sp_irp() {
        (*this)->on_format("sphl"); }
    void on_ld_irp_at_nn(fast_u16 nn) {
        (*this)->on_format("lhld W", nn); }
    void on_ld_at_nn_irp(fast_u16 nn) {
        (*this)->on_format("shld W", nn); }
    void on_pop_rp(regp2 rp) {
        (*this)->on_format("pop G", rp); }
    void on_push_rp(regp2 rp) {
        (*this)->on_format("push G", rp); }
    void on_rla() {
        (*this)->on_format("ral"); }
    void on_rra() {
        (*this)->on_format("rar"); }
    void on_rlca() {
        (*this)->on_format("rlc"); }
    void on_rrca() {
        (*this)->on_format("rrc"); }
    void on_ret_cc(condition cc) {
        (*this)->on_format("rC", cc); }
    void on_scf() {
        (*this)->on_format("stc"); }

    void disassemble() { (*this)->decode(); }

protected:
    template<typename T>
    static T get_arg(const void **&args) {
        return base::template get_arg<T>(args);
    }

    static const char *get_reg_name(reg r) {
        switch(r) {
        case reg::b: return "b";
        case reg::c: return "c";
        case reg::d: return "d";
        case reg::e: return "e";
        case reg::a: return "a";
        case reg::h: return "h";
        case reg::l: return "l";
        case reg::at_hl: return "m";
        }
        unreachable("Unknown register.");
    }

    static const char *get_reg_name(regp rp) {
        switch(rp) {
        case regp::bc: return "b";
        case regp::de: return "d";
        case regp::hl: return "h";
        case regp::sp: return "sp";
        }
        unreachable("Unknown register.");
    }

    static const char *get_reg_name(regp2 rp) {
        switch(rp) {
        case regp2::bc: return "b";
        case regp2::de: return "d";
        case regp2::hl: return "h";
        case regp2::af: return "psw";
        }
        unreachable("Unknown register.");
    }

    static const char *get_mnemonic_r(alu k) {
        switch(k) {
        case alu::add: return "add";
        case alu::adc: return "adc";
        case alu::sub: return "sub";
        case alu::sbc: return "sbb";
        case alu::and_a: return "ana";
        case alu::xor_a: return "xra";
        case alu::or_a: return "ora";
        case alu::cp: return "cmp";
        }
        unreachable("Unknown ALU operation.");
    }
};

template<typename D>
class z80_disassembler : public disassembler_base<z80_decoder<D>> {
public:
    typedef D derived;
    typedef disassembler_base<z80_decoder<derived>> base;
    typedef typename base::state state;

    using state::get_index_rp_kind;

    z80_disassembler() {}

    // TODO: What if to replace 'on_fetch(false)' with a special
    // method like 'on_non_m1_fetch()'?
    fast_u8 on_fetch(bool m1 = true) {
        unused(m1);
        return (*this)->on_read_next_byte(); }
    void on_6t_fetch_cycle() {}

    // 'call cc, nn' instructions require this function to disambiguate between
    // read cycles of various lengths. This disambiguation does not affect
    // disassembling so we just return false here.
    bool check_condition(condition cc) {
        unused(cc);
        return false;
    }

    fast_u8 on_5t_imm8_read() { return (*this)->on_read_next_byte(); }

    fast_u8 on_disp_read() { return (*this)->on_read_next_byte(); }

    void on_3t_exec_cycle() {}
    void on_4t_exec_cycle() {}
    void on_5t_exec_cycle() {}

    void on_format_char(char c, const void **&args,
                        typename base::output_buff &out) {
        switch(c) {
        case 'A': {  // ALU mnemonic.
            auto k = get_arg<alu>(args);
            out.append(get_mnemonic(k));
            if(is_two_operand_alu_instr(k))
                out.append(" a,");
            break; }
        case 'O': {  // Rotation mnemonic.
            auto k = get_arg<rot>(args);
            out.append(z80::get_mnemonic(k));
            break; }
        case 'R': {  // A register.
            auto r = get_arg<reg>(args);
            auto irp = get_arg<index_regp>(args);
            auto d = get_arg<fast_u8>(args);
            if(r != reg::at_hl || irp == index_regp::hl) {
                out.append(get_reg_name(r, irp));
            } else {
                out.append('(');
                out.append(get_index_reg_name(irp));
                out.append(' ');
                out.append_disp(sign_extend8(d));
                out.append(')');
            }
            break; }
        case 'P': {  // A register pair.
            auto rp = get_arg<regp>(args);
            auto irp = get_arg<index_regp>(args);
            out.append(get_reg_name(rp, irp));
            break; }
        case 'G': {  // An alternative register pair.
            auto rp = get_arg<regp2>(args);
            auto irp = get_arg<index_regp>(args);
            out.append(get_reg_name(rp, irp));
            break; }
        case 'N': {  // An 8-bit immediate operand.
            auto n = get_arg<fast_u8>(args);
            out.append_u8(n);
            break; }
        case 'U': {  // A decimal number.
            auto u = get_arg<unsigned>(args);
            out.append_u(u);
            break; }
        case 'D': {  // A relative address.
            auto d = get_arg<int>(args);
            out.append("$ ");
            out.append_disp(d);
            break; }
        case 'L': {  // A block transfer instruction.
            auto k = get_arg<block_ld>(args);
            out.append(z80::get_mnemonic(k));
            break; }
        case 'M': {  // A block comparison instruction.
            auto k = get_arg<block_cp>(args);
            out.append(z80::get_mnemonic(k));
            break; }
        default:
            base::on_format_char(c, args, out);
        }
    }

    void on_noni_ed(fast_u8 op) {
        (*this)->on_format("noni N, N", 0xed, op); }

    void on_add_irp_rp(regp rp) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("add P, P", regp::hl, irp, rp, irp); }
    void on_adc_hl_rp(regp rp) {
        (*this)->on_format("adc hl, P", rp, index_regp::hl); }
    void on_alu_n(alu k, fast_u8 n) {
        (*this)->on_format("A N", k, n); }
    void on_alu_r(alu k, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("A R", k, r, irp, d); }
    void on_block_cp(block_cp k) {
        (*this)->on_format("M", k); }
    void on_block_ld(block_ld k) {
        (*this)->on_format("L", k); }
    void on_bit(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("bit U, R", b, r, irp, d); }
    void on_call_cc_nn(condition cc, fast_u16 nn) {
        (*this)->on_format("call C, W", cc, nn); }
    void on_ccf() {
        (*this)->on_format("ccf"); }
    void on_cpl() {
        (*this)->on_format("cpl"); }
    void on_daa() {
        (*this)->on_format("daa"); }
    void on_dec_r(reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("dec R", r, irp, d); }
    void on_dec_rp(regp rp) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("dec P", rp, irp); }
    void on_djnz(fast_u8 d) {
        (*this)->on_format("djnz D", sign_extend8(d) + 2); }
    void on_ex_af_alt_af() {
        (*this)->on_format("ex af, af'"); }
    void on_ex_de_hl() {
        (*this)->on_format("ex de, hl"); }
    void on_ex_at_sp_irp() {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ex (sp), P", regp::hl, irp); }
    void on_exx() {
        (*this)->on_format("exx"); }
    void on_halt() {
        (*this)->on_format("halt"); }
    void on_im(unsigned mode) {
        (*this)->on_format("im U", mode); }
    void on_in_a_n(fast_u8 n) {
        (*this)->on_format("in a, (N)", n); }
    void on_in_r_c(reg r) {
        if(r == reg::at_hl)
            (*this)->on_format("in (c)");
        else
            (*this)->on_format("in R, (c)", r, index_regp::hl, 0); }
    void on_inc_r(reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("inc R", r, irp, d); }
    void on_inc_rp(regp rp) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("inc P", rp, irp); }
    void on_jp_cc_nn(condition cc, fast_u16 nn) {
        (*this)->on_format("jp C, W", cc, nn); }
    void on_jp_irp() {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("jp (P)", regp::hl, irp); }
    void on_jp_nn(fast_u16 nn) {
        (*this)->on_format("jp W", nn); }
    void on_jr(fast_u8 d) {
        (*this)->on_format("jr D", sign_extend8(d) + 2); }
    void on_jr_cc(condition cc, fast_u8 d) {
        (*this)->on_format("jr C, D", cc, sign_extend8(d) + 2); }
    void on_ld_a_r() {
        (*this)->on_format("ld a, r"); }
    void on_ld_r_a() {
        (*this)->on_format("ld r, a"); }
    void on_ld_i_a() {
        (*this)->on_format("ld i, a"); }
    void on_ld_r_r(reg rd, reg rs, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        index_regp irpd = rs == reg::at_hl ? index_regp::hl : irp;
        index_regp irps = rd == reg::at_hl ? index_regp::hl : irp;
        (*this)->on_format("ld R, R", rd, irpd, d, rs, irps, d); }
    void on_ld_r_n(reg r, fast_u8 d, fast_u8 n) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ld R, N", r, irp, d, n); }
    void on_ld_rp_nn(regp rp, fast_u16 nn) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ld P, W", rp, irp, nn); }
    void on_ld_irp_at_nn(fast_u16 nn) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ld P, (W)", regp::hl, irp, nn); }
    void on_ld_at_nn_irp(fast_u16 nn) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ld (W), P", nn, regp::hl, irp); }
    void on_ld_rp_at_nn(regp rp, fast_u16 nn) {
        (*this)->on_format("ld P, (W)", rp, index_regp::hl, nn); }
    void on_ld_at_nn_rp(fast_u16 nn, regp rp) {
        (*this)->on_format("ld (W), P", nn, rp, index_regp::hl); }
    void on_ld_a_at_nn(fast_u16 nn) {
        (*this)->on_format("ld a, (W)", nn); }
    void on_ld_at_nn_a(fast_u16 nn) {
        (*this)->on_format("ld (W), a", nn); }
    void on_ld_a_at_rp(regp rp) {
        (*this)->on_format("ld a, (P)", rp, index_regp::hl); }
    void on_ld_at_rp_a(regp rp) {
        (*this)->on_format("ld (P), a", rp, index_regp::hl); }
    void on_ld_sp_irp() {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("ld sp, P", regp::hl, irp); }
    void on_neg() {
        (*this)->on_format("neg"); }
    void on_out_c_r(reg r) {
        if(r == reg::at_hl)
            (*this)->on_format("out (c), 0");
        else
            (*this)->on_format("out (c), R", r, index_regp::hl, 0); }
    void on_out_n_a(fast_u8 n) {
        (*this)->on_format("out (N), a", n); }
    void on_pop_rp(regp2 rp) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("pop G", rp, irp); }
    void on_push_rp(regp2 rp) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_format("push G", rp, irp); }
    void on_res(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        if(irp == index_regp::hl || r == reg::at_hl)
            (*this)->on_format("res U, R", b, r, irp, d);
        else
            (*this)->on_format("res U, R, R", b, reg::at_hl, irp, d,
                               r, index_regp::hl, /* d= */ 0); }
    void on_ret_cc(condition cc) {
        (*this)->on_format("ret C", cc); }
    void on_reti() {
        (*this)->on_format("reti"); }
    void on_retn() {
        (*this)->on_format("retn"); }
    void on_rla() {
        (*this)->on_format("rla"); }
    void on_rlca() {
        (*this)->on_format("rlca"); }
    void on_rld() {
        (*this)->on_format("rld"); }
    void on_rot(rot k, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        if(irp == index_regp::hl || r == reg::at_hl)
            (*this)->on_format("O R", k, r, irp, d);
        else
            (*this)->on_format("O R, R", k, reg::at_hl, irp, d,
                               r, index_regp::hl, /* d= */ 0); }
    void on_rra() {
        (*this)->on_format("rra"); }
    void on_rrca() {
        (*this)->on_format("rrca"); }
    void on_rrd() {
        (*this)->on_format("rrd"); }
    void on_scf() {
        (*this)->on_format("scf"); }
    void on_set(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        if(irp == index_regp::hl || r == reg::at_hl)
            (*this)->on_format("set U, R", b, r, irp, d);
        else
            (*this)->on_format("set U, R, R", b, reg::at_hl, irp, d,
                               r, index_regp::hl, /* d= */ 0); }
    void on_sbc_hl_rp(regp rp) {
        (*this)->on_format("sbc hl, P", rp, index_regp::hl); }

    void disassemble() { (*this)->decode(); }

protected:
    D *operator -> () { return static_cast<D*>(this); }

    template<typename T>
    static T get_arg(const void **&args) {
        return base::template get_arg<T>(args);
    }

    static const char *get_reg_name(reg r, index_regp irp = index_regp::hl) {
        switch(r) {
        case reg::b: return "b";
        case reg::c: return "c";
        case reg::d: return "d";
        case reg::e: return "e";
        case reg::a: return "a";
        case reg::h:
            switch(irp) {
            case index_regp::hl: return "h";
            case index_regp::ix: return "ixh";
            case index_regp::iy: return "iyh";
            }
            break;
        case reg::l:
            switch(irp) {
            case index_regp::hl: return "l";
            case index_regp::ix: return "ixl";
            case index_regp::iy: return "iyl";
            }
            break;
        case reg::at_hl:
            switch(irp) {
            case index_regp::hl: return "(hl)";
            case index_regp::ix: return "(ix)";
            case index_regp::iy: return "(iy)";
            }
            break;
        }
        unreachable("Unknown register.");
    }

    static const char *get_reg_name(regp rp, index_regp irp = index_regp::hl) {
        switch(rp) {
        case regp::bc: return "bc";
        case regp::de: return "de";
        case regp::hl: return z80::get_reg_name(irp);
        case regp::sp: return "sp";
        }
        unreachable("Unknown register.");
    }

    static const char *get_reg_name(regp2 rp,
                                    index_regp irp = index_regp::hl) {
        switch(rp) {
        case regp2::bc: return "bc";
        case regp2::de: return "de";
        case regp2::hl: return z80::get_reg_name(irp);
        case regp2::af: return "af";
        }
        unreachable("Unknown register.");
    }

    static const char *get_mnemonic(alu k) {
        switch(k) {
        case alu::add: return "add";
        case alu::adc: return "adc";
        case alu::sub: return "sub";
        case alu::sbc: return "sbc";
        case alu::and_a: return "and";
        case alu::xor_a: return "xor";
        case alu::or_a: return "or";
        case alu::cp: return "cp";
        }
        unreachable("Unknown ALU operation.");
    }
};

template<typename S>
class processor_state_base : public S {
public:
    typedef S decoder_state;

    fast_u8 get_b() const { return get_high8(bc); }
    void set_b(fast_u8 b) { bc = make16(b, get_c()); }

    fast_u8 get_c() const { return get_low8(bc); }
    void set_c(fast_u8 c) { bc = make16(get_b(), c); }

    fast_u8 get_d() const { return get_high8(de); }
    void set_d(fast_u8 d) { de = make16(d, get_e()); }

    fast_u8 get_e() const { return get_low8(de); }
    void set_e(fast_u8 e) { de = make16(get_d(), e); }

    fast_u8 get_h() const { return get_high8(hl); }
    void set_h(fast_u8 h) { hl = make16(h, get_l()); }

    fast_u8 get_l() const { return get_low8(hl); }
    void set_l(fast_u8 l) { hl = make16(get_h(), l); }

    fast_u8 get_a() const { return get_high8(af); }
    void set_a(fast_u8 a) { af = make16(a, get_f()); }

    fast_u8 get_f() const { return get_low8(af); }
    void set_f(fast_u8 f) { af = make16(get_a(), f); }

    fast_u16 get_af() const { return af; }
    void set_af(fast_u16 n) { af = n; }

    fast_u16 get_hl() const { return hl; }
    void set_hl(fast_u16 n) { hl = n; }

    fast_u16 get_bc() const { return bc; }
    void set_bc(fast_u16 n) { bc = n; }

    fast_u16 get_de() const { return de; }
    void set_de(fast_u16 n) { de = n; }

    fast_u16 get_sp() const { return sp; }
    void set_sp(fast_u16 n) { sp = n; }

    fast_u16 get_pc() const { return pc; }
    void set_pc(fast_u16 n) { pc = n; }

    // TODO: Rename to WZ.
    // TODO: Do we really need it for i8080?
    fast_u16 get_memptr() const { return memptr; }
    void set_memptr(fast_u16 n) { memptr = n; }

    bool is_int_disabled() const { return int_disabled; }
    void set_is_int_disabled(bool disabled) { int_disabled = disabled; }
    void enable_int() { set_is_int_disabled(false); }
    void disable_int() { set_is_int_disabled(true); }

    bool get_iff() const { return iff; }
    void set_iff(bool new_iff) { iff = new_iff; }

    bool is_halted() const { return halted; }
    void set_is_halted(bool is_halted) { halted = is_halted; }
    void halt() { set_is_halted(true); }

    fast_u16 get_last_read_addr() const { return last_read_addr; }
    void set_last_read_addr(fast_u16 addr) { last_read_addr = addr; }

    void ex_de_hl() { std::swap(de, hl); }

    void swap_bc(fast_u16 &alt_bc) { std::swap(bc, alt_bc); }
    void swap_de(fast_u16 &alt_de) { std::swap(de, alt_de); }
    void swap_hl(fast_u16 &alt_hl) { std::swap(hl, alt_hl); }
    void swap_af(fast_u16 &alt_af) { std::swap(af, alt_af); }

private:
    fast_u16 bc = 0, de = 0, hl = 0, af = 0;
    fast_u16 pc = 0, sp = 0, memptr = 0;
    bool int_disabled = false;
    bool iff = false;
    bool halted = false;
    fast_u16 last_read_addr = 0;
};

class i8080_state : public processor_state_base<i8080_decoder_state>
{};

class z80_state : public processor_state_base<z80_decoder_state> {
public:
    z80_state() {}

    fast_u8 get_ixh() const { return get_high8(ix); }
    void set_ixh(fast_u8 ixh) { ix = make16(ixh, get_ixl()); }

    fast_u8 get_ixl() const { return get_low8(ix); }
    void set_ixl(fast_u8 ixl) { ix = make16(get_ixh(), ixl); }

    fast_u8 get_iyh() const { return get_high8(iy); }
    void set_iyh(fast_u8 iyh) { iy = make16(iyh, get_iyl()); }

    fast_u8 get_iyl() const { return get_low8(iy); }
    void set_iyl(fast_u8 iyl) { iy = make16(get_iyh(), iyl); }

    fast_u8 get_i() const { return get_high8(ir); }
    void set_i(fast_u8 i) { ir = make16(i, get_r_reg()); }

    fast_u8 get_r_reg() const { return get_low8(ir); }
    void set_r_reg(fast_u8 r) { ir = make16(get_i(), r); }

    fast_u16 get_alt_af() const { return alt_af; }
    void set_alt_af(fast_u16 n) { alt_af = n; }

    fast_u16 get_alt_hl() const { return alt_hl; }
    void set_alt_hl(fast_u16 n) { alt_hl = n; }

    fast_u16 get_alt_bc() const { return alt_bc; }
    void set_alt_bc(fast_u16 n) { alt_bc = n; }

    fast_u16 get_alt_de() const { return alt_de; }
    void set_alt_de(fast_u16 n) { alt_de = n; }

    fast_u16 get_ix() const { return ix; }
    void set_ix(fast_u16 n) { ix = n; }

    fast_u16 get_iy() const { return iy; }
    void set_iy(fast_u16 n) { iy = n; }

    fast_u16 get_ir() const { return ir; }
    void set_ir(fast_u16 n) { ir = n; }

    bool get_iff1() const { return iff1; }
    void set_iff1(bool iff) { iff1 = iff; }

    bool get_iff2() const { return iff2; }
    void set_iff2(bool iff) { iff2 = iff; }

    unsigned get_int_mode() const { return int_mode; }
    void set_int_mode(unsigned mode) { int_mode = mode; }

    fast_u8 get_r(reg r) {
        switch(r) {
        case reg::b: return get_b();
        case reg::c: return get_c();
        case reg::d: return get_d();
        case reg::e: return get_e();
        case reg::h: return get_h();
        case reg::l: return get_l();
        case reg::at_hl: unreachable("Can't get (HL) value.");
        case reg::a: return get_a();
        }
        unreachable("Unknown register.");
    }

    fast_u16 get_index_rp(index_regp irp) {
        switch(irp) {
        case index_regp::hl: return get_hl();
        case index_regp::ix: return get_ix();
        case index_regp::iy: return get_iy();
        }
        unreachable("Unknown index register.");
    }

    void ex_af_alt_af() {
        swap_af(alt_af);
    }

    void exx() {
        swap_bc(alt_bc);
        swap_de(alt_de);
        swap_hl(alt_hl);
    }

private:
    fast_u16 ix = 0, iy = 0;
    fast_u16 alt_bc = 0, alt_de = 0, alt_hl = 0, alt_af = 0;
    fast_u16 ir = 0;
    bool iff1 = false, iff2 = false;
    unsigned int_mode = 0;
};

template<typename E>
class processor_base : public E {
public:
    typedef E decoder;
    typedef typename decoder::state state;
    typedef typename decoder::derived derived;

    processor_base() {}

    using state::get_b;
    using state::set_b;
    using state::get_c;
    using state::set_c;
    using state::get_d;
    using state::set_d;
    using state::get_e;
    using state::set_e;
    using state::get_h;
    using state::set_h;
    using state::get_l;
    using state::set_l;
    using state::get_a;
    using state::set_a;
    using state::get_f;
    using state::set_f;
    using state::get_sp;
    using state::set_sp;
    using state::get_pc;
    using state::set_pc;
    using state::get_memptr;
    using state::set_memptr;

    fast_u8 on_get_b() const { return get_b(); }
    void on_set_b(fast_u8 b) { set_b(b); }

    fast_u8 on_get_c() const { return get_c(); }
    void on_set_c(fast_u8 c) { set_c(c); }

    fast_u8 on_get_d() const { return get_d(); }
    void on_set_d(fast_u8 d) { set_d(d); }

    fast_u8 on_get_e() const { return get_e(); }
    void on_set_e(fast_u8 e) { set_e(e); }

    fast_u8 on_get_h() const { return get_h(); }
    void on_set_h(fast_u8 h) { set_h(h); }

    fast_u8 on_get_l() const { return get_l(); }
    void on_set_l(fast_u8 l) { set_l(l); }

    fast_u8 on_get_a() const { return get_a(); }
    void on_set_a(fast_u8 a) { set_a(a); }

    fast_u8 on_get_f() const { return get_f(); }
    void on_set_f(fast_u8 f) { set_f(f); }

    fast_u16 on_get_af() {
        // Always get the low byte first.
        fast_u8 f = (*this)->on_get_f();
        fast_u8 a = (*this)->on_get_a();
        return make16(a, f); }
    void on_set_af(fast_u16 af) {
        // Always set the low byte first.
        (*this)->on_set_f(get_low8(af));
        (*this)->on_set_a(get_high8(af)); }

    fast_u16 on_get_hl() {
        // Always get the low byte first.
        fast_u8 l = (*this)->on_get_l();
        fast_u8 h = (*this)->on_get_h();
        return make16(h, l); }
    void on_set_hl(fast_u16 hl) {
        // Always set the low byte first.
        (*this)->on_set_l(get_low8(hl));
        (*this)->on_set_h(get_high8(hl)); }

    fast_u16 on_get_bc() {
        // Always get the low byte first.
        fast_u8 l = (*this)->on_get_c();
        fast_u8 h = (*this)->on_get_b();
        return make16(h, l); }
    void on_set_bc(fast_u16 bc) {
        // Always set the low byte first.
        (*this)->on_set_c(get_low8(bc));
        (*this)->on_set_b(get_high8(bc)); }

    fast_u16 on_get_de() {
        // Always get the low byte first.
        fast_u8 l = (*this)->on_get_e();
        fast_u8 h = (*this)->on_get_d();
        return make16(h, l); }
    void on_set_de(fast_u16 de) {
        // Always set the low byte first.
        (*this)->on_set_e(get_low8(de));
        (*this)->on_set_d(get_high8(de)); }

    fast_u16 on_get_sp() { return get_sp(); }
    void on_set_sp(fast_u16 sp) { set_sp(sp); }

    fast_u16 on_get_pc() const { return get_pc(); }
    void on_set_pc(fast_u16 pc) { set_pc(pc); }

    fast_u16 get_pc_on_fetch() const { return (*this)->on_get_pc(); }
    void set_pc_on_fetch(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 get_pc_on_jump() const { return (*this)->on_get_pc(); }
    void set_pc_on_jump(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 get_pc_on_imm8_read() const { return (*this)->on_get_pc(); }
    void set_pc_on_imm8_read(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 get_pc_on_imm16_read() const { return (*this)->on_get_pc(); }
    void set_pc_on_imm16_read(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 get_pc_on_halt() const { return (*this)->on_get_pc(); }
    void set_pc_on_halt(fast_u16 pc) { (*this)->on_set_pc(pc); }

    void set_pc_on_call(fast_u16 pc) { (*this)->on_set_pc(pc); }
    void set_pc_on_return(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 on_get_memptr() const { return get_memptr(); }
    void on_set_memptr(fast_u16 memptr) { set_memptr(memptr); }

    void on_disable_int() { state::disable_int(); }
    void disable_int_on_ei() { (*this)->on_disable_int(); }

    fast_u8 get_flag_mask(condition cc) {
        switch(cc / 2) {
        case 0: return zf_mask;
        case 1: return cf_mask;
        case 2: return pf_mask;
        case 3: return sf_mask;
        }
        unreachable("Unknown condition code.");
    }

    bool check_condition(condition cc) {
        bool actual = (*this)->on_get_f() & get_flag_mask(cc);
        bool expected = cc & 1;
        return actual == expected;
    }

    void on_5t_fetch_cycle() {
        (*this)->tick(1);
    }

    fast_u8 on_read_cycle(fast_u16 addr, unsigned ticks) {
        (*this)->on_set_addr_bus(addr);
        fast_u8 b = (*this)->on_read_access(addr);
        (*this)->tick(ticks);
        state::set_last_read_addr(addr);
        return b; }
    fast_u8 on_3t_read_cycle(fast_u16 addr) {
        return (*this)->on_read_cycle(addr, /* ticks= */ 3); }
    fast_u8 on_4t_read_cycle(fast_u16 addr) {
        return (*this)->on_read_cycle(addr, /* ticks= */ 4); }

    fast_u8 on_3t_imm8_read() {
        fast_u16 pc = (*this)->get_pc_on_imm8_read();
        fast_u8 op = (*this)->on_3t_read_cycle(pc);
        (*this)->set_pc_on_imm8_read(inc16(pc));
        return op; }
    fast_u16 on_3t_3t_imm16_read() {
        fast_u16 pc = (*this)->get_pc_on_imm16_read();
        fast_u8 lo = (*this)->on_3t_read_cycle(pc);
        pc = inc16(pc);
        fast_u8 hi = (*this)->on_3t_read_cycle(pc);
        (*this)->set_pc_on_imm16_read(inc16(pc));
        return make16(hi, lo); }
    fast_u16 on_3t_4t_imm16_read() {
        fast_u16 pc = (*this)->get_pc_on_imm16_read();
        fast_u8 lo = (*this)->on_3t_read_cycle(pc);
        pc = inc16(pc);
        fast_u8 hi = (*this)->on_4t_read_cycle(pc);
        (*this)->set_pc_on_imm16_read(inc16(pc));
        return make16(hi, lo); }

    void on_write_cycle(fast_u16 addr, fast_u8 n, unsigned ticks) {
        (*this)->on_set_addr_bus(addr);
        (*this)->on_write_access(addr, n);
        (*this)->tick(ticks); }
    void on_3t_write_cycle(fast_u16 addr, fast_u8 n) {
        (*this)->on_write_cycle(addr, n, /* ticks= */ 3); }
    void on_5t_write_cycle(fast_u16 addr, fast_u8 n) {
        (*this)->on_write_cycle(addr, n, /* ticks= */ 5); }

    void on_3t_exec_cycle() {
        (*this)->tick(3); }

    void on_push(fast_u16 nn) {
        fast_u16 sp = (*this)->on_get_sp();
        sp = dec16(sp);
        (*this)->on_3t_write_cycle(sp, get_high8(nn));
        sp = dec16(sp);
        (*this)->on_3t_write_cycle(sp, get_low8(nn));
        (*this)->on_set_sp(sp); }
    fast_u16 on_pop() {
        fast_u16 sp = (*this)->on_get_sp();
        fast_u8 lo = (*this)->on_3t_read_cycle(sp);
        sp = inc16(sp);
        fast_u8 hi = (*this)->on_3t_read_cycle(sp);
        sp = inc16(sp);
        (*this)->on_set_sp(sp);
        return make16(hi, lo); }
    void on_call(fast_u16 nn) {
        (*this)->on_push((*this)->on_get_pc());
        (*this)->on_set_memptr(nn);
        (*this)->set_pc_on_call(nn); }
    void on_return() {
        fast_u16 pc = (*this)->on_pop();
        (*this)->on_set_memptr(pc);
        (*this)->set_pc_on_return(pc); }
    void on_jump(fast_u16 nn) {
        (*this)->on_set_memptr(nn);
        (*this)->set_pc_on_jump(nn); }

    void on_dec_rp(regp rp) {
        (*this)->on_set_rp(rp, dec16((*this)->on_get_rp(rp))); }
    void on_call_nn(fast_u16 nn) {
        (*this)->on_call(nn); }
    void on_call_cc_nn(condition cc, fast_u16 nn) {
        if(check_condition(cc))
            (*this)->on_call(nn);
        else
            (*this)->on_set_memptr(nn); }
    void on_ex_de_hl() {
        state::ex_de_hl(); }
    void on_halt() {
        state::halt();
        // TODO: It seems 'HLT' doesn't really reset PC? Does 'HALT' do?
        (*this)->set_pc_on_halt(dec16((*this)->get_pc_on_halt())); }
    void on_jp_nn(fast_u16 nn) {
        (*this)->on_jump(nn); }
    void on_jp_cc_nn(condition cc, fast_u16 nn) {
        if(check_condition(cc))
            (*this)->on_jump(nn);
        else
            (*this)->on_set_memptr(nn); }
    void on_inc_rp(regp rp) {
        (*this)->on_set_rp(rp, inc16((*this)->on_get_rp(rp))); }
    void on_ld_a_at_nn(fast_u16 nn) {
        (*this)->on_set_memptr(inc16(nn));
        (*this)->on_set_a((*this)->on_3t_read_cycle(nn)); }
    void on_ld_at_nn_a(fast_u16 nn) {
        fast_u8 a = (*this)->on_get_a();
        (*this)->on_set_memptr(make16(a, inc8(get_low8(nn))));
        (*this)->on_3t_write_cycle(nn, a); }
    void on_ld_a_at_rp(regp rp) {
        fast_u16 nn = (*this)->on_get_rp(rp);
        (*this)->on_set_memptr(inc16(nn));
        (*this)->on_set_a((*this)->on_3t_read_cycle(nn)); }
    void on_ld_at_rp_a(regp rp) {
        fast_u16 nn = (*this)->on_get_rp(rp);
        fast_u8 a = (*this)->on_get_a();
        (*this)->on_set_memptr(make16(a, get_low8(nn + 1)));
        (*this)->on_3t_write_cycle(nn, a); }
    void on_ld_rp_nn(regp rp, fast_u16 nn) {
        (*this)->on_set_rp(rp, nn); }
    void on_nop() {}
    void on_pop_rp(regp2 rp) {
        (*this)->on_set_rp2(rp, (*this)->on_pop()); }
    void on_push_rp(regp2 rp) {
        (*this)->on_push((*this)->on_get_rp2(rp)); }
    void on_ret() {
        (*this)->on_return(); }
    void on_ret_cc(condition cc) {
        if(check_condition(cc))
            (*this)->on_return(); }
    void on_rst(fast_u16 nn) {
        (*this)->on_call(nn); }

    void on_step() {
        state::enable_int();
        (*this)->decode();
    }

    // TODO: Remove. Use on_step() instead.
    void step() { return (*this)->on_step(); }

protected:
    derived *operator -> () {
        return static_cast<derived*>(this); }
    const derived *operator -> () const {
        return static_cast<const derived*>(this); }

    static const unsigned sf_bit = 7;
    static const unsigned zf_bit = 6;
    static const unsigned yf_bit = 5;
    static const unsigned hf_bit = 4;
    static const unsigned xf_bit = 3;
    static const unsigned pf_bit = 2;
    static const unsigned nf_bit = 1;
    static const unsigned cf_bit = 0;

    static const fast_u8 sf_mask = 1 << sf_bit;
    static const fast_u8 zf_mask = 1 << zf_bit;
    static const fast_u8 yf_mask = 1 << yf_bit;
    static const fast_u8 hf_mask = 1 << hf_bit;
    static const fast_u8 xf_mask = 1 << xf_bit;
    static const fast_u8 pf_mask = 1 << pf_bit;
    static const fast_u8 nf_mask = 1 << nf_bit;
    static const fast_u8 cf_mask = 1 << cf_bit;

    template<typename T>
    static fast_u8 zf_ari(T n) {
        return (n == 0 ? 1u : 0u) << zf_bit;
    }

    template<typename T>
    fast_u8 hf_ari(T r, T a, T b) {
        return (r ^ a ^ b) & hf_mask;
    }

    fast_u8 hf_dec(fast_u8 n) {
        return (n & 0xf) == 0xf ? hf_mask : 0;
    }

    fast_u8 hf_inc(fast_u8 n) {
        return (n & 0xf) == 0x0 ? hf_mask : 0;
    }

    template<typename T>
    fast_u8 pf_ari(T r, T a, T b) {
        fast_u16 x = r ^ a ^ b;
        return ((x >> 6) ^ (x >> 5)) & pf_mask;
    }

    bool pf_log4(fast_u8 n) {
        return 0x9669 & (1 << (n & 0xf));
    }

    fast_u8 pf_log(fast_u8 n) {
        bool lo = pf_log4(n);
        bool hi = pf_log4(n >> 4);
        return lo == hi ? pf_mask : 0;
    }

    fast_u8 pf_dec(fast_u8 n) {
        return n == 0x7f ? pf_mask : 0;
    }

    fast_u8 pf_inc(fast_u8 n) {
        return n == 0x80 ? pf_mask : 0;
    }

    fast_u8 cf_ari(bool c) {
        return c ? cf_mask : 0;
    }
};

template<typename D>
class i8080_processor : public processor_base<i8080_decoder<D, i8080_state>> {
public:
    typedef i8080_state state;
    typedef i8080_decoder<D, i8080_state> decoder;
    typedef processor_base<decoder> base;

    using base::cf_mask;
    using base::nf_mask;
    using base::sf_mask;
    using base::xf_mask;
    using base::yf_mask;

    using base::cf_ari;
    using base::hf_ari;
    using base::hf_dec;
    using base::hf_inc;
    using base::pf_log;
    using base::zf_ari;

    bool on_get_iff() const { return state::get_iff(); }
    void on_set_iff(bool iff) { state::set_iff(iff); }

    void set_iff_on_di(bool iff) { (*this)->on_set_iff(iff); }
    void set_iff_on_ei(bool iff) { (*this)->on_set_iff(iff); }

    fast_u8 on_get_m() {
        return (*this)->on_3t_read_cycle((*this)->on_get_hl()); }
    void on_set_m(fast_u8 n) {
        (*this)->on_3t_write_cycle((*this)->on_get_hl(), n); }

    fast_u8 on_get_r(reg r) {
        switch(r) {
        case reg::b: return (*this)->on_get_b();
        case reg::c: return (*this)->on_get_c();
        case reg::d: return (*this)->on_get_d();
        case reg::e: return (*this)->on_get_e();
        case reg::at_hl: return (*this)->on_get_m();
        case reg::a: return (*this)->on_get_a();
        case reg::h: return (*this)->on_get_h();
        case reg::l: return (*this)->on_get_l();
        }
        unreachable("Unknown register.");
    }

    void on_set_r(reg r, fast_u8 n) {
        switch(r) {
        case reg::b: return (*this)->on_set_b(n);
        case reg::c: return (*this)->on_set_c(n);
        case reg::d: return (*this)->on_set_d(n);
        case reg::e: return (*this)->on_set_e(n);
        case reg::at_hl: return (*this)->on_set_m(n);
        case reg::a: return (*this)->on_set_a(n);
        case reg::h: return (*this)->on_set_h(n);
        case reg::l: return (*this)->on_set_l(n);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_rp(regp rp) {
        switch(rp) {
        case regp::bc: return (*this)->on_get_bc();
        case regp::de: return (*this)->on_get_de();
        case regp::hl: return (*this)->on_get_hl();
        case regp::sp: return (*this)->on_get_sp();
        }
        unreachable("Unknown register.");
    }

    void on_set_rp(regp rp, fast_u16 nn) {
        switch(rp) {
        case regp::bc: return (*this)->on_set_bc(nn);
        case regp::de: return (*this)->on_set_de(nn);
        case regp::hl: return (*this)->on_set_hl(nn);
        case regp::sp: return (*this)->on_set_sp(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_rp2(regp2 rp) {
        switch(rp) {
        case regp2::bc: return (*this)->on_get_bc();
        case regp2::de: return (*this)->on_get_de();
        case regp2::hl: return (*this)->on_get_hl();
        case regp2::af: return (*this)->on_get_af();
        }
        unreachable("Unknown register.");
    }

    void on_set_rp2(regp2 rp, fast_u16 nn) {
        switch(rp) {
        case regp2::bc: return (*this)->on_set_bc(nn);
        case regp2::de: return (*this)->on_set_de(nn);
        case regp2::hl: return (*this)->on_set_hl(nn);
        case regp2::af: return (*this)->on_set_af(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u8 on_fetch_cycle(fast_u16 addr) {
        (*this)->on_set_addr_bus(addr);
        fast_u8 b = (*this)->on_read_access(addr);
        (*this)->tick(4);
        state::set_last_read_addr(addr);
        return b; }
    void on_7t_fetch_cycle() {
        (*this)->tick(3); }

    fast_u8 on_fetch() {
        fast_u16 pc = (*this)->get_pc_on_fetch();
        fast_u8 op = (*this)->on_fetch_cycle(pc);
        (*this)->set_pc_on_fetch(inc16(pc));
        return op;
    }

    void do_alu(alu k, fast_u8 n) {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = 0;
        switch(k) {
        case alu::add: {
            fast_u8 t = add8(a, n);
            f = (t & (sf_mask | yf_mask | xf_mask | nf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_log(t) | cf_ari(t < a);
            a = t;
            break; }
        case alu::adc: {
            f = (*this)->on_get_f();
            fast_u8 cfv = (f & cf_mask) ? 1 : 0;
            fast_u8 t = mask8(a + n + cfv);
            f = (t & (sf_mask | yf_mask | xf_mask | nf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_log(t) |
                    cf_ari(t < a || (cfv && n == 0xff));
            a = t;
            break; }
        case alu::sub: {
            assert(0);  // TODO
#if 0
            do_sub(a, f, n);
#endif
            break; }
        case alu::sbc: {
            assert(0);  // TODO
#if 0
            f = (*this)->on_get_f();
            fast_u8 cfv = (f & cf_mask) ? 1 : 0;
            fast_u8 t = mask8(a - n - cfv);
            f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_ari(a - n - cfv, a, n) |
                    cf_ari(t > a || (cfv && n == 0xff)) | nf_mask;
            a = t;
#endif
            break; }
        case alu::and_a:
            assert(0);  // TODO
#if 0
            a &= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a) |
                    hf_mask;
#endif
            break;
        case alu::xor_a:
            assert(0);  // TODO
#if 0
            a ^= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a);
#endif
            break;
        case alu::or_a:
            assert(0);  // TODO
#if 0
            a |= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a);
#endif
            break;
        case alu::cp:
            assert(0);  // TODO
#if 0
            do_cp(a, f, n);
#endif
            break;
        }
        if(k != alu::cp)
            (*this)->on_set_a(a);
        (*this)->on_set_f(f);
    }

    void on_add_irp_rp(regp rp) {
        (*this)->on_3t_exec_cycle();
        (*this)->on_3t_exec_cycle();

        fast_u16 i = (*this)->on_get_hl();
        fast_u16 n = (*this)->on_get_rp(rp);
        fast_u8 f = (*this)->on_get_f();
        fast_u16 r = add16(i, n);
        f = (f & ~base::cf_mask) | base::cf_ari(r < i);
        (*this)->on_set_memptr(inc16(i));
        (*this)->on_set_hl(r);
        (*this)->on_set_f(f); }
    void on_alu_r(alu k, reg r) {
        do_alu(k, (*this)->on_get_r(r)); }
    void on_ccf() {
        (*this)->on_set_f((*this)->on_get_f() ^ base::cf_mask); }
    void on_cpl() {
        (*this)->on_set_a((*this)->on_get_a() ^ 0xff); }
    void on_dec_r(reg r) {
        fast_u8 n = (*this)->on_get_r(r);
        fast_u8 f = (*this)->on_get_f();
        n = dec8(n);
        f = (f & (cf_mask | yf_mask | xf_mask | nf_mask)) |
                (n & sf_mask) | zf_ari(n) | hf_dec(n) | pf_log(n);
        (*this)->on_set_r(r, n);
        (*this)->on_set_f(f); }
    void on_di() {
        (*this)->set_iff_on_di(false); }
    void on_ei() {
        (*this)->set_iff_on_ei(true);
        (*this)->disable_int_on_ei(); }
    void on_ex_at_sp_irp() {
        fast_u16 sp = (*this)->on_get_sp();
        fast_u8 lo = (*this)->on_3t_read_cycle(sp);
        sp = inc16(sp);
        fast_u8 hi = (*this)->on_3t_read_cycle(sp);
        fast_u16 nn = make16(hi, lo);
        fast_u16 hl = (*this)->on_get_hl();
        std::swap(nn, hl);
        (*this)->on_3t_write_cycle(sp, get_high8(nn));
        sp = dec16(sp);
        (*this)->on_5t_write_cycle(sp, get_low8(nn));
        (*this)->on_set_memptr(hl);
        (*this)->on_set_hl(hl); }
    void on_jp_irp() {
        (*this)->set_pc_on_jump((*this)->on_get_hl()); }
    void on_inc_r(reg r) {
        fast_u8 n = (*this)->on_get_r(r);
        fast_u8 f = (*this)->on_get_f();
        n = inc8(n);
        f = (f & (cf_mask | yf_mask | xf_mask | nf_mask)) |
                (n & sf_mask) | zf_ari(n) | hf_inc(n) | pf_log(n);
        (*this)->on_set_r(r, n);
        (*this)->on_set_f(f); }
    void on_ld_r_n(reg r, fast_u8 n) {
        (*this)->on_set_r(r, n); }
    void on_ld_r_r(reg rd, reg rs) {
        (*this)->on_5t_fetch_cycle();
        (*this)->on_set_r(rd, (*this)->on_get_r(rs)); }
    void on_ld_sp_irp() {
        (*this)->on_set_sp((*this)->on_get_hl()); }
    void on_ld_irp_at_nn(fast_u16 nn) {
        fast_u8 lo = (*this)->on_3t_read_cycle(nn);
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        fast_u8 hi = (*this)->on_3t_read_cycle(nn);
        (*this)->on_set_hl(make16(hi, lo)); }
    void on_ld_at_nn_irp(fast_u16 nn) {
        fast_u16 irp = (*this)->on_get_hl();
        (*this)->on_3t_write_cycle(nn, get_low8(irp));
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        (*this)->on_3t_write_cycle(nn, get_high8(irp)); }
    void on_rla() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u8 r = mask8(a << 1) | ((f & base::cf_mask) ? 1 : 0);
        f = (f & (0xff & ~base::cf_mask)) | base::cf_ari(a & 0x80);
        (*this)->on_set_a(r);
        (*this)->on_set_f(f); }
    void on_rra() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u8 r = (a >> 1) | ((f & base::cf_mask) ? 0x80 : 0);
        f = (f & ~base::cf_mask) | base::cf_ari(a & 0x1);
        (*this)->on_set_a(r);
        (*this)->on_set_f(f); }
    void on_rlca() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        a = rol8(a);
        f = (f & ~base::cf_mask) | base::cf_ari(a & 0x1);
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_rrca() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        a = ror8(a);
        f = (f & ~base::cf_mask) | base::cf_ari(a & 0x80);
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_scf() {
        (*this)->on_set_f((*this)->on_get_f() | base::cf_mask); }
};

template<typename D>
class z80_processor : public processor_base<z80_decoder<D, z80_state>> {
public:
    typedef z80_state state;
    typedef z80_decoder<D, z80_state> decoder;
    typedef processor_base<decoder> base;

    z80_processor() {}

    using state::get_index_rp_kind;
    using state::set_index_rp_kind;
    using state::is_index_rp_hl;
    using state::get_b;
    using state::set_b;
    using state::get_c;
    using state::set_c;
    using state::get_d;
    using state::set_d;
    using state::get_e;
    using state::set_e;
    using state::get_h;
    using state::set_h;
    using state::get_l;
    using state::set_l;
    using state::get_a;
    using state::set_a;
    using state::get_f;
    using state::set_f;
    using state::get_ixh;
    using state::set_ixh;
    using state::get_ixl;
    using state::set_ixl;
    using state::get_iyh;
    using state::set_iyh;
    using state::get_iyl;
    using state::set_iyl;
    using state::get_i;
    using state::set_i;
    using state::get_r_reg;
    using state::set_r_reg;
    using state::get_sp;
    using state::set_sp;
    using state::get_pc;
    using state::set_pc;
    using state::get_memptr;
    using state::set_memptr;
    using state::get_iff1;
    using state::set_iff1;
    using state::get_iff2;
    using state::set_iff2;
    using state::get_int_mode;
    using state::set_int_mode;
    using state::is_int_disabled;
    using state::enable_int;
    using state::disable_int;
    using state::is_halted;
    using state::set_is_halted;
    using state::halt;
    using state::set_last_read_addr;
    using state::ex_af_alt_af;
    using state::exx;

    using base::sf_bit;
    using base::zf_bit;
    using base::yf_bit;
    using base::hf_bit;
    using base::xf_bit;
    using base::pf_bit;
    using base::nf_bit;
    using base::cf_bit;

    using base::sf_mask;
    using base::zf_mask;
    using base::yf_mask;
    using base::hf_mask;
    using base::xf_mask;
    using base::pf_mask;
    using base::nf_mask;
    using base::cf_mask;

    using base::zf_ari;
    using base::hf_ari;
    using base::hf_dec;
    using base::hf_inc;
    using base::pf_ari;
    using base::pf_log;
    using base::pf_dec;
    using base::pf_inc;
    using base::cf_ari;

    fast_u8 on_get_ixh() const { return get_ixh(); }
    void on_set_ixh(fast_u8 ixh) { set_ixh(ixh); }

    fast_u8 on_get_ixl() const { return get_ixl(); }
    void on_set_ixl(fast_u8 ixl) { set_ixl(ixl); }

    fast_u8 on_get_iyh() const { return get_iyh(); }
    void on_set_iyh(fast_u8 iyh) { set_iyh(iyh); }

    fast_u8 on_get_iyl() const { return get_iyl(); }
    void on_set_iyl(fast_u8 iyl) { set_iyl(iyl); }

    fast_u8 on_get_i() const { return get_i(); }
    void on_set_i(fast_u8 i) { set_i(i); }

    void set_i_on_ld(fast_u8 i) { (*this)->on_set_i(i); }

    fast_u8 on_get_r_reg() const { return get_r_reg(); }
    void on_set_r_reg(fast_u8 r) { set_r_reg(r); }

    void on_inc_r_reg() {
        // TODO: Consider splitting R into R[7] and R[6:0].
        fast_u8 r = (*this)->on_get_r_reg();
        r = (r & 0x80) | (inc8(r) & 0x7f);
        (*this)->set_r_reg(r);
    }

    fast_u16 on_get_ix() {
        // Always get the low byte first.
        fast_u8 l = (*this)->on_get_ixl();
        fast_u8 h = (*this)->on_get_ixh();
        return make16(h, l); }
    void on_set_ix(fast_u16 ix) {
        // Always set the low byte first.
        (*this)->on_set_ixl(get_low8(ix));
        (*this)->on_set_ixh(get_high8(ix)); }

    fast_u16 on_get_iy() {
        // Always get the low byte first.
        fast_u8 l = (*this)->on_get_iyl();
        fast_u8 h = (*this)->on_get_iyh();
        return make16(h, l); }
    void on_set_iy(fast_u16 iy) {
        // Always set the low byte first.
        (*this)->on_set_iyl(get_low8(iy));
        (*this)->on_set_iyh(get_high8(iy)); }

    fast_u16 get_pc_on_disp_read() const { return (*this)->on_get_pc(); }
    void set_pc_on_disp_read(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 get_pc_on_block_instr() const { return (*this)->on_get_pc(); }
    void set_pc_on_block_instr(fast_u16 pc) { (*this)->on_set_pc(pc); }

    fast_u16 on_get_ir() const { return (*this)->get_ir(); }

    fast_u16 get_ir_on_refresh() const { return (*this)->on_get_ir(); }

    bool on_get_iff1() const { return get_iff1(); }
    void on_set_iff1(bool iff1) { set_iff1(iff1); }

    void set_iff1_on_di(bool iff1) { (*this)->on_set_iff1(iff1); }
    void set_iff1_on_ei(bool iff1) { (*this)->on_set_iff1(iff1); }
    void set_iff1_on_retn(bool iff1) { (*this)->on_set_iff1(iff1); }

    bool on_get_iff2() const { return get_iff2(); }
    void on_set_iff2(bool iff2) { set_iff2(iff2); }

    void set_iff2_on_di(bool iff2) { (*this)->on_set_iff2(iff2); }
    void set_iff2_on_ei(bool iff2) { (*this)->on_set_iff2(iff2); }

    bool get_iff2_on_retn() const { return (*this)->on_get_iff2(); }

    bool on_get_int_mode() const { return get_int_mode(); }
    void on_set_int_mode(unsigned mode) { set_int_mode(mode); }

    fast_u16 get_disp_target(fast_u16 base, fast_u8 d) {
        return !get_sign8(d) ? add16(base, d) : sub16(base, neg8(d));
    }

    fast_u8 read_at_disp(fast_u8 d, bool long_read_cycle = false) {
        fast_u16 addr = get_disp_target((*this)->on_get_index_rp(), d);
        fast_u8 res = long_read_cycle ? (*this)->on_4t_read_cycle(addr) :
                                        (*this)->on_3t_read_cycle(addr);
        if(!is_index_rp_hl())
            (*this)->on_set_memptr(addr);
        return res;
    }

    void write_at_disp(fast_u8 d, fast_u8 n) {
        fast_u16 addr = get_disp_target((*this)->on_get_index_rp(), d);
        (*this)->on_3t_write_cycle(addr, n);
        if(!is_index_rp_hl())
            (*this)->on_set_memptr(addr);
    }

    fast_u8 on_get_r(reg r, index_regp irp, fast_u8 d = 0,
                     bool long_read_cycle = false) {
        switch(r) {
        case reg::b: return (*this)->on_get_b();
        case reg::c: return (*this)->on_get_c();
        case reg::d: return (*this)->on_get_d();
        case reg::e: return (*this)->on_get_e();
        case reg::at_hl: return read_at_disp(d, long_read_cycle);
        case reg::a: return (*this)->on_get_a();
        case reg::h:
            switch(irp) {
            case index_regp::hl: return (*this)->on_get_h();
            case index_regp::ix: return (*this)->on_get_ixh();
            case index_regp::iy: return (*this)->on_get_iyh();
            }
            break;
        case reg::l:
            switch(irp) {
            case index_regp::hl: return (*this)->on_get_l();
            case index_regp::ix: return (*this)->on_get_ixl();
            case index_regp::iy: return (*this)->on_get_iyl();
            }
            break;
        }
        unreachable("Unknown register.");
    }

    void on_set_r(reg r, index_regp irp, fast_u8 d, fast_u8 n) {
        switch(r) {
        case reg::b: return (*this)->on_set_b(n);
        case reg::c: return (*this)->on_set_c(n);
        case reg::d: return (*this)->on_set_d(n);
        case reg::e: return (*this)->on_set_e(n);
        case reg::at_hl: return write_at_disp(d, n);
        case reg::a: return (*this)->on_set_a(n);
        case reg::h:
            switch(irp) {
            case index_regp::hl: return (*this)->on_set_h(n);
            case index_regp::ix: return (*this)->on_set_ixh(n);
            case index_regp::iy: return (*this)->on_set_iyh(n);
            }
            break;
        case reg::l:
            switch(irp) {
            case index_regp::hl: return (*this)->on_set_l(n);
            case index_regp::ix: return (*this)->on_set_ixl(n);
            case index_regp::iy: return (*this)->on_set_iyl(n);
            }
            break;
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_rp(regp rp) {
        switch(rp) {
        case regp::bc: return (*this)->on_get_bc();
        case regp::de: return (*this)->on_get_de();
        case regp::hl: return (*this)->on_get_index_rp();
        case regp::sp: return (*this)->on_get_sp();
        }
        unreachable("Unknown register.");
    }

    void on_set_rp(regp rp, fast_u16 nn) {
        switch(rp) {
        case regp::bc: return (*this)->on_set_bc(nn);
        case regp::de: return (*this)->on_set_de(nn);
        case regp::hl: return (*this)->on_set_index_rp(nn);
        case regp::sp: return (*this)->on_set_sp(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_rp2(regp2 rp) {
        switch(rp) {
        case regp2::bc: return (*this)->on_get_bc();
        case regp2::de: return (*this)->on_get_de();
        case regp2::hl: return (*this)->on_get_index_rp();
        case regp2::af: return (*this)->on_get_af();
        }
        unreachable("Unknown register.");
    }

    void on_set_rp2(regp2 rp, fast_u16 nn) {
        switch(rp) {
        case regp2::bc: return (*this)->on_set_bc(nn);
        case regp2::de: return (*this)->on_set_de(nn);
        case regp2::hl: return (*this)->on_set_index_rp(nn);
        case regp2::af: return (*this)->on_set_af(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_index_rp() {
        switch(get_index_rp_kind()) {
        case index_regp::hl: return (*this)->on_get_hl();
        case index_regp::ix: return (*this)->on_get_ix();
        case index_regp::iy: return (*this)->on_get_iy();
        }
        unreachable("Unknown index register.");
    }

    void on_set_index_rp(fast_u16 nn) {
        switch(get_index_rp_kind()) {
        case index_regp::hl: return (*this)->on_set_hl(nn);
        case index_regp::ix: return (*this)->on_set_ix(nn);
        case index_regp::iy: return (*this)->on_set_iy(nn);
        }
        unreachable("Unknown index register.");
    }

    void do_sub(fast_u8 &a, fast_u8 &f, fast_u8 n) {
        fast_u8 t = sub8(a, n);
        f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                hf_ari(t, a, n) | pf_ari(a - n, a, n) | cf_ari(t > a) | nf_mask;
        a = t;
    }

    void do_cp(fast_u8 a, fast_u8 &f, fast_u8 n) {
        fast_u8 t = sub8(a, n);
        f = (t & sf_mask) | zf_ari(t) | (n & (yf_mask | xf_mask)) |
            hf_ari(t, a, n) | pf_ari(a - n, a, n) | cf_ari(t > a) | nf_mask;
    }

    void do_alu(alu k, fast_u8 n) {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = 0;
        switch(k) {
        case alu::add: {
            fast_u8 t = add8(a, n);
            f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_ari(a + n, a, n) | cf_ari(t < a);
            a = t;
            break; }
        case alu::adc: {
            f = (*this)->on_get_f();
            fast_u8 cfv = (f & cf_mask) ? 1 : 0;
            fast_u8 t = mask8(a + n + cfv);
            f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_ari(a + n + cfv, a, n) |
                    cf_ari(t < a || (cfv && n == 0xff));
            a = t;
            break; }
        case alu::sub: {
            do_sub(a, f, n);
            break; }
        case alu::sbc: {
            f = (*this)->on_get_f();
            fast_u8 cfv = (f & cf_mask) ? 1 : 0;
            fast_u8 t = mask8(a - n - cfv);
            f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_ari(a - n - cfv, a, n) |
                    cf_ari(t > a || (cfv && n == 0xff)) | nf_mask;
            a = t;
            break; }
        case alu::and_a:
            a &= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a) |
                    hf_mask;
            break;
        case alu::xor_a:
            a ^= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a);
            break;
        case alu::or_a:
            a |= n;
            f = (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) | pf_log(a);
            break;
        case alu::cp:
            do_cp(a, f, n);
            break;
        }
        if(k != alu::cp)
            (*this)->on_set_a(a);
        (*this)->on_set_f(f);
    }

    void do_rot(rot k, fast_u8 &n, fast_u8 &f) {
        fast_u8 t = n;
        bool cf = f & cf_mask;
        switch(k) {
        case rot::rlc:
            n = rol8(n);
            f = (n & (sf_mask | yf_mask | xf_mask | cf_mask)) | zf_ari(n) |
                    pf_log(n);
            break;
        case rot::rrc:
            n = mask8((n >> 1) | (n << 7));
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                cf_ari(t & 0x01);
            break;
        case rot::rl:
            n = mask8((n << 1) | (cf ? 1 : 0));
            // TODO: We don't need to read F here.
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x80);
            break;
        case rot::rr:
            n = (n >> 1) | ((cf ? 1u : 0u) << 7);
            // TODO: We don't need to read F here.
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x01);
            break;
        case rot::sla:
            n = mask8(n << 1);
            // TODO: We don't need to read F here.
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x80);
            break;
        case rot::sra:
            n = (n >> 1) | (n & 0x80);
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x01);
            break;
        case rot::sll:
            n = mask8(n << 1) | 1;
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x80);
            break;
        case rot::srl:
            n >>= 1;
            // TODO: We don't need to read F here.
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x1);
            break;
        }
    }

    void on_noni_ed(fast_u8 op) {
        // TODO: Forbid INT after this instruction.
        unused(op);
    }

    void on_relative_jump(fast_u8 d) {
        (*this)->on_5t_exec_cycle();
        (*this)->on_jump(get_disp_target((*this)->get_pc_on_jump(), d));
    }

    void on_add_irp_rp(regp rp) {
        fast_u16 i = (*this)->on_get_index_rp();
        fast_u16 n = (*this)->on_get_rp(rp);
        fast_u8 f = (*this)->on_get_f();

        (*this)->on_4t_exec_cycle();
        (*this)->on_3t_exec_cycle();

        fast_u16 r = add16(i, n);
        f = (f & (sf_mask | zf_mask | pf_mask)) |
                (get_high8(r) & (yf_mask | xf_mask)) |
                hf_ari(r >> 8, i >> 8, n >> 8) | cf_ari(r < i);

        (*this)->on_set_memptr(inc16(i));
        (*this)->on_set_index_rp(r);
        (*this)->on_set_f(f); }
    void on_adc_hl_rp(regp rp) {
        fast_u16 hl = (*this)->on_get_hl();
        fast_u16 n = (*this)->on_get_rp(rp);
        bool cf = (*this)->on_get_f() & cf_mask;

        (*this)->on_4t_exec_cycle();
        (*this)->on_3t_exec_cycle();

        fast_u16 t = add16(n, cf);
        bool of = cf && t == 0;
        fast_u32 r32 = hl + t;
        fast_u16 r16 = mask16(r32);
        fast_u8 f = (get_high8(r16) & (sf_mask | yf_mask | xf_mask)) |
                        zf_ari(r16) | hf_ari(r16 >> 8, hl >> 8, n >> 8) |
                        (pf_ari(r32 >> 8, hl >> 8, n >> 8) ^
                             (of ? pf_mask : 0)) |
                        cf_ari(r16 < hl || of);

        (*this)->on_set_memptr(inc16(hl));
        (*this)->on_set_hl(r16);
        (*this)->on_set_f(f); }
    void on_alu_n(alu k, fast_u8 n) {
        do_alu(k, n); }
    void on_alu_r(alu k, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        do_alu(k, (*this)->on_get_r(r, irp, d)); }
    void on_block_cp(block_cp k) {
        fast_u16 bc = (*this)->on_get_bc();
        fast_u16 memptr = (*this)->on_get_memptr();
        fast_u16 hl = (*this)->on_get_hl();
        // TODO: Block comparisons implicitly depend on the
        // register 'a'. We probably want to request its value
        // here with a special handler.
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();

        fast_u8 t = (*this)->on_3t_read_cycle(hl);
        fast_u8 tf = f;
        do_cp(a, tf, t);

        (*this)->on_5t_exec_cycle();
        bc = dec16(bc);

        t = a - t - ((tf & hf_mask) ? 1 : 0);
        f = (tf & (sf_mask | zf_mask | hf_mask)) |
                ((t << 4) & yf_mask) | (t & xf_mask) |
                (bc != 0 ? pf_mask : 0) | nf_mask | (f & cf_mask);

        if(static_cast<unsigned>(k) & 1) {
            // CPI, CPIR
            hl = dec16(hl);
            memptr = dec16(memptr);
        } else {
            // CPD, CPDR
            hl = inc16(hl);
            memptr = inc16(memptr);
        }

        (*this)->on_set_bc(bc);
        (*this)->on_set_memptr(memptr);
        (*this)->on_set_hl(hl);
        (*this)->on_set_f(f);

        // CPIR, CPDR
        if((static_cast<unsigned>(k) & 2) && bc && !(f & zf_mask)) {
            (*this)->on_5t_exec_cycle();
            fast_u16 pc = (*this)->get_pc_on_block_instr();
            (*this)->on_set_memptr(dec16(pc));
            (*this)->set_pc_on_block_instr(sub16(pc, 2));
        } }
    void on_block_ld(block_ld k) {
        fast_u16 bc = (*this)->on_get_bc();
        fast_u16 de = (*this)->on_get_de();
        fast_u16 hl = (*this)->on_get_hl();
        // TODO: Block loads implicitly depend on the register 'a'. We probably
        // want to request its value here with a special handler.
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();

        fast_u8 t = (*this)->on_3t_read_cycle(hl);

        (*this)->on_5t_write_cycle(de, t);
        bc = dec16(bc);

        t += a;
        f = (f & (sf_mask | zf_mask | cf_mask)) |
                ((t << 4) & yf_mask) | (t & xf_mask) | (bc != 0 ? pf_mask : 0);
        if(static_cast<unsigned>(k) & 1) {
            // LDI, LDIR
            hl = dec16(hl);
            de = dec16(de);
        } else {
            // LDD, LDDR
            hl = inc16(hl);
            de = inc16(de);
        }

        (*this)->on_set_bc(bc);
        (*this)->on_set_de(de);
        (*this)->on_set_hl(hl);
        (*this)->on_set_f(f);

        // LDIR, LDDR
        if((static_cast<unsigned>(k) & 2) && bc) {
            (*this)->on_5t_exec_cycle();
            fast_u16 pc = (*this)->get_pc_on_block_instr();
            (*this)->on_set_memptr(dec16(pc));
            (*this)->set_pc_on_block_instr(sub16(pc, 2));
        } }
    void on_bit(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        fast_u8 v = (*this)->on_get_r(r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = (*this)->on_get_f();
        fast_u8 m = v & (1u << b);
        f = (f & cf_mask) | hf_mask | (m ? (m & sf_mask) : (zf_mask | pf_mask));
        if(!is_index_rp_hl() || r == reg::at_hl)
            v = get_high8((*this)->on_get_memptr());
        f |= v & (xf_mask | yf_mask);
        (*this)->on_set_f(f); }
    void on_ccf() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        bool cf = f & cf_mask;
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                (cf ? hf_mask : 0) | cf_ari(!cf);
        (*this)->on_set_f(f); }
    void on_daa() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        bool cf = f & cf_mask;
        bool hf = f & hf_mask;
        bool nf = f & nf_mask;

        fast_u8 d = 0x00;
        if(cf || a >= 0x9a) {
            d |= 0x60;
            f |= cf_mask;
        }
        if(hf || (a & 0x0f) >= 0x0a) {
            d |= 0x06;
        }

        if(!nf) {
            f = (f & cf_mask) | ((a & 0x0f) >= 0x0a ? hf_mask : 0);
            a = add8(a, d);
        } else {
            f = (f & cf_mask) | (hf && (a & 0x0f) <= 0x05 ? hf_mask : 0) |
                    nf_mask;
            a = sub8(a, d);
        }
        f |= (a & (sf_mask | xf_mask | yf_mask)) | pf_log(a) | zf_ari(a);

        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_cpl() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        a ^= 0xff;
        f = (f & (sf_mask | zf_mask | pf_mask | cf_mask)) |
                (a & (yf_mask | xf_mask)) | hf_mask | nf_mask;
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_dec_r(reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        fast_u8 v = (*this)->on_get_r(r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = (*this)->on_get_f();
        v = dec8(v);
        f = (f & cf_mask) | (v & (sf_mask | yf_mask | xf_mask)) | zf_ari(v) |
                hf_dec(v) | pf_dec(v) | nf_mask;
        (*this)->on_set_r(r, irp, d, v);
        (*this)->on_set_f(f); }
    void on_di() {
        (*this)->set_iff1_on_di(false);
        (*this)->set_iff2_on_di(false); }
    void on_djnz(fast_u8 d) {
        fast_u8 b = (*this)->on_get_b();
        b = dec8(b);
        (*this)->on_set_b(b);
        if(b)
            (*this)->on_relative_jump(d); }
    void on_ei() {
        (*this)->set_iff1_on_ei(true);
        (*this)->set_iff2_on_ei(true);
        (*this)->disable_int_on_ei(); }
    void on_ex_af_alt_af() {
        ex_af_alt_af(); }
    void on_ex_at_sp_irp() {
        fast_u16 sp = (*this)->on_get_sp();
        fast_u8 lo = (*this)->on_3t_read_cycle(sp);
        sp = inc16(sp);
        fast_u8 hi = (*this)->on_4t_read_cycle(sp);
        fast_u16 nn = make16(hi, lo);
        fast_u16 irp = (*this)->on_get_index_rp();
        std::swap(nn, irp);
        (*this)->on_3t_write_cycle(sp, get_high8(nn));
        sp = dec16(sp);
        (*this)->on_5t_write_cycle(sp, get_low8(nn));
        (*this)->on_set_memptr(irp);
        (*this)->on_set_index_rp(irp); }
    void on_exx() {
        exx(); }
    void on_im(unsigned mode) {
        (*this)->on_set_int_mode(mode); }
    void on_in_a_n(fast_u8 n) {
        fast_u8 a = (*this)->on_get_a();
        fast_u16 addr = make16(a, n);
        (*this)->on_set_memptr(inc16(addr));
        (*this)->on_set_a((*this)->on_input_cycle(addr)); }
    void on_in_r_c(reg r) {
        fast_u16 bc = (*this)->on_get_bc();
        fast_u8 f = (*this)->on_get_f();
        (*this)->on_set_memptr(inc16(bc));
        fast_u8 n = (*this)->on_input_cycle(bc);
        index_regp irp = get_index_rp_kind();
        if(r != reg::at_hl)
            (*this)->on_set_r(r, irp, /* d= */ 0, n);
        f = (f & cf_mask) | (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) |
                pf_log(n);
        (*this)->on_set_f(f); }
    void on_inc_r(reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        fast_u8 v = (*this)->on_get_r(r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = (*this)->on_get_f();
        v = inc8(v);
        f = (f & cf_mask) | (v & (sf_mask | yf_mask | xf_mask)) | zf_ari(v) |
                hf_inc(v) | pf_inc(v);
        (*this)->on_set_r(r, irp, d, v);
        (*this)->on_set_f(f); }
    void on_jp_irp() {
        (*this)->set_pc_on_jump((*this)->on_get_index_rp()); }
    void on_jr(fast_u8 d) {
        (*this)->on_relative_jump(d); }
    void on_jr_cc(condition cc, fast_u8 d) {
        if(base::check_condition(cc))
            (*this)->on_relative_jump(d); }
    void on_ld_a_r() {
        fast_u8 n = (*this)->on_get_r_reg();
        fast_u8 f = (*this)->on_get_f();
        f = (f & cf_mask) | (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) |
                ((get_iff2() ? 1u : 0u) << pf_bit);
        (*this)->on_set_a(n);
        (*this)->on_set_f(f); }
    void on_ld_r_a() {
        (*this)->on_set_r_reg((*this)->on_get_a()); }
    void on_ld_i_a() {
        (*this)->set_i_on_ld((*this)->on_get_a()); }
    void on_ld_r_r(reg rd, reg rs, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        index_regp irpd = rs == reg::at_hl ? index_regp::hl : irp;
        index_regp irps = rd == reg::at_hl ? index_regp::hl : irp;
        (*this)->on_set_r(rd, irpd, d, (*this)->on_get_r(rs, irps, d)); }
    void on_ld_r_n(reg r, fast_u8 d, fast_u8 n) {
        index_regp irp = get_index_rp_kind();
        (*this)->on_set_r(r, irp, d, n); }
    void on_ld_irp_at_nn(fast_u16 nn) {
        fast_u8 lo = (*this)->on_3t_read_cycle(nn);
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        fast_u8 hi = (*this)->on_3t_read_cycle(nn);
        (*this)->on_set_index_rp(make16(hi, lo)); }
    void on_ld_at_nn_irp(fast_u16 nn) {
        fast_u16 irp = (*this)->on_get_index_rp();
        (*this)->on_3t_write_cycle(nn, get_low8(irp));
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        (*this)->on_3t_write_cycle(nn, get_high8(irp)); }

    void on_ld_rp_at_nn(regp rp, fast_u16 nn) {
        fast_u8 lo = (*this)->on_3t_read_cycle(nn);
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        fast_u8 hi = (*this)->on_3t_read_cycle(nn);
        (*this)->on_set_rp(rp, make16(hi, lo)); }
    void on_ld_at_nn_rp(fast_u16 nn, regp rp) {
        fast_u16 rpv = (*this)->on_get_rp(rp);
        (*this)->on_3t_write_cycle(nn, get_low8(rpv));
        nn = inc16(nn);
        (*this)->on_set_memptr(nn);
        (*this)->on_3t_write_cycle(nn, get_high8(rpv)); }
    void on_ld_sp_irp() {
        (*this)->on_set_sp((*this)->on_get_index_rp()); }
    void on_neg() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u8 n = a;
        a = 0;
        do_sub(a, f, n);
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_out_c_r(reg r) {
        fast_u16 bc = (*this)->on_get_bc();
        (*this)->on_set_memptr(inc16(bc));
        index_regp irp = get_index_rp_kind();
        fast_u8 n = (r == reg::at_hl) ?
            0 : (*this)->on_get_r(r, irp, /* d= */ 0);
        (*this)->on_output_cycle(bc, n); }
    void on_out_n_a(fast_u8 n) {
        fast_u8 a = (*this)->on_get_a();
        (*this)->on_output_cycle(make16(a, n), a);
        (*this)->on_set_memptr(make16(a, inc8(n))); }
    void on_res(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        reg access_r = irp == index_regp::hl ? r : reg::at_hl;
        fast_u8 v = (*this)->on_get_r(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        v &= ~(1u << b);
        (*this)->on_set_r(access_r, irp, d, v);
        if(irp != index_regp::hl && r != reg::at_hl)
            (*this)->on_set_r(r, irp, /* d= */ 0, v); }
    void on_reti() {
        (*this)->on_return(); }
    void on_retn() {
        (*this)->set_iff1_on_retn((*this)->get_iff2_on_retn());
        (*this)->on_return(); }
    void on_rla() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        bool cf = f & cf_mask;
        fast_u8 r = mask8((a << 1) | (cf ? 1 : 0));
        f = (f & (sf_mask | zf_mask | pf_mask)) | (r & (yf_mask | xf_mask)) |
                cf_ari(a & 0x80);
        (*this)->on_set_a(r);
        (*this)->on_set_f(f); }
    void on_rlca() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        a = rol8(a);
        f = (f & (sf_mask | zf_mask | pf_mask)) |
                (a & (yf_mask | xf_mask | cf_mask));
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_rld() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u16 hl = (*this)->on_get_hl();
        (*this)->on_set_memptr(inc16(hl));
        fast_u16 t = make16(a, (*this)->on_3t_read_cycle(hl));
        (*this)->on_4t_exec_cycle();

        t = (t & 0xf000) | ((t & 0xff) << 4) | ((t & 0x0f00) >> 8);
        a = get_high8(t);
        f = (f & cf_mask) | (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) |
                pf_log(a);

        (*this)->on_set_a(a);
        (*this)->on_set_f(f);
        (*this)->on_3t_write_cycle(hl, get_low8(t)); }
    void on_rot(rot k, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        reg access_r = irp == index_regp::hl ? r : reg::at_hl;
        fast_u8 n = (*this)->on_get_r(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        fast_u8 f = (*this)->on_get_f();
        do_rot(k, n, f);
        (*this)->on_set_r(access_r, irp, d, n);
        if(irp != index_regp::hl && r != reg::at_hl)
            (*this)->on_set_r(r, irp, /* d= */ 0, n);
        (*this)->on_set_f(f); }
    void on_rra() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u8 r = (a >> 1) | ((f & cf_mask) ? 0x80 : 0);
        f = (f & (sf_mask | zf_mask | pf_mask)) | (r & (yf_mask | xf_mask)) |
                cf_ari(a & 0x1);
        (*this)->on_set_a(r);
        (*this)->on_set_f(f); }
    void on_rrca() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        a = ror8(a);
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                cf_ari(a & 0x80);
        (*this)->on_set_a(a);
        (*this)->on_set_f(f); }
    void on_rrd() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        fast_u16 hl = (*this)->on_get_hl();
        (*this)->on_set_memptr(inc16(hl));
        fast_u16 t = make16(a, (*this)->on_3t_read_cycle(hl));
        (*this)->on_4t_exec_cycle();

        t = (t & 0xf000) | ((t & 0xf) << 8) | ((t & 0x0ff0) >> 4);
        a = get_high8(t);
        f = (f & cf_mask) | (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) |
                pf_log(a);

        (*this)->on_set_a(a);
        (*this)->on_set_f(f);
        (*this)->on_3t_write_cycle(hl, get_low8(t)); }
    void on_scf() {
        fast_u8 a = (*this)->on_get_a();
        fast_u8 f = (*this)->on_get_f();
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                cf_mask;
        (*this)->on_set_f(f); }
    void on_set(unsigned b, reg r, fast_u8 d) {
        index_regp irp = get_index_rp_kind();
        reg access_r = irp == index_regp::hl ? r : reg::at_hl;
        fast_u8 v = (*this)->on_get_r(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        v |= 1u << b;
        (*this)->on_set_r(access_r, irp, d, v);
        if(irp != index_regp::hl && r != reg::at_hl)
            (*this)->on_set_r(r, irp, /* d= */ 0, v); }
    void on_sbc_hl_rp(regp rp) {
        fast_u16 hl = (*this)->on_get_hl();
        fast_u16 n = (*this)->on_get_rp(rp);
        bool cf = (*this)->on_get_f() & cf_mask;

        (*this)->on_4t_exec_cycle();
        (*this)->on_3t_exec_cycle();

        fast_u16 t = add16(n, cf);
        bool of = cf && t == 0;
        fast_u32 r32 = hl - t;
        fast_u16 r16 = mask16(r32);
        fast_u8 f = (get_high8(r16) & (sf_mask | yf_mask | xf_mask)) |
                        zf_ari(r16) | hf_ari(r16 >> 8, hl >> 8, n >> 8) |
                        (pf_ari(r32 >> 8, hl >> 8, n >> 8) ^
                             (of ? pf_mask : 0)) |
                        cf_ari(r16 > hl || of) | nf_mask;

        (*this)->on_set_memptr(inc16(hl));
        (*this)->on_set_hl(r16);
        (*this)->on_set_f(f); }

    fast_u8 on_fetch(bool m1 = true) {
        fast_u16 pc = (*this)->get_pc_on_fetch();
        fast_u8 op = (*this)->on_fetch_cycle(pc, m1);
        (*this)->set_pc_on_fetch(inc16(pc));
        return op;
    }

    void on_set_addr_bus(fast_u16 addr) {
        unused(addr);
    }

    fast_u8 on_fetch_cycle(fast_u16 addr, bool m1 = true) {
        (*this)->on_set_addr_bus(addr);
        fast_u8 b = (*this)->on_read_access(addr);
        (*this)->tick(2);
        (*this)->on_set_addr_bus((*this)->get_ir_on_refresh());
        if(m1)
            (*this)->on_inc_r_reg();
        (*this)->tick(2);
        set_last_read_addr(addr);
        return b;
    }

    void on_6t_fetch_cycle() {
        (*this)->tick(2);
    }

    fast_u8 on_5t_read_cycle(fast_u16 addr) {
        return (*this)->on_read_cycle(addr, /* ticks= */ 5);
    }

    fast_u8 on_disp_read_cycle(fast_u16 addr) {
        return (*this)->on_3t_read_cycle(addr);
    }

    void on_4t_exec_cycle() {
        (*this)->tick(4);
    }

    void on_5t_exec_cycle() {
        (*this)->tick(5);
    }

    fast_u8 on_input_cycle(fast_u16 addr) {
        // Z80 samples the value at t4 of the input cycle, see
        // <http://ramsoft.bbk.org.omegahg.com/floatingbus.html>.
        (*this)->tick(4);
        // TODO: Shall we set the address bus here?
        fast_u8 n = (*this)->on_input(addr);
        return n;
    }

    void on_output_cycle(fast_u16 addr, fast_u8 n) {
        // TODO: Shall we set the address bus here?
        unused(addr, n);
        (*this)->tick(4);
    }

    fast_u8 on_5t_imm8_read() {
        fast_u16 pc = (*this)->get_pc_on_imm8_read();
        fast_u8 op = (*this)->on_5t_read_cycle(pc);
        (*this)->set_pc_on_imm8_read(inc16(pc));
        return op;
    }

    fast_u8 on_disp_read() {
        fast_u16 pc = (*this)->get_pc_on_disp_read();
        fast_u8 op = (*this)->on_disp_read_cycle(pc);
        (*this)->set_pc_on_disp_read(inc16(pc));
        return op;
    }

    void initiate_int() {
        set_iff1(false);
        set_iff2(false);

        fast_u16 pc = (*this)->on_get_pc();

        // Get past the HALT instruction, if halted. Note that
        // HALT instructions need to be executed at least once to
        // be skipped on an interrupt, so checking if the PC is
        // at a HALT instruction is not enough here.
        if(is_halted()) {
            pc = inc16(pc);
            (*this)->on_set_pc(pc);
            set_is_halted(false);
        }

        (*this)->on_inc_r_reg();
        (*this)->tick(7);
        (*this)->on_push(pc);

        fast_u16 isr_addr;
        switch(get_int_mode()) {
        case 0:
            isr_addr = 0;
            assert(0);  // TODO
            break;
        case 1:
            // ack(7) w(3) w(3)
            isr_addr = 0x0038;
            break;
        case 2: {
            // ack(7) w(3) w(3) r(3) r(3)
            fast_u16 vector_addr = make16((*this)->on_get_i(), 0xff);
            fast_u8 lo = (*this)->on_3t_read_cycle(vector_addr);
            fast_u8 hi = (*this)->on_3t_read_cycle(inc16(vector_addr));
            isr_addr = make16(hi, lo); }
            break;
        default:
            unreachable("Unknown interrupts mode.");
        }

        (*this)->on_jump(isr_addr);
    }

    bool handle_active_int() {
        if(!is_int_disabled() && get_iff1()) {
            initiate_int();
            return true;
        }
        return false;
    }

protected:
    D *operator -> () { return static_cast<D*>(this); }
    const D *operator -> () const { return static_cast<const D*>(this); }
};

}  // namespace z80

#endif  // Z80_H
