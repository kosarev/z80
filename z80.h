
/*  Z80 CPU Emulator.
    https://github.com/kosarev/z80

    Copyright (c) 2017 Ivan Kosarev <mail@ivankosarev.com>
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
typedef uint_least32_t least_u32;

static inline void unused(...) {}

[[noreturn]] static inline void unreachable(const char *msg) {
#if !defined(NDEBUG)
    std::fprintf(stderr, "%s\n", msg);
    std::abort();
#elif defined(_MSC_VER)
    __assume(0);
#else
    __builtin_unreachable();
#endif
}

template<typename T>
static inline constexpr fast_u8 mask8(T n) {
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
enum class iregp { hl, ix, iy };

enum class alu { add, adc, sub, sbc, and_a, xor_a, or_a, cp };
enum class rot { rlc, rrc, rl, rr, sla, sra, sll, srl };
enum class block_ld { ldi, ldd, ldir, lddr };
enum class block_cp { cpi, cpd, cpir, cpdr };
enum class block_in { ini, ind, inir, indr };
enum class block_out { outi, outd, otir, otdr };

enum class condition { nz, z, nc, c, po, pe, p, m };

enum class z80_variant {
    common,
    cmos,  // Newer chips.
};

// Entities for internal needs of the library.
class internals {
private:
    // Returns false, but not earlier than on instantiation.
    template<typename T> static constexpr bool get_false() { return false; }

    template<typename B> class decoder_base;
    template<typename B> class disasm_base;
    template<typename B> class cpu_state_base;
    template<typename B> class executor_base;

    template<typename D> friend class root;

    template<typename B> friend class i8080_decoder;
    template<typename B> friend class z80_decoder;

    template<typename D> friend class i8080_disasm;
    template<typename D> friend class z80_disasm;

    template<typename B> friend class i8080_state;
    template<typename B> friend class z80_state;

    template<typename B> friend class i8080_executor;
    template<typename B> friend class z80_executor;
};

template<typename D>
class root {
public:
    typedef D derived;

    iregp on_get_iregp_kind() const { return iregp::hl; }
    void on_set_iregp_kind(iregp r) { unused(r); }
    fast_u8 on_get_b() const { return 0; }
    void on_set_b(fast_u8 n) { unused(n); }
    fast_u8 on_get_c() const { return 0; }
    void on_set_c(fast_u8 n) { unused(n); }
    fast_u8 on_get_d() const { return 0; }
    void on_set_d(fast_u8 n) { unused(n); }
    fast_u8 on_get_e() const { return 0; }
    void on_set_e(fast_u8 n) { unused(n); }
    fast_u8 on_get_h() const { return 0; }
    void on_set_h(fast_u8 n) { unused(n); }
    fast_u8 on_get_l() const { return 0; }
    void on_set_l(fast_u8 n) { unused(n); }
    fast_u8 on_get_a() const { return 0; }
    void on_set_a(fast_u8 n) { unused(n); }
    fast_u8 on_get_f() const { return 0; }
    void on_set_f(fast_u8 n) { unused(n); }
    fast_u8 on_get_ixh() const { return 0; }
    void on_set_ixh(fast_u8 n) { unused(n); }
    fast_u8 on_get_ixl() const { return 0; }
    void on_set_ixl(fast_u8 n) { unused(n); }
    fast_u8 on_get_iyh() const { return 0; }
    void on_set_iyh(fast_u8 n) { unused(n); }
    fast_u8 on_get_iyl() const { return 0; }
    void on_set_iyl(fast_u8 n) { unused(n); }
    fast_u8 on_get_i() const { return 0; }
    void on_set_i(fast_u8 n) { unused(n); }
    fast_u8 on_get_r() const { return 0; }
    void on_set_r(fast_u8 n) { unused(n); }
    fast_u16 on_get_pc() const { return 0; }
    void on_set_pc(fast_u16 n) { unused(n); }
    fast_u16 on_get_sp() const { return 0; }
    void on_set_sp(fast_u16 n) { unused(n); }
    fast_u16 on_get_wz() const { return 0; }
    void on_set_wz(fast_u16 n) { unused(n); }
    bool on_is_halted() const { return false; }
    void on_set_is_halted(bool f) { unused(f); }
    bool on_get_iff() const { return false; }
    void on_set_iff(bool f) { unused(f); }
    bool on_get_iff1() const { return false; }
    void on_set_iff1(bool f) { unused(f); }
    bool on_get_iff2() const { return false; }
    void on_set_iff2(bool f) { unused(f); }
    unsigned on_get_int_mode() const { return 0; }
    void on_set_int_mode(unsigned mode) { unused(mode); }
    void on_set_is_int_disabled(bool f) { unused(f); }
    fast_u8 on_get_int_vector() { return 0xff; }

    void set_i_on_ld(fast_u8 i) { self().on_set_i(i); }

    fast_u16 get_pc_on_disp_read() { return self().on_get_pc(); }
    void set_pc_on_disp_read(fast_u16 pc) { self().on_set_pc(pc); }

    fast_u16 get_pc_on_block_instr() { return self().on_get_pc(); }
    void set_pc_on_block_instr(fast_u16 pc) { self().on_set_pc(pc); }

    void set_iff_on_di(bool iff) { self().on_set_iff(iff); }
    void set_iff_on_ei(bool iff) { self().on_set_iff(iff); }

    void set_iff1_on_di(bool f) { self().on_set_iff1(f); }
    void set_iff1_on_ei(bool f) { self().on_set_iff1(f); }
    void set_iff1_on_reti_retn(bool f) { self().on_set_iff1(f); }

    void set_iff2_on_di(bool f) { self().on_set_iff2(f); }
    void set_iff2_on_ei(bool f) { self().on_set_iff2(f); }

    bool get_iff2_on_reti_retn() { return self().on_get_iff2(); }

    fast_u16 on_get_bc() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_c();
        fast_u8 h = self().on_get_b();
        return make16(h, l); }
    void on_set_bc(fast_u16 n) {
        // Always set the low byte first.
        self().on_set_c(get_low8(n));
        self().on_set_b(get_high8(n)); }
    fast_u16 on_get_de() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_e();
        fast_u8 h = self().on_get_d();
        return make16(h, l); }
    void on_set_de(fast_u16 n) {
        // Always set the low byte first.
        self().on_set_e(get_low8(n));
        self().on_set_d(get_high8(n)); }
    fast_u16 on_get_hl() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_l();
        fast_u8 h = self().on_get_h();
        return make16(h, l); }
    void on_set_hl(fast_u16 n) {
        // Always set the low byte first.
        self().on_set_l(get_low8(n));
        self().on_set_h(get_high8(n)); }
    fast_u16 on_get_af() {
        // Always get the low byte first.
        fast_u8 f = self().on_get_f();
        fast_u8 a = self().on_get_a();
        return make16(a, f); }
    void on_set_af(fast_u16 n) {
        // Always set the low byte first.
        self().on_set_f(get_low8(n));
        self().on_set_a(get_high8(n)); }
    fast_u16 on_get_ix() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_ixl();
        fast_u8 h = self().on_get_ixh();
        return make16(h, l); }
    void on_set_ix(fast_u16 ix) {
        // Always set the low byte first.
        self().on_set_ixl(get_low8(ix));
        self().on_set_ixh(get_high8(ix)); }
    fast_u16 on_get_iy() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_iyl();
        fast_u8 h = self().on_get_iyh();
        return make16(h, l); }
    void on_set_iy(fast_u16 iy) {
        // Always set the low byte first.
        self().on_set_iyl(get_low8(iy));
        self().on_set_iyh(get_high8(iy)); }
    fast_u16 on_get_ir() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_r();
        fast_u8 h = self().on_get_i();
        return make16(h, l); }

    fast_u8 on_get_reg(reg r) {
        switch(r) {
        case reg::b: return self().on_get_b();
        case reg::c: return self().on_get_c();
        case reg::d: return self().on_get_d();
        case reg::e: return self().on_get_e();
        case reg::at_hl: unreachable("Can't get (HL) value.");
        case reg::a: return self().on_get_a();
        case reg::h: return self().on_get_h();
        case reg::l: return self().on_get_l();
        }
        unreachable("Unknown register.");
    }

    void on_set_reg(reg r, fast_u8 n) {
        switch(r) {
        case reg::b: return self().on_set_b(n);
        case reg::c: return self().on_set_c(n);
        case reg::d: return self().on_set_d(n);
        case reg::e: return self().on_set_e(n);
        case reg::at_hl: unreachable("Can't set (HL) value.");
        case reg::a: return self().on_set_a(n);
        case reg::h: return self().on_set_h(n);
        case reg::l: return self().on_set_l(n);
        }
        unreachable("Unknown register.");
    }

    fast_u8 on_get_reg(reg r, iregp irp) {
        switch(r) {
        case reg::b: return self().on_get_b();
        case reg::c: return self().on_get_c();
        case reg::d: return self().on_get_d();
        case reg::e: return self().on_get_e();
        case reg::at_hl: unreachable("Can't get (HL) value.");
        case reg::a: return self().on_get_a();
        case reg::h:
            switch(irp) {
            case iregp::hl: return self().on_get_h();
            case iregp::ix: return self().on_get_ixh();
            case iregp::iy: return self().on_get_iyh();
            }
            break;
        case reg::l:
            switch(irp) {
            case iregp::hl: return self().on_get_l();
            case iregp::ix: return self().on_get_ixl();
            case iregp::iy: return self().on_get_iyl();
            }
            break;
        }
        unreachable("Unknown register.");
    }

    void on_set_reg(reg r, iregp irp, fast_u8 n) {
        switch(r) {
        case reg::b: return self().on_set_b(n);
        case reg::c: return self().on_set_c(n);
        case reg::d: return self().on_set_d(n);
        case reg::e: return self().on_set_e(n);
        case reg::at_hl: unreachable("Can't set (HL) value.");
        case reg::a: return self().on_set_a(n);
        case reg::h:
            switch(irp) {
            case iregp::hl: return self().on_set_h(n);
            case iregp::ix: return self().on_set_ixh(n);
            case iregp::iy: return self().on_set_iyh(n);
            }
            break;
        case reg::l:
            switch(irp) {
            case iregp::hl: return self().on_set_l(n);
            case iregp::ix: return self().on_set_ixl(n);
            case iregp::iy: return self().on_set_iyl(n);
            }
            break;
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_iregp(iregp irp) {
        switch(irp) {
        case iregp::hl: return self().on_get_hl();
        case iregp::ix: return self().on_get_ix();
        case iregp::iy: return self().on_get_iy();
        }
        unreachable("Unknown index register.");
    }

    void on_set_iregp(iregp irp, fast_u16 nn) {
        switch(irp) {
        case iregp::hl: return self().on_set_hl(nn);
        case iregp::ix: return self().on_set_ix(nn);
        case iregp::iy: return self().on_set_iy(nn);
        }
        unreachable("Unknown index register.");
    }

    fast_u16 on_get_regp(regp rp) {
        switch(rp) {
        case regp::bc: return self().on_get_bc();
        case regp::de: return self().on_get_de();
        case regp::hl: return self().on_get_hl();
        case regp::sp: return self().on_get_sp();
        }
        unreachable("Unknown register.");
    }

    void on_set_regp(regp rp, fast_u16 nn) {
        switch(rp) {
        case regp::bc: return self().on_set_bc(nn);
        case regp::de: return self().on_set_de(nn);
        case regp::hl: return self().on_set_hl(nn);
        case regp::sp: return self().on_set_sp(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_regp(regp rp, iregp irp) {
        return rp == regp::hl ? self().on_get_iregp(irp) :
                                self().on_get_regp(rp);
    }

    void on_set_regp(regp rp, iregp irp, fast_u16 nn) {
        return rp == regp::hl ? self().on_set_iregp(irp, nn) :
                                self().on_set_regp(rp, nn);
    }

    fast_u16 on_get_regp2(regp2 rp) {
        switch(rp) {
        case regp2::bc: return self().on_get_bc();
        case regp2::de: return self().on_get_de();
        case regp2::hl: return self().on_get_hl();
        case regp2::af: return self().on_get_af();
        }
        unreachable("Unknown register.");
    }

    void on_set_regp2(regp2 rp, fast_u16 nn) {
        switch(rp) {
        case regp2::bc: return self().on_set_bc(nn);
        case regp2::de: return self().on_set_de(nn);
        case regp2::hl: return self().on_set_hl(nn);
        case regp2::af: return self().on_set_af(nn);
        }
        unreachable("Unknown register.");
    }

    fast_u16 on_get_regp2(regp2 rp, iregp irp) {
        return rp == regp2::hl ? self().on_get_iregp(irp) :
                                 self().on_get_regp2(rp);
    }

    void on_set_regp2(regp2 rp, iregp irp, fast_u16 nn) {
        return rp == regp2::hl ? self().on_set_iregp(irp, nn) :
                                 self().on_set_regp2(rp, nn);
    }

    // No dummy implementations for the following handlers as
    // being forgotten to be implemented, they would lead to
    // problems that are hard to diagnose.
    void on_ex_de_hl_regs() {
        static_assert(internals::get_false<derived>(),
                      "on_ex_de_hl_regs() has to be implemented!"); }
    void on_ex_af_alt_af_regs() {
        static_assert(!derived::z80_enabled,
                      "on_ex_af_alt_af_regs() has to be implemented!"); }
    void on_exx_regs() {
        static_assert(!derived::z80_enabled,
                      "on_exx_regs() has to be implemented!"); }

    fast_u8 on_read(fast_u16 addr) {
        unused(addr);
        return 0x00; }
    void on_write(fast_u16 addr, fast_u8 n) {
        unused(addr, n); }
    // TODO: Should we provide separate 8-bit and 16-bit versions
    //       of these?
    fast_u8 on_input(fast_u16 port) {
        unused(port);
        return 0xff; }
    void on_output(fast_u16 port, fast_u8 n) {
        unused(port, n); }

    void on_tick(unsigned t) {
        unused(t); }

    void on_reset_decoder() {}
    void on_reset_cpu(bool soft = false) {
        unused(soft);
        self().on_reset_decoder(); }
    void on_reset_memory() {}
    void on_reset(bool soft = false) {
        self().on_reset_cpu(soft);
        if (!soft)
            self().on_reset_memory(); }

    fast_u8 on_m1_fetch_cycle() {
        fast_u8 n = self().on_fetch_cycle();
        return n; }
    void on_fetch_cycle_extra_1t() {
        self().on_tick(1); }
    void on_fetch_cycle_extra_2t() {
        self().on_tick(2); }
    void on_fetch_cycle_extra_3t() {
        self().on_tick(3); }
    fast_u8 on_read_cycle(fast_u16 addr) {
        self().on_set_addr_bus(addr);
        fast_u8 n = self().on_read(addr);
        self().on_tick(2);
        self().on_mreq_wait(addr);
        self().on_tick(1);
        return n; }
    void on_read_cycle_extra_1t() {
        self().on_tick(1); }
    void on_read_cycle_extra_2t() {
        self().on_tick(2); }
    void on_write_cycle(fast_u16 addr, fast_u8 n) {
        self().on_set_addr_bus(addr);
        self().on_write(addr, n);
        self().on_tick(2);
        self().on_mreq_wait(addr);
        self().on_tick(1); }
    void on_write_cycle_extra_2t() {
        self().on_tick(2); }
    void on_3t_exec_cycle() {
        self().on_tick(3); }
    void on_4t_exec_cycle() {
        self().on_tick(4); }
    void on_5t_exec_cycle() {
        self().on_tick(5); }

    void on_mreq_wait(fast_u16 addr) {
        unused(addr); }
    void on_iorq_wait(fast_u16 port) {
        unused(port); }

    void on_ed_xnop(fast_u8 op) {
        unused(op);
        self().on_nop(); }
    void on_xcall_nn(fast_u8 op, fast_u16 nn) {
        unused(op);
        self().on_call_nn(nn); }
    void on_xim(fast_u8 op, fast_u8 mode) {
        unused(op);
        self().on_im(mode); }
    void on_xjp_nn(fast_u16 nn) {
        self().on_jp_nn(nn); }
    void on_xneg(fast_u8 op) {
        unused(op);
        self().on_neg(); }
    void on_xnop(fast_u8 op) {
        unused(op);
        self().on_nop(); }
    void on_xret() {
        self().on_ret(); }
    void on_xretn(fast_u8 op) {
        unused(op);
        self().on_retn(); }

    z80_variant on_get_z80_variant() {
        return z80_variant::common; }

    fast_u8 on_get_out_c_r_op() {
        switch(self().on_get_z80_variant()) {
        case z80_variant::common: return 0;
        case z80_variant::cmos: return 0xff;
        }
        unreachable("Unknown Z80 variant."); }

protected:
    const derived &self() const{ return static_cast<const derived&>(*this); }
    derived &self() { return static_cast<derived&>(*this); }
};

template<typename B>
class z80_decoder_state : public B {
public:
    using base = B;

    z80_decoder_state() {}

    iregp get_iregp_kind() const { return fields.irp; }
    void set_iregp_kind(iregp r) { fields.irp = r; }

    iregp on_get_iregp_kind() const { return get_iregp_kind(); }
    void on_set_iregp_kind(iregp r) { set_iregp_kind(r); }

    void on_reset_decoder() {
        base::on_reset_decoder();
        fields = state_fields();
    }

private:
    struct state_fields {
        iregp irp = iregp::hl;
    };

    state_fields fields;
};

template<typename B>
class internals::decoder_base : public B {
public:
    typedef B base;

    void disable_int_on_index_prefix() {
        self().on_set_is_int_disabled(true); }

    void on_instr_prefix(iregp irp) {
        self().on_set_iregp_kind(irp);
        self().disable_int_on_index_prefix(); }

protected:
    bool is_hl_iregp() {
        return self().on_get_iregp_kind() == iregp::hl;
    }

    fast_u8 read_disp_or_null(bool may_need_disp) {
        if(!may_need_disp || is_hl_iregp())
            return 0;
        fast_u8 d = self().on_disp_read();
        self().on_5t_exec_cycle();
        return d;
    }

    fast_u8 read_disp_or_null(reg r) {
        return read_disp_or_null(r == reg::at_hl);
    }

    fast_u8 read_disp_or_null(reg r1, reg r2) {
        return read_disp_or_null(r1 == reg::at_hl || r2 == reg::at_hl);
    }

    // Transfers.
public:
    void on_decode_ld_r_n(reg r) {
        fast_u8 d, n;
        if(!self().on_is_z80() || r != reg::at_hl || is_hl_iregp()) {
            d = 0;
            n = self().on_imm8_read();
        } else {
            d = self().on_disp_read();
            n = self().on_imm8_read();
            self().on_read_cycle_extra_2t();
        }
        self().on_ld_r_n(r, d, n); }
    void on_decode_ld_r_r(reg rd, reg rs) {
        fast_u8 d = !self().on_is_z80() ? 0 : read_disp_or_null(rd, rs);
        self().on_ld_r_r(rd, rs, d); }
    void on_decode_ld_sp_irp() {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        else
            self().on_fetch_cycle_extra_2t();
        self().on_ld_sp_irp(); }

    // Swaps.
    void on_decode_ex_af_alt_af() {
        if(!self().on_is_z80())
            return self().on_xnop(/* op= */ 0x08);
        self().on_ex_af_alt_af(); }
    void on_decode_ex_de_hl() {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        self().on_ex_de_hl(); }
    void on_decode_exx() {
        if(!self().on_is_z80())
            return self().on_xret();
        self().on_exx(); }

    // Arithmetic.
    void on_decode_alu_r(alu k, reg r) {
        fast_u8 d = !self().on_is_z80() ? 0 : read_disp_or_null(r);
        self().on_alu_r(k, r, d); }
    void on_decode_inc_r(reg r) {
        fast_u8 d;
        if(!self().on_is_z80()) {
            d = 0;
            if(r != reg::at_hl)
                self().on_fetch_cycle_extra_1t();
        } else {
            d = read_disp_or_null(r);
        }
        self().on_inc_r(r, d); }
    void on_decode_dec_r(reg r) {
        fast_u8 d;
        if(!self().on_is_z80()) {
            d = 0;
            if(r != reg::at_hl)
                self().on_fetch_cycle_extra_1t();
        } else {
            d = read_disp_or_null(r);
        }
        self().on_dec_r(r, d); }
    void on_decode_inc_rp(regp rp) {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        else
            self().on_fetch_cycle_extra_2t();
        self().on_inc_rp(rp); }
    void on_decode_dec_rp(regp rp) {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        else
            self().on_fetch_cycle_extra_2t();
        self().on_dec_rp(rp); }

    // Jumps.
    void on_decode_xcall_nn(fast_u8 op) {
        fast_u16 nn = self().on_imm16_read();
        self().on_read_cycle_extra_1t();
        self().on_xcall_nn(op, nn); }
    void on_decode_call_cc_nn(condition cc) {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        self().on_call_cc_nn(cc, self().on_imm16_read()); }
    void on_decode_jp_irp() {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_1t();
        self().on_jp_irp(); }
    void on_decode_jr() {
        if(!self().on_is_z80())
            return self().on_xnop(/* op= */ 0x18);
        self().on_jr(self().on_disp_read()); }
    void on_decode_jr_cc(fast_u8 op) {
        if(!self().on_is_z80())
            return self().on_xnop(op);
        auto cc = static_cast<condition>((op & (y_mask - 0040)) >> 3);
        self().on_jr_cc(cc, self().on_disp_read()); }
    void on_decode_djnz() {
        if(!self().on_is_z80())
            return self().on_xnop(/* op= */ 0x10);
        self().on_fetch_cycle_extra_1t();
        self().on_djnz(self().on_disp_read()); }

    // Interrupts.
    void on_decode_halt() {
        if(!self().on_is_z80())
            self().on_fetch_cycle_extra_3t();
        self().on_halt(); }

    // Prefixes.
    void on_decode_dd_prefix() {
        if(!self().on_is_z80())
            return self().on_decode_xcall_nn(0xdd);
        self().on_instr_prefix(iregp::ix); }
    void on_decode_fd_prefix() {
        if(!self().on_is_z80())
            return self().on_decode_xcall_nn(0xfd);
        self().on_instr_prefix(iregp::iy); }

    void on_decode_cb_prefix() {
        if(!self().on_is_z80())
            return self().on_xjp_nn(self().on_imm16_read());

        fast_u8 d = 0;
        iregp irp = self().on_get_iregp_kind();
        if(irp != iregp::hl)
            d = self().on_disp_read();

        fast_u8 op;
        if(irp == iregp::hl) {
            op = self().on_m1_fetch_cycle();
        } else {
            // In ddcb- and fdcb-prefixed instructions the
            // reading of the 3rd opcode is not an M1 cycle.
            op = self().on_fetch_cycle();
            self().on_fetch_cycle_extra_1t();
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
            return self().on_rot(k, r, d); }
        case 0100: {
            // BIT y, r[z]
            // BIT b, r             f(4)      f(4)
            // BIT b, (HL)          f(4)      f(4) r(4)
            // BIT b, (i+d)    f(4) f(4) r(3) f(5) r(4)
            auto b = static_cast<unsigned>(y);
            return self().on_bit(b, r, d); }
        case 0200: {
            // RES y, r[z]
            // RES b, r             f(4)      f(4)
            // RES b, (HL)          f(4)      f(4) r(4) w(3)
            // RES b, (i+d)    f(4) f(4) r(3) f(5) r(4) w(3)
            auto b = static_cast<unsigned>(y);
            return self().on_res(b, r, d); }
        case 0300: {
            // SET y, r[z]
            // SET b, r             f(4)      f(4)
            // SET b, (HL)          f(4)      f(4) r(4) w(3)
            // SET b, (i+d)    f(4) f(4) r(3) f(5) r(4) w(3)
            auto b = static_cast<unsigned>(y);
            return self().on_set(b, r, d); }
        }

        unreachable("Unknown opcode encountered!");
    }

    void on_decode_ed_prefix() {
        if(!self().on_is_z80())
            return self().on_decode_xcall_nn(0xed);

        fast_u8 op = self().on_m1_fetch_cycle();
        fast_u8 y = get_y_part(op);
        fast_u8 p = get_p_part(op);

        switch(op) {
        // TODO: Combine with decoding xneg's.
        case 0x44:
            // NEG  f(4) f(4)
            return self().on_neg();
        // TODO: Combine with decoding xim's.
        case 0x46:
            // IM im[y]  f(4) f(4)
            return self().on_im(0);
        case 0x56:
        case 0x5e:
            return self().on_im(y - 1);
        // TODO: Combine with decoding xretn's.
        case 0x4d:
            // RETI  f(4) f(4) r(3) r(3)
            return self().on_reti();
        case 0x45:
            // RETN  f(4) f(4) r(3) r(3)
            return self().on_retn();
        case 0x47:
            // LD I, A  f(4) f(5)
            self().on_fetch_cycle_extra_1t();
            return self().on_ld_i_a();
        case 0x4f:
            // LD R, A  f(4) f(5)
            self().on_fetch_cycle_extra_1t();
            return self().on_ld_r_a();
        case 0x57:
            // LD A, I  f(4) f(5)
            self().on_fetch_cycle_extra_1t();
            return self().on_ld_a_i();
        case 0x5f:
            // LD A, R  f(4) f(5)
            self().on_fetch_cycle_extra_1t();
            return self().on_ld_a_r();
        case 0x67:
            // RRD  f(4) f(4) r(3) e(4) w(3)
            return self().on_rrd();
        case 0x6f:
            // RLD  f(4) f(4) r(3) e(4) w(3)
            return self().on_rld();
        }
        // TODO: Can be just switch(op & (x_mask | z_mask)) ?
        if((op & x_mask) == 0100) {
            switch(op & z_mask) {
            case 0: {
                // IN r[y], (C)  f(4) f(4) i(4)
                // IN (C)        f(4) f(4) i(4)
                auto r = static_cast<reg>(y);
                return self().on_in_r_c(r); }
            case 1: {
                // OUT (C), r[y]  f(4) f(4) o(4)
                // OUT (C), 0     f(4) f(4) o(4)
                auto r = static_cast<reg>(y);
                return self().on_out_c_r(r); }
            case 2: {
                // ADC HL, rp[p]  f(4) f(4) e(4) e(3)
                // SBC HL, rp[p]  f(4) f(4) e(4) e(3)
                auto rp = static_cast<regp>(p);
                return op & q_mask ? self().on_adc_hl_rp(rp) :
                                     self().on_sbc_hl_rp(rp); }
            case 3: {
                // LD rp[p], (nn)  f(4) f(4) r(3) r(3) r(3) r(3)
                // LD (nn), rp[p]  f(4) f(4) r(3) r(3) w(3) w(3)
                auto rp = static_cast<regp>(p);
                fast_u16 nn = self().on_imm16_read();
                return op & q_mask ? self().on_ld_rp_at_nn(rp, nn) :
                                     self().on_ld_at_nn_rp(nn, rp); }
            case 4:
                // XNEG  f(4) f(4)
                return self().on_xneg(op);
            case 5:
                // XRETN  f(4) f(4) r(3) r(3)
                return self().on_xretn(op);
            case 6: {
                // IM im[y]  f(4) f(4)
                fast_u8 n = y & 3;
                return self().on_xim(op, n == 0 ? 0 : n - 1); }
            }
        }
        if((op & x_mask) == 0200 && y >= 4) {
            fast_u8 n = y - 4;
            switch(op & z_mask) {
            case 0: {
                // LDI, LDD, LDIR, LDDR  f(4) f(4) r(3) w(5) + e(5)
                auto k = static_cast<block_ld>(n);
                return self().on_block_ld(k); }
            case 1: {
                // CPI, CPD, CPIR, CPDR  f(4) f(4) r(3) e(5) + e(5)
                auto k = static_cast<block_cp>(n);
                return self().on_block_cp(k); }
            case 2: {
                // INI, IND, INIR, INDR  f(4) f(5) i(4) w(3) + e(5)
                auto k = static_cast<block_in>(n);
                return self().on_block_in(k); }
            case 3: {
                // OUTI, OUTD, OTIR, OTDR  f(4) f(5) r(3) o(4) + e(5)
                auto k = static_cast<block_out>(n);
                return self().on_block_out(k); }
            }
        }

        return self().on_ed_xnop(op);
    }

private:
    void do_decode(fast_u8 op) {
        fast_u8 y = get_y_part(op);
        fast_u8 z = get_z_part(op);
        fast_u8 p = get_p_part(op);

        // TODO: Collect some statistics and see if these
        //       switches come in a good order.
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
                return self().on_decode_halt();
            return self().on_decode_ld_r_r(rd, rs); }
        case 0200: {
            // alu[y] r[z]
            // alu r            f(4)                 (both i8080 and z80)
            // alu M            f(4)           r(3)
            // alu (HL)         f(4)           r(3)
            // alu (i+d)   f(4) f(4) r(3) e(5) r(3)
            auto k = static_cast<alu>(y);
            auto r = static_cast<reg>(z);
            return self().on_decode_alu_r(k, r); }
        }
        switch(op & (x_mask | z_mask)) {
        case 0004: {
            // INR/INC r[y]
            // INR r            f(5)
            // INR M            f(4)           r(3) w(3)
            // INC r            f(4)
            // INC (HL)         f(4)           r(4) w(3)
            // INC (i+d)   f(4) f(4) r(3) e(5) r(4) w(3)
            auto r = static_cast<reg>(y);
            return self().on_decode_inc_r(r); }
        case 0005: {
            // DCR/DEC r[y]
            // DCR r            f(5)
            // DCR M            f(4)           r(3) w(3)
            // DEC r            f(4)
            // DEC (HL)         f(4)           r(4) w(3)
            // DEC (i+d)   f(4) f(4) r(3) e(5) r(4) w(3)
            auto r = static_cast<reg>(y);
            return self().on_decode_dec_r(r); }
        case 0006: {
            // LD/MVI r[y], n
            // MVI r, n             f(4)      r(3)
            // LD r, n              f(4)      r(3)
            // LD (HL), n           f(4)      r(3) w(3)
            // LD (i+d), n     f(4) f(4) r(3) r(5) w(3)
            auto r = static_cast<reg>(y);
            return self().on_decode_ld_r_n(r); }
        case 0300: {
            // RET cc[y]/Rcc[y]  f(5) + r(3) r(3)
            self().on_fetch_cycle_extra_1t();
            auto cc = static_cast<condition>(y);
            return self().on_ret_cc(cc); }
        case 0302: {
            // Jcc[y] nn     f(4) r(3) r(3)
            // JP cc[y], nn  f(4) r(3) r(3)
            auto cc = static_cast<condition>(y);
            return self().on_jp_cc_nn(cc, self().on_imm16_read()); }
        case 0304: {
            // Ccc[y], nn
            // cc met:      f(5) r(3) r(3) w(3) w(3)
            // cc not met:  f(5) r(3) r(3)
            //
            // CALL cc[y], nn
            // cc met:      f(4) r(3) r(4) w(3) w(3)
            // cc not met:  f(4) r(3) r(3)
            auto cc = static_cast<condition>(y);
            return self().on_decode_call_cc_nn(cc); }
        case 0306: {
            // alu[y] n  f(4) r(3)  (both i8080 and z80)
            auto k = static_cast<alu>(y);
            return self().on_alu_n(k, self().on_imm8_read()); }
        case 0307:
            // RST y*8  f(5) w(3) w(3)
            self().on_fetch_cycle_extra_1t();
            return self().on_rst(y * 8);
        }
        switch(op & (x_mask | z_mask | q_mask)) {
        case 0001: {
            // LD/LXI rp[p], nn
            // LXI rp, nn       f(4) r(3) r(3)
            // LD rp, nn        f(4) r(3) r(3)
            // LD i, nn    f(4) f(4) r(3) r(3)
            auto rp = static_cast<regp>(p);
            return self().on_ld_rp_nn(rp, self().on_imm16_read()); }
        case 0011: {
            // ADD HL, rp[p] / DAD rp
            // DAD rp               f(4) e(3) e(3)
            // ADD HL, rp           f(4) e(4) e(3)
            // ADD i, rp       f(4) f(4) e(4) e(3)
            auto rp = static_cast<regp>(p);
            return self().on_add_irp_rp(rp); }
        case 0013: {
            // DEC/DCX rp[p]
            // DCX rp           f(5)
            // DEC rp           f(6)
            // DEC i       f(4) f(6)
            auto rp = static_cast<regp>(p);
            return self().on_decode_dec_rp(rp); }
        case 0003: {
            // INC/INX rp[p]
            // INX rp           f(5)
            // INC rp           f(6)
            // INC i       f(4) f(6)
            auto rp = static_cast<regp>(p);
            return self().on_decode_inc_rp(rp); }
        case 0301: {
            // POP rp2[p]
            // POP rr           f(4) r(3) r(3)
            // POP i       f(4) f(4) r(3) r(3)
            auto rp = static_cast<regp2>(p);
            return self().on_pop_rp(rp); }
        case 0305: {
            // PUSH rp2[p]
            // PUSH rr          f(5) w(3) w(3)
            // PUSH i      f(4) f(5) w(3) w(3)
            self().on_fetch_cycle_extra_1t();
            auto rp = static_cast<regp2>(p);
            return self().on_push_rp(rp); }
        }
        switch(op & (x_mask | z_mask | q_mask | (p_mask - 1))) {
        case 0002: {
            // STAX rp[p]     f(4) w(3)
            // LD (rp[p]), A  f(4) w(3)
            auto rp = static_cast<regp>(p);
            return self().on_ld_at_rp_a(rp); }
        case 0012: {
            // LDAX rp[p]     f(4) r(3)
            // LD A, (rp[p])  f(4) r(3)
            auto rp = static_cast<regp>(p);
            return self().on_ld_a_at_rp(rp); }
        }
        if((op & (x_mask | z_mask | (y_mask - 0030))) == 0040) {
            // JR cc[y-4], d  f(4) r(3) + e(5)
            return self().on_decode_jr_cc(op);
        }
        switch(op) {
        case 0x00:
            // NOP  f(4)
            return self().on_nop();
        case 0x07:
            // RLC   f(4)
            // RLCA  f(4)
            return self().on_rlca();
        case 0x08:
            // EX AF, AF'  f(4)
            return self().on_decode_ex_af_alt_af();
        case 0x0f:
            // RRC   f(4)
            // RRCA  f(4)
            return self().on_rrca();
        case 0x10:
            // DJNZ  f(5) r(3) + e(5)
            return self().on_decode_djnz();
        case 0x17:
            // RAL  f(4)
            // RLA  f(4)
            return self().on_rla();
        case 0x18:
            // JR d  f(4) r(3) e(5)
            return self().on_decode_jr();
        case 0x1f:
            // RAR  f(4)
            // RRA  f(4)
            return self().on_rra();
        case 0x22:
            // SHLD nn              f(4) r(3) r(3) w(3) w(3)
            // LD (nn), HL          f(4) r(3) r(3) w(3) w(3)
            // LD (nn), i      f(4) f(4) r(3) r(3) w(3) w(3)
            return self().on_ld_at_nn_irp(self().on_imm16_read());
        case 0x27:
            // DAA  f(4)  (both i8080 and z80)
            return self().on_daa();
        case 0x2a:
            // LHLD nn              f(4) r(3) r(3) r(3) r(3)
            // LD HL, (nn)          f(4) r(3) r(3) r(3) r(3)
            // LD i, (nn)      f(4) f(4) r(3) r(3) r(3) r(3)
            return self().on_ld_irp_at_nn(self().on_imm16_read());
        case 0x2f:
            // CMA  f(4)
            // CPL  f(4)
            return self().on_cpl();
        case 0x32:
            // STA nn      f(4) r(3) r(3) w(3)
            // LD (nn), A  f(4) r(3) r(3) w(3)
            return self().on_ld_at_nn_a(self().on_imm16_read());
        case 0x37:
            // STC  f(4)
            // SCF  f(4)
            return self().on_scf();
        case 0x3f:
            // CMC  f(4)
            // CCF  f(4)
            return self().on_ccf();
        case 0x3a:
            // LDA nn      f(4) r(3) r(3) r(3)
            // LD A, (nn)  f(4) r(3) r(3) r(3)
            return self().on_ld_a_at_nn(self().on_imm16_read());
        case 0xc3:
            // JMP nn  f(4) r(3) r(3)
            // JP nn   f(4) r(3) r(3)
            return self().on_jp_nn(self().on_imm16_read());
        case 0xc9:
            // RET  f(4) r(3) r(3)
            return self().on_ret();
        case 0xcb:
            // CB prefix   f(4)
            // XJMP nn     f(4) r(3) r(3)
            return self().on_decode_cb_prefix();
        case 0xcd: {
            // CALL nn  f(4) r(3) r(4) w(3) w(3)
            fast_u16 nn = self().on_imm16_read();
            self().on_read_cycle_extra_1t();
            return self().on_call_nn(nn); }
        case 0xd3:
            // OUT n       f(4) r(3) o(3)
            // OUT (n), A  f(4) r(3) o(4)
            return self().on_out_n_a(self().on_imm8_read());
        case 0xd9:
            // EXX   f(4)
            // XRET  f(4)
            return self().on_decode_exx();
        case 0xdb:
            // IN n       f(4) r(3) i(3)
            // IN A, (n)  f(4) r(3) i(4)
            return self().on_in_a_n(self().on_imm8_read());
        case 0xdd:
            // DD prefix (IX-indexed instructions)
            // DD        f(4)
            // XCALL nn  f(4) r(3) r(4) w(3) w(3)
            return self().on_decode_dd_prefix();
        case 0xe3:
            // EX (SP), irp / XHTL
            // XTHL                 f(4) r(3) r(3) w(3) w(5)
            // EX (SP), HL          f(4) r(3) r(4) w(3) w(5)
            // EX (SP), i      f(4) f(4) r(3) r(4) w(3) w(5)
            return self().on_ex_at_sp_irp();
        case 0xe9:
            // PCHL        f(5)
            // JP HL       f(4)
            // JP i   f(4) f(4)
            return self().on_decode_jp_irp();
        case 0xeb:
            // XCHG       f(5)
            // EX DE, HL  f(4)
            return self().on_decode_ex_de_hl();
        case 0xed:
            // ED prefix  f(4)
            // XCALL nn   f(4) r(3) r(4) w(3) w(3)
            return self().on_decode_ed_prefix();
        case 0xf3:
            // DI  f(4)
            return self().on_di();
        case 0xf9:
            // SPHL            f(5)
            // LD SP, HL       f(6)
            // LD SP, i   f(4) f(6)
            return self().on_decode_ld_sp_irp();
        case 0xfb:
            // EI  f(4)
            return self().on_ei();
        case 0xfd:
            // FD prefix (IY-indexed instructions)
            // FD        f(4)
            // XCALL nn  f(4) r(3) r(4) w(3) w(3)
            return self().on_decode_fd_prefix();
        }

        unreachable("Unknown opcode encountered!");
    }

public:
    void on_decode(fast_u8 op) {
        do_decode(op);

        // Reset current index register.
        if(self().on_is_z80() && op != 0xdd && op != 0xfd)
            self().on_set_iregp_kind(iregp::hl);
    }

    void on_fetch_and_decode() {
        self().on_decode(self().on_m1_fetch_cycle());
    }

protected:
    using base::self;

    static const fast_u8 x_mask = 0300;

    static const fast_u8 y_mask = 0070;
    fast_u8 get_y_part(fast_u8 op) { return (op & y_mask) >> 3; }

    static const fast_u8 z_mask = 0007;
    fast_u8 get_z_part(fast_u8 op) { return op & z_mask; }

    static const fast_u8 p_mask = 0060;
    fast_u8 get_p_part(fast_u8 op) { return (op & p_mask) >> 4; }

    static const fast_u8 q_mask = 0010;
};

template<typename B>
class i8080_decoder : public internals::decoder_base<B> {
public:
    static const bool z80_enabled = false;
    bool on_is_z80() { return false; }
};

template<typename B>
class z80_decoder : public internals::decoder_base<B> {
public:
    static const bool z80_enabled = true;
    bool on_is_z80() { return true; }
};

template<typename B>
class internals::disasm_base : public B {
protected:
    class output_buff;

public:
    typedef B base;

    disasm_base() {}

    fast_u8 on_fetch_cycle() {
        return self().on_read_next_byte(); }

    fast_u8 on_disp_read() { return self().on_read_next_byte(); }

    fast_u8 on_imm8_read() { return self().on_read_next_byte(); }

    fast_u16 on_imm16_read() {
        fast_u8 lo = self().on_read_next_byte();
        fast_u8 hi = self().on_read_next_byte();
        return make16(hi, lo); }

    void on_format(const char *fmt) {
        self().on_format_impl(fmt, /* args= */ nullptr);
    }

    template<typename... types>
    void on_format(const char *fmt, const types &... args) {
        const void *ptrs[] = { static_cast<const void*>(&args)... };
        self().on_format_impl(fmt, ptrs);
    }

    void on_format_char(char c, const void **&args, output_buff &out) {
        switch(c) {
        case 'A': {  // Register-operand i8080 ALU mnemonic or
                     // Z80 ALU mnemonic.
            auto k = get_arg<alu>(args);
            if(!self().on_is_z80()) {
                out.append(get_mnemonic_r(k));
            } else {
                out.append(get_mnemonic(k));
                if(is_two_operand_alu_instr(k))
                    out.append(" a,");
            }
            break; }
        case 'B': {  // Immediate-operand ALU mnemonic.
            auto k = get_arg<alu>(args);
            out.append(get_mnemonic_imm(k));
            break; }
        case 'C': {  // A condition operand.
            auto cc = get_arg<condition>(args);
            out.append(get_condition_name(cc));
            break; }
        case 'D': {  // A relative address.
            auto d = get_arg<int>(args);
            out.append("$ ");
            out.append_disp(d);
            break; }
        case 'G': {  // An alternative register pair.
            auto rp = get_arg<regp2>(args);
            auto irp = !self().on_is_z80() ? iregp::hl : get_arg<iregp>(args);
            out.append(get_reg_name(rp, irp));
            break; }
        case 'N': {  // An 8-bit immediate operand.
            auto n = get_arg<fast_u8>(args);
            out.append_u8(n);
            break; }
        case 'O': {  // Rotation mnemonic.
            auto k = get_arg<rot>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'P': {  // A register pair.
            auto rp = get_arg<regp>(args);
            auto irp = !self().on_is_z80() ? iregp::hl : get_arg<iregp>(args);
            out.append(get_reg_name(rp, irp));
            break; }
        case 'R': {  // A register.
            bool z80 = self().on_is_z80();
            auto r = get_arg<reg>(args);
            auto irp = !z80 ? iregp::hl : get_arg<iregp>(args);
            auto d = !z80 ? 0 : get_arg<fast_u8>(args);
            if(!z80 || r != reg::at_hl || irp == iregp::hl) {
                out.append(get_reg_name(r, irp));
            } else {
                out.append('(');
                out.append(get_reg_name(irp));
                out.append(' ');
                out.append_disp(sign_extend8(d));
                out.append(')');
            }
            break; }
        case 'W': {  // A 16-bit immediate operand.
            auto nn = get_arg<fast_u16>(args);
            out.append_u16(nn);
            break; }
        case 'L': {  // A block transfer instruction.
            auto k = get_arg<block_ld>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'M': {  // A block comparison instruction.
            auto k = get_arg<block_cp>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'I': {  // A block in instruction.
            auto k = get_arg<block_in>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'T': {  // A block out instruction.
            auto k = get_arg<block_out>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'U': {  // A decimal number.
            auto u = get_arg<unsigned>(args);
            out.append_u(u);
            break; }
        default:
            out.append(c);
        }
    }

    void on_format_impl(const char *fmt, const void *args[]) {
        output_buff out;
        for(const char *p = fmt; *p != '\0'; ++p)
            self().on_format_char(*p, args, out);
        out.append('\0');
        self().on_emit(out.get_buff());
    }

    void on_bit(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        if(irp == iregp::hl || r == reg::at_hl)
            self().on_format("bit U, R", b, r, irp, d);
        else
            self().on_format("bit R, U, R", r, iregp::hl, /* d= */ 0, b,
                             reg::at_hl, irp, d); }
    void on_res(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        if(irp == iregp::hl || r == reg::at_hl)
            self().on_format("res U, R", b, r, irp, d);
        else
            self().on_format("res R, U, R", r, iregp::hl, /* d= */ 0, b,
                             reg::at_hl, irp, d); }
    void on_set(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        if(irp == iregp::hl || r == reg::at_hl)
            self().on_format("set U, R", b, r, irp, d);
        else
            self().on_format("set R, U, R", r, iregp::hl, /* d= */ 0, b,
                             reg::at_hl, irp, d); }
    void on_call_nn(fast_u16 nn) {
        self().on_format("call W", nn); }
    void on_xcall_nn(fast_u8 op, fast_u16 nn) {
        self().on_format("xcall N, W", op, nn); }
    void on_ccf() {
        self().on_format(self().on_is_z80() ? "ccf" : "cmc"); }
    void on_scf() {
        self().on_format(self().on_is_z80() ? "scf" : "stc"); }

    // Interrupts.
    void on_di() {
        self().on_format("di"); }
    void on_ei() {
        self().on_format("ei"); }
    void on_halt() {
        self().on_format(self().on_is_z80() ? "halt" : "hlt"); }
    void on_im(unsigned mode) {
        self().on_format("im U", mode); }
    void on_xim(fast_u8 op, fast_u8 mode) {
        self().on_format("xim W, U", 0xed00 | op, mode); }

    // Arithmetic.
    void on_alu_r(alu k, reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            self().on_format("A R", k, r);
        } else {
            iregp irp = get_iregp_kind_or_hl(r);
            self().on_format("A R", k, r, irp, d); } }
    void on_alu_n(alu k, fast_u8 n) {
        self().on_format(self().on_is_z80() ? "A N" : "B N", k, n); }
    void on_daa() {
        self().on_format("daa"); }
    void on_cpl() {
        self().on_format(self().on_is_z80() ? "cpl" : "cma"); }
    void on_inc_r(reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            self().on_format("inr R", r);
        } else {
            iregp irp = get_iregp_kind_or_hl(r);
            self().on_format("inc R", r, irp, d); } }
    void on_dec_r(reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            self().on_format("dcr R", r);
        } else {
            iregp irp = get_iregp_kind_or_hl(r);
            self().on_format("dec R", r, irp, d); } }
    void on_inc_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_format("inx P", rp);
        } else {
            iregp irp = get_iregp_kind_or_hl(rp);
            self().on_format("inc P", rp, irp); } }
    void on_dec_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_format("dcx P", rp);
        } else {
            iregp irp = get_iregp_kind_or_hl(rp);
            self().on_format("dec P", rp, irp); } }
    void on_adc_hl_rp(regp rp) {
        self().on_format("adc hl, P", rp, iregp::hl); }
    void on_add_irp_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_format("dad P", rp);
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("add P, P", regp::hl, irp, rp, irp); } }

    // Jumps.
    void on_call_cc_nn(condition cc, fast_u16 nn) {
        self().on_format(self().on_is_z80() ? "call C, W" : "cC W", cc, nn); }
    void on_djnz(fast_u8 d) {
        self().on_format("djnz D", sign_extend8(d) + 2); }
    void on_jp_cc_nn(condition cc, fast_u16 nn) {
        self().on_format(self().on_is_z80() ? "jp C, W" : "jC W", cc, nn); }
    void on_jp_irp() {
        if(!self().on_is_z80()) {
            self().on_format("pchl");
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("jp (P)", regp::hl, irp); } }
    void on_jp_nn(fast_u16 nn) {
        self().on_format(self().on_is_z80() ? "jp W" : "jmp W", nn); }
    void on_xjp_nn(fast_u16 nn) {
        self().on_format("xjmp N, W", 0xcb, nn); }
    void on_jr(fast_u8 d) {
        self().on_format("jr D", sign_extend8(d) + 2); }
    void on_jr_cc(condition cc, fast_u8 d) {
        self().on_format("jr C, D", cc, sign_extend8(d) + 2); }
    void on_xretn(fast_u8 op) {
        self().on_format("xretn W", 0xed00 | op); }

    // Swaps.
    void on_ex_de_hl() {
        self().on_format(self().on_is_z80() ? "ex de, hl" : "xchg"); }
    void on_ex_af_alt_af() {
        self().on_format("ex af, af'"); }
    void on_ex_at_sp_irp() {
        if(!self().on_is_z80()) {
            self().on_format("xthl");
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("ex (sp), P", regp::hl, irp); } }
    void on_exx() {
        self().on_format("exx"); }

    // Transfers.
    void on_ld_a_r() {
        self().on_format("ld a, r"); }
    void on_ld_r_a() {
        self().on_format("ld r, a"); }
    void on_ld_a_i() {
        self().on_format("ld a, i"); }
    void on_ld_i_a() {
        self().on_format("ld i, a"); }
    void on_ld_r_r(reg rd, reg rs, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            self().on_format("mov R, R", rd, rs);
        } else {
            iregp irpd = get_iregp_kind_or_hl(rs != reg::at_hl &&
                                              is_indexable(rd));
            iregp irps = get_iregp_kind_or_hl(rd != reg::at_hl &&
                                              is_indexable(rs));
        self().on_format("ld R, R", rd, irpd, d, rs, irps, d); } }
    void on_ld_r_n(reg r, fast_u8 d, fast_u8 n) {
        if(!self().on_is_z80()) {
            self().on_format("mvi R, N", r, n);
        } else {
            iregp irp = get_iregp_kind_or_hl(r);
            self().on_format("ld R, N", r, irp, d, n); } }
    void on_ld_a_at_nn(fast_u16 nn) {
        self().on_format(self().on_is_z80() ? "ld a, (W)" : "lda W", nn); }
    void on_ld_at_nn_a(fast_u16 nn) {
        self().on_format(self().on_is_z80() ? "ld (W), a" : "sta W", nn); }
    void on_ld_irp_at_nn(fast_u16 nn) {
        if(!self().on_is_z80()) {
            self().on_format("lhld W", nn);
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("ld P, (W)", regp::hl, irp, nn); } }
    void on_ld_at_nn_irp(fast_u16 nn) {
        if(!self().on_is_z80()) {
            self().on_format("shld W", nn);
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("ld (W), P", nn, regp::hl, irp); } }
    void on_ld_a_at_rp(regp rp) {
        // TODO: Unify handling of the P specifier.
        if(!self().on_is_z80())
            self().on_format("ldax P", rp);
        else
            self().on_format("ld a, (P)", rp, iregp::hl); }
    void on_ld_at_rp_a(regp rp) {
        // TODO: Unify handling of the P specifier.
        if(!self().on_is_z80())
            self().on_format("stax P", rp);
        else
            self().on_format("ld (P), a", rp, iregp::hl); }
    void on_ld_rp_at_nn(regp rp, fast_u16 nn) {
        // The HL case repeats the unprefixed LD HL, (nn)
        // instruction, so we have to represent it as an
        // x-instruction.
        if(rp == regp::hl)
            self().on_format("xld W, P, (W)", 0xed6b, rp, iregp::hl, nn);
        else
            self().on_format("ld P, (W)", rp, iregp::hl, nn); }
    void on_ld_at_nn_rp(fast_u16 nn, regp rp) {
        // The HL case repeats the unprefixed LD (nn), HL
        // instruction, so we have to represent it as an
        // x-instruction.
        if(rp == regp::hl)
            self().on_format("xld W, (W), P", 0xed63, nn, rp, iregp::hl);
        else
            self().on_format("ld (W), P", nn, rp, iregp::hl); }
    void on_ld_sp_irp() {
        if(!self().on_is_z80()) {
            self().on_format("sphl");
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_format("ld sp, P", regp::hl, irp); } }
    void on_ld_rp_nn(regp rp, fast_u16 nn) {
        if(!self().on_is_z80()) {
            self().on_format("lxi P, W", rp, nn);
        } else {
            iregp irp = get_iregp_kind_or_hl(rp);
            self().on_format("ld P, W", rp, irp, nn); } }

    void on_nop() {
        self().on_format("nop"); }
    void on_xnop(fast_u8 op) {
        self().on_format("xnop N", op); }
    void on_ed_xnop(fast_u8 op) {
        self().on_format("xnop W", 0xed00 | op); }
    void on_neg() {
        self().on_format("neg"); }
    void on_xneg(fast_u8 op) {
        self().on_format("xneg W", 0xed00 | op); }
    void on_in_a_n(fast_u8 n) {
        self().on_format(self().on_is_z80() ? "in a, (N)" : "in N", n); }
    void on_in_r_c(reg r) {
        if(r == reg::at_hl)
            self().on_format("in (c)");
        else
            self().on_format("in R, (c)", r, iregp::hl, 0); }
    void on_out_c_r(reg r) {
        if(r == reg::at_hl)
            self().on_format("out (c), N", self().on_get_out_c_r_op());
        else
            self().on_format("out (c), R", r, iregp::hl, 0); }
    void on_out_n_a(fast_u8 n) {
        self().on_format(self().on_is_z80() ? "out (N), a" : "out N", n); }
    void on_push_rp(regp2 rp) {
        // TODO: Unify handling the G specifier.
        if(!self().on_is_z80()) {
            self().on_format("push G", rp);
        } else {
            iregp irp = get_iregp_kind_or_hl(rp);
            self().on_format("push G", rp, irp); } }
    void on_pop_rp(regp2 rp) {
        // TODO: Unify handling the G specifier.
        if(!self().on_is_z80()) {
            self().on_format("pop G", rp);
        } else {
            iregp irp = get_iregp_kind_or_hl(rp);
            self().on_format("pop G", rp, irp); } }
    void on_ret() {
        self().on_format("ret"); }
    void on_xret() {
        self().on_format("xret N", 0xd9); }
    void on_ret_cc(condition cc) {
        self().on_format(self().on_is_z80() ? "ret C" : "rC", cc); }
    void on_reti() {
        self().on_format("reti"); }
    void on_retn() {
        self().on_format("retn"); }
    void on_rst(fast_u16 nn) {
        self().on_format("rst W", nn); }
    void on_rot(rot k, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        if(irp == iregp::hl || r == reg::at_hl)
            self().on_format("O R", k, r, irp, d);
        else
            self().on_format("O R, R", k, reg::at_hl, irp, d,
                               r, iregp::hl, /* d= */ 0); }
    void on_rla() {
        self().on_format(self().on_is_z80() ? "rla" : "ral"); }
    void on_rlca() {
        self().on_format(self().on_is_z80() ? "rlca" : "rlc"); }
    void on_rld() {
        self().on_format("rld"); }
    void on_rra() {
        self().on_format(self().on_is_z80() ? "rra" : "rar"); }
    void on_rrca() {
        self().on_format(self().on_is_z80() ? "rrca" : "rrc"); }
    void on_rrd() {
        self().on_format("rrd"); }
    void on_sbc_hl_rp(regp rp) {
        self().on_format("sbc hl, P", rp, iregp::hl); }

    // Block operations.
    void on_block_cp(block_cp k) {
        self().on_format("M", k); }
    void on_block_ld(block_ld k) {
        self().on_format("L", k); }
    void on_block_in(block_in k) {
        self().on_format("I", k); }
    void on_block_out(block_out k) {
        self().on_format("T", k); }

    static const char *get_condition_name(condition cc) {
        switch(cc) {
        case condition::nz: return "nz";
        case condition::z: return "z";
        case condition::nc: return "nc";
        case condition::c: return "c";
        case condition::po: return "po";
        case condition::pe: return "pe";
        case condition::p: return "p";
        case condition::m: return "m";
        }
        unreachable("Unknown condition code.");
    }

    void on_disassemble() { self().on_fetch_and_decode(); }

protected:
    using base::self;

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

    static bool is_indexable(reg r) {
        return r == reg::at_hl || r == reg::h || r == reg::l;
    }

    iregp get_iregp_kind_or_hl(bool indexable) {
        return indexable ? self().on_get_iregp_kind() : iregp::hl;
    }

    iregp get_iregp_kind_or_hl(reg r) {
        return get_iregp_kind_or_hl(is_indexable(r));
    }

    iregp get_iregp_kind_or_hl(regp rp) {
        return get_iregp_kind_or_hl(rp == regp::hl);
    }

    iregp get_iregp_kind_or_hl(regp2 rp) {
        return get_iregp_kind_or_hl(rp == regp2::hl);
    }

public:
    const char *get_reg_name(reg r, iregp irp = iregp::hl) {
        bool z80 = self().on_is_z80();
        switch(r) {
        case reg::b: return "b";
        case reg::c: return "c";
        case reg::d: return "d";
        case reg::e: return "e";
        case reg::a: return "a";
        case reg::h:
            if(!z80)
                return "h";
            switch(irp) {
            case iregp::hl: return "h";
            case iregp::ix: return "ixh";
            case iregp::iy: return "iyh";
            }
            break;
        case reg::l:
            if(!z80)
                return "l";
            switch(irp) {
            case iregp::hl: return "l";
            case iregp::ix: return "ixl";
            case iregp::iy: return "iyl";
            }
            break;
        case reg::at_hl:
            if(!z80)
                return "m";
            switch(irp) {
            case iregp::hl: return "(hl)";
            case iregp::ix: return "(ix)";
            case iregp::iy: return "(iy)";
            }
            break;
        }
        unreachable("Unknown register.");
    }

    const char *get_reg_name(regp rp, iregp irp = iregp::hl) {
        bool z80 = self().on_is_z80();
        switch(rp) {
        case regp::bc: return z80 ? "bc" : "b";
        case regp::de: return z80 ? "de" : "d";
        case regp::hl: return z80 ? get_reg_name(irp) : "h";
        case regp::sp: return "sp";
        }
        unreachable("Unknown register.");
    }

    const char *get_reg_name(regp2 rp, iregp irp = iregp::hl) {
        bool z80 = self().on_is_z80();
        switch(rp) {
        case regp2::bc: return z80 ? "bc" : "b";
        case regp2::de: return z80 ? "de" : "d";
        case regp2::hl: return z80 ? get_reg_name(irp) : "h";
        case regp2::af: return z80 ? "af" : "psw";
        }
        unreachable("Unknown register.");
    }

    static const char *get_reg_name(iregp irp) {
        switch(irp) {
        case iregp::hl: return "hl";
        case iregp::ix: return "ix";
        case iregp::iy: return "iy";
        }
        unreachable("Unknown register.");
    }

protected:
    static bool is_two_operand_alu_instr(alu k) {
        return k == alu::add || k == alu::adc || k == alu::sbc;
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

    static const char *get_mnemonic_imm(alu k) {
        switch(k) {
        case alu::add: return "adi";
        case alu::adc: return "aci";
        case alu::sub: return "sui";
        case alu::sbc: return "sbi";
        case alu::and_a: return "ani";
        case alu::xor_a: return "xri";
        case alu::or_a: return "ori";
        case alu::cp: return "cpi";
        }
        unreachable("Unknown ALU operation.");
    }

    static const char *get_mnemonic(rot k) {
        switch(k) {
        case rot::rlc: return "rlc";
        case rot::rrc: return "rrc";
        case rot::rl: return "rl";
        case rot::rr: return "rr";
        case rot::sla: return "sla";
        case rot::sra: return "sra";
        case rot::sll: return "sll";
        case rot::srl: return "srl";
        }
        unreachable("Unknown rotation operation.");
    }

    static const char *get_mnemonic(block_ld k) {
        switch(k) {
        case block_ld::ldi: return "ldi";
        case block_ld::ldd: return "ldd";
        case block_ld::ldir: return "ldir";
        case block_ld::lddr: return "lddr";
        }
        unreachable("Unknown block load operation.");
    }

    static const char *get_mnemonic(block_cp k) {
        switch(k) {
        case block_cp::cpi: return "cpi";
        case block_cp::cpd: return "cpd";
        case block_cp::cpir: return "cpir";
        case block_cp::cpdr: return "cpdr";
        }
        unreachable("Unknown block compare operation.");
    }

    static const char *get_mnemonic(block_in k) {
        switch(k) {
        case block_in::ini: return "ini";
        case block_in::ind: return "ind";
        case block_in::inir: return "inir";
        case block_in::indr: return "indr";
        }
        unreachable("Unknown block input operation.");
    }

    static const char *get_mnemonic(block_out k) {
        switch(k) {
        case block_out::outi: return "outi";
        case block_out::outd: return "outd";
        case block_out::otir: return "otir";
        case block_out::otdr: return "otdr";
        }
        unreachable("Unknown block output operation.");
    }
};

// TODO: Split to a instructions verbalizer and a disassembler.
template<typename D>
class i8080_disasm : public internals::disasm_base<i8080_decoder<root<D>>>
{};

template<typename D>
class z80_disasm
    : public internals::disasm_base<z80_decoder<z80_decoder_state<root<D>>>>
{};

// Provides access to the value of a 16-bit register. Supposed to
// be as efficient as possible.
class reg16_value {
public:
    reg16_value() {}
    reg16_value(fast_u16 v) : v(v) {}
    reg16_value(const reg16_value &other) = default;
    reg16_value &operator = (const reg16_value &other) = default;

    fast_u16 get() const { return v; }
    void set(fast_u16 n) { v = n; }

    void swap(fast_u16 &n) { std::swap(v, n); }
    void swap(reg16_value &other) { swap(other.v); }

protected:
    // TODO: Would representing it as a pair of fast_u8 halves be
    // generally more efficient?
    fast_u16 v = 0;
};

// Provides interface to a register pair.
class regp_value : public reg16_value {
public:
    regp_value() {}
    regp_value(fast_u16 v) : reg16_value(v) {}
    regp_value(const regp_value &other) = default;
    regp_value &operator = (const regp_value &other) = default;

    fast_u8 get_low() const { return get_low8(v); }
    void set_low(fast_u8 n) { v = make16(get_high(), n); }

    fast_u8 get_high() const { return get_high8(v); }
    void set_high(fast_u8 n) { v = make16(n, get_low()); }
};

// The interface to a state's flip-flop.
class flipflop {
public:
    flipflop() {}
    flipflop(const flipflop &other) = default;
    flipflop &operator = (const flipflop &other) = default;

    bool get() const { return v; }
    void set(bool n) { v = n; }

private:
    bool v = false;
};

template<typename B>
class internals::cpu_state_base : public B {
public:
    typedef B base;

    fast_u8 get_b() const { return fields.ref(regp2::bc).get_high(); }
    void set_b(fast_u8 n) { fields.ref(regp2::bc).set_high(n); }

    fast_u8 get_c() const { return fields.ref(regp2::bc).get_low(); }
    void set_c(fast_u8 n) { fields.ref(regp2::bc).set_low(n); }

    fast_u8 get_d() const { return fields.ref(regp2::de).get_high(); }
    void set_d(fast_u8 n) { fields.ref(regp2::de).set_high(n); }

    fast_u8 get_e() const { return fields.ref(regp2::de).get_low(); }
    void set_e(fast_u8 n) { fields.ref(regp2::de).set_low(n); }

    fast_u8 get_h() const { return fields.ref(regp2::hl).get_high(); }
    void set_h(fast_u8 n) { fields.ref(regp2::hl).set_high(n); }

    fast_u8 get_l() const { return fields.ref(regp2::hl).get_low(); }
    void set_l(fast_u8 n) { fields.ref(regp2::hl).set_low(n); }

    fast_u8 get_a() const { return fields.ref(regp2::af).get_high(); }
    void set_a(fast_u8 n) { fields.ref(regp2::af).set_high(n); }

    fast_u8 get_f() const { return fields.ref(regp2::af).get_low(); }
    void set_f(fast_u8 n) { fields.ref(regp2::af).set_low(n); }

    fast_u8 get_reg(reg r) {
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

    fast_u16 get_bc() const { return fields.ref(regp2::bc).get(); }
    void set_bc(fast_u16 n) { fields.ref(regp2::bc).set(n); }

    fast_u16 get_de() const { return fields.ref(regp2::de).get(); }
    void set_de(fast_u16 n) { fields.ref(regp2::de).set(n); }

    fast_u16 get_hl() const { return fields.ref(regp2::hl).get(); }
    void set_hl(fast_u16 n) { fields.ref(regp2::hl).set(n); }

    fast_u16 get_af() const { return fields.ref(regp2::af).get(); }
    void set_af(fast_u16 n) { fields.ref(regp2::af).set(n); }

    fast_u16 get_pc() const { return fields.pc.get(); }
    void set_pc(fast_u16 n) { fields.pc.set(n); }

    fast_u16 get_sp() const { return fields.ref(regp::sp).get(); }
    void set_sp(fast_u16 n) { fields.ref(regp::sp).set(n); }

    bool is_int_disabled() const {
        return fields.int_disabled.get(); }
    void set_is_int_disabled(bool disabled) {
        fields.int_disabled.set(disabled); }

    bool is_halted() const { return fields.halted.get(); }
    void set_is_halted(bool is_halted) { fields.halted.set(is_halted); }

    void on_ex_de_hl_regs() {
        fields.ref(regp2::de).swap(fields.ref(regp2::hl)); }

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

    fast_u16 on_get_pc() const { return get_pc(); }
    void on_set_pc(fast_u16 n) { set_pc(n); }

    fast_u16 on_get_sp() { return get_sp(); }
    void on_set_sp(fast_u16 n) { set_sp(n); }

    // Returns true if generic register accessors like
    // on_get_regp2() are required to operate via specific
    // register accessors like on_get_hl(). Overriding this to
    // return false allows such generic accessors to avoid
    // calling register-specific handlers, thus potentially
    // increasing performance.
    bool on_dispatch_register_accesses() { return true; }

    using base::on_get_regp;
    fast_u16 on_get_regp(regp rp) {
        return self().on_dispatch_register_accesses() ?
            base::on_get_regp(rp) : fields.ref(rp).get(); }

    using base::on_set_regp;
    void on_set_regp(regp rp, fast_u16 nn) {
        return self().on_dispatch_register_accesses() ?
            base::on_set_regp(rp, nn) : fields.ref(rp).set(nn); }

    using base::on_get_regp2;
    fast_u16 on_get_regp2(regp2 rp) {
        return self().on_dispatch_register_accesses() ?
            base::on_get_regp2(rp) : fields.ref(rp).get(); }

    using base::on_set_regp2;
    void on_set_regp2(regp2 rp, fast_u16 nn) {
        return self().on_dispatch_register_accesses() ?
            base::on_set_regp2(rp, nn) : fields.ref(rp).set(nn); }

    bool on_is_int_disabled() const { return is_int_disabled(); }
    void on_set_is_int_disabled(bool f) { set_is_int_disabled(f); }

    bool on_is_halted() const { return is_halted(); }
    void on_set_is_halted(bool f) { set_is_halted(f); }

    void on_reset_cpu(bool soft = false) {
        base::on_reset_cpu(soft);

        state_fields power_on_state;
        if(!soft) {
            fields = power_on_state;
        } else {
            fields.pc = power_on_state.pc;
            fields.int_disabled = power_on_state.int_disabled;
            fields.ref(regp::sp) = power_on_state.ref(regp::sp);
            fields.ref(regp2::af) = power_on_state.ref(regp2::af);
            fields.halted = power_on_state.halted;
        }
    }

protected:
    using base::self;

private:

    // Most frequently used registers shall come first to reduce cache stress.
    struct state_fields {
        regp_value &ref(regp rp) {
            return rps[rp == regp::sp ? 4 : static_cast<unsigned>(rp)]; }
        const regp_value &ref(regp rp) const {
            return rps[rp == regp::sp ? 4 : static_cast<unsigned>(rp)]; }

        regp_value &ref(regp2 rp) {
            return rps[static_cast<unsigned>(rp)]; }
        const regp_value &ref(regp2 rp) const {
            return rps[static_cast<unsigned>(rp)]; }

        reg16_value pc;
        flipflop int_disabled;
        regp_value rps[5] = {{0}, {0}, {0},
                             {0xffff}, {0xffff}};  // bc, de, hl, af, sp
        flipflop halted;
    };

    state_fields fields;

    template<typename XB> friend class i8080_state;
    template<typename XB> friend class z80_state;
};

template<typename B>
class i8080_state : public internals::cpu_state_base<B> {
public:
    typedef internals::cpu_state_base<B> base;

    bool get_iff() const { return fields.iff.get(); }
    void set_iff(bool f) { fields.iff.set(f); }

    void on_reset_cpu(bool soft = false) {
        base::on_reset_cpu(soft);
        fields = state_fields();
    }

private:
    struct state_fields {
        flipflop iff;
    };

    state_fields fields;
};

class int_mode {
public:
    int_mode() {}
    int_mode(const int_mode &other) = default;
    int_mode &operator = (const int_mode &other) = default;

    unsigned get() const { return v; }

    void set(unsigned n) {
        assert(n <= 2);
        v = n;
    }

private:
    unsigned v = 0;
};

template<typename B>
class z80_state : public internals::cpu_state_base<z80_decoder_state<B>> {
public:
    typedef internals::cpu_state_base<z80_decoder_state<B>> base;

    z80_state() {}

    fast_u8 get_ixh() const { return fields.ix.get_high(); }
    void set_ixh(fast_u8 n) { fields.ix.set_high(n); }

    fast_u8 get_ixl() const { return fields.ix.get_low(); }
    void set_ixl(fast_u8 n) { fields.ix.set_low(n); }

    fast_u8 get_iyh() const { return fields.iy.get_high(); }
    void set_iyh(fast_u8 n) { fields.iy.set_high(n); }

    fast_u8 get_iyl() const { return fields.iy.get_low(); }
    void set_iyl(fast_u8 n) { fields.iy.set_low(n); }

    fast_u8 get_i() const { return fields.ir.get_high(); }
    void set_i(fast_u8 n) { fields.ir.set_high(n); }

    fast_u8 get_r() const { return fields.ir.get_low(); }
    void set_r(fast_u8 n) { fields.ir.set_low(n); }

    fast_u16 get_alt_af() const { return fields.alt_af.get(); }
    void set_alt_af(fast_u16 n) { fields.alt_af.set(n); }

    fast_u16 get_alt_hl() const { return fields.alt_hl.get(); }
    void set_alt_hl(fast_u16 n) { fields.alt_hl.set(n); }

    fast_u16 get_alt_bc() const { return fields.alt_bc.get(); }
    void set_alt_bc(fast_u16 n) { fields.alt_bc.set(n); }

    fast_u16 get_alt_de() const { return fields.alt_de.get(); }
    void set_alt_de(fast_u16 n) { fields.alt_de.set(n); }

    fast_u16 get_ix() const { return fields.ix.get(); }
    void set_ix(fast_u16 n) { fields.ix.set(n); }

    fast_u16 get_iy() const { return fields.iy.get(); }
    void set_iy(fast_u16 n) { fields.iy.set(n); }

    fast_u16 get_ir() const { return fields.ir.get(); }
    void set_ir(fast_u16 n) { fields.ir.set(n); }

    fast_u16 on_get_wz() const { return get_wz(); }
    void on_set_wz(fast_u16 n) { set_wz(n); }

    bool get_iff1() const { return fields.iff1.get(); }
    void set_iff1(bool f) { fields.iff1.set(f); }

    bool get_iff2() const { return fields.iff2.get(); }
    void set_iff2(bool f) { fields.iff2.set(f); }

    unsigned get_int_mode() const { return fields.im.get(); }
    void set_int_mode(unsigned mode) { fields.im.set(mode); }

    fast_u16 get_index_rp(iregp irp) {
        switch(irp) {
        case iregp::hl: return base::get_hl();
        case iregp::ix: return get_ix();
        case iregp::iy: return get_iy();
        }
        unreachable("Unknown index register.");
    }

    void ex_af_alt_af_regs() {
        base::fields.ref(regp2::af).swap(fields.alt_af);
    }

    void exx_regs() {
        base::fields.ref(regp2::bc).swap(fields.alt_bc);
        base::fields.ref(regp2::de).swap(fields.alt_de);
        base::fields.ref(regp2::hl).swap(fields.alt_hl);
    }

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

    fast_u8 on_get_r() const { return get_r(); }
    void on_set_r(fast_u8 r) { set_r(r); }

    // TODO: on_get_i() + on_get_r() ?
    fast_u16 on_get_ir() const { return get_ir(); }

    fast_u16 get_wz() const { return fields.wz.get(); }
    void set_wz(fast_u16 n) { fields.wz.set(n); }

    bool on_get_iff1() const { return get_iff1(); }
    void on_set_iff1(bool f) { set_iff1(f); }

    bool on_get_iff2() const { return get_iff2(); }
    void on_set_iff2(bool f) { set_iff2(f); }

    unsigned on_get_int_mode() const { return get_int_mode(); }
    void on_set_int_mode(unsigned mode) { set_int_mode(mode); }

    void on_ex_af_alt_af_regs() { ex_af_alt_af_regs(); }
    void on_exx_regs() { exx_regs(); }

    void on_reset_cpu(bool soft = false) {
        base::on_reset_cpu(soft);

        state_fields power_on_state;
        if(!soft) {
            fields = power_on_state;
        } else {
            fields.ir = power_on_state.ir;
            fields.iff1 = power_on_state.iff1;
            fields.iff2 = power_on_state.iff2;
            fields.im = power_on_state.im;
        }
    }

private:
    struct state_fields {
        regp_value ix, iy, ir;
        reg16_value wz;
        reg16_value alt_bc, alt_de, alt_hl, alt_af;
        flipflop iff1, iff2;
        int_mode im;
    };

    state_fields fields;
};

template<typename B>
class generic_cpu_state : public z80_state<B> {
    using base = z80_state<B>;

public:
    bool get_iff() { return base::get_iff1(); }
    void set_iff(bool f) { base::set_iff1(f); }

    bool on_get_iff() { return get_iff(); }
    void on_set_iff(bool f) { set_iff(f); }
};

template<typename B>
class internals::executor_base : public B {
public:
    typedef B base;

    executor_base() {}

    // TODO: Eliminate?
    fast_u8 on_get_m() {
        return self().on_read_cycle(self().on_get_hl()); }
    void on_set_m(fast_u8 n) {
        self().on_write_cycle(self().on_get_hl(), n); }

    fast_u8 on_get_reg(reg r) {
        return r == reg::at_hl ? self().on_get_m() :
                                 base::on_get_reg(r);
    }

    void on_set_reg(reg r, fast_u8 n) {
        return r == reg::at_hl ? self().on_set_m(n) :
                                 base::on_set_reg(r, n);
    }

    fast_u16 get_disp_target(fast_u16 base, fast_u8 d) {
        return !get_sign8(d) ? add16(base, d) : sub16(base, neg8(d));
    }

    fast_u8 read_at_disp(fast_u8 d, bool long_read_cycle = false) {
        iregp irp = self().on_get_iregp_kind();
        fast_u16 addr = get_disp_target(self().on_get_iregp(irp), d);
        fast_u8 res = self().on_read_cycle(addr);
        if(long_read_cycle)  // TODO: Remove. Do extra ticks manually.
            self().on_read_cycle_extra_1t();
        if(irp != iregp::hl)
            self().on_set_wz(addr);
        return res;
    }

    void write_at_disp(fast_u8 d, fast_u8 n) {
        iregp irp = self().on_get_iregp_kind();
        fast_u16 addr = get_disp_target(self().on_get_iregp(irp), d);
        self().on_write_cycle(addr, n);
        if(irp != iregp::hl)
            self().on_set_wz(addr);
    }

    fast_u8 on_get_reg(reg r, iregp irp, fast_u8 d = 0,
                       bool long_read_cycle = false) {
        return r == reg::at_hl ? read_at_disp(d, long_read_cycle) :
                                 base::on_get_reg(r, irp);
    }

    void on_set_reg(reg r, iregp irp, fast_u8 d, fast_u8 n) {
        return r == reg::at_hl ? write_at_disp(d, n) :
                                 base::on_set_reg(r, irp, n);
    }

    fast_u8 on_fetch_cycle() {
        fast_u16 addr = self().get_pc_on_fetch();
        self().on_set_addr_bus(addr);
        fast_u8 n = self().on_read(addr);
        self().on_tick(2);
        self().on_mreq_wait(addr);
        if(self().on_is_z80())
            self().on_set_addr_bus(self().get_ir_on_refresh());
        self().on_tick(2);
        if (self().on_is_halted())
            return 0; // nop
        self().set_pc_on_fetch(inc16(addr));
        return n; }

    void on_inc_r_reg() {
        fast_u8 r = self().on_get_r();
        self().on_set_r((r & 0x80) | ((r + 1) & 0x7f)); }

    fast_u8 on_m1_fetch_cycle() {
        fast_u8 n = self().on_fetch_cycle();
        if(self().on_is_z80())
            self().on_inc_r_reg();
        return n; }

    fast_u8 on_disp_read_cycle(fast_u16 addr) {
        assert(self().on_is_z80());
        return self().on_read_cycle(addr); }

    fast_u8 on_disp_read() {
        fast_u16 pc = self().get_pc_on_disp_read();
        fast_u8 op = self().on_disp_read_cycle(pc);
        self().set_pc_on_disp_read(inc16(pc));
        return op;
    }

    fast_u16 get_pc_on_fetch() { return self().on_get_pc(); }
    void set_pc_on_fetch(fast_u16 pc) { self().on_set_pc(pc); }

    fast_u16 get_pc_on_jump() { return self().on_get_pc(); }
    void set_pc_on_jump(fast_u16 pc) { self().on_set_pc(pc); }

    fast_u16 get_pc_on_imm8_read() { return self().on_get_pc(); }
    void set_pc_on_imm8_read(fast_u16 pc) { self().on_set_pc(pc); }

    fast_u16 get_pc_on_imm16_read() { return self().on_get_pc(); }
    void set_pc_on_imm16_read(fast_u16 pc) { self().on_set_pc(pc); }

    void set_pc_on_call(fast_u16 pc) { self().on_set_pc(pc); }
    void set_pc_on_return(fast_u16 pc) { self().on_set_pc(pc); }

    fast_u16 get_ir_on_refresh() { return self().on_get_ir(); }

    void disable_int_on_ei() { self().on_set_is_int_disabled(true); }

    fast_u8 on_imm8_read() {
        fast_u16 pc = self().get_pc_on_imm8_read();
        fast_u8 op = self().on_read_cycle(pc);
        self().set_pc_on_imm8_read(inc16(pc));
        return op; }
    fast_u16 on_imm16_read() {
        fast_u16 pc = self().get_pc_on_imm16_read();
        fast_u8 lo = self().on_read_cycle(pc);
        pc = inc16(pc);
        fast_u8 hi = self().on_read_cycle(pc);
        self().set_pc_on_imm16_read(inc16(pc));
        return make16(hi, lo); }

    fast_u8 on_input_cycle(fast_u16 port) {
        // Z80 samples the value at t4 of the input cycle, see
        // <http://ramsoft.bbk.org.omegahg.com/floatingbus.html>.
        bool z80 = self().on_is_z80();
        if(z80) {
            // TODO: Should we do the same for i8080?
            self().on_set_addr_bus(port);
        }
        self().on_tick(z80 ? 3 : 2);
        self().on_iorq_wait(port);
        self().on_tick(1);
        return self().on_input(port); }
    void on_output_cycle(fast_u16 port, fast_u8 n) {
        bool z80 = self().on_is_z80();
        if(z80) {
            // TODO: Should we do the same for i8080?
            self().on_set_addr_bus(port);
        }
        self().on_tick(z80 ? 3 : 2);
        self().on_iorq_wait(port);
        self().on_tick(1);
        self().on_output(port, n); }

    void on_set_addr_bus(fast_u16 addr) {
        unused(addr); }

protected:
    static fast_u8 cf(fast_u8 f) {
        return f & cf_mask; }
    static fast_u8 sf(fast_u8 f) {
        return f & sf_mask; }
    static fast_u8 zf(fast_u8 n) {
        return zf_ari(n); }
    static fast_u8 pf(fast_u8 n) {
        return pf_log(n); }
    static fast_u8 cf9(fast_u16 res) {
        return (res >> (8 - cf_bit)) & cf_mask; }
    static fast_u8 hf(fast_u8 n) {
        return n & hf_mask; }

    struct flag_set {
        bool is_lazy;
        fast_u8 raw = 0;
        fast_u16 lazy = 0;

        flag_set(bool is_lazy) : is_lazy(is_lazy) {}

        // TODO
        static fast_u8 eval(fast_u8 ops, fast_u16 res9) {
            fast_u8 res8 = mask8(res9);
            return sf(res8) | zf(res8) | hf(ops) | pf(res8) | cf9(res9);
        }

        fast_u8 get_cf() const {
            return (is_lazy ? (lazy >> 8) : raw) & 0x1;
        }

        void set_cf(fast_u8 v) {
            if(is_lazy)
                lazy = (lazy & ~0x100u) | ((v << 8) & 0x100);
            else
                raw = (raw & ~0x1u) | (v & 0x1);
        }

        fast_u8 get_hf_cf() const {
            return (is_lazy ? (lazy >> 8) : raw) & (hf_mask | cf_mask);
        }

        fast_u8 get_f() const {
            if(!is_lazy)
                return raw;

            fast_u8 ops = static_cast<fast_u8>(lazy >> 8);
            fast_u16 res9 = lazy;
            return eval(ops, res9) | ops;
        }

        void set(fast_u8 ops, fast_u16 res9) {
            if(is_lazy)
                lazy = (ops << 8) | res9;
            else
                raw = eval(ops, res9);
        }
    };

    bool on_is_to_use_lazy_flags() {
        return false;
    }

    // TODO: Move to the root module.
    fast_u16 on_get_flags() {
#if 0  // TODO
        return 0;
#else
        fast_u8 f = self().on_get_f();
        return (static_cast<fast_u16>(f) << 8) | 1;
#endif
    }

    // TODO: Move to the root module.
    void on_set_flags(fast_u16 flags) {
#if 0  // TODO
        unused(flags);
#else
        fast_u8 ops = static_cast<fast_u8>(flags >> 8);
        fast_u16 res9 = flags;
        self().on_set_f(flag_set::eval(ops, res9) | ops);
#endif
    }

    flag_set get_flags(bool is_lazy) {
        flag_set flags(is_lazy);
        if(flags.is_lazy)
            flags.lazy = self().on_get_flags();
        else
            flags.raw = self().on_get_f();
        return flags;
    }

    flag_set get_flags() {
        return get_flags(self().on_is_to_use_lazy_flags());
    }

    void set_flags(flag_set flags) {
        if(flags.is_lazy)
            self().on_set_flags(flags.lazy);
        else
            self().on_set_f(flags.raw);
    }

    bool check_condition(condition cc) {
        if(self().on_is_z80()) {
            // TODO: Would it make sense to store flags in a different order?
            auto n = static_cast<unsigned>(cc);
            unsigned flag_bits = (zf_bit << 0) | (zf_bit << 4) |
                                 (cf_bit << 8) | (cf_bit << 12) |
                                 (pf_bit << 16) | (pf_bit << 20) |
                                 (sf_bit << 24) | (sf_bit << 28);
            fast_u8 f = self().on_get_f();
            unsigned pos = (flag_bits >> (n * 4)) & 0xf;
            return !(((f >> pos) ^ n) & 0x1);
        }

        auto n = static_cast<unsigned>(cc);
        if(self().on_is_to_use_lazy_flags()) {
            fast_u16 flags = self().on_get_flags();
            fast_u8 res8 = get_low8(flags);
            switch(cc) {
            case condition::nz:
            case condition::z:
                return (((res8 == 0) | (flags >> (zf_bit + 8))) ^ n ^ 1) & 0x1;
            case condition::nc:
            case condition::c:
                return ((flags >> (cf_bit + 8)) ^ n ^ 1) & 0x1;
            case condition::po:
            case condition::pe:
                return ((pf_log(res8) | (flags >> (pf_bit + 8))) ^ n ^ 1) & 0x1;
            case condition::p:
            case condition::m:
                return (((flags >> (sf_bit + 8)) |
                         (flags >> sf_bit)) ^ n ^ 1) & 0x1;
            }
            unreachable("Unknown condition code.");
        }

        // TODO: Would it make sense to store flags in a different order?
        unsigned flag_bits = (zf_bit << 0) | (zf_bit << 4) |
                             (cf_bit << 8) | (cf_bit << 12) |
                             (pf_bit << 16) | (pf_bit << 20) |
                             (sf_bit << 24) | (sf_bit << 28);
        fast_u8 f = self().on_get_f();
        unsigned pos = (flag_bits >> (n * 4)) & 0xf;
        return !(((f >> pos) ^ n) & 0x1);
    }

    void do_cp(fast_u8 a, fast_u8 &f, fast_u8 n) {
        fast_u8 t = sub8(a, n);
        f = (t & sf_mask) | zf_ari(t) | (n & (yf_mask | xf_mask)) |
            hf_ari(t, a, n) | pf_ari(a - n, a, n) | cf_ari(t > a) | nf_mask;
    }

    void do_sub(fast_u8 &a, fast_u8 &f, fast_u8 n) {
        fast_u8 t = sub8(a, n);
        f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                hf_ari(t, a, n) | pf_ari(a - n, a, n) | cf_ari(t > a) | nf_mask;
        a = t;
    }

public:
    void on_bit(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        reg access_r = irp == iregp::hl ? r : reg::at_hl;
        fast_u8 v = self().on_get_reg(access_r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = self().on_get_f();
        fast_u8 m = v & (1u << b);
        // TODO: Is it always (m & sf_mask)? Regardless of whether m is zero or not?
        f = (f & cf_mask) | hf_mask | (m ? (m & sf_mask) : (zf_mask | pf_mask));
        if(irp != iregp::hl || access_r == reg::at_hl)
            v = get_high8(self().on_get_wz());
        f |= v & (xf_mask | yf_mask);
        self().on_set_f(f); }
    void on_res(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        reg access_r = irp == iregp::hl ? r : reg::at_hl;
        fast_u8 v = self().on_get_reg(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        v &= ~(1u << b);
        self().on_set_reg(access_r, irp, d, v);
        if(irp != iregp::hl && r != reg::at_hl)
            self().on_set_reg(r, iregp::hl, /* d= */ 0, v); }
    void on_set(unsigned b, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        reg access_r = irp == iregp::hl ? r : reg::at_hl;
        fast_u8 v = self().on_get_reg(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        v |= 1u << b;
        self().on_set_reg(access_r, irp, d, v);
        if(irp != iregp::hl && r != reg::at_hl)
            self().on_set_reg(r, iregp::hl, /* d= */ 0, v); }

    // Arithmetic.
private:
    void do_i8080_alu(alu k, fast_u8 n) {
        fast_u8 a = self().on_get_a();
        fast_u16 t;
        fast_u8 b;
        bool is_lazy = self().on_is_to_use_lazy_flags();
        if(((static_cast<unsigned>(k) + 1) & 0x7) < 5) {
            // ADD, ADC, SUB, SBC, CP
            fast_u8 cfv = (k == alu::adc || k == alu::sbc) ?
                get_flags(is_lazy).get_cf() : 0;

            if(k <= alu::adc) {
                t = a + n + cfv;
                b = a ^ n ^ static_cast<fast_u8>(t);
            } else {
                t = a - n - cfv;
                b = a ^ n ^ static_cast<fast_u8>(t) ^ hf_mask;
            }
        } else {
            // AND, XOR, OR
            if(k == alu::and_a) {
                // Alexander Demin notes that the half-carry flag has
                // its own special logic for the ANA and ANI
                // instructions.
                // http://demin.ws/blog/english/2012/12/24/my-i8080-collection/
                // TODO: AMD chips do not set the flag. Support them
                // as a variant of the original Intel chip.
                t = a & n;
                b = (a | n) << (hf_bit - 3);
            } else {
                t = (k == alu::xor_a) ? a ^ n : a | n;
                b = 0;
            }
        }
        if(k != alu::cp)
            self().on_set_a(mask8(t));
        flag_set flags(is_lazy);
        flags.set(b & hf_mask, t & 0x1ff);
        set_flags(flags);
    }

    void do_z80_alu(alu k, fast_u8 n) {
        fast_u8 a = self().on_get_a();
        fast_u8 f = 0;
        switch(k) {
        case alu::add: {
            fast_u8 t = add8(a, n);
            f = (t & (sf_mask | yf_mask | xf_mask)) | zf_ari(t) |
                    hf_ari(t, a, n) | pf_ari(a + n, a, n) | cf_ari(t < a);
            a = t;
            break; }
        case alu::adc: {
            f = self().on_get_f();
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
            f = self().on_get_f();
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
            self().on_set_a(a);
        self().on_set_f(f);
    }

public:
    void on_alu_r(alu k, reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            do_i8080_alu(k, self().on_get_reg(r));
        } else {
            iregp irp = self().on_get_iregp_kind();
            do_z80_alu(k, self().on_get_reg(r, irp, d)); } }

    void on_alu_n(alu k, fast_u8 n) {
        if(!self().on_is_z80())
            do_i8080_alu(k, n);
        else
            do_z80_alu(k, n); }

private:
    void do_i8080_daa() {
        fast_u8 a = self().on_get_a();
        flag_set flags = get_flags();
        fast_u8 f = flags.get_hf_cf();

        fast_u8 r = a;
        fast_u8 t = r + 6;
        fast_u8 hfv = a ^ t;
        if((hfv | f) & hf_mask)
            r = t;

        fast_u16 t2 = r + 0x60;
        fast_u16 w = ((t2 >> 8) | f) << 8;
        if(w & 0x100)
            r = mask8(t2);

        self().on_set_a(r);
        flags.set(hfv & hf_mask, (w | r) & 0x1ff);
        set_flags(flags); }
    void do_z80_daa() {
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
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

        self().on_set_a(a);
        self().on_set_f(f); }

public:
    void on_daa() {
        if(!self().on_is_z80())
            do_i8080_daa();
        else
            do_z80_daa(); }
    void on_cpl() {
        if(!self().on_is_z80()) {
            self().on_set_a(self().on_get_a() ^ 0xff);
        } else {
            fast_u8 a = self().on_get_a();
            fast_u8 f = self().on_get_f();
            a ^= 0xff;
            f = (f & (sf_mask | zf_mask | pf_mask | cf_mask)) |
                    (a & (yf_mask | xf_mask)) | hf_mask | nf_mask;
            self().on_set_a(a);
            self().on_set_f(f); } }
    void on_inc_r(reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            fast_u8 n = self().on_get_reg(r);
            flag_set flags = get_flags();
            fast_u8 t = mask8(n + 1);
            self().on_set_reg(r, t);
            flags.set((n ^ t) & hf_mask, (flags.get_cf() << 8) | t);
            set_flags(flags);
            return;
        }

        iregp irp = self().on_get_iregp_kind();
        fast_u8 v = self().on_get_reg(r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = self().on_get_f();
        v = inc8(v);
        f = (f & cf_mask) | (v & (sf_mask | yf_mask | xf_mask)) | zf_ari(v) |
                hf_inc(v) | pf_inc(v);
        self().on_set_reg(r, irp, d, v);
        self().on_set_f(f); }
    void on_dec_r(reg r, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            fast_u8 n = self().on_get_reg(r);
            flag_set flags = get_flags();
            fast_u8 t = mask8(n - 1);
            self().on_set_reg(r, t);
            flags.set((n ^ t ^ hf_mask) & hf_mask, (flags.get_cf() << 8) | t);
            set_flags(flags);
            return;
        }

        iregp irp = self().on_get_iregp_kind();
        fast_u8 v = self().on_get_reg(r, irp, d, /* long_read_cycle= */ true);
        fast_u8 f = self().on_get_f();
        v = dec8(v);
        f = (f & cf_mask) | (v & (sf_mask | yf_mask | xf_mask)) | zf_ari(v) |
                hf_dec(v) | pf_dec(v) | nf_mask;
        self().on_set_reg(r, irp, d, v);
        self().on_set_f(f); }
    void on_inc_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_set_regp(rp, inc16(self().on_get_regp(rp)));
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_regp(rp, irp, inc16(self().on_get_regp(rp, irp))); } }
    void on_dec_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_set_regp(rp, dec16(self().on_get_regp(rp)));
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_regp(rp, irp, dec16(self().on_get_regp(rp, irp))); } }
    void on_adc_hl_rp(regp rp) {
        fast_u16 hl = self().on_get_hl();
        fast_u16 n = self().on_get_regp(rp, iregp::hl);
        bool cf = self().on_get_f() & cf_mask;

        self().on_4t_exec_cycle();
        self().on_3t_exec_cycle();

        fast_u16 t = add16(n, cf);
        bool of = cf && t == 0;
        fast_u32 r32 = hl + t;
        fast_u16 r16 = mask16(r32);
        fast_u8 f = (get_high8(r16) & (sf_mask | yf_mask | xf_mask)) |
                        zf_ari(r16) | hf_ari(r16 >> 8, hl >> 8, n >> 8) |
                        (pf_ari(r32 >> 8, hl >> 8, n >> 8) ^
                             (of ? pf_mask : 0)) |
                        cf_ari(r16 < hl || of);

        self().on_set_wz(inc16(hl));
        self().on_set_hl(r16);
        self().on_set_f(f); }
    void on_add_irp_rp(regp rp) {
        if(!self().on_is_z80()) {
            self().on_3t_exec_cycle();
            self().on_3t_exec_cycle();

            fast_u16 i = self().on_get_hl();
            fast_u16 n = self().on_get_regp(rp);
            flag_set flags = get_flags();
            fast_u32 r32 = i + n;
            self().on_set_wz(inc16(i));
            self().on_set_hl(mask16(r32));
            // TODO: Should set_cf() take fast_u16?
            flags.set_cf(static_cast<fast_u8>(r32 >> 16));
            set_flags(flags);
            return;
        }

        iregp irp = self().on_get_iregp_kind();
        fast_u16 i = self().on_get_iregp(irp);
        fast_u16 n = self().on_get_regp(rp, irp);
        fast_u8 f = self().on_get_f();

        self().on_4t_exec_cycle();
        self().on_3t_exec_cycle();

        fast_u16 r = add16(i, n);
        f = (f & (sf_mask | zf_mask | pf_mask)) |
                (get_high8(r) & (yf_mask | xf_mask)) |
                hf_ari(r >> 8, i >> 8, n >> 8) | cf_ari(r < i);

        self().on_set_wz(inc16(i));
        self().on_set_iregp(irp, r);
        self().on_set_f(f); }

    // Swaps.
    void on_ex_de_hl() {
        self().on_ex_de_hl_regs(); }
    void on_ex_af_alt_af() {
        self().on_ex_af_alt_af_regs(); }
    void on_ex_at_sp_irp() {
        fast_u16 sp = self().on_get_sp();
        fast_u8 lo = self().on_read_cycle(sp);
        sp = inc16(sp);
        fast_u8 hi = self().on_read_cycle(sp);
        bool z80 = self().on_is_z80();
        if(z80)
            self().on_read_cycle_extra_1t();
        fast_u16 nn = make16(hi, lo);
        fast_u16 i;
        iregp irp;
        if(!z80) {
            irp = iregp::hl;
            i = self().on_get_hl();
        } else {
            irp = self().on_get_iregp_kind();
            i = self().on_get_iregp(irp);
        }
        std::swap(nn, i);
        self().on_write_cycle(sp, get_high8(nn));
        sp = dec16(sp);
        self().on_write_cycle(sp, get_low8(nn));
        self().on_write_cycle_extra_2t();
        self().on_set_wz(i);
        if(!z80)
            self().on_set_hl(i);
        else
            self().on_set_iregp(irp, i); }
    void on_exx() {
        self().on_exx_regs(); }

    // Transfers.
    void on_ld_a_r() {
        fast_u8 n = self().on_get_r();
        fast_u8 f = self().on_get_f();
        f = (f & cf_mask) | (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) |
                ((self().on_get_iff2() ? 1u : 0u) << pf_bit);
        self().on_set_a(n);
        self().on_set_f(f); }
    void on_ld_r_a() {
        self().on_set_r(self().on_get_a()); }
    void on_ld_a_i() {
        fast_u8 i = self().on_get_i();
        fast_u8 f = self().on_get_f();
        f = (f & cf_mask) | (i & (sf_mask | yf_mask | xf_mask)) |
                zf_ari(i) | (self().on_get_iff2() ? pf_mask : 0);
        self().on_set_a(i);
        self().on_set_f(f); }
    void on_ld_i_a() {
        self().set_i_on_ld(self().on_get_a()); }
    void on_ld_r_r(reg rd, reg rs, fast_u8 d = 0) {
        if(!self().on_is_z80()) {
            self().on_fetch_cycle_extra_1t();
            self().on_set_reg(rd, self().on_get_reg(rs));
        } else {
            iregp irp = self().on_get_iregp_kind();
            iregp irpd = rs == reg::at_hl ? iregp::hl : irp;
            iregp irps = rd == reg::at_hl ? iregp::hl : irp;
            self().on_set_reg(rd, irpd, d, self().on_get_reg(rs, irps, d)); } }
    void on_ld_r_n(reg r, fast_u8 d, fast_u8 n) {
        if(!self().on_is_z80()) {
            self().on_set_reg(r, n);
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_reg(r, irp, d, n); } }
    void on_ld_irp_at_nn(fast_u16 nn) {
        fast_u8 lo = self().on_read_cycle(nn);
        nn = inc16(nn);
        self().on_set_wz(nn);
        fast_u8 hi = self().on_read_cycle(nn);

        if(!self().on_is_z80()) {
            self().on_set_hl(make16(hi, lo));
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_iregp(irp, make16(hi, lo)); } }
    void on_ld_at_nn_irp(fast_u16 nn) {
        fast_u16 i;
        // TODO: Can we remove the condition? on_get_iregp_kind()
        // would just return HL for i8080. But then, what if a
        // generic state is used? Should then on_get_iregp_kind()
        // check for Z80 mode itself? +Same for similar cases.
        if(!self().on_is_z80()) {
            i = self().on_get_hl();
        } else {
            iregp irp = self().on_get_iregp_kind();
            i = self().on_get_iregp(irp);
        }

        self().on_write_cycle(nn, get_low8(i));
        nn = inc16(nn);
        self().on_set_wz(nn);
        self().on_write_cycle(nn, get_high8(i)); }
    void on_ld_at_rp_a(regp rp) {
        fast_u16 nn;
        if(!self().on_is_z80()) {
            nn = self().on_get_regp(rp);
        } else {
            iregp irp = self().on_get_iregp_kind();
            nn = self().on_get_regp(rp, irp);
        }
        fast_u8 a = self().on_get_a();
        self().on_set_wz(make16(a, get_low8(nn + 1)));
        self().on_write_cycle(nn, a); }
    void on_ld_a_at_rp(regp rp) {
        fast_u16 nn;
        if(!self().on_is_z80()) {
            nn = self().on_get_regp(rp);
        } else {
            iregp irp = self().on_get_iregp_kind();
            nn = self().on_get_regp(rp, irp);
        }
        self().on_set_wz(inc16(nn));
        self().on_set_a(self().on_read_cycle(nn)); }
    void on_ld_rp_at_nn(regp rp, fast_u16 nn) {
        fast_u8 lo = self().on_read_cycle(nn);
        nn = inc16(nn);
        self().on_set_wz(nn);
        fast_u8 hi = self().on_read_cycle(nn);
        self().on_set_regp(rp, iregp::hl, make16(hi, lo)); }
    void on_ld_at_nn_rp(fast_u16 nn, regp rp) {
        fast_u16 rpv = self().on_get_regp(rp, iregp::hl);
        self().on_write_cycle(nn, get_low8(rpv));
        nn = inc16(nn);
        self().on_set_wz(nn);
        self().on_write_cycle(nn, get_high8(rpv)); }
    void on_ld_sp_irp() {
        if(!self().on_is_z80()) {
            self().on_set_sp(self().on_get_hl());
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_sp(self().on_get_iregp(irp)); } }
    void on_ld_rp_nn(regp rp, fast_u16 nn) {
        // TODO: Unify on_set_regp()?
        if(!self().on_is_z80()) {
            self().on_set_regp(rp, nn);
        } else {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_regp(rp, irp, nn); } }

    void on_push(fast_u16 nn) {
        fast_u16 sp = self().on_get_sp();
        sp = dec16(sp);
        self().on_write_cycle(sp, get_high8(nn));
        sp = dec16(sp);
        self().on_write_cycle(sp, get_low8(nn));
        self().on_set_sp(sp); }
    fast_u16 on_pop() {
        fast_u16 sp = self().on_get_sp();
        fast_u8 lo = self().on_read_cycle(sp);
        sp = inc16(sp);
        fast_u8 hi = self().on_read_cycle(sp);
        sp = inc16(sp);
        self().on_set_sp(sp);
        return make16(hi, lo); }
    void on_push_rp(regp2 rp) {
        if(self().on_is_z80()) {
            iregp irp = self().on_get_iregp_kind();
            self().on_push(self().on_get_regp2(rp, irp));
            return;
        }

        fast_u16 nn;
        if(rp == regp2::af && self().on_is_to_use_lazy_flags()) {
            flag_set flags = get_flags();
            nn = make16(self().on_get_a(), flags.get_f());
        } else {
            nn = self().on_get_regp2(rp);
        }
        if(rp == regp2::af) {
            // NF is always raised on i8080.
            nn |= nf_mask;
        }
        self().on_push(nn); }
    void on_pop_rp(regp2 rp) {
        if(self().on_is_z80()) {
            iregp irp = self().on_get_iregp_kind();
            self().on_set_regp2(rp, irp, self().on_pop());
            return;
        }

        fast_u16 nn = self().on_pop();
        if(rp == regp2::af) {
            // Not all flags are updated on pop psw.
            nn = (nn & ~(xf_mask | yf_mask | nf_mask));
        }
        if(rp == regp2::af && self().on_is_to_use_lazy_flags()) {
            flag_set flags(/* is_lazy= */ true);
            flags.set(get_low8(nn), 1);
            set_flags(flags);

            self().on_set_a(get_high8(nn));
        } else {
            self().on_set_regp2(rp, nn);
        } }

    // Jumps.
    void on_jp_cc_nn(condition cc, fast_u16 nn) {
        if(check_condition(cc))
            self().on_jump(nn);
        else
            self().on_set_wz(nn); }
    void on_jp_irp() {
        fast_u16 t;
        if(!self().on_is_z80()) {
            t = self().on_get_hl();
        } else {
            iregp irp = self().on_get_iregp_kind();
            t = self().on_get_iregp(irp);
        }
        self().set_pc_on_jump(t); }
    void on_jp_nn(fast_u16 nn) {
        self().on_jump(nn); }
    void on_relative_jump(fast_u8 d) {
        self().on_5t_exec_cycle();
        self().on_jump(get_disp_target(self().get_pc_on_jump(), d)); }
    void on_jr(fast_u8 d) {
        self().on_relative_jump(d); }
    void on_jr_cc(condition cc, fast_u8 d) {
        if(check_condition(cc))
            self().on_relative_jump(d); }
    void on_djnz(fast_u8 d) {
        fast_u8 b = self().on_get_b();
        b = dec8(b);
        self().on_set_b(b);
        if(b)
            self().on_relative_jump(d); }
    void on_call(fast_u16 nn) {
        self().on_push(self().on_get_pc());
        self().on_set_wz(nn);
        self().set_pc_on_call(nn); }
    void on_call_cc_nn(condition cc, fast_u16 nn) {
        if(check_condition(cc)) {
            if(self().on_is_z80())
                self().on_read_cycle_extra_1t();
            self().on_call(nn);
        } else {
            self().on_set_wz(nn);
        } }
    void on_return() {
        fast_u16 pc = self().on_pop();
        self().on_set_wz(pc);
        self().set_pc_on_return(pc); }
    void on_ret_cc(condition cc) {
        if(check_condition(cc))
            self().on_return(); }
    void on_reti_retn() {
        // According to Sean Young's The Undocumented Z80
        // Documented, both RETI and RETN copy IFF2 to IFF1.
        // http://z80.info/zip/z80-documented.pdf
        self().set_iff1_on_reti_retn(self().get_iff2_on_reti_retn());
        self().on_return(); }
    void on_reti() {
        self().on_reti_retn(); }
    void on_retn() {
        self().on_reti_retn(); }
    void on_jump(fast_u16 nn) {
        self().on_set_wz(nn);
        self().set_pc_on_jump(nn); }

    void on_call_nn(fast_u16 nn) {
        self().on_call(nn); }
    void on_ccf() {
        if(!self().on_is_z80()) {
            flag_set flags = get_flags();
            // TODO: Make sure this is optimised for lazy flags.
            flags.set_cf(flags.get_cf() ^ 1);
            set_flags(flags);
            return;
        }

        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        bool cf = f & cf_mask;
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                (cf ? hf_mask : 0) | cf_ari(!cf);
        self().on_set_f(f); }
    void on_scf() {
        if(!self().on_is_z80()) {
            flag_set flags = get_flags();
            flags.set_cf(1);
            set_flags(flags);
            return;
        }

        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                cf_mask;
        self().on_set_f(f); }

    // Interrupts.
    void on_di() {
        if(!self().on_is_z80()) {
            self().set_iff_on_di(false);
        } else {
            self().set_iff1_on_di(false);
            self().set_iff2_on_di(false); } }
    void on_ei() {
        if(!self().on_is_z80()) {
            self().set_iff_on_ei(true);
        } else {
            self().set_iff1_on_ei(true);
            self().set_iff2_on_ei(true);
        }
        self().disable_int_on_ei(); }
    void on_halt() {
        self().on_set_is_halted(true); }
    void on_im(unsigned mode) {
        self().on_set_int_mode(mode); }

    void on_ld_a_at_nn(fast_u16 nn) {
        self().on_set_wz(inc16(nn));
        self().on_set_a(self().on_read_cycle(nn)); }
    void on_ld_at_nn_a(fast_u16 nn) {
        fast_u8 a = self().on_get_a();
        self().on_set_wz(make16(a, inc8(get_low8(nn))));
        self().on_write_cycle(nn, a); }
    void on_nop() {}
    void on_neg() {
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        fast_u8 n = a;
        a = 0;
        do_sub(a, f, n);
        self().on_set_a(a);
        self().on_set_f(f); }
    void on_in_a_n(fast_u8 n) {
        fast_u16 port;
        if(!self().on_is_z80()) {
            port = n;
        } else {
            fast_u8 a = self().on_get_a();
            port = make16(a, n);
            self().on_set_wz(inc16(port));
        }
        self().on_set_a(self().on_input_cycle(port)); }
    void on_in_r_c(reg r) {
        fast_u16 bc = self().on_get_bc();
        fast_u8 f = self().on_get_f();
        self().on_set_wz(inc16(bc));
        fast_u8 n = self().on_input_cycle(bc);
        if(r != reg::at_hl)
            self().on_set_reg(r, iregp::hl, /* d= */ 0, n);
        f = (f & cf_mask) | (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) |
                pf_log(n);
        self().on_set_f(f); }
    void on_out_c_r(reg r) {
        fast_u16 bc = self().on_get_bc();
        self().on_set_wz(inc16(bc));
        fast_u8 n = (r == reg::at_hl) ?
            self().on_get_out_c_r_op() :
            self().on_get_reg(r, iregp::hl, /* d= */ 0);
        self().on_output_cycle(bc, n); }
    void on_out_n_a(fast_u8 n) {
        fast_u8 a = self().on_get_a();
        if(!self().on_is_z80()) {
            self().on_output_cycle(n, a);
        } else {
            self().on_output_cycle(make16(a, n), a);
            self().on_set_wz(make16(a, inc8(n))); } }
    void on_ret() {
        self().on_return(); }
    void on_rst(fast_u16 nn) {
        self().on_call(nn); }
    void on_rot(rot k, reg r, fast_u8 d) {
        iregp irp = self().on_get_iregp_kind();
        reg access_r = irp == iregp::hl ? r : reg::at_hl;
        fast_u8 n = self().on_get_reg(access_r, irp, d,
                                      /* long_read_cycle= */ true);
        fast_u8 f = self().on_get_f();
        do_rot(k, n, f);
        self().on_set_reg(access_r, irp, d, n);
        if(irp != iregp::hl && r != reg::at_hl)
            self().on_set_reg(r, iregp::hl, /* d= */ 0, n);
        self().on_set_f(f); }
    void on_rla() {
        if(!self().on_is_z80()) {
            fast_u8 a = self().on_get_a();
            flag_set flags = get_flags();
            fast_u16 t = (a << 1) | flags.get_cf();
            self().on_set_a(mask8(t));
            flags.set_cf(static_cast<unsigned>(t >> 8));
            set_flags(flags);
            return;
        }

        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        bool cf = f & cf_mask;
        fast_u8 r = mask8((a << 1) | (cf ? 1 : 0));
        f = (f & (sf_mask | zf_mask | pf_mask)) | (r & (yf_mask | xf_mask)) |
                cf_ari(a & 0x80);
        self().on_set_a(r);
        self().on_set_f(f); }
    void on_rlca() {
        if(!self().on_is_z80()) {
            fast_u8 a = self().on_get_a();
            flag_set flags = get_flags();
            a = rol8(a);
            self().on_set_a(a);
            flags.set_cf(a);
            set_flags(flags);
            return;
        }

        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        a = rol8(a);
        f = (f & (sf_mask | zf_mask | pf_mask)) |
                (a & (yf_mask | xf_mask | cf_mask));
        self().on_set_a(a);
        self().on_set_f(f); }
    void on_rld() {
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        fast_u16 hl = self().on_get_hl();
        self().on_set_wz(inc16(hl));
        fast_u16 t = make16(a, self().on_read_cycle(hl));
        self().on_4t_exec_cycle();

        t = (t & 0xf000) | ((t & 0xff) << 4) | ((t & 0x0f00) >> 8);
        a = get_high8(t);
        f = (f & cf_mask) | (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) |
                pf_log(a);

        self().on_set_a(a);
        self().on_set_f(f);
        self().on_write_cycle(hl, get_low8(t)); }
    void on_rra() {
        if(!self().on_is_z80()) {
            fast_u8 a = self().on_get_a();
            flag_set flags = get_flags();
            fast_u8 r = (a >> 1) | (flags.get_cf() << 7);
            self().on_set_a(r);
            flags.set_cf(a);
            set_flags(flags);
            return;
        }
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        fast_u8 r = (a >> 1) | ((f & cf_mask) ? 0x80 : 0);
        f = (f & (sf_mask | zf_mask | pf_mask)) | (r & (yf_mask | xf_mask)) |
                cf_ari(a & 0x1);
        self().on_set_a(r);
        self().on_set_f(f); }
    void on_rrca() {
        if(!self().on_is_z80()) {
            fast_u8 a = self().on_get_a();
            flag_set flags = get_flags();
            a = ror8(a);
            self().on_set_a(a);
            flags.set_cf(a >> 7);
            set_flags(flags);
            return;
        }

        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        a = ror8(a);
        f = (f & (sf_mask | zf_mask | pf_mask)) | (a & (yf_mask | xf_mask)) |
                cf_ari(a & 0x80);
        self().on_set_a(a);
        self().on_set_f(f); }
    void on_rrd() {
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();
        fast_u16 hl = self().on_get_hl();
        self().on_set_wz(inc16(hl));
        fast_u16 t = make16(a, self().on_read_cycle(hl));
        self().on_4t_exec_cycle();

        t = (t & 0xf000) | ((t & 0xf) << 8) | ((t & 0x0ff0) >> 4);
        a = get_high8(t);
        f = (f & cf_mask) | (a & (sf_mask | yf_mask | xf_mask)) | zf_ari(a) |
                pf_log(a);

        self().on_set_a(a);
        self().on_set_f(f);
        self().on_write_cycle(hl, get_low8(t)); }
    // TODO: Combine with on_adc_hl_rp()?
    void on_sbc_hl_rp(regp rp) {
        fast_u16 hl = self().on_get_hl();
        fast_u16 n = self().on_get_regp(rp, iregp::hl);
        bool cf = self().on_get_f() & cf_mask;

        self().on_4t_exec_cycle();
        self().on_3t_exec_cycle();

        fast_u16 t = add16(n, cf);
        bool of = cf && t == 0;
        fast_u32 r32 = hl - t;
        fast_u16 r16 = mask16(r32);
        fast_u8 f = (get_high8(r16) & (sf_mask | yf_mask | xf_mask)) |
                        zf_ari(r16) | hf_ari(r16 >> 8, hl >> 8, n >> 8) |
                        (pf_ari(r32 >> 8, hl >> 8, n >> 8) ^
                             (of ? pf_mask : 0)) |
                        // TODO: Can we just look at bit 16 of the 32-bit result?
                        cf_ari(r16 > hl || of) | nf_mask;

        self().on_set_wz(inc16(hl));
        self().on_set_hl(r16);
        self().on_set_f(f); }

    // Block operations.
    // TODO: Combine with other block instructions?
    void on_block_cp(block_cp k) {
        fast_u16 bc = self().on_get_bc();
        fast_u16 wz = self().on_get_wz();
        fast_u16 hl = self().on_get_hl();
        // TODO: Block comparisons implicitly depend on the
        // register 'a'. We probably want to request its value
        // here with a special handler.
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();

        fast_u8 t = self().on_read_cycle(hl);
        fast_u8 tf = f;
        do_cp(a, tf, t);

        self().on_5t_exec_cycle();
        bc = dec16(bc);

        t = a - t - ((tf & hf_mask) ? 1 : 0);
        f = (tf & (sf_mask | zf_mask | hf_mask)) |
                ((t << 4) & yf_mask) | (t & xf_mask) |
                (bc != 0 ? pf_mask : 0) | nf_mask | (f & cf_mask);

        if(static_cast<unsigned>(k) & 1) {
            // CPD, CPDR
            hl = dec16(hl);
            wz = dec16(wz);
        } else {
            // CPI, CPIR
            hl = inc16(hl);
            wz = inc16(wz);
        }

        self().on_set_bc(bc);
        self().on_set_wz(wz);
        self().on_set_hl(hl);
        self().on_set_f(f);

        // CPIR, CPDR
        if((static_cast<unsigned>(k) & 2) && bc && !(f & zf_mask)) {
            self().on_5t_exec_cycle();
            fast_u16 pc = self().get_pc_on_block_instr();
            self().on_set_wz(dec16(pc));
            self().set_pc_on_block_instr(sub16(pc, 2));
        } }
    void on_block_ld(block_ld k) {
        fast_u16 bc = self().on_get_bc();
        fast_u16 de = self().on_get_de();
        fast_u16 hl = self().on_get_hl();
        // TODO: Block loads implicitly depend on the register 'a'. We probably
        // want to request its value here with a special handler.
        fast_u8 a = self().on_get_a();
        fast_u8 f = self().on_get_f();

        fast_u8 t = self().on_read_cycle(hl);

        self().on_write_cycle(de, t);
        self().on_write_cycle_extra_2t();
        bc = dec16(bc);

        t += a;
        f = (f & (sf_mask | zf_mask | cf_mask)) |
                ((t << 4) & yf_mask) | (t & xf_mask) | (bc != 0 ? pf_mask : 0);
        if(static_cast<unsigned>(k) & 1) {
            // LDD, LDDR
            hl = dec16(hl);
            de = dec16(de);
        } else {
            // LDI, LDIR
            hl = inc16(hl);
            de = inc16(de);
        }

        self().on_set_bc(bc);
        self().on_set_de(de);
        self().on_set_hl(hl);
        self().on_set_f(f);

        // LDIR, LDDR
        if((static_cast<unsigned>(k) & 2) && bc) {
            self().on_5t_exec_cycle();
            fast_u16 pc = self().get_pc_on_block_instr();
            self().on_set_wz(dec16(pc));
            self().set_pc_on_block_instr(sub16(pc, 2));
        } }
    void on_block_in(block_in k) {
        fast_u16 bc = self().on_get_bc();
        fast_u16 wz = self().on_get_wz();
        fast_u16 hl = self().on_get_hl();
        fast_u8 f = self().on_get_f();

        self().on_fetch_cycle_extra_1t();
        fast_u8 r = self().on_input_cycle(bc);
        bc = sub16(bc, 0x0100);
        self().on_write_cycle(hl, r);
        fast_u8 s = get_high8(bc);

        if(static_cast<unsigned>(k) & 1) {
            // IND, INR
            hl = dec16(hl);
            wz = dec16(bc);
        } else {
            // INI, INIR
            hl = inc16(hl);
            wz = inc16(bc);
        }

        fast_u16 cf = get_low8(wz) + r;
        fast_u8 pf = (get_low8(cf) & 7) ^ s;
        f = (s & (sf_mask | yf_mask | xf_mask)) | zf_ari(s) |
            ((r & 0x80) >> (7 - nf_bit)) | pf_log(pf) |
            ((cf < 0x100) ? 0 : (hf_mask | cf_mask));

        self().on_set_bc(bc);
        self().on_set_wz(wz);
        self().on_set_hl(hl);
        self().on_set_f(f);

        // INIR, INDR
        if((static_cast<unsigned>(k) & 2) && s) {
            self().on_5t_exec_cycle();
            fast_u16 pc = self().get_pc_on_block_instr();
            self().set_pc_on_block_instr(sub16(pc, 2));
        } }
    void on_block_out(block_out k) {
        fast_u16 bc = self().on_get_bc();
        fast_u16 wz = self().on_get_wz();
        fast_u16 hl = self().on_get_hl();
        fast_u8 f = self().on_get_f();

        self().on_fetch_cycle_extra_1t();
        fast_u8 r = self().on_read_cycle(hl);
        bc = sub16(bc, 0x0100);
        self().on_output_cycle(bc, r);
        fast_u8 s = get_high8(bc);

        if(static_cast<unsigned>(k) & 1) {
            // OUTD, OTDR
            hl = dec16(hl);
            wz = dec16(bc);
        } else {
            // OUTI, OTIR
            hl = inc16(hl);
            wz = inc16(bc);
        }

        fast_u16 cf = get_low8(hl) + r;
        fast_u8 pf = (get_low8(cf) & 7) ^ s;
        f = (s & (sf_mask | yf_mask | xf_mask)) | zf_ari(s) |
            ((r & 0x80) >> (7 - nf_bit)) | pf_log(pf) |
            ((cf < 0x100) ? 0 : (hf_mask | cf_mask));

        self().on_set_bc(bc);
        self().on_set_wz(wz);
        self().on_set_hl(hl);
        self().on_set_f(f);

        // OTIR, OTDR
        if((static_cast<unsigned>(k) & 2) && s) {
            self().on_5t_exec_cycle();
            fast_u16 pc = self().get_pc_on_block_instr();
            self().set_pc_on_block_instr(sub16(pc, 2));
        } }

    void on_step() {
        self().on_set_is_int_disabled(false);  // TODO: Should we really do that for both the CPUs?
        self().on_fetch_and_decode();
    }

private:
    void do_rot(rot k, fast_u8 &n, fast_u8 &f) {
        fast_u8 t = n;
        bool cf = f & cf_mask;

        // TODO: Can this be simplified? E.g., in all cases the
        // direction is defined by bit 0. See testing nodes in
        // z80sim.py.
        switch(k) {
        case rot::rlc:
            n = rol8(n);
            f = (n & (sf_mask | yf_mask | xf_mask | cf_mask)) | zf_ari(n) |
                    pf_log(n);
            break;
        case rot::rrc:
            // TODO: Use ror()?
            n = mask8((n >> 1) | (n << 7));
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                cf_ari(t & 0x01);
            break;
        case rot::rl:
            n = mask8((n << 1) | (cf ? 1 : 0));
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x80);
            break;
        case rot::rr:
            n = (n >> 1) | ((cf ? 1u : 0u) << 7);
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
            // TODO: We don't need to read F here.
            f = (n & (sf_mask | yf_mask | xf_mask)) | zf_ari(n) | pf_log(n) |
                    cf_ari(t & 0x01);
            break;
        case rot::sll:
            n = mask8(n << 1) | 1;
            // TODO: We don't need to read F here.
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


    void initiate_interrupt(bool is_nmi) {
        assert(self().on_is_z80());

        self().on_set_iff1(false);

        if(!is_nmi)
            self().on_set_iff2(false);

        fast_u16 pc = self().on_get_pc();

        if(self().on_is_halted())
            self().on_set_is_halted(false);

        self().on_inc_r_reg();
        self().on_tick(is_nmi ? 5 : 7);
        self().on_push(pc);

        fast_u16 isr_addr;
        if(is_nmi) {
            // f(5) w(3) w(3)
            isr_addr = 0x0066;
        } else {
            switch(self().on_get_int_mode()) {
            // TODO: Provide a mean to customise handling of IM 0 interrupts.
            case 0:
            case 1:
                // ack(7) w(3) w(3)
                isr_addr = 0x0038;
                break;
            case 2: {
                // ack(7) w(3) w(3) r(3) r(3)
                fast_u8 v = self().on_get_int_vector();
                fast_u8 i = self().on_get_i();
                fast_u16 vector_addr = make16(i, v);
                fast_u8 lo = self().on_read_cycle(vector_addr);
                fast_u8 hi = self().on_read_cycle(inc16(vector_addr));
                isr_addr = make16(hi, lo); }
                break;
            default:
                unreachable("Unknown interrupt mode.");
            }
        }

        self().on_jump(isr_addr);
    }

public:
    void initiate_int() {
        initiate_interrupt(/* is_nmi= */ false);
    }

    void initiate_nmi() {
        initiate_interrupt(/* is_nmi= */ true);
    }

    bool on_handle_active_int() {
        bool accepted = false;
        if(!self().on_is_int_disabled() && self().on_get_iff1()) {
            initiate_int();
            accepted = true;
        }
        return accepted;
    }

protected:
    using base::self;

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
        // TODO: Would something like this be clearer?
        //       + Revisit the code that calls this function.
        // return ((r ^ (r >> 1) ^ a ^ b) >> (7 - pf_bit)) & pf_mask;
        fast_u16 x = r ^ a ^ b;
        return ((x >> 6) ^ (x >> 5)) & pf_mask;
    }

    // TODO: Rename to parity8(). Deprecate pf_log().
    static fast_u8 pf_log(fast_u8 n) {
        // Compute parity. First, half the range of bits to
        // consider by xor'ing nibbles of the passed value. Then,
        // use a bit pattern to determine whether the resulting
        // four-bit value has an even number of raised bits.
        fast_u8 n4 = ((n >> 4) ^ n) & 0xf;
        return ((0x9669 << pf_bit) >> n4) & pf_mask;
    }

    static fast_u8 pf_dec(fast_u8 n) {
        return n == 0x7f ? pf_mask : 0;
    }

    static fast_u8 pf_inc(fast_u8 n) {
        return n == 0x80 ? pf_mask : 0;
    }

    static fast_u8 cf_ari(bool c) {
        return c ? cf_mask : 0;
    }
};

template<typename B>
class i8080_executor : public internals::executor_base<B>
{};

template<typename B>
class z80_executor : public internals::executor_base<B>
{};

template<typename D>
class i8080_cpu : public i8080_executor<i8080_decoder<i8080_state<root<D>>>>
{};

template<typename D>
class z80_cpu : public z80_executor<z80_decoder<z80_state<root<D>>>>
{};

static const fast_u32 address_space_size = 0x10000;  // 64K bytes.

template<typename B>
class machine_memory : public B {
public:
    typedef B base;

    machine_memory() {}

    fast_u8 read(fast_u16 addr) const {
        assert(addr < address_space_size);
        return image.bytes[addr];
    }

    void write(fast_u16 addr, fast_u8 n) {
        assert(addr < address_space_size);
        image.bytes[addr] = static_cast<least_u8>(n);
    }

    fast_u8 on_read(fast_u16 addr) { return read(addr); }
    void on_write(fast_u16 addr, fast_u8 n) { write(addr, n); }

    void on_reset_memory() {
        base::on_reset_memory();
        image = memory_image();
    }

protected:
    using base::self;

private:
    struct memory_image {
        memory_image() {
            fast_u32 rnd = 0xde347a01;
            for(auto &b : bytes) {
                b = static_cast<least_u8>(rnd & 0xff);
                rnd = (rnd * 0x74392cef) ^ ((rnd >> 16) & 0xffff);
            }
        }

        least_u8 bytes[address_space_size] = {};
    };

    memory_image image;
};

class events_mask {
public:
    typedef fast_u32 type;

    static const type end_of_frame = 1u << 0;
    static const type breakpoint_hit = 1u << 1;
    static const type ticks_limit_hit = 1u << 2;
    static const type end = 1u << 3;
};

template<typename B>
class machine_state : public B {
public:
    typedef B base;
    typedef unsigned ticks_type;

    machine_state() {}

    bool is_marked_addr(fast_u16 addr, fast_u8 marks) const {
        return (fields.address_marks[mask16(addr)] & marks) != 0;
    }

    void mark_addr(fast_u16 addr, fast_u8 marks) {
        fields.address_marks[mask16(addr)] |= static_cast<least_u8>(marks);
    }

    void mark_addrs(fast_u16 addr, fast_u16 size, fast_u8 marks) {
        for(fast_u16 i = 0; i != size; ++i)
            mark_addr(addr + i, marks);
    }

    void unmark_addr(fast_u16 addr, fast_u8 marks) {
        fields.address_marks[addr] &= ~marks;
    }

    void unmark_addrs(fast_u16 addr, fast_u16 size, fast_u8 marks) {
        for(fast_u16 i = 0; i != size; ++i)
            unmark_addr(addr + i, marks);
    }

    bool is_breakpoint_addr(fast_u16 addr) const {
        return is_marked_addr(addr, state_fields::breakpoint_mark);
    }

    void set_breakpoint(fast_u16 addr) {
        mark_addr(addr, state_fields::breakpoint_mark);
    }

    void clear_breakpoint(fast_u16 addr) {
        unmark_addr(addr, state_fields::breakpoint_mark);
    }

    void on_tick(unsigned t) {
        base::on_tick(t);

        fields.frame_tick += t;
        if(fields.frame_tick >= state_fields::ticks_per_frame) {
            fields.frame_tick %= state_fields::ticks_per_frame;
            fields.events |= events_mask::end_of_frame;
        }
    }

    void on_set_pc(fast_u16 n) {
        if(is_breakpoint_addr(n))
            fields.events |= events_mask::breakpoint_hit;
        base::on_set_pc(n);
    }

    void on_raise_events(events_mask::type new_events) {
        fields.events |= new_events;
    }

    events_mask::type on_run() {
        fields.events = 0;
        while(!fields.events)
            self().on_step();
        return fields.events;
    }

    void on_reset(bool soft = false) {
        base::on_reset(soft);
        fields = state_fields();
    }

protected:
    using base::self;

private:
    struct state_fields {
        ticks_type frame_tick = 0;
        static const ticks_type ticks_per_frame = 100 * 1000;

        events_mask::type events = 0;

        static const fast_u8 breakpoint_mark = 1u << 0;
        least_u8 address_marks[address_space_size] = {};
    };

    state_fields fields;
};

template<typename D>
class i8080_machine : public machine_memory<machine_state<i8080_cpu<D>>>
{};

template<typename D>
class z80_machine : public machine_memory<machine_state<z80_cpu<D>>>
{};

}  // namespace z80

#endif  // Z80_H
