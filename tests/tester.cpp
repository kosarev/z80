
/*  Z80 CPU Emulator.
    https://github.com/kosarev/z80

    Copyright (C) 2017-2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cerrno>
#include <cstdarg>
#include <cstdio>
#include <cstring>

#include "z80.h"

namespace {

#if defined(__GNUC__) || defined(__clang__)
# define LIKE_PRINTF(format, args) \
      __attribute__((__format__(__printf__, format, args)))
#else
# define LIKE_PRINTF(format, args) /* nothing */
#endif

const char program_name[] = "tester";

[[noreturn]] LIKE_PRINTF(1, 0)
void verror(const char *format, va_list args) {
    std::fprintf(stderr, "%s: ", program_name);
    std::vfprintf(stderr, format, args);
    std::fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

[[noreturn]] LIKE_PRINTF(1, 2)
void error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    verror(format, args);
    va_end(args);
}

using z80::fast_u8;
using z80::fast_u16;
using z80::fast_u32;
using z80::least_u8;
using z80::reg;
using z80::unreachable;

class test_input {
public:
    test_input(FILE *stream)
        : stream(stream), read(false), eof(false), line_no(0), level(0)
    {}

    const char *read_line() {
        const char *res = fgets(line, max_line_size, stream);
        if(ferror(stream))
            error("cannot read test input: %s", std::strerror(errno));
        read = true;

        if(!eof)
            ++line_no;

        // Return empty string instead of null.
        if(!res) {
            eof = true;
            line[0] = '\0';
            return line;
        }

        // Strip trailing new-line marker.
        line[std::strlen(line) - 1] = '\0';
        return line;
    }

    bool is_eof() const {
        // Note that we cannot rely on feof() until we tried to read.
        return read && feof(stream);
    }

    explicit operator bool () const {
        return !is_eof();
    }

    const char *get_line() const {
        return line;
    }

    void step_in() {
        ++level;
    }

    void step_out() {
        assert(level > 0);
        --level;
    }

    void reset_skipping_mode() {
        in_skipping_mode = false;
    }

    LIKE_PRINTF(2, 4)
    void read_and_match(const char *format, unsigned ticks, ...) {
        // Handle the skip directive.
        if(!in_skipping_mode) {
            read_line();
            in_skipping_mode = (std::strcmp(line, "...") == 0);
            if(in_skipping_mode)
                read_line();
        }

        char buff[max_line_size];
        va_list args;
        va_start(args, ticks);
        std::vsnprintf(buff, max_line_size, format, args);
        buff[max_line_size - 1] = '\0';
        va_end(args);

        char buff2[max_line_size];
        std::snprintf(buff2, max_line_size, "%2u %*s%.100s",
                      static_cast<unsigned>(ticks),
                      static_cast<int>(level * 2), "", buff);

        if(std::strcmp(buff2, line) == 0) {
            reset_skipping_mode();
            return;
        }

        if(in_skipping_mode)
            return;

        error("mismatch: expected '%s'", buff2);
    }

    void handle_end_of_test_entry() {
        if(in_skipping_mode && *line != '\0')
            error("this line is expected, but not found");

        reset_skipping_mode();
    }

    void quote_line() const {
        assert(read);
        std::fprintf(stderr, "%s: line %lu: '%s'\n", program_name,
                     static_cast<unsigned long>(line_no), line);
    }

    [[noreturn]] LIKE_PRINTF(2, 3)
    void error(const char *format, ...) const {
        quote_line();

        va_list args;
        va_start(args, format);
        ::verror(format, args);
        va_end(args);
    }

private:
    FILE *stream;
    bool read;
    bool eof;
    unsigned long line_no;
    unsigned level;
    bool in_skipping_mode = false;

    static const std::size_t max_line_size = 1024;
    char line[max_line_size];
};

class input_level_guard {
public:
    input_level_guard(test_input &input)
            : input(input) {
        input.step_in();
    }

    ~input_level_guard() {
        input.step_out();
    }

private:
    test_input &input;
};

static const unsigned max_instr_size = 4;

template<typename B>
class disasm_base : public B {
public:
    typedef B base;

    disasm_base()
        : index(0), instr_size(0)
    {}

    const char *get_output() const {
        return output_buff;
    }

    void on_emit(const char *out) {
        std::snprintf(output_buff, max_output_buff_size, "%s", out);
    }

    fast_u8 on_read_next_byte() {
        assert(index < instr_size);
        return instr_code[index++];
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        assert(size <= max_instr_size);
        std::memcpy(instr_code, code, size);
        instr_size = size;
        index = 0;
        output_buff[0] = '\0';
    }

private:
    unsigned index;
    least_u8 instr_code[max_instr_size];
    unsigned instr_size;

    static const std::size_t max_output_buff_size = 32;
    char output_buff[max_output_buff_size];
};

class i8080_disasm : public disasm_base<z80::i8080_disasm<i8080_disasm>>
{};

class z80_disasm : public disasm_base<z80::z80_disasm<z80_disasm>> {
public:
    void on_disassemble() {
        // Skip prefixes.
        base::on_disassemble();
        while(base::get_iregp_kind() != z80::iregp::hl)
            base::on_disassemble();
    }
};

template<typename B>
class machine_base : public B {
public:
    typedef B base;
    typedef typename B::derived derived;
    typedef z80::z80_disasm<derived> disasm;
    typedef fast_u32 ticks_type;

    machine_base(test_input &input)
        : input(input)
    {}

    void on_tick(unsigned t) { ticks += t; }

    ticks_type get_ticks() const { return ticks; }

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        return image[addr];
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        image[addr] = static_cast<least_u8>(n);
    }

    fast_u8 on_input(fast_u16 port) {
        z80::unused(port);
        return 0xff;
    }

    void on_output(fast_u16 port, fast_u8 n) {
        z80::unused(port, n);
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        fast_u16 pc = base::get_pc();
        for(least_u8 &cell : image)
            cell = 0;
        for(unsigned i = 0; i != size; ++i)
            on_write(z80::add16(pc, i), code[i]);
    }

    void match_get_r(const char *name, fast_u8 n) {
        input.read_and_match("get_%s %02x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(n));
    }

    void match_set_r(const char *name, fast_u8 old_n, fast_u8 new_n) {
        input.read_and_match("set_%s %02x -> %02x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(old_n),
                             static_cast<unsigned>(new_n));
    }

    void match_get_rp(const char *name, fast_u16 nn) {
        input.read_and_match("get_%s %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(nn));
    }

    void match_set_rp(const char *name, fast_u16 old_nn, fast_u16 new_nn) {
        input.read_and_match("set_%s %04x -> %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(old_nn),
                             static_cast<unsigned>(new_nn));
    }

    fast_u8 on_get_b() { match_get_r("b", base::get_b());
                         return base::on_get_b(); }
    void on_set_b(fast_u8 b) { match_set_r("b", base::get_b(), b);
                               return base::on_set_b(b); }

    fast_u8 on_get_c() { match_get_r("c", base::get_c());
                         return base::on_get_c(); }
    void on_set_c(fast_u8 c) { match_set_r("c", base::get_c(), c);
                               return base::on_set_c(c); }

    fast_u8 on_get_d() { match_get_r("d", base::get_d());
                         return base::on_get_d(); }
    void on_set_d(fast_u8 d) { match_set_r("d", base::get_d(), d);
                               return base::on_set_d(d); }

    fast_u8 on_get_e() { match_get_r("e", base::get_e());
                         return base::on_get_e(); }
    void on_set_e(fast_u8 e) { match_set_r("e", base::get_e(), e);
                               return base::on_set_e(e); }

    fast_u8 on_get_h() { match_get_r("h", base::get_h());
                         return base::on_get_h(); }
    void on_set_h(fast_u8 h) { match_set_r("h", base::get_h(), h);
                               return base::on_set_h(h); }

    fast_u8 on_get_l() { match_get_r("l", base::get_l());
                         return base::on_get_l(); }
    void on_set_l(fast_u8 l) { match_set_r("l", base::get_l(), l);
                               return base::on_set_l(l); }

    fast_u8 on_get_a() { match_get_r("a", base::get_a());
                         return base::on_get_a(); }
    void on_set_a(fast_u8 a) { match_set_r("a", base::get_a(), a);
                               return base::on_set_a(a); }

    fast_u8 on_get_f() { match_get_r("f", base::get_f());
                         return base::on_get_f(); }
    void on_set_f(fast_u8 f) { match_set_r("f", base::get_f(), f);
                               return base::on_set_f(f); }

    fast_u8 on_get_ixh() { match_get_r("ixh", base::get_ixh());
                           return base::on_get_ixh(); }
    void on_set_ixh(fast_u8 ixh) { match_set_r("ixh", base::get_ixh(), ixh);
                                   return base::on_set_ixh(ixh); }

    fast_u8 on_get_ixl() { match_get_r("ixl", base::get_ixl());
                           return base::on_get_ixl(); }
    void on_set_ixl(fast_u8 ixl) { match_set_r("ixl", base::get_ixl(), ixl);
                                   return base::on_set_ixl(ixl); }

    fast_u8 on_get_iyh() { match_get_r("iyh", base::get_iyh());
                           return base::on_get_iyh(); }
    void on_set_iyh(fast_u8 iyh) { match_set_r("iyh", base::get_iyh(), iyh);
                                   return base::on_set_iyh(iyh); }

    fast_u8 on_get_iyl() { match_get_r("iyl", base::get_iyl());
                           return base::on_get_iyl(); }
    void on_set_iyl(fast_u8 iyl) { match_set_r("iyl", base::get_iyl(), iyl);
                                   return base::on_set_iyl(iyl); }

#if 0  // TODO
    fast_u8 on_get_i() { match_get_r("i", get_i());
                         return base::on_get_i(); }
#endif
    void on_set_i(fast_u8 i) { match_set_r("i", base::get_i(), i);
                               return base::on_set_a(i); }

#if 0  // TODO
    fast_u8 on_get_r() { match_get_r("r", get_r());
                         return base::on_get_r(); }
#endif
    void on_set_r(fast_u8 r) { match_set_r("r", base::get_r(), r);
                               return base::on_set_r(r); }

    fast_u16 on_get_sp() { match_get_rp("sp", base::get_sp());
                           return base::on_get_sp(); }
    void on_set_sp(fast_u16 sp) { match_set_rp("sp", base::get_sp(), sp);
                                  return base::on_set_sp(sp); }

    void match_get_pc(const char *name) const {
        input.read_and_match("get_pc_on_%s %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(base::get_pc())); }
    void match_set_pc(const char *name, fast_u16 pc) {
        input.read_and_match("set_pc_on_%s %04x -> %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(base::get_pc()),
                             static_cast<unsigned>(pc)); }

    fast_u16 get_pc_on_fetch() {
        match_get_pc("fetch");
        return base::get_pc_on_fetch(); }
    void set_pc_on_fetch(fast_u16 pc) {
        match_set_pc("fetch", pc);
        base::set_pc_on_fetch(pc); }

    fast_u16 get_pc_on_imm8_read() {
        match_get_pc("imm8_read");
        return base::get_pc_on_imm8_read(); }
    void set_pc_on_imm8_read(fast_u16 pc) {
        match_set_pc("imm8_read", pc);
        base::set_pc_on_imm8_read(pc); }

    fast_u16 get_pc_on_imm16_read() {
        match_get_pc("imm16_read");
        return base::get_pc_on_imm16_read(); }
    void set_pc_on_imm16_read(fast_u16 pc) {
        match_set_pc("imm16_read", pc);
        base::set_pc_on_imm16_read(pc); }

    fast_u16 get_pc_on_disp_read() {
        match_get_pc("disp_read");
        return base::get_pc_on_disp_read(); }
    void set_pc_on_disp_read(fast_u16 pc) {
        match_set_pc("disp_read", pc);
        base::set_pc_on_disp_read(pc); }

    fast_u16 get_pc_on_jump() {
        match_get_pc("jump");
        return base::get_pc_on_jump(); }
    void set_pc_on_jump(fast_u16 pc) {
        match_set_pc("jump", pc);
        base::set_pc_on_jump(pc); }

    fast_u16 get_pc_on_block_instr() {
        match_get_pc("block_instr");
        return base::get_pc_on_block_instr(); }
    void set_pc_on_block_instr(fast_u16 pc) {
        match_set_pc("block_instr", pc);
        base::set_pc_on_block_instr(pc); }

    void set_pc_on_call(fast_u16 pc) {
        match_set_pc("call", pc);
        base::set_pc_on_call(pc); }

    void set_pc_on_return(fast_u16 pc) {
        match_set_pc("return", pc);
        base::set_pc_on_return(pc); }

    fast_u16 get_pc_on_halt() {
        match_get_pc("halt");
        return base::get_pc_on_halt(); }
    void set_pc_on_halt(fast_u16 pc) {
        match_set_pc("halt", pc);
        base::set_pc_on_halt(pc); }

    void match_get_ir(const char *name) const {
        input.read_and_match("get_ir_on_%s %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(base::get_ir())); }

    fast_u16 get_ir_on_refresh() {
        match_get_ir("refresh");
        return base::get_ir_on_refresh(); }

    void on_set_addr_bus(fast_u16 addr) {
        input.read_and_match("set_addr_bus %04x -> %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(addr_bus),
                             static_cast<unsigned>(addr));
        addr_bus = addr;
    }

    fast_u8 on_fetch_cycle() {
        fast_u16 addr = base::get_pc();
        input.read_and_match("fetch %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_read(addr)),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        return base::on_fetch_cycle();
    }

    void on_fetch_cycle_extra_1t() {
        input.read_and_match("fetch_cycle_extra_1t",
                             static_cast<unsigned>(get_ticks()));
        base::on_fetch_cycle_extra_1t();
    }

    void on_fetch_cycle_extra_2t() {
        input.read_and_match("fetch_cycle_extra_2t",
                             static_cast<unsigned>(get_ticks()));
        base::on_fetch_cycle_extra_2t();
    }

    void on_fetch_cycle_extra_3t() {
        input.read_and_match("fetch_cycle_extra_3t",
                             static_cast<unsigned>(get_ticks()));
        base::on_fetch_cycle_extra_3t();
    }

    fast_u8 on_read_cycle(fast_u16 addr) {
        input.read_and_match("read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_read(addr)),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        return base::on_read_cycle(addr);
    }

    void on_read_cycle_extra_1t() {
        input.read_and_match("read_cycle_extra_1t",
                             static_cast<unsigned>(get_ticks()));
        base::on_read_cycle_extra_1t();
    }

    void on_read_cycle_extra_2t() {
        input.read_and_match("read_cycle_extra_2t",
                             static_cast<unsigned>(get_ticks()));
        base::on_read_cycle_extra_2t();
    }

    void on_write_cycle(fast_u16 addr, fast_u8 n) {
        input.read_and_match("write %02x -> %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_read(addr)),
                             static_cast<unsigned>(n),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        base::on_write_cycle(addr, n);
    }

    void on_write_cycle_extra_2t() {
        input.read_and_match("write_cycle_extra_2t",
                             static_cast<unsigned>(get_ticks()));
        base::on_write_cycle_extra_2t();
    }

    void on_3t_exec_cycle() {
        input.read_and_match("3t_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_3t_exec_cycle();
    }

    void on_4t_exec_cycle() {
        input.read_and_match("4t_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_4t_exec_cycle();
    }

    void on_5t_exec_cycle() {
        input.read_and_match("5t_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_5t_exec_cycle();
    }

    fast_u8 on_input_cycle(fast_u8 n) {
        input.read_and_match("input at %02x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(n));
        return base::on_input_cycle(n);
    }

    fast_u8 on_input_cycle(fast_u16 addr) {
        input.read_and_match("input at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(addr));
        return base::on_input_cycle(addr);
    }

    void on_output_cycle(fast_u8 n, fast_u8 b) {
        input.read_and_match("output %02x at %02x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(b),
                             static_cast<unsigned>(n));
        base::on_output_cycle(n, b);
    }

    void on_output_cycle(fast_u16 addr, fast_u8 b) {
        input.read_and_match("output %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(b),
                             static_cast<unsigned>(addr));
        base::on_output_cycle(addr, b);
    }

    fast_u8 on_imm8_read() {
        fast_u16 addr = base::get_pc();
        input.read_and_match("imm8_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_read(addr)),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        return base::on_imm8_read();
    }

    fast_u16 on_imm16_read() {
        fast_u16 addr = base::get_pc();
        fast_u16 v = z80::make16(on_read(z80::inc16(addr)),
                                 on_read(addr));
        input.read_and_match("imm16_read %04x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(v),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        return base::on_imm16_read();
    }

    fast_u8 on_disp_read() {
        fast_u16 addr = base::get_pc();
        input.read_and_match("disp_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_read(addr)),
                             static_cast<unsigned>(addr));
        input_level_guard guard(input);
        return base::on_disp_read();
    }

    void set_iff_on_di(bool iff) {
        input.read_and_match("set_iff_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff()),
                             static_cast<unsigned>(iff));
        input_level_guard guard(input);
        base::set_iff_on_di(iff);
    }

    void set_iff1_on_di(bool f) {
        input.read_and_match("set_iff1_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff1()),
                             static_cast<unsigned>(f));
        input_level_guard guard(input);
        base::set_iff1_on_di(f);
    }

    void set_iff2_on_di(bool f) {
        input.read_and_match("set_iff2_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff2()),
                             static_cast<unsigned>(f));
        input_level_guard guard(input);
        base::set_iff2_on_di(f);
    }

    void set_iff_on_ei(bool iff) {
        input.read_and_match("set_iff_on_ei %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff()),
                             static_cast<unsigned>(iff));
        input_level_guard guard(input);
        base::set_iff_on_ei(iff);
    }

    void set_iff1_on_ei(bool f) {
        input.read_and_match("set_iff1_on_ei %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff1()),
                             static_cast<unsigned>(f));
        input_level_guard guard(input);
        base::set_iff1_on_ei(f);
    }

    void set_iff2_on_ei(bool f) {
        input.read_and_match("set_iff2_on_ei %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_iff2()),
                             static_cast<unsigned>(f));
        input_level_guard guard(input);
        base::set_iff2_on_ei(f);
    }

    void on_set_int_mode(unsigned mode) {
        input.read_and_match("set_int_mode %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(base::get_int_mode()),
                             static_cast<unsigned>(mode));
        base::on_set_int_mode(mode);
    }

    void on_set_is_int_disabled(bool f) {
        if(f) {
            input.read_and_match("disable_int",
                                 static_cast<unsigned>(get_ticks()));
        }
        base::on_set_is_int_disabled(f);
    }

    void on_set_iregp_kind(z80::iregp irp) {
        if(irp != base::get_iregp_kind()) {
            input.read_and_match(
                "set_index_rp %s -> %s",
                static_cast<unsigned>(get_ticks()),
                disasm::get_reg_name(base::get_iregp_kind()),
                disasm::get_reg_name(irp));
        }
        base::on_set_iregp_kind(irp);
    }

protected:
    test_input &input;

private:
    ticks_type ticks = 0;
    fast_u16 addr_bus = 0;

    least_u8 image[z80::address_space_size];
};

class i8080_machine : public machine_base<z80::i8080_cpu<i8080_machine>> {
public:
    i8080_machine(test_input &input)
        : machine_base<z80::i8080_cpu<i8080_machine>>(input)
    {}

    void on_set_wz(fast_u16 wz) { match_set_rp("wz", 0, wz);
                                  return base::on_set_wz(wz); }

    void on_step() {
        base::on_step();
        input.read_and_match("done", static_cast<unsigned>(get_ticks()));
    }
};

class z80_machine : public machine_base<z80::z80_cpu<z80_machine>> {
public:
    z80_machine(test_input &input)
        : machine_base<z80::z80_cpu<z80_machine>>(input)
    {}

    fast_u8 on_m1_fetch_cycle() {
        input.read_and_match("m1_fetch",
                             static_cast<unsigned>(get_ticks()));
        input_level_guard guard(input);
        return base::on_m1_fetch_cycle();
    }

    void on_set_wz(fast_u16 wz) { match_set_rp("wz", base::get_wz(), wz);
                                  return base::on_set_wz(wz); }

    void on_step() {
        // Skip prefixes.
        base::on_step();
        while(base::get_iregp_kind() != z80::iregp::hl)
            base::on_step();

        input.read_and_match("done", static_cast<unsigned>(get_ticks()));
    }
};

bool parse_hex_digit(const char *&p, fast_u8 &res) {
    auto c = static_cast<unsigned char>(*p);
    if(c >= static_cast<unsigned char>('0') &&
           c <= static_cast<unsigned char>('9')) {
        res = static_cast<fast_u8>(c - '0');
        ++p;
        return true;
    }
    if(c >= static_cast<unsigned char>('a') &&
           c <= static_cast<unsigned char>('f')) {
        res = static_cast<fast_u8>(c - 'a' + 10);
        ++p;
        return true;
    }
    return false;
}

bool parse_u8(const char *&p, fast_u8 &res) {
    const char *original_p = p;
    fast_u8 hi, lo;
    if(!parse_hex_digit(p, hi) || !parse_hex_digit(p, lo)) {
        p = original_p;
        return false;
    }
    res = static_cast<fast_u8>((hi << 4) | lo);
    return true;
}

void skip_whitespace(const char *&p) {
    while(*p == ' ')
        ++p;
}

bool parse(const char *&p, const char *str) {
    const char *t = p;
    while(*str) {
        if(*t != *str)
            return false;

        ++t;
        ++str;
    }

    p = t;
    return true;
}

bool parse_set_r_directive(const char *r, fast_u8 &n,
                           const test_input &input) {
    const char *p = input.get_line();
    if(*p++ != '.' || !parse(p, r) || !parse(p, "="))
        return false;

    if(!parse_u8(p, n) || *p != '\0')
        input.error("malformed operand");
    return true;
}

template<typename M>
void handle_directive(const test_input &input, M &mach) {
    fast_u8 n;
    if(parse_set_r_directive("b", n, input))
        return mach.set_b(n);
    if(parse_set_r_directive("c", n, input))
        return mach.set_c(n);

    input.error("unknown directive");
}

template<typename M, typename D>
void handle_test_entry(test_input &input) {
    typedef M machine;
    typedef D disasm;

    // Handle directives.
    machine mach(input);
    const char *p = input.get_line();
    while(*p == '.') {
        handle_directive(input, mach);
        p = input.read_line();
    }

    // Parse instruction bytes.
    least_u8 instr_code[max_instr_size];
    unsigned instr_size = 0;
    fast_u8 instr_byte;
    while(parse_u8(p, instr_byte)) {
        if(instr_size == max_instr_size)
            input.error("intstruction code is too large");
        instr_code[instr_size++] = static_cast<least_u8>(instr_byte);
    }
    if(instr_size == 0)
        input.error("expected instruction code");
    skip_whitespace(p);

    // Test instruction disassembly.
    disasm dis;
    dis.set_instr_code(instr_code, instr_size);
    dis.on_disassemble();
    const char *instr = dis.get_output();
    if(std::strcmp(instr, p) != 0)
        input.error("instruction disassembly mismatch: '%s' vs '%s'",
                    instr, p);

    mach.set_instr_code(instr_code, instr_size);
    mach.on_step();

    input.handle_end_of_test_entry();
}

enum class cpu_kind {
    unknown,
    i8080,
    z80,
};

cpu_kind get_cpu_kind(const char *id) {
    if(std::strcmp(id, "i8080") == 0)
        return cpu_kind::i8080;
    if(std::strcmp(id, "z80") == 0)
        return cpu_kind::z80;
    return cpu_kind::unknown;
}

}  // anonymous namespace

int main(int argc, char *argv[]) {
    if(argc != 3)
        error("usage: tester <cpu> <test-input>");

    const char *cpu_id = argv[1];
    cpu_kind cpu = get_cpu_kind(cpu_id);
    if(cpu == cpu_kind::unknown)
        error("unknown cpu '%s'", cpu_id);

    const char *filename = argv[2];
    FILE *f = fopen(filename, "r");
    if(!f) {
        error("cannot open test input '%s': %s", filename,
              std::strerror(errno));
    }

    test_input input(f);
    while(input) {
        // Skip empty lines and comments.
        const char *line = input.read_line();
        if(line[0] == '\0' || line[0] == '#')
            continue;

        switch(cpu) {
        case cpu_kind::i8080:
            handle_test_entry<i8080_machine, i8080_disasm>(input);
            break;
        case cpu_kind::z80:
            handle_test_entry<z80_machine, z80_disasm>(input);
            break;
        case cpu_kind::unknown:
            unreachable("Unknown CPU.");
        }
    }

    if(fclose(f) != 0)
        error("cannot close test input '%s': %s", argv[1],
              std::strerror(errno));
}
