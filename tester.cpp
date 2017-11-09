
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
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
using z80::least_u8;
using z80::reg;

class test_input {
public:
    test_input(FILE *stream)
        : stream(stream), read(false), eof(false), line_no(0)
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

    LIKE_PRINTF(2, 3)
    void read_and_match(const char *format, ...) {
        read_line();

        char buff[max_line_size];
        va_list args;
        va_start(args, format);
        std::vsnprintf(buff, max_line_size, format, args);
        buff[max_line_size - 1] = '\0';
        va_end(args);

        if(std::strcmp(buff, line) != 0)
            error("mismatch: expected '%s'", buff);
    }

    void quote_line() const {
        assert(read);
        std::fprintf(stderr, "%s: line %lu: '%s'\n", program_name,
                     static_cast<unsigned long>(line_no), line);
    }

    [[noreturn]] LIKE_PRINTF(2, 3)
    void error(const char *format, ...) {
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

    static const std::size_t max_line_size = 1024;
    char line[max_line_size];
};

static const unsigned max_instr_size = 4;

class disassembler : public z80::disassembler<disassembler> {
public:
    typedef z80::disassembler<disassembler> base;

    disassembler()
        : index(0), instr_size(0)
    {}

    const char *get_output() const {
        return output_buff;
    }

    void on_output(const char *out) override {
        std::snprintf(output_buff, max_output_buff_size, "%s", out);
    }

    fast_u8 on_read() {
        assert(index < instr_size);
        return instr_code[index++];
    }

    fast_u16 get_last_read_addr() const {
        assert(index != 0);
        return index - 1;
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        assert(size <= max_instr_size);
        std::memcpy(instr_code, code, size);
        instr_size = size;
        index = 0;
        output_buff[0] = '\0';
    }

    void disassemble() {
        // Skip prefixes.
        base::disassemble();
        while(get_prefix() != z80::instruction_prefix::none ||
              get_next_index_rp_kind() != z80::index_regp::hl)
            base::disassemble();
    }

private:
    unsigned index;
    least_u8 instr_code[max_instr_size];
    unsigned instr_size;

    static const std::size_t max_output_buff_size = 32;
    char output_buff[max_output_buff_size];
};

class machine : public z80::processor<machine> {
public:
    typedef processor<machine> base;
    typedef uint_fast32_t ticks_type;

    machine(test_input &input)
        : ticks(0), input(input)
    {}

    void tick(unsigned t) { ticks += t; }

    ticks_type get_ticks() const { return ticks; }

    least_u8 &on_access(fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        fast_u16 pc = get_pc();
        for(unsigned i = 0; i != size; ++i)
            on_access(z80::add16(pc, i)) = code[i];
    }

    void match_get_r(const char *name, fast_u8 n) {
        input.read_and_match("%2u get_%s %02x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(n));
    }

    void match_set_r(const char *name, fast_u8 old_n, fast_u8 new_n) {
        input.read_and_match("%2u set_%s %02x -> %02x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(old_n),
                             static_cast<unsigned>(new_n));
    }

    void match_get_rp(const char *name, fast_u16 nn) {
        input.read_and_match("%2u get_%s %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(nn));
    }

    void match_set_rp(const char *name, fast_u16 old_nn, fast_u16 new_nn) {
        input.read_and_match("%2u set_%s %04x -> %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(old_nn),
                             static_cast<unsigned>(new_nn));
    }

    fast_u8 on_get_b() { match_get_r("b", get_b());
                         return base::on_get_b(); }
    void on_set_b(fast_u8 b) { match_set_r("b", get_b(), b);
                               return base::on_set_b(b); }

    fast_u8 on_get_c() { match_get_r("c", get_c());
                         return base::on_get_c(); }
    void on_set_c(fast_u8 c) { match_set_r("c", get_c(), c);
                               return base::on_set_c(c); }

    fast_u8 on_get_d() { match_get_r("d", get_d());
                         return base::on_get_d(); }
    void on_set_d(fast_u8 d) { match_set_r("d", get_d(), d);
                               return base::on_set_d(d); }

    fast_u8 on_get_e() { match_get_r("e", get_e());
                         return base::on_get_e(); }
    void on_set_e(fast_u8 e) { match_set_r("e", get_e(), e);
                               return base::on_set_e(e); }

    fast_u8 on_get_h() { match_get_r("h", get_h());
                         return base::on_get_h(); }
    void on_set_h(fast_u8 h) { match_set_r("h", get_h(), h);
                               return base::on_set_h(h); }

    fast_u8 on_get_l() { match_get_r("l", get_l());
                         return base::on_get_l(); }
    void on_set_l(fast_u8 l) { match_set_r("l", get_l(), l);
                               return base::on_set_l(l); }

    fast_u8 on_get_a() { match_get_r("a", get_a());
                         return base::on_get_a(); }
    void on_set_a(fast_u8 a) { match_set_r("a", get_a(), a);
                               return base::on_set_a(a); }

    fast_u8 on_get_f() { match_get_r("f", get_f());
                         return base::on_get_f(); }
    void on_set_f(fast_u8 f) { match_set_r("f", get_f(), f);
                               return base::on_set_f(f); }

    fast_u8 on_get_ixh() { match_get_r("ixh", get_ixh());
                           return base::on_get_ixh(); }
    void on_set_ixh(fast_u8 ixh) { match_set_r("ixh", get_ixh(), ixh);
                                   return base::on_set_ixh(ixh); }

    fast_u8 on_get_ixl() { match_get_r("ixl", get_ixl());
                           return base::on_get_ixl(); }
    void on_set_ixl(fast_u8 ixl) { match_set_r("ixl", get_ixl(), ixl);
                                   return base::on_set_ixl(ixl); }

    fast_u8 on_get_iyh() { match_get_r("iyh", get_iyh());
                           return base::on_get_iyh(); }
    void on_set_iyh(fast_u8 iyh) { match_set_r("iyh", get_iyh(), iyh);
                                   return base::on_set_iyh(iyh); }

    fast_u8 on_get_iyl() { match_get_r("iyl", get_iyl());
                           return base::on_get_iyl(); }
    void on_set_iyl(fast_u8 iyl) { match_set_r("iyl", get_iyl(), iyl);
                                   return base::on_set_iyl(iyl); }

#if 0  // TODO
    fast_u8 on_get_i() { match_get_r("i", get_i());
                         return base::on_get_i(); }
#endif
    void on_set_i(fast_u8 i) { match_set_r("i", get_i(), i);
                               return base::on_set_a(i); }

    fast_u16 on_get_sp() { match_get_rp("sp", get_sp());
                           return base::on_get_sp(); }
    void on_set_sp(fast_u16 sp) { match_set_rp("sp", get_sp(), sp);
                                  return base::on_set_sp(sp); }

    void on_set_memptr(fast_u16 mp) { match_set_rp("memptr", get_memptr(), mp);
                                      return base::on_set_memptr(mp); }

    void match_get_pc(const char *name) const {
        input.read_and_match("%2u get_pc_on_%s %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(get_pc())); }
    void match_set_pc(const char *name, fast_u16 pc) {
        input.read_and_match("%2u set_pc_on_%s %04x -> %04x",
                             static_cast<unsigned>(get_ticks()), name,
                             static_cast<unsigned>(get_pc()),
                             static_cast<unsigned>(pc)); }

    fast_u16 get_pc_on_fetch() const {
        match_get_pc("fetch");
        return base::get_pc_on_fetch(); }
    void set_pc_on_fetch(fast_u16 pc) {
        match_set_pc("fetch", pc);
        base::set_pc_on_fetch(pc); }

    fast_u16 get_pc_on_imm8_read() const {
        match_get_pc("imm8_read");
        return base::get_pc_on_imm8_read(); }
    void set_pc_on_imm8_read(fast_u16 pc) {
        match_set_pc("imm8_read", pc);
        base::set_pc_on_imm8_read(pc); }

    fast_u16 get_pc_on_imm16_read() const {
        match_get_pc("imm16_read");
        return base::get_pc_on_imm16_read(); }
    void set_pc_on_imm16_read(fast_u16 pc) {
        match_set_pc("imm16_read", pc);
        base::set_pc_on_imm16_read(pc); }

    fast_u16 get_pc_on_disp_read() const {
        match_get_pc("disp_read");
        return base::get_pc_on_disp_read(); }
    void set_pc_on_disp_read(fast_u16 pc) {
        match_set_pc("disp_read", pc);
        base::set_pc_on_disp_read(pc); }

    fast_u16 get_pc_on_jump() const {
        match_get_pc("jump");
        return base::get_pc_on_jump(); }
    void set_pc_on_jump(fast_u16 pc) {
        match_set_pc("jump", pc);
        base::set_pc_on_jump(pc); }

    fast_u16 get_pc_on_block_instr() const {
        match_get_pc("block_instr");
        return base::get_pc_on_block_instr(); }
    void set_pc_on_block_instr(fast_u16 pc) {
        match_set_pc("block_instr", pc);
        base::set_pc_on_block_instr(pc); }

    void set_pc_on_call(fast_u16 pc) {
        match_set_pc("call", pc);
        base::set_pc_on_call(pc); }

    fast_u8 on_fetch_cycle(fast_u16 addr) {
        input.read_and_match("%2u fetch %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_fetch_cycle(addr);
    }

    void on_5t_fetch_cycle() {
        input.read_and_match("%2u 5t_fetch",
                             static_cast<unsigned>(get_ticks()));
        base::on_5t_fetch_cycle();
    }

    void on_6t_fetch_cycle() {
        input.read_and_match("%2u 6t_fetch",
                             static_cast<unsigned>(get_ticks()));
        base::on_6t_fetch_cycle();
    }

    fast_u8 on_3t_read_cycle(fast_u16 addr) {
        input.read_and_match("%2u 3t_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_3t_read_cycle(addr);
    }

    fast_u8 on_4t_read_cycle(fast_u16 addr) {
        input.read_and_match("%2u 4t_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_4t_read_cycle(addr);
    }

    fast_u8 on_5t_read_cycle(fast_u16 addr) {
        input.read_and_match("%2u 5t_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_5t_read_cycle(addr);
    }

    void on_3t_write_cycle(fast_u16 addr, fast_u8 n) {
        input.read_and_match("%2u 3t_write %02x -> %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(n),
                             static_cast<unsigned>(addr));
        base::on_3t_write_cycle(addr, n);
    }

    void on_5t_write_cycle(fast_u16 addr, fast_u8 n) {
        input.read_and_match("%2u 5t_write %02x -> %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(n),
                             static_cast<unsigned>(addr));
        base::on_5t_write_cycle(addr, n);
    }

    void on_5t_pc_exec_cycle() {
        input.read_and_match("%2u 5t_pc_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_5t_pc_exec_cycle();
    }

    void on_3t_ir_exec_cycle() {
        input.read_and_match("%2u 3t_ir_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_3t_ir_exec_cycle();
    }

    void on_4t_ir_exec_cycle() {
        input.read_and_match("%2u 4t_ir_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_4t_ir_exec_cycle();
    }

    void on_5t_de_exec_cycle() {
        input.read_and_match("%2u 5t_de_exec",
                             static_cast<unsigned>(get_ticks()));
        base::on_5t_de_exec_cycle();
    }

    void on_output_cycle(fast_u16 addr, fast_u8 b) {
        input.read_and_match("%2u output %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(b),
                             static_cast<unsigned>(addr));
        base::on_output_cycle(addr, b);
    }

    fast_u8 on_3t_imm8_read() {
        fast_u16 addr = get_pc();
        input.read_and_match("%2u 3t_imm8_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_3t_imm8_read();
    }

    fast_u8 on_5t_imm8_read() {
        fast_u16 addr = get_pc();
        input.read_and_match("%2u 5t_imm8_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_5t_imm8_read();
    }

    fast_u16 on_imm16_read(bool long_second_cycle = false) {
        fast_u16 addr = get_pc();
        fast_u16 v = z80::make16(on_access(z80::inc16(addr)), on_access(addr));
        input.read_and_match("%2u imm16_read %04x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(v),
                             static_cast<unsigned>(addr));
        return base::on_imm16_read(long_second_cycle);
    }

    fast_u8 on_disp_read() {
        fast_u16 addr = get_pc();
        input.read_and_match("%2u disp_read %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(on_access(addr)),
                             static_cast<unsigned>(addr));
        return base::on_disp_read();
    }

    void set_iff1_on_di(bool iff1) {
        input.read_and_match("%2u set_iff1_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff1()),
                             static_cast<unsigned>(iff1));
        base::set_iff1_on_di(iff1);
    }

    void set_iff2_on_di(bool iff2) {
        input.read_and_match("%2u set_iff2_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff2()),
                             static_cast<unsigned>(iff2));
        base::set_iff2_on_di(iff2);
    }

    void set_iff1_on_ei(bool iff1) {
        input.read_and_match("%2u set_iff1_on_ei %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff1()),
                             static_cast<unsigned>(iff1));
        base::set_iff1_on_ei(iff1);
    }

    void set_iff2_on_ei(bool iff2) {
        input.read_and_match("%2u set_iff2_on_ei %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff2()),
                             static_cast<unsigned>(iff2));
        base::set_iff2_on_ei(iff2);
    }

    void on_set_int_mode(unsigned mode) {
        input.read_and_match("%2u set_int_mode %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_int_mode()),
                             static_cast<unsigned>(mode));
        base::on_set_int_mode(mode);
    }

    void on_ed_prefix() {
        input.read_and_match("%2u ed_prefix",
                             static_cast<unsigned>(get_ticks()));
        base::on_ed_prefix();
    }

    void on_prefix_reset() {
        input.read_and_match("%2u prefix_reset",
                             static_cast<unsigned>(get_ticks()));
        base::on_prefix_reset();
    }

    void on_disable_int() {
        input.read_and_match("%2u disable_int",
                             static_cast<unsigned>(get_ticks()));
        base::on_disable_int();
    }

    void on_set_next_index_rp(z80::index_regp irp) {
        input.read_and_match("%2u set_next_index_rp %s -> %s",
                             static_cast<unsigned>(get_ticks()),
                             get_reg_name(get_index_rp_kind()),
                             get_reg_name(irp));
        base::on_set_next_index_rp(irp);
    }

    void step() {
        // Skip prefixes.
        base::step();
        while(get_prefix() != z80::instruction_prefix::none ||
              get_next_index_rp_kind() != z80::index_regp::hl)
            base::step();

        input.read_and_match("%2u done", static_cast<unsigned>(get_ticks()));
    }

private:
    ticks_type ticks;

    test_input &input;

    static const z80::size_type image_size = 0x10000;  // 64K bytes.
    least_u8 image[image_size];
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

void handle_test_entry(test_input &input) {
    // Parse instruction bytes.
    least_u8 instr_code[max_instr_size];
    unsigned instr_size = 0;
    const char *p = input.get_line();
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
    disassembler disasm;
    disasm.set_instr_code(instr_code, instr_size);
    disasm.disassemble();
    const char *instr = disasm.get_output();
    if(std::strcmp(instr, p) != 0)
        input.error("instruction disassembly mismatch: '%s' vs '%s'",
                    instr, p);

    machine mach(input);
    mach.set_instr_code(instr_code, instr_size);
    mach.step();
}

}  // anonymous namespace

int main(int argc, char *argv[]) {
    if(argc != 2)
        error("usage: tester <test-input>");

    FILE *f = fopen(argv[1], "r");
    if(!f)
        error("cannot open test input '%s': %s", argv[1],
              std::strerror(errno));

    test_input input(f);
    while(input) {
        // Skip empty lines and comments.
        const char *line = input.read_line();
        if(line[0] == '\0' || line[0] == '#')
            continue;

        handle_test_entry(input);
    }

    if(fclose(f) != 0)
        error("cannot close test input '%s': %s", argv[1],
              std::strerror(errno));
}
