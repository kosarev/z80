
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
using z80::least_u8;

using z80::fast_u16;

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

static const unsigned max_instr_size = 1;

class disassembler : public z80::instructions_decoder<disassembler>,
                     public z80::disassembler<disassembler> {
public:
    disassembler()
        : index(0), instr_size(0)
    {}

    const char *get_output() const {
        return output_buff;
    }

    void output(const char *str) {
        std::snprintf(output_buff, max_output_buff_size, "%s", str);
    }

    fast_u8 fetch_next_opcode() {
        assert(index < instr_size);
        return instr_code[index++];
    }

    fast_u16 get_last_fetch_addr() const {
        assert(index != 0);
        return index - 1;
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        assert(size <= max_instr_size);
        std::memcpy(instr_code, code, size);
        instr_size = size;
        index = 0;
    }

private:
    unsigned index;
    least_u8 instr_code[max_instr_size];
    unsigned instr_size;

    static const std::size_t max_output_buff_size = 32;
    char output_buff[max_output_buff_size];
};

class machine : public z80::instructions_decoder<machine>,
                public z80::processor<machine> {
public:
    typedef processor<machine> processor;
    typedef uint_fast32_t ticks_type;

    machine(test_input &input)
        : ticks(0), input(input)
    {}

    void tick(unsigned t) { ticks += t; }

    ticks_type get_ticks() const { return ticks; }

    least_u8 &at(fast_u16 addr) {
        assert(addr < image_size);
        return image[addr];
    }

    void set_instr_code(const least_u8 *code, unsigned size) {
        fast_u16 pc = get_pc();
        for(unsigned i = 0; i != size; ++i)
            at(z80::add16(pc, i)) = code[i];
    }

    fast_u16 get_pc_on_fetch() const {
        input.read_and_match("%2u get_pc_on_fetch %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_pc()));
        return processor::get_pc_on_fetch();
    }

    void set_pc_on_fetch(fast_u16 pc) {
        input.read_and_match("%2u set_pc_on_fetch %04x -> %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_pc()),
                             static_cast<unsigned>(pc));
        processor::set_pc_on_fetch(pc);
    }

    fast_u8 fetch_opcode(fast_u16 addr) {
        input.read_and_match("%2u fetch %02x at %04x",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(at(addr)),
                             static_cast<unsigned>(addr));
        return processor::fetch_opcode(addr);
    }

    void set_iff1_on_di(bool iff1) {
        input.read_and_match("%2u set_iff1_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff1()),
                             static_cast<unsigned>(iff1));
        processor::set_iff1_on_di(iff1);
    }

    void set_iff2_on_di(bool iff2) {
        input.read_and_match("%2u set_iff2_on_di %u -> %u",
                             static_cast<unsigned>(get_ticks()),
                             static_cast<unsigned>(get_iff2()),
                             static_cast<unsigned>(iff2));
        processor::set_iff2_on_di(iff2);
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
