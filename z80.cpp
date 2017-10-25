
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include <cstdarg>

#include "z80.h"

namespace z80 {

const char *get_reg_name(reg r) {
    switch(r) {
    case reg::b: return "b";
    case reg::c: return "c";
    case reg::d: return "d";
    case reg::e: return "e";
    case reg::h: return "h";
    case reg::l: return "l";
    case reg::at_hl: return "(hl)";
    case reg::a: return "a";
    }
    assert(0);
}

const char *get_reg_name(regp r) {
    switch(r) {
    case regp::bc: return "bc";
    case regp::de: return "de";
    case regp::hl: assert(0); break; // TODO
    case regp::sp: return "sp";
    }
    assert(0);
}

namespace {

class output_buff {
public:
    output_buff()
        : size(0)
    {}

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

private:
    static const unsigned max_size = 32;
    unsigned size;
    char buff[max_size];
};

const char *get_alu_mnemonic(alu k) {
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
    assert(0);
}

bool is_two_operand_alu_instr(alu k) {
    return k == alu::add || k == alu::adc || k == alu::sbc;
}

const char *get_index_reg_name(index_regp ip) {
    switch(ip) {
    case index_regp::hl: return "hl";
    case index_regp::ix: return "ix";
    case index_regp::iy: return "iy";
    }
    assert(0);
}

}  // anonymous namespace

void disassembler_base::on_format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    output_buff out;
    for(const char *p = fmt; *p != '\0'; ++p) {
        switch(*p) {
        case 'A': {  // ALU mnemonic.
            auto k = static_cast<alu>(va_arg(args, int));
            out.append(get_alu_mnemonic(k));
            if(is_two_operand_alu_instr(k))
                out.append(" a,");
            break; }
        case 'R': {  // A register.
            auto r = static_cast<reg>(va_arg(args, int));
            auto ip = static_cast<index_regp>(va_arg(args, int));
            auto d = static_cast<fast_u8>(va_arg(args, int));
            if(r != reg::at_hl || ip == index_regp::hl) {
                out.append(get_reg_name(r));
            } else {
                out.append('(');
                out.append(get_index_reg_name(ip));
                out.append(!get_sign8(d) ? '+' : '-');
                out.append(abs8(d));
                out.append(')');
            }
            break; }
        case 'P': {  // A register pair.
            auto rp = static_cast<regp>(va_arg(args, int));
            out.append(get_reg_name(rp));
            break; }
        case 'N': {  // An 8-bit immediate operand.
            auto n = static_cast<fast_u8>(va_arg(args, unsigned));
            char buff[32];
            std::snprintf(buff, sizeof(buff), "0x%02x",
                          static_cast<unsigned>(n));
            out.append(buff);
            break; }
        case 'W': {  // A 16-bit immediate operand.
            auto nn = static_cast<fast_u16>(va_arg(args, unsigned));
            char buff[32];
            std::snprintf(buff, sizeof(buff), "0x%04x",
                          static_cast<unsigned>(nn));
            out.append(buff);
            break; }
        default:
            out.append(*p);
        }
    }
    va_end(args);
    out.append('\0');

    on_output(out.get_buff());
}

}  // namespace z80
