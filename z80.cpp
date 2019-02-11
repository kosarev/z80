
/*  Z80 CPU Simulator.

    Copyright (C) 2017 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

namespace z80 {

const char *get_reg_name(reg r, index_regp irp) {
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

const char *get_reg_name(regp rp, index_regp irp) {
    switch(rp) {
    case regp::bc: return "bc";
    case regp::de: return "de";
    case regp::hl: return get_reg_name(irp);
    case regp::sp: return "sp";
    }
    assert(0);
}

const char *get_reg_name(regp2 rp, index_regp irp) {
    switch(rp) {
    case regp2::bc: return "bc";
    case regp2::de: return "de";
    case regp2::hl: return get_reg_name(irp);
    case regp2::af: return "af";
    }
    assert(0);
}

const char *get_reg_name(index_regp irp) {
    switch(irp) {
    case index_regp::hl: return "hl";
    case index_regp::ix: return "ix";
    case index_regp::iy: return "iy";
    }
    assert(0);
}

const char *get_mnemonic(alu k) {
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

const char *get_mnemonic(rot k) {
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
    assert(0);
}

const char *get_mnemonic(block_ld k) {
    switch(k) {
    case block_ld::ldi: return "ldi";
    case block_ld::ldd: return "ldd";
    case block_ld::ldir: return "ldir";
    case block_ld::lddr: return "lddr";
    }
    assert(0);
}

const char *get_mnemonic(block_cp k) {
    switch(k) {
    case block_cp::cpi: return "cpi";
    case block_cp::cpd: return "cpd";
    case block_cp::cpir: return "cpir";
    case block_cp::cpdr: return "cpdr";
    }
    assert(0);
}


bool is_two_operand_alu_instr(alu k) {
    return k == alu::add || k == alu::adc || k == alu::sbc;
}

const char *get_index_reg_name(index_regp irp) {
    switch(irp) {
    case index_regp::hl: return "hl";
    case index_regp::ix: return "ix";
    case index_regp::iy: return "iy";
    }
    assert(0);
}

const char *get_condition_name(condition cc) {
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
    unsigned size;
    char buff[max_size];
};

template<typename T>
T get_arg(const void **&args) {
    const T &value = *static_cast<const T*>(*args);
    ++args;
    return value;
}

}  // anonymous namespace

void disassembler_base::on_format_impl(const char *fmt, const void *args[]) {
    output_buff out;
    for(const char *p = fmt; *p != '\0'; ++p) {
        switch(*p) {
        case 'A': {  // ALU mnemonic.
            auto k = get_arg<alu>(args);
            out.append(get_mnemonic(k));
            if(is_two_operand_alu_instr(k))
                out.append(" a,");
            break; }
        case 'O': {  // Rotation mnemonic.
            auto k = get_arg<rot>(args);
            out.append(get_mnemonic(k));
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
        case 'W': {  // A 16-bit immediate operand.
            auto nn = get_arg<fast_u16>(args);
            out.append_u16(nn);
            break; }
        case 'U': {  // A decimal number.
            auto u = get_arg<unsigned>(args);
            out.append_u(u);
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
        case 'L': {  // A block transfer instruction.
            auto k = get_arg<block_ld>(args);
            out.append(get_mnemonic(k));
            break; }
        case 'M': {  // A block comparison instruction.
            auto k = get_arg<block_cp>(args);
            out.append(get_mnemonic(k));
            break; }
        default:
            out.append(*p);
        }
    }
    out.append('\0');

    on_output(out.get_buff());
}

}  // namespace z80
