
/*  Z80 CPU Simulator.
    https://github.com/kosarev/z80

    Copyright (C) 2017-2019 Ivan Kosarev.
    ivan@kosarev.info

    Published under the MIT license.
*/

#include "z80.h"

namespace z80 {

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

}  // namespace z80
