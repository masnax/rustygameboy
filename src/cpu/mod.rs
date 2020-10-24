mod instructions;
mod cb_instructions;
use crate::memory::Memory;
use crate::register::{ALUFlag, REG::*, Register};
use instructions::InstructionSet;
use cb_instructions::CBInstructionSet;
//single core
pub struct Cpu<'a> {
    instr: InstructionSet,
    mem: &'a mut Memory,
}


impl<'a> Cpu<'a> {
    pub fn init(registers: Register, mem: &'a mut Memory) -> Cpu<'a> {
        Cpu {
            instr: InstructionSet::init(registers),
            mem,
        }
    }

    pub fn cycle(&mut self, cycles: u16) -> u16 {
        return self.exec() + self.mem.lcdc.cycle(cycles);
    }

    pub fn get_frame(&self) -> &[u32]  {
        self.mem.lcdc.get_background()
    }

    // Returns number of cycles
    fn exec(&mut self) -> u16 {
        let opcode = if self.instr.halted {0} else {self.instr.fetch(self.mem)};
        match opcode {
            // NOP
            0x00 => { 1 },
            // LD BC, d16
            0x01 => { self.instr.ld_rw(self.mem, BC); 3 },
            // LD (BC), A
            0x02 => { self.instr.ld_mr(self.mem, BC, A); 2 },
            // INC BC
            0x03 => { self.instr.inc(BC); 2 },
            // INC B
            0x04 => { self.instr.inc(B); 1 },
            // DEC B
            0x05 => { self.instr.dec(B); 1 },
            // LD B, u8
            0x06 => { self.instr.ld_rb(self.mem, B); 2 },
            // RLCA
            0x07 => { self.instr.rlca(A); 1 },
            // LD (u16), SP
            0x08 => { self.instr.ld_mw(self.mem, SP); 5 },
            // ADD HL, BC
            0x09 => { self.instr.add(self.mem, HL, BC); 2 },
            // LD A, (BC)
            0x0A => { self.instr.ld_rm(self.mem, A, BC); 2 },
            // DEC BC
            0x0B => { self.instr.dec(BC); 2 },
            // INC C
            0x0C => { self.instr.inc(C); 1 },
            // DEC C
            0x0D => { self.instr.dec(C); 1 },
            // LD C, u8
            0x0E => { self.instr.ld_rb(self.mem, C); 2 },
            // RRCA
            0x0F => { self.instr.rrca(A); 1 },
            // STOP
            0x10 => { self.instr.stop(); 1 },
            // LD DE, U16
            0x11 => { self.instr.ld_rw(self.mem, DE); 3 },
            // LD (DE), A
            0x12 => { self.instr.ld_mr(self.mem, DE, A); 2 },
            // INC DE
            0x13 => { self.instr.inc(DE); 2 },
            // INC D
            0x14 => { self.instr.inc(D); 1 },
            // DEC D
            0x15 => { self.instr.dec(D); 1 },
            // LD D, u8
            0x16 => { self.instr.ld_rb(self.mem, D); 2 },
            // RLA
            0x17 => { self.instr.rla(A); 1 },
            // JR s8
            0x18 => { self.instr.jr(self.mem, true) },
            // ADD HL, DE
            0x19 => { self.instr.add(self.mem, HL, DE); 2 },
            // LD A, (DE)
            0x1A => { self.instr.ld_rm(self.mem, A, DE); 2 },
            // DEC DE
            0x1B => { self.instr.dec(DE); 2 },
            // INC E
            0x1C => { self.instr.inc(E); 1 },
            // DEC E
            0x1D => { self.instr.dec(E); 1 },
            // LD E, u8
            0x1E => { self.instr.ld_rb(self.mem, E); 2 },
            // RRA
            0x1F => { self.instr.rra(A); 1 },
            // JR NZ, s8
            0x20 => { self.instr.jr(self.mem, !self.instr.get_flag(ALUFlag::Z)) },
            // LD HL, U16
            0x21 => { self.instr.ld_rw(self.mem, HL); 3 },
            // LD (HL+), A
            0x22 => { self.instr.ld_mem_inc(self.mem); 2 },
            // INC HL
            0x23 => { self.instr.inc(HL); 2 },
            // INC H
            0x24 => { self.instr.inc(H); 1 },
            // DEC H
            0x25 => { self.instr.dec(H); 1 },
            // LD H, u8
            0x26 => { self.instr.ld_rb(self.mem, H); 2 },
            // DAA
            0x27 => { self.instr.daa(); 1 },
            // JR Z, s8
            0x28 => { self.instr.jr(self.mem, self.instr.get_flag(ALUFlag::Z)) },
            // ADD HL, HL
            0x29 => { self.instr.add(self.mem, HL, HL); 2 },
            // LD A, (HL+)
            0x2A => { self.instr.ld_inc(self.mem); 2 },
            // DEC HL
            0x2B => { self.instr.dec(HL); 2 },
            // INC L
            0x2C => { self.instr.inc(L); 1 },
            // DEC L
            0x2D => { self.instr.dec(L); 1 },
            // LD L, u8
            0x2E => { self.instr.ld_rb(self.mem, L); 2 },
            // CPL
            0x2F => { self.instr.cpl(); 1 },
            // JR NC, s8
            0x30 => { self.instr.jr(self.mem, !self.instr.get_flag(ALUFlag::C)) },
            // LD SP, U16
            0x31 => { self.instr.ld_rw(self.mem, SP); 3 },
            // LD (HL-), A
            0x32 => { self.instr.ld_mem_dec(self.mem); 2 },
            // INC SP
            0x33 => { self.instr.inc(SP); 2 },
            // INC (HL)
            0x34 => { self.instr.inc_mem(self.mem); 3 },
            // DEC (HL)
            0x35 => { self.instr.dec_mem(self.mem); 3 },
            // LD (HL), u8
            0x36 => { self.instr.ld_mb(self.mem, HL); 3 },
            // SCF
            0x37 => { self.instr.scf(); 1 },
            // JR C, s8
            0x38 => { self.instr.jr(self.mem, self.instr.get_flag(ALUFlag::C)) },
            // ADD HL, SP
            0x39 => { self.instr.add(self.mem, HL, SP); 2 },
            // LD A, (HL-)
            0x3A => { self.instr.ld_dec(self.mem); 2 },
            // DEC SP
            0x3B => { self.instr.dec(SP); 2 },
            // INC A
            0x3C => { self.instr.inc(A); 1 },
            // DEC A
            0x3D => { self.instr.dec(A); 1 },
            // LD A, u8
            0x3E => { self.instr.ld_rb(self.mem, A); 2 },
            // CCF
            0x3F => { self.instr.ccf(); 1 },
            // LD B, B
            0x40 => { self.instr.ld_rr(B, B); 1 },
            // LD B, C
            0x41 => { self.instr.ld_rr(B, C); 1 },
            // LD B, D
            0x42 => { self.instr.ld_rr(B, D); 1 },
            // LD B, E
            0x43 => { self.instr.ld_rr(B, E); 1 },
            // LD B, H
            0x44 => { self.instr.ld_rr(B, H); 1 },
            // LD B, L
            0x45 => { self.instr.ld_rr(B, L); 1 },
            // LD B, (HL)
            0x46 => { self.instr.ld_rm(self.mem, B, HL); 2 },
            // LD B, A
            0x47 => { self.instr.ld_rr(B, A); 1 },
            // LD C, B
            0x48 => { self.instr.ld_rr(C, B); 1 },
            // LD C, C
            0x49 => { self.instr.ld_rr(C, C); 1 },
            // LD C, D
            0x4A => { self.instr.ld_rr(C, D); 1 },
            // LD C, E
            0x4B => { self.instr.ld_rr(C, E); 1 },
            // LD C, H
            0x4C => { self.instr.ld_rr(C, H); 1 },
            // LD C, L
            0x4D => { self.instr.ld_rr(C, L); 1 },
            // LD C, (HL)
            0x4E => { self.instr.ld_rm(self.mem, C, HL); 2 },
            // LD C, A
            0x4F => { self.instr.ld_rr(C, A); 1 },
            // LD D, B
            0x50 => { self.instr.ld_rr(D, B); 1 },
            // LD D, C
            0x51 => { self.instr.ld_rr(D, C); 1 },
            // LD D, D
            0x52 => { self.instr.ld_rr(D, D); 1 },
            // LD D, E
            0x53 => { self.instr.ld_rr(D, E); 1 },
            // LD D, H
            0x54 => { self.instr.ld_rr(D, H); 1 },
            // LD D, L
            0x55 => { self.instr.ld_rr(D, L); 1 },
            // LD D, (HL)
            0x56 => { self.instr.ld_rm(self.mem, D, HL); 2 },
            // LD D, A
            0x57 => { self.instr.ld_rr(D, A); 1 },
            // LD E, B
            0x58 => { self.instr.ld_rr(E, B); 1 },
            // LD E, C
            0x59 => { self.instr.ld_rr(E, C); 1 },
            // LD E, D
            0x5A => { self.instr.ld_rr(E, D); 1 },
            // LD E, E
            0x5B => { self.instr.ld_rr(E, E); 1 },
            // LD E, H
            0x5C => { self.instr.ld_rr(E, H); 1 },
            // LD E, L
            0x5D => { self.instr.ld_rr(E, L); 1 },
            // LD E, (HL)
            0x5E => { self.instr.ld_rm(self.mem, E, HL); 2 },
            // LD E, A
            0x5F => { self.instr.ld_rr(E, A); 1 },
            // LD H, B
            0x60 => { self.instr.ld_rr(H, B); 1 },
            // LD H, C
            0x61 => { self.instr.ld_rr(H, C); 1 },
            // LD H, D
            0x62 => { self.instr.ld_rr(H, D); 1 },
            // LD H, E
            0x63 => { self.instr.ld_rr(H, E); 1 },
            // LD H, H
            0x64 => { self.instr.ld_rr(H, H); 1 },
            // LD H, L
            0x65 => { self.instr.ld_rr(H, L); 1 },
            // LD H, (HL)
            0x66 => { self.instr.ld_rm(self.mem, H, HL); 2 },
            // LD H, A
            0x67 => { self.instr.ld_rr(H, A); 1 },
            // LD L, B
            0x68 => { self.instr.ld_rr(L, B); 1 },
            // LD L, C
            0x69 => { self.instr.ld_rr(L, C); 1 },
            // LD L, D
            0x6A => { self.instr.ld_rr(L, D); 1 },
            // LD L, E
            0x6B => { self.instr.ld_rr(L, E); 1 },
            // LD L, H
            0x6C => { self.instr.ld_rr(L, H); 1 },
            // LD L, L
            0x6D => { self.instr.ld_rr(L, L); 1 },
            // LD L, (HL)
            0x6E => { self.instr.ld_rm(self.mem, L, HL); 2 },
            // LD L, A
            0x6F => { self.instr.ld_rr(L, A); 1 },
            // LD (HL), B
            0x70 => { self.instr.ld_mr(self.mem, HL, B); 2 },
            // LD (HL), C
            0x71 => { self.instr.ld_mr(self.mem, HL, C); 2 },
            // LD (HL), D
            0x72 => { self.instr.ld_mr(self.mem, HL, D); 2 },
            // LD (HL), E
            0x73 => { self.instr.ld_mr(self.mem, HL, E); 2 },
            // LD (HL), H
            0x74 => { self.instr.ld_mr(self.mem, HL, H); 2 },
            // LD (HL), L
            0x75 => { self.instr.ld_mr(self.mem, HL, L); 2 },
            // HALT
            0x76 => { self.instr.halt(); 1 },
            // LD (HL), A
            0x77 => { self.instr.ld_mr(self.mem, HL, A); 2 },
            // LD A, B
            0x78 => { self.instr.ld_rr(A, B); 1 },
            // LD A, C
            0x79 => { self.instr.ld_rr(A, C); 1 },
            // LD A, D
            0x7A => { self.instr.ld_rr(A, D); 1 },
            // LD A, E
            0x7B => { self.instr.ld_rr(A, E); 1 },
            // LD A, H
            0x7C => { self.instr.ld_rr(A, H); 1 },
            // LD A, L
            0x7D => { self.instr.ld_rr(A, L); 1 },
            // LD A, (HL)
            0x7E => { self.instr.ld_rm(self.mem, A, HL); 2 },
            // LD A, A
            0x7F => { 1 },
            // ADD A, B
            0x80 => { self.instr.add(self.mem, A, B); 2 },
            // ADD A, C
            0x81 => { self.instr.add(self.mem, A, C); 2 },
            // ADD A, D
            0x82 => { self.instr.add(self.mem, A, D); 2 },
            // ADD A, E
            0x83 => { self.instr.add(self.mem, A, E); 2 },
            // ADD A, H
            0x84 => { self.instr.add(self.mem, A, H); 2 },
            // ADD A, L
            0x85 => { self.instr.add(self.mem, A, L); 2 },
            // ADD A, (HL)
            0x86 => { self.instr.add(self.mem, A, HL); 2 },
            // ADD A, A
            0x87 => { self.instr.add(self.mem, A, A); 2 },
            // ADC A, B
            0x88 => { self.instr.adc(self.mem, B); 2 },
            // ADC A, C
            0x89 => { self.instr.adc(self.mem, C); 2 },
            // ADC A, D
            0x8A => { self.instr.adc(self.mem, D); 2 },
            // ADC A, E
            0x8B => { self.instr.adc(self.mem, E); 2 },
            // ADC A, H
            0x8C => { self.instr.adc(self.mem, H); 2 },
            // ADC A, L
            0x8D => { self.instr.adc(self.mem, L); 2 },
            // ADC A, (HL)
            0x8E => { self.instr.adc(self.mem, HL); 2 },
            // ADC A, A
            0x8F => { self.instr.adc(self.mem, A); 2 },
            // SUB B
            0x90 => { self.instr.sub(self.mem, A, B); 1 },
            // SUB C
            0x91 => { self.instr.sub(self.mem, A, C); 1 },
            // SUB D
            0x92 => { self.instr.sub(self.mem, A, D); 1 },
            // SUB E
            0x93 => { self.instr.sub(self.mem, A, E); 1 },
            // SUB H
            0x94 => { self.instr.sub(self.mem, A, H); 1 },
            // SUB L
            0x95 => { self.instr.sub(self.mem, A, L); 1 },
            // SUB (HL)
            0x96 => { self.instr.sub(self.mem, A, HL); 2 },
            // SUB A
            0x97 => { self.instr.sub(self.mem, A, A); 1 },
            // SBC A, B
            0x98 => { self.instr.sbc(self.mem, B); 1 },
            // SBC A, C
            0x99 => { self.instr.sbc(self.mem, C); 1 },
            // SBC A, D
            0x9A => { self.instr.sbc(self.mem, D); 1 },
            // SBC A, E
            0x9B => { self.instr.sbc(self.mem, E); 1 },
            // SBC A, H
            0x9C => { self.instr.sbc(self.mem, H); 1 },
            // SBC A, L
            0x9D => { self.instr.sbc(self.mem, L); 1 },
            // SBC A, (HL)
            0x9E => { self.instr.sbc(self.mem, HL); 2 },
            // SBC A, A
            0x9F => { self.instr.sbc(self.mem, A); 1 },
            // AND B
            0xA0 => { self.instr.and(self.mem, B); 1 },
            // AND C
            0xA1 => { self.instr.and(self.mem, C); 1 },
            // AND D
            0xA2 => { self.instr.and(self.mem, D); 1 },
            // AND E
            0xA3 => { self.instr.and(self.mem, E); 1 },
            // AND H
            0xA4 => { self.instr.and(self.mem, H); 1 },
            // AND L
            0xA5 => { self.instr.and(self.mem, L); 1 },
            // AND (HL)
            0xA6 => { self.instr.and(self.mem, HL); 2 },
            // AND A
            0xA7 => { self.instr.and(self.mem, A); 1 },
            // XOR B
            0xA8 => { self.instr.xor(self.mem, B); 1 },
            // XOR C
            0xA9 => { self.instr.xor(self.mem, C); 1 },
            // XOR D
            0xAA => { self.instr.xor(self.mem, D); 1 },
            // XOR E
            0xAB => { self.instr.xor(self.mem, E); 1 },
            // XOR H
            0xAC => { self.instr.xor(self.mem, H); 1 },
            // XOR L
            0xAD => { self.instr.xor(self.mem, L); 1 },
            // XOR (HL)
            0xAE => { self.instr.xor(self.mem, HL); 2 },
            // XOR A
            0xAF => { self.instr.xor(self.mem, A); 1 },
            // OR B
            0xB0 => { self.instr.or(self.mem, B); 1 },
            // OR C
            0xB1 => { self.instr.or(self.mem, C); 1 },
            // OR D
            0xB2 => { self.instr.or(self.mem, D); 1 },
            // OR E
            0xB3 => { self.instr.or(self.mem, E); 1 },
            // OR H
            0xB4 => { self.instr.or(self.mem, H); 1 },
            // OR L
            0xB5 => { self.instr.or(self.mem, L); 1 },
            // OR (HL)
            0xB6 => { self.instr.or(self.mem, HL); 2 },
            // OR A
            0xB7 => { self.instr.or(self.mem, A); 1 },
            // CP B
            0xB8 => { self.instr.cp(self.mem, B); 1 },
            // CP C
            0xB9 => { self.instr.cp(self.mem, C); 1 },
            // CP D
            0xBA => { self.instr.cp(self.mem, D); 1 },
            // CP E
            0xBB => { self.instr.cp(self.mem, E); 1 },
            // CP H
            0xBC => { self.instr.cp(self.mem, H); 1 },
            // CP L
            0xBD => { self.instr.cp(self.mem, L); 1 },
            // CP (HL)
            0xBE => { self.instr.cp(self.mem, HL); 2 },
            // CP A
            0xBF => { self.instr.cp(self.mem, A); 1 },
            // RET NZ
            0xC0 => { self.instr.retc(self.mem, !self.instr.get_flag(ALUFlag::Z)) },
            // POP BC
            0xC1 => { self.instr.pop(self.mem, BC); 3 },
            // JP NZ, a16
            0xC2 => { self.instr.jp(self.mem, !self.instr.get_flag(ALUFlag::Z)) },
            // JP a16
            0xC3 => { self.instr.jp(self.mem, true) },
            // CALL NZ, a16
            0xC4 => { self.instr.call(self.mem, !self.instr.get_flag(ALUFlag::Z)) },
            // PUSH BC
            0xC5 => { self.instr.push(self.mem, BC); 4 },
            // ADD A, d8
            0xC6 => { self.instr.add_val(self.mem, A); 2 },
            // RST 0
            0xC7 => { self.instr.rst(self.mem, 0); 4 },
            // RET Z
            0xC8 => { self.instr.retc(self.mem, self.instr.get_flag(ALUFlag::Z)) },
            // RET
            0xC9 => { self.instr.ret(self.mem); 4 },
            // JP Z, a16
            0xCA => { self.instr.jp(self.mem, self.instr.get_flag(ALUFlag::Z)) },
            // CB Instruction
            0xCB => { self.exec_cb() },
            // CALL Z, a16
            0xCC => { self.instr.call(self.mem, self.instr.get_flag(ALUFlag::Z)) },
            // CALL a16
            0xCD => { self.instr.call(self.mem, true) },
            // ADC d8
            0xCE => { self.instr.adc_val(self.mem); 2 },
            // RST 1
            0xCF => { self.instr.rst(self.mem, 1); 4 },
            // RET NC
            0xD0 => { self.instr.retc(self.mem, !self.instr.get_flag(ALUFlag::C)) },
            // POP DE
            0xD1 => { self.instr.pop(self.mem, DE); 3 },
            // JP NC, a16
            0xD2 => { self.instr.jp(self.mem, !self.instr.get_flag(ALUFlag::C)) },
            // CALL NC, a16
            0xD4 => { self.instr.call(self.mem, !self.instr.get_flag(ALUFlag::C)) },
            // PUSH DE
            0xD5 => { self.instr.push(self.mem, DE); 4 },
            // SUB d8
            0xD6 => { self.instr.sub_val(self.mem, A); 2 },
            // RST 2
            0xD7 => { self.instr.rst(self.mem, 2); 4 },
            // RET C
            0xD8 => { self.instr.retc(self.mem, self.instr.get_flag(ALUFlag::C)) },
            // RETI
            0xD9 => { self.instr.reti(self.mem); 4 },
            // JP C, a16
            0xDA => { self.instr.jp(self.mem, self.instr.get_flag(ALUFlag::C)) },
            // CALL C, a16
            0xDC => { self.instr.call(self.mem, self.instr.get_flag(ALUFlag::C)) },
            // SBC d8
            0xDE => { self.instr.sbc_val(self.mem); 2 },
            // RST 3
            0xDF => { self.instr.rst(self.mem, 3); 4 },
            // LD (a8), A
            0xE0 => { self.instr.ld_mbr(self.mem, A); 3 },
            // POP HL
            0xE1 => { self.instr.pop(self.mem, HL); 3 },
            // LD (C), A
            0xE2 => { self.instr.ld_mrc(self.mem); 2 },
            // PUSH HL
            0xE5 => { self.instr.push(self.mem, HL); 4 },
            // AND d8
            0xE6 => { self.instr.and_val(self.mem); 2 },
            // RST 4
            0xE7 => { self.instr.rst(self.mem, 4); 4 },
            // ADD SP, s8
            0xE8 => { self.instr.add_stack(self.mem); 4 },
            // JP (HL)
            0xE9 => { self.instr.jp_hl(); 4 },
            // LD (a16), A
            0xEA => { self.instr.ld_mwr(self.mem, A); 4 },
            // XOR d8
            0xEE => { self.instr.xor_val(self.mem); 2 },
            // RST 5
            0xEF => { self.instr.rst(self.mem, 5); 4 },
            // LD A, (a8)
            0xF0 => { self.instr.ld_rmb(self.mem, A); 3 },
            // POP AF
            0xF1 => { self.instr.pop(self.mem, AF); 3 },
            // LD A, (C)
            0xF2 => { self.instr.ld_rmc(self.mem); 2 },
            // DI
            0xF3 => { self.instr.di(); 1 },
            // PUSH AF
            0xF5 => { self.instr.push(self.mem, AF); 4 },
            // OR d8
            0xF6 => { self.instr.or_val(self.mem); 2 },
            // RST 6
            0xF7 => { self.instr.rst(self.mem, 6); 4 },
            // LD HL, SP+s8
            0xF8 => { self.instr.add_stack(self.mem); 3 },
            // LD SP, HL
            0xF9 => { self.instr.ld_rrw(SP, HL); 2 },
            // LD A, (a16)
            0xFA => { self.instr.ld_rmw(self.mem, A); 4 },
            // EI
            0xFB => {self.instr.ei(); 1 },
            // CP d8
            0xFE => { self.instr.cp_val(self.mem); 2 },
            // RST 7
            0xFF => { self.instr.rst(self.mem, 7); 4 },
            _other => { panic!("[ERROR] invalid opcode") }

        }
    }

    fn exec_cb(&mut self) -> u16 {
        let opcode = self.instr.fetch(self.mem);
        match opcode {
            // RLC B
            0x00 => { self.instr.rlc(B); 2 },
            // RLC C
            0x01 => { self.instr.rlc(C); 2 },
            // RLC D
            0x02 => { self.instr.rlc(D); 2 },
            // RLC E
            0x03 => { self.instr.rlc(E); 2 },
            // RLC H
            0x04 => { self.instr.rlc(H); 2 },
            // RLC L
            0x05 => { self.instr.rlc(L); 2 },
            // RLC (HL)
            0x06 => { self.instr.rlc_mem(self.mem, HL); 4 },
            // RLC A
            0x07 => { self.instr.rlc(A); 2 },
            // RRC B
            0x08 => { self.instr.rrc(B); 2 },
            // RRC C
            0x09 => { self.instr.rrc(C); 2 },
            // RRC D
            0x0A => { self.instr.rrc(D); 2 },
            // RRC E
            0x0B => { self.instr.rrc(E); 2 },
            // RRC H
            0x0C => { self.instr.rrc(H); 2 },
            // RRC L
            0x0D => { self.instr.rrc(L); 2 },
            // RRC (HL)
            0x0E => { self.instr.rrc_mem(self.mem, HL); 4 },
            // RRC A
            0x0F => { self.instr.rrc(A); 2 },
            // RL B
            0x10 => { self.instr.rl(B); 2 },
            // RL C
            0x11 => { self.instr.rl(C); 2 },
            // RL D
            0x12 => { self.instr.rl(D); 2 },
            // RL E
            0x13 => { self.instr.rl(E); 2 },
            // RL H
            0x14 => { self.instr.rl(H); 2 },
            // RL L
            0x15 => { self.instr.rl(L); 2 },
            // RL (HL)
            0x16 => { self.instr.rl_mem(self.mem, HL); 4 },
            // RL A
            0x17 => { self.instr.rl(A); 2 },
            // RR B
            0x18 => { self.instr.rr(B); 2 },
            // RR C
            0x19 => { self.instr.rr(C); 2 },
            // RR D
            0x1A => { self.instr.rr(D); 2 },
            // RR E
            0x1B => { self.instr.rr(E); 2 },
            // RR H
            0x1C => { self.instr.rr(H); 2 },
            // RR L
            0x1D => { self.instr.rr(L); 2 },
            // RR (HL)
            0x1E => { self.instr.rr_mem(self.mem, HL); 4 },
            // RR A
            0x1F => { self.instr.rr(A); 2 },
            // SLA B
            0x20 => { self.instr.sla(B); 2 },
            // SLA C
            0x21 => { self.instr.sla(C); 2 },
            // SLA D
            0x22 => { self.instr.sla(D); 2 },
            // SLA E
            0x23 => { self.instr.sla(E); 2 },
            // SLA H
            0x24 => { self.instr.sla(H); 2 },
            // SLA L
            0x25 => { self.instr.sla(L); 2 },
            // SLA (HL)
            0x26 => { self.instr.sla_mem(self.mem, HL); 4 },
            // SLA A
            0x27 => { self.instr.sla(A); 2 },
            // SRA B
            0x28 => { self.instr.sra(B); 2 },
            // SRA C
            0x29 => { self.instr.sra(C); 2 },
            // SRA D
            0x2A => { self.instr.sra(D); 2 },
            // SRA E
            0x2B => { self.instr.sra(E); 2 },
            // SRA H
            0x2C => { self.instr.sra(H); 2 },
            // SRA L
            0x2D => { self.instr.sra(L); 2 },
            // SRA (HL)
            0x2E => { self.instr.sra_mem(self.mem, HL); 4 },
            // SRA A
            0x2F => { self.instr.sra(A); 2 },
            // SWAP B
            0x30 => { self.instr.swap(B); 2 },
            // SWAP C
            0x31 => { self.instr.swap(C); 2 },
            // SWAP D
            0x32 => { self.instr.swap(D); 2 },
            // SWAP E
            0x33 => { self.instr.swap(E); 2 },
            // SWAP H
            0x34 => { self.instr.swap(H); 2 },
            // SWAP L
            0x35 => { self.instr.swap(L); 2 },
            // SWAP (HL)
            0x36 => { self.instr.swap_mem(self.mem, HL); 4 },
            // SWAP A
            0x37 => { self.instr.swap(A); 2 },
            // SRL B
            0x38 => { self.instr.srl(B); 2 },
            // SRL C
            0x39 => { self.instr.srl(C); 2 },
            // SRL D
            0x3A => { self.instr.srl(D); 2 },
            // SRL E
            0x3B => { self.instr.srl(E); 2 },
            // SRL H
            0x3C => { self.instr.srl(H); 2 },
            // SRL L
            0x3D => { self.instr.srl(L); 2 },
            // SRL (HL)
            0x3E => { self.instr.srl_mem(self.mem, HL); 4 },
            // SRL A
            0x3F => { self.instr.srl(A); 2 },
            // BIT 0, B
            0x40 => { self.instr.bit(0, B); 2 },
            // BIT 0, C
            0x41 => { self.instr.bit(0, C); 2 },
            // BIT 0, D
            0x42 => { self.instr.bit(0, D); 2 },
            // BIT 0, E
            0x43 => { self.instr.bit(0, E); 2 },
            // BIT 0, H
            0x44 => { self.instr.bit(0, H); 2 },
            // BIT 0, L
            0x45 => { self.instr.bit(0, L); 2 },
            // BIT 0, (HL)
            0x46 => { self.instr.bit_mem(self.mem, 0, HL); 4 },
            // BIT 0, A
            0x47 => { self.instr.bit(0, A); 2 },
            // BIT 1, B
            0x48 => { self.instr.bit(1, B); 2 },
            // BIT 1, C
            0x49 => { self.instr.bit(1, C); 2 },
            // BIT 1, D
            0x4A => { self.instr.bit(1, D); 2 },
            // BIT 1, E
            0x4B => { self.instr.bit(1, E); 2 },
            // BIT 1, H
            0x4C => { self.instr.bit(1, H); 2 },
            // BIT 1, L
            0x4D => { self.instr.bit(1, L); 2 },
            // BIT 1, (HL)
            0x4E => { self.instr.bit_mem(self.mem, 1, HL); 4 },
            // BIT 1, A
            0x4F => { self.instr.bit(1, A); 2 },
            // BIT 2, B
            0x50 => { self.instr.bit(2, B); 2 },
            // BIT 2, C
            0x51 => { self.instr.bit(2, C); 2 },
            // BIT 2, D
            0x52 => { self.instr.bit(2, D); 2 },
            // BIT 2, E
            0x53 => { self.instr.bit(2, E); 2 },
            // BIT 2, H
            0x54 => { self.instr.bit(2, H); 2 },
            // BIT 2, L
            0x55 => { self.instr.bit(2, L); 2 },
            // BIT 2, (HL)
            0x56 => { self.instr.bit_mem(self.mem, 2, HL); 4 },
            // BIT 2, A
            0x57 => { self.instr.bit(2, A); 2 },
            // BIT 3, B
            0x58 => { self.instr.bit(3, B); 2 },
            // BIT 3, C
            0x59 => { self.instr.bit(3, C); 2 },
            // BIT 3, D
            0x5A => { self.instr.bit(3, D); 2 },
            // BIT 3, E
            0x5B => { self.instr.bit(3, E); 2 },
            // BIT 3, H
            0x5C => { self.instr.bit(3, H); 2 },
            // BIT 3, L
            0x5D => { self.instr.bit(3, L); 2 },
            // BIT 3, (HL)
            0x5E => { self.instr.bit_mem(self.mem, 3, HL); 4 },
            // BIT 3, A
            0x5F => { self.instr.bit(3, A); 2 },
            // BIT 4, B
            0x60 => { self.instr.bit(4, B); 2 },
            // BIT 4, C
            0x61 => { self.instr.bit(4, C); 2 },
            // BIT 4, D
            0x62 => { self.instr.bit(4, D); 2 },
            // BIT 4, E
            0x63 => { self.instr.bit(4, E); 2 },
            // BIT 4, H
            0x64 => { self.instr.bit(4, H); 2 },
            // BIT 4, L
            0x65 => { self.instr.bit(4, L); 2 },
            // BIT 4, (HL)
            0x66 => { self.instr.bit_mem(self.mem, 4, HL); 4 },
            // BIT 4, A
            0x67 => { self.instr.bit(4, A); 2 },
            // BIT 5, B
            0x68 => { self.instr.bit(5, B); 2 },
            // BIT 5, C
            0x69 => { self.instr.bit(5, C); 2 },
            // BIT 5, D
            0x6A => { self.instr.bit(5, D); 2 },
            // BIT 5, E
            0x6B => { self.instr.bit(5, E); 2 },
            // BIT 5, H
            0x6C => { self.instr.bit(5, H); 2 },
            // BIT 5, L
            0x6D => { self.instr.bit(5, L); 2 },
            // BIT 5, (HL)
            0x6E => { self.instr.bit_mem(self.mem, 5, HL); 4 },
            // BIT 5, A
            0x6F => { self.instr.bit(5, A); 2 },
            // BIT 6, B
            0x70 => { self.instr.bit(6, B); 2 },
            // BIT 6, C
            0x71 => { self.instr.bit(6, C); 2 },
            // BIT 6, D
            0x72 => { self.instr.bit(6, D); 2 },
            // BIT 6, E
            0x73 => { self.instr.bit(6, E); 2 },
            // BIT 6, H
            0x74 => { self.instr.bit(6, H); 2 },
            // BIT 6, L
            0x75 => { self.instr.bit(6, L); 2 },
            // BIT 6, (HL)
            0x76 => { self.instr.bit_mem(self.mem, 6, HL); 4 },
            // BIT 6, A
            0x77 => { self.instr.bit(6, A); 2 },
            // BIT 7, B
            0x78 => { self.instr.bit(7, B); 2 },
            // BIT 7, C
            0x79 => { self.instr.bit(7, C); 2 },
            // BIT 7, D
            0x7A => { self.instr.bit(7, D); 2 },
            // BIT 7, E
            0x7B => { self.instr.bit(7, E); 2 },
            // BIT 7, H
            0x7C => { self.instr.bit(7, H); 2 },
            // BIT 7, L
            0x7D => { self.instr.bit(7, L); 2 },
            // BIT 7, (HL)
            0x7E => { self.instr.bit_mem(self.mem, 7, HL); 4 },
            // BIT 7, A
            0x7F => { self.instr.bit(7, A); 2 },
            // RES 0, B
            0x80 => { self.instr.res(0, B); 2 },
            // RES 0, C
            0x81 => { self.instr.res(0, C); 2 },
            // RES 0, D
            0x82 => { self.instr.res(0, D); 2 },
            // RES 0, E
            0x83 => { self.instr.res(0, E); 2 },
            // RES 0, H
            0x84 => { self.instr.res(0, H); 2 },
            // RES 0, L
            0x85 => { self.instr.res(0, L); 2 },
            // RES 0, (HL)
            0x86 => { self.instr.res_mem(self.mem, 0, HL); 4 },
            // RES 0, A
            0x87 => { self.instr.res(0, A); 2 },
            // RES 1, B
            0x88 => { self.instr.res(1, B); 2 },
            // RES 1, C
            0x89 => { self.instr.res(1, C); 2 },
            // RES 1, D
            0x8A => { self.instr.res(1, D); 2 },
            // RES 1, E
            0x8B => { self.instr.res(1, E); 2 },
            // RES 1, H
            0x8C => { self.instr.res(1, H); 2 },
            // RES 1, L
            0x8D => { self.instr.res(1, L); 2 },
            // RES 1, (HL)
            0x8E => { self.instr.res_mem(self.mem, 1, HL); 4 },
            // RES 1, A
            0x8F => { self.instr.res(1, A); 2 },
            // RES 2, B
            0x90 => { self.instr.res(2, B); 2 },
            // RES 2, C
            0x91 => { self.instr.res(2, C); 2 },
            // RES 2, D
            0x92 => { self.instr.res(2, D); 2 },
            // RES 2, E
            0x93 => { self.instr.res(2, E); 2 },
            // RES 2, H
            0x94 => { self.instr.res(2, H); 2 },
            // RES 2, L
            0x95 => { self.instr.res(2, L); 2 },
            // RES 2, (HL)
            0x96 => { self.instr.res_mem(self.mem, 2, HL); 4 },
            // RES 2, A
            0x97 => { self.instr.res(2, A); 2 },
            // RES 3, B
            0x98 => { self.instr.res(3, B); 2 },
            // RES 3, C
            0x99 => { self.instr.res(3, C); 2 },
            // RES 3, D
            0x9A => { self.instr.res(3, D); 2 },
            // RES 3, E
            0x9B => { self.instr.res(3, E); 2 },
            // RES 3, H
            0x9C => { self.instr.res(3, H); 2 },
            // RES 3, L
            0x9D => { self.instr.res(3, L); 2 },
            // RES 3, (HL)
            0x9E => { self.instr.res_mem(self.mem, 3, HL); 4 },
            // RES 3, A
            0x9F => { self.instr.res(3, A); 2 },
            // RES 4, B
            0xA0 => { self.instr.res(4, B); 2 },
            // RES 4, C
            0xA1 => { self.instr.res(4, C); 2 },
            // RES 4, D
            0xA2 => { self.instr.res(4, D); 2 },
            // RES 4, E
            0xA3 => { self.instr.res(4, E); 2 },
            // RES 4, H
            0xA4 => { self.instr.res(4, H); 2 },
            // RES 4, L
            0xA5 => { self.instr.res(4, L); 2 },
            // RES 4, (HL)
            0xA6 => { self.instr.res_mem(self.mem, 4, HL); 4 },
            // RES 4, A
            0xA7 => { self.instr.res(4, A); 2 },
            // RES 5, B
            0xA8 => { self.instr.res(5, B); 2 },
            // RES 5, C
            0xA9 => { self.instr.res(5, C); 2 },
            // RES 5, D
            0xAA => { self.instr.res(5, D); 2 },
            // RES 5, E
            0xAB => { self.instr.res(5, E); 2 },
            // RES 5, H
            0xAC => { self.instr.res(5, H); 2 },
            // RES 5, L
            0xAD => { self.instr.res(5, L); 2 },
            // RES 5, (HL)
            0xAE => { self.instr.res_mem(self.mem, 5, HL); 4 },
            // RES 5, A
            0xAF => { self.instr.res(5, A); 2 },
            // RES 6, B
            0xB0 => { self.instr.res(6, B); 2 },
            // RES 6, C
            0xB1 => { self.instr.res(6, C); 2 },
            // RES 6, D
            0xB2 => { self.instr.res(6, D); 2 },
            // RES 6, E
            0xB3 => { self.instr.res(6, E); 2 },
            // RES 6, H
            0xB4 => { self.instr.res(6, H); 2 },
            // RES 6, L
            0xB5 => { self.instr.res(6, L); 2 },
            // RES 6, (HL)
            0xB6 => { self.instr.res_mem(self.mem, 6, HL); 4 },
            // RES 6, A
            0xB7 => { self.instr.res(6, A); 2 },
            // RES 7, B
            0xB8 => { self.instr.res(7, B); 2 },
            // RES 7, C
            0xB9 => { self.instr.res(7, C); 2 },
            // RES 7, D
            0xBA => { self.instr.res(7, D); 2 },
            // RES 7, E
            0xBB => { self.instr.res(7, E); 2 },
            // RES 7, H
            0xBC => { self.instr.res(7, H); 2 },
            // RES 7, L
            0xBD => { self.instr.res(7, L); 2 },
            // RES 7, (HL)
            0xBE => { self.instr.res_mem(self.mem, 7, HL); 4 },
            // RES 7, A
            0xBF => { self.instr.res(7, A); 2 },
            // SET 0, B
            0xC0 => { self.instr.set(0, B); 2 },
            // SET 0, C
            0xC1 => { self.instr.set(0, C); 2 },
            // SET 0, D
            0xC2 => { self.instr.set(0, D); 2 },
            // SET 0, E
            0xC3 => { self.instr.set(0, E); 2 },
            // SET 0, H
            0xC4 => { self.instr.set(0, H); 2 },
            // SET 0, L
            0xC5 => { self.instr.set(0, L); 2 },
            // SET 0, (HL)
            0xC6 => { self.instr.set_mem(self.mem, 0, HL); 4 },
            // SET 0, A
            0xC7 => { self.instr.set(0, A); 2 },
            // SET 1, B
            0xC8 => { self.instr.set(1, B); 2 },
            // SET 1, C
            0xC9 => { self.instr.set(1, C); 2 },
            // SET 1, D
            0xCA => { self.instr.set(1, D); 2 },
            // SET 1, E
            0xCB => { self.instr.set(1, E); 2 },
            // SET 1, H
            0xCC => { self.instr.set(1, H); 2 },
            // SET 1, L
            0xCD => { self.instr.set(1, L); 2 },
            // SET 1, (HL)
            0xCE => { self.instr.set_mem(self.mem, 1, HL); 4 },
            // SET 1, A
            0xCF => { self.instr.set(1, A); 2 },
            // SET 2, B
            0xD0 => { self.instr.set(2, B); 2 },
            // SET 2, C
            0xD1 => { self.instr.set(2, C); 2 },
            // SET 2, D
            0xD2 => { self.instr.set(2, D); 2 },
            // SET 2, E
            0xD3 => { self.instr.set(2, E); 2 },
            // SET 2, H
            0xD4 => { self.instr.set(2, H); 2 },
            // SET 2, L
            0xD5 => { self.instr.set(2, L); 2 },
            // SET 2, (HL)
            0xD6 => { self.instr.set_mem(self.mem, 2, HL); 4 },
            // SET 2, A
            0xD7 => { self.instr.set(2, A); 2 },
            // SET 3, B
            0xD8 => { self.instr.set(3, B); 2 },
            // SET 3, C
            0xD9 => { self.instr.set(3, C); 2 },
            // SET 3, D
            0xDA => { self.instr.set(3, D); 2 },
            // SET 3, E
            0xDB => { self.instr.set(3, E); 2 },
            // SET 3, H
            0xDC => { self.instr.set(3, H); 2 },
            // SET 3, L
            0xDD => { self.instr.set(3, L); 2 },
            // SET 3, (HL)
            0xDE => { self.instr.set_mem(self.mem, 3, HL); 4 },
            // SET 3, A
            0xDF => { self.instr.set(3, A); 2 },
            // SET 4, B
            0xE0 => { self.instr.set(4, B); 2 },
            // SET 4, C
            0xE1 => { self.instr.set(4, C); 2 },
            // SET 4, D
            0xE2 => { self.instr.set(4, D); 2 },
            // SET 4, E
            0xE3 => { self.instr.set(4, E); 2 },
            // SET 4, H
            0xE4 => { self.instr.set(4, H); 2 },
            // SET 4, L
            0xE5 => { self.instr.set(4, L); 2 },
            // SET 4, (HL)
            0xE6 => { self.instr.set_mem(self.mem, 4, HL); 4 },
            // SET 4, A
            0xE7 => { self.instr.set(4, A); 2 },
            // SET 5, B
            0xE8 => { self.instr.set(5, B); 2 },
            // SET 5, C
            0xE9 => { self.instr.set(5, C); 2 },
            // SET 5, D
            0xEA => { self.instr.set(5, D); 2 },
            // SET 5, E
            0xEB => { self.instr.set(5, E); 2 },
            // SET 5, H
            0xEC => { self.instr.set(5, H); 2 },
            // SET 5, L
            0xED => { self.instr.set(5, L); 2 },
            // SET 5, (HL)
            0xEE => { self.instr.set_mem(self.mem, 5, HL); 4 },
            // SET 5, A
            0xEF => { self.instr.set(5, A); 2 },
            // SET 6, B
            0xF0 => { self.instr.set(6, B); 2 },
            // SET 6, C
            0xF1 => { self.instr.set(6, C); 2 },
            // SET 6, D
            0xF2 => { self.instr.set(6, D); 2 },
            // SET 6, E
            0xF3 => { self.instr.set(6, E); 2 },
            // SET 6, H
            0xF4 => { self.instr.set(6, H); 2 },
            // SET 6, L
            0xF5 => { self.instr.set(6, L); 2 },
            // SET 6, (HL)
            0xF6 => { self.instr.set_mem(self.mem, 6, HL); 4 },
            // SET 6, A
            0xF7 => { self.instr.set(6, A); 2 },
            // SET 7, B
            0xF8 => { self.instr.set(7, B); 2 },
            // SET 7, C
            0xF9 => { self.instr.set(7, C); 2 },
            // SET 7, D
            0xFA => { self.instr.set(7, D); 2 },
            // SET 7, E
            0xFB => { self.instr.set(7, E); 2 },
            // SET 7, H
            0xFC => { self.instr.set(7, H); 2 },
            // SET 7, L
            0xFD => { self.instr.set(7, L); 2 },
            // SET 7, (HL)
            0xFE => { self.instr.set_mem(self.mem, 7, HL); 4 },
            // SET 7, A
            0xFF => { self.instr.set(7, A); 2 },
        }

    }




    // **Interrupts**
    // Only 1 active at a time
    // PC pushed to stack
    // Interrupts -- in race condition first in order is activated, then action is undefined
    // (1) VBLANK -- LCD has drawn full frame --- Call 0x40 ---- Edit VRAM in between frames
    // (2) LCDC ---- LCD controller changed ----- Call 0x48 ---- Cause modifiable by STAT reg ; Can wait for screen to draw specific line via LYC reg
    // (3) SERIAL -- Serial Transfer completed -- Call 0x50 ---- Use of serial port for multiplayer
    // (4) TIMER --- Serial Transfer completed -- Call 0x58 ---- Activates once the timer register overflows, then the timer resets to a predefined value, and starts moving. 
    // (5) HiToLo -- User pressed a button ------ Call 0x60 ---- Interrupt to update action based on input. Input is still not reflected until next frame
    // All interrupts are enabled/disabled via register at address 0xFFFF
    // Timer Registers
    // 0xFF04 -- DIV --- Divider Register --- increments at freq. 16384 Hz. Resets if written to.
    // 0xFF05 -- TIMA -- Timer Counter ------ increments at freq. of TAC. Resets at overflow to TMA value and triggers interrupt. Cpu jumps to 0x50 at TIMA overflow, where the next instruction is read.
    // 0xFF06 -- TMA --- Timer Mod ---------- contains an offset value for the timer to restart with
    // 0xFF07 -- TAC --- Timer Control
    //                   0 (Stop)     00 -- 4096 Hz
    //                   1 (Start)    01 -- 262144 Hz
    //                                10 -- 65536 Hz
    //                                11 -- 16384 Hz

    //Stalls Cpu until interrupt is triggered, at which point HALT is broken and Cpu switches to interrupt vector. After the interrupt returns, instructions continue immediately following the halt.
    //pub fn halt() {}

    // these two are interrupt enablers
    //pub fn ei() {}

    //pub fn reti() {}
}

