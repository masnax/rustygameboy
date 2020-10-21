mod instructions;
mod cb_instructions;
use crate::memory::Memory;
use crate::register::{ALUFlag, REG::*, Register};
use instructions::InstructionSet;
use cb_instructions::CBInstructionSet;
//single core
pub struct CPU<'a> {
    instr: InstructionSet<'a>,
}


impl<'a> CPU<'a> {
    pub fn init(registers: Register, memory: &'a mut Memory) -> CPU<'a> {
        CPU {
            instr: InstructionSet::init(registers,  memory),
        }
    }

    pub fn get_frame_info(&self) -> ([u8; 0x800], usize, [u8; 0x1800], u8) {
        return self.instr.get_frame_info();
    }

    pub fn _cycle(&mut self) {
        //self.tile_set.get_tile_set(self.instr.get_vram());
        loop {
            self.exec();
        }
    }

    // Returns number of cycles
    pub fn exec(&mut self) -> u8 {
        print!("PC: 0x{:X} =>", self.instr.r.get_word(PC));
        let opcode = if self.instr.halted {0} else {self.instr.fetch()};
        self.instr.m.write_byte(0xFF44, 0x90);
        match opcode {
            // NOP
            0x00 => { print!("NOP"); 1 },
            // LD BC, d16
            0x01 => { print!("LD BC, d16"); self.instr.ld_rw(BC); 3 },
            // LD (BC), A
            0x02 => { print!("LD (BC), A"); self.instr.ld_mr(BC, A); 2 },
            // INC BC
            0x03 => { print!("INC BC"); self.instr.inc(BC); 2 },
            // INC B
            0x04 => { print!("INC B"); self.instr.inc(B); 1 },
            // DEC B
            0x05 => { print!("DEC B"); self.instr.dec(B); 1 },
            // LD B, u8
            0x06 => { print!("LD B, u8"); self.instr.ld_rb(B); 2 },
            // RLCA
            0x07 => { print!("RLCA"); self.instr.rlca(A); 1 },
            // LD (u16), SP
            0x08 => { print!("LD (u16), SP"); self.instr.ld_mw(SP); 5 },
            // ADD HL, BC
            0x09 => { print!("ADD HL, BC"); self.instr.add(HL, BC); 2 },
            // LD A, (BC)
            0x0A => { print!("LD A, (BC)"); self.instr.ld_rm(A, BC); 2 },
            // DEC BC
            0x0B => { print!("DEC BC"); self.instr.dec(BC); 2 },
            // INC C
            0x0C => { print!("INC C"); self.instr.inc(C); 1 },
            // DEC C
            0x0D => { print!("DEC C"); self.instr.dec(C); 1 },
            // LD C, u8
            0x0E => { print!("LD C, u8"); self.instr.ld_rb(C); 2 },
            // RRCA
            0x0F => { print!("RRCA"); self.instr.rrca(A); 1 },
            // STOP
            0x10 => { print!("STOP"); self.instr.stop(); 1 },
            // LD DE, U16
            0x11 => { print!("LD DE, U16"); self.instr.ld_rw(DE); 3 },
            // LD (DE), A
            0x12 => { print!("LD (DE), A"); self.instr.ld_mr(DE, A); 2 },
            // INC DE
            0x13 => { print!("INC DE"); self.instr.inc(DE); 2 },
            // INC D
            0x14 => { print!("INC D"); self.instr.inc(D); 1 },
            // DEC D
            0x15 => { print!("DEC D"); self.instr.dec(D); 1 },
            // LD D, u8
            0x16 => { print!("LD D, u8"); self.instr.ld_rb(D); 2 },
            // RLA
            0x17 => { print!("RLA"); self.instr.rla(A); 1 },
            // JR s8
            0x18 => { print!("JR s8"); self.instr.jr(true) },
            // ADD HL, DE
            0x19 => { print!("ADD HL, DE"); self.instr.add(HL, DE); 2 },
            // LD A, (DE)
            0x1A => { print!("LD A, (DE)"); self.instr.ld_rm(A, DE); 2 },
            // DEC DE
            0x1B => { print!("DEC DE"); self.instr.dec(DE); 2 },
            // INC E
            0x1C => { print!("INC E"); self.instr.inc(E); 1 },
            // DEC E
            0x1D => { print!("DEC E"); self.instr.dec(E); 1 },
            // LD E, u8
            0x1E => { print!("LD E, u8"); self.instr.ld_rb(E); 2 },
            // RRA
            0x1F => { print!("RRA"); self.instr.rra(A); 1 },
            // JR NZ, s8
            0x20 => { print!("JR NZ, s8"); self.instr.jr(!self.instr.get_flag(ALUFlag::Z)) },
            // LD HL, U16
            0x21 => { print!("LD HL, U16"); self.instr.ld_rw(HL); 3 },
            // LD (HL+), A
            0x22 => { print!("LD (HL+), A"); self.instr.ld_mem_inc(); 2 },
            // INC HL
            0x23 => { print!("INC HL"); self.instr.inc(HL); 2 },
            // INC H
            0x24 => { print!("INC H"); self.instr.inc(H); 1 },
            // DEC H
            0x25 => { print!("DEC H"); self.instr.dec(H); 1 },
            // LD H, u8
            0x26 => { print!("LD H, u8"); self.instr.ld_rb(H); 2 },
            // DAA
            0x27 => { print!("DAA"); self.instr.daa(); 1 },
            // JR Z, s8
            0x28 => { print!("JR Z, s8"); self.instr.jr(self.instr.get_flag(ALUFlag::Z)) },
            // ADD HL, HL
            0x29 => { print!("ADD HL, HL"); self.instr.add(HL, HL); 2 },
            // LD A, (HL+)
            0x2A => { print!("LD A, (HL+)"); self.instr.ld_inc(); 2 },
            // DEC HL
            0x2B => { print!("DEC HL"); self.instr.dec(HL); 2 },
            // INC L
            0x2C => { print!("INC L"); self.instr.inc(L); 1 },
            // DEC L
            0x2D => { print!("DEC L"); self.instr.dec(L); 1 },
            // LD L, u8
            0x2E => { print!("LD L, u8"); self.instr.ld_rb(L); 2 },
            // CPL
            0x2F => { print!("CPL"); self.instr.cpl(); 1 },
            // JR NC, s8
            0x30 => { print!("JR NC, s8"); self.instr.jr(!self.instr.get_flag(ALUFlag::C)) },
            // LD SP, U16
            0x31 => { print!("LD SP, U16"); self.instr.ld_rw(SP); 3 },
            // LD (HL-), A
            0x32 => { print!("LD (HL-), A"); self.instr.ld_mem_dec(); 2 },
            // INC SP
            0x33 => { print!("INC SP"); self.instr.inc(SP); 2 },
            // INC (HL)
            0x34 => { print!("INC (HL)"); self.instr.inc_mem(); 3 },
            // DEC (HL)
            0x35 => { print!("DEC (HL)"); self.instr.dec_mem(); 3 },
            // LD (HL), u8
            0x36 => { print!("LD (HL), u8"); self.instr.ld_mb(HL); 3 },
            // SCF
            0x37 => { print!("SCF"); self.instr.scf(); 1 },
            // JR C, s8
            0x38 => { print!("JR C, s8"); self.instr.jr(self.instr.get_flag(ALUFlag::C)) },
            // ADD HL, SP
            0x39 => { print!("ADD HL, SP"); self.instr.add(HL, SP); 2 },
            // LD A, (HL-)
            0x3A => { print!("LD A, (HL-)"); self.instr.ld_dec(); 2 },
            // DEC SP
            0x3B => { print!("DEC SP"); self.instr.dec(SP); 2 },
            // INC A
            0x3C => { print!("INC A"); self.instr.inc(A); 1 },
            // DEC A
            0x3D => { print!("DEC A"); self.instr.dec(A); 1 },
            // LD A, u8
            0x3E => { print!("LD A, u8"); self.instr.ld_rb(A); 2 },
            // CCF
            0x3F => { print!("CCF"); self.instr.ccf(); 1 },
            // LD B, B
            0x40 => { print!("LD B, B"); self.instr.ld_rr(B, B); 1 },
            // LD B, C
            0x41 => { print!("LD B, C"); self.instr.ld_rr(B, C); 1 },
            // LD B, D
            0x42 => { print!("LD B, D"); self.instr.ld_rr(B, D); 1 },
            // LD B, E
            0x43 => { print!("LD B, E"); self.instr.ld_rr(B, E); 1 },
            // LD B, H
            0x44 => { print!("LD B, H"); self.instr.ld_rr(B, H); 1 },
            // LD B, L
            0x45 => { print!("LD B, L"); self.instr.ld_rr(B, L); 1 },
            // LD B, (HL)
            0x46 => { print!("LD B, (HL)"); self.instr.ld_rm(B, HL); 2 },
            // LD B, A
            0x47 => { print!("LD B, A"); self.instr.ld_rr(B, A); 1 },
            // LD C, B
            0x48 => { print!("LD C, B"); self.instr.ld_rr(C, B); 1 },
            // LD C, C
            0x49 => { print!("LD C, C"); self.instr.ld_rr(C, C); 1 },
            // LD C, D
            0x4A => { print!("LD C, D"); self.instr.ld_rr(C, D); 1 },
            // LD C, E
            0x4B => { print!("LD C, E"); self.instr.ld_rr(C, E); 1 },
            // LD C, H
            0x4C => { print!("LD C, H"); self.instr.ld_rr(C, H); 1 },
            // LD C, L
            0x4D => { print!("LD C, L"); self.instr.ld_rr(C, L); 1 },
            // LD C, (HL)
            0x4E => { print!("LD C, (HL)"); self.instr.ld_rm(C, HL); 2 },
            // LD C, A
            0x4F => { print!("LD C, A"); self.instr.ld_rr(C, A); 1 },
            // LD D, B
            0x50 => { print!("LD D, B"); self.instr.ld_rr(D, B); 1 },
            // LD D, C
            0x51 => { print!("LD D, C"); self.instr.ld_rr(D, C); 1 },
            // LD D, D
            0x52 => { print!("LD D, D"); self.instr.ld_rr(D, D); 1 },
            // LD D, E
            0x53 => { print!("LD D, E"); self.instr.ld_rr(D, E); 1 },
            // LD D, H
            0x54 => { print!("LD D, H"); self.instr.ld_rr(D, H); 1 },
            // LD D, L
            0x55 => { print!("LD D, L"); self.instr.ld_rr(D, L); 1 },
            // LD D, (HL)
            0x56 => { print!("LD D, (HL)"); self.instr.ld_rm(D, HL); 2 },
            // LD D, A
            0x57 => { print!("LD D, A"); self.instr.ld_rr(D, A); 1 },
            // LD E, B
            0x58 => { print!("LD E, B"); self.instr.ld_rr(E, B); 1 },
            // LD E, C
            0x59 => { print!("LD E, C"); self.instr.ld_rr(E, C); 1 },
            // LD E, D
            0x5A => { print!("LD E, D"); self.instr.ld_rr(E, D); 1 },
            // LD E, E
            0x5B => { print!("LD E, E"); self.instr.ld_rr(E, E); 1 },
            // LD E, H
            0x5C => { print!("LD E, H"); self.instr.ld_rr(E, H); 1 },
            // LD E, L
            0x5D => { print!("LD E, L"); self.instr.ld_rr(E, L); 1 },
            // LD E, (HL)
            0x5E => { print!("LD E, (HL)"); self.instr.ld_rm(E, HL); 2 },
            // LD E, A
            0x5F => { print!("LD E, A"); self.instr.ld_rr(E, A); 1 },
            // LD H, B
            0x60 => { print!("LD H, B"); self.instr.ld_rr(H, B); 1 },
            // LD H, C
            0x61 => { print!("LD H, C"); self.instr.ld_rr(H, C); 1 },
            // LD H, D
            0x62 => { print!("LD H, D"); self.instr.ld_rr(H, D); 1 },
            // LD H, E
            0x63 => { print!("LD H, E"); self.instr.ld_rr(H, E); 1 },
            // LD H, H
            0x64 => { print!("LD H, H"); self.instr.ld_rr(H, H); 1 },
            // LD H, L
            0x65 => { print!("LD H, L"); self.instr.ld_rr(H, L); 1 },
            // LD H, (HL)
            0x66 => { print!("LD H, (HL)"); self.instr.ld_rm(H, HL); 2 },
            // LD H, A
            0x67 => { print!("LD H, A"); self.instr.ld_rr(H, A); 1 },
            // LD L, B
            0x68 => { print!("LD L, B"); self.instr.ld_rr(L, B); 1 },
            // LD L, C
            0x69 => { print!("LD L, C"); self.instr.ld_rr(L, C); 1 },
            // LD L, D
            0x6A => { print!("LD L, D"); self.instr.ld_rr(L, D); 1 },
            // LD L, E
            0x6B => { print!("LD L, E"); self.instr.ld_rr(L, E); 1 },
            // LD L, H
            0x6C => { print!("LD L, H"); self.instr.ld_rr(L, H); 1 },
            // LD L, L
            0x6D => { print!("LD L, L"); self.instr.ld_rr(L, L); 1 },
            // LD L, (HL)
            0x6E => { print!("LD L, (HL)"); self.instr.ld_rm(L, HL); 2 },
            // LD L, A
            0x6F => { print!("LD L, A"); self.instr.ld_rr(L, A); 1 },
            // LD (HL), B
            0x70 => { print!("LD (HL), B"); self.instr.ld_mr(HL, B); 2 },
            // LD (HL), C
            0x71 => { print!("LD (HL), C"); self.instr.ld_mr(HL, C); 2 },
            // LD (HL), D
            0x72 => { print!("LD (HL), D"); self.instr.ld_mr(HL, D); 2 },
            // LD (HL), E
            0x73 => { print!("LD (HL), E"); self.instr.ld_mr(HL, E); 2 },
            // LD (HL), H
            0x74 => { print!("LD (HL), H"); self.instr.ld_mr(HL, H); 2 },
            // LD (HL), L
            0x75 => { print!("LD (HL), L"); self.instr.ld_mr(HL, L); 2 },
            // HALT
            0x76 => { print!("HALT"); self.instr.halt(); 1 },
            // LD (HL), A
            0x77 => { print!("LD (HL), A"); self.instr.ld_mr(HL, A); 2 },
            // LD A, B
            0x78 => { print!("LD A, B"); self.instr.ld_rr(A, B); 1 },
            // LD A, C
            0x79 => { print!("LD A, C"); self.instr.ld_rr(A, C); 1 },
            // LD A, D
            0x7A => { print!("LD A, D"); self.instr.ld_rr(A, D); 1 },
            // LD A, E
            0x7B => { print!("LD A, E"); self.instr.ld_rr(A, E); 1 },
            // LD A, H
            0x7C => { print!("LD A, H"); self.instr.ld_rr(A, H); 1 },
            // LD A, L
            0x7D => { print!("LD A, L"); self.instr.ld_rr(A, L); 1 },
            // LD A, (HL)
            0x7E => { print!("LD A, (HL)"); self.instr.ld_rm(A, HL); 2 },
            // LD A, A
            0x7F => { print!("LD A, A"); 1 },
            // ADD A, B
            0x80 => { print!("ADD A, B"); self.instr.add(A, B); 2 },
            // ADD A, C
            0x81 => { print!("ADD A, C"); self.instr.add(A, C); 2 },
            // ADD A, D
            0x82 => { print!("ADD A, D"); self.instr.add(A, D); 2 },
            // ADD A, E
            0x83 => { print!("ADD A, E"); self.instr.add(A, E); 2 },
            // ADD A, H
            0x84 => { print!("ADD A, H"); self.instr.add(A, H); 2 },
            // ADD A, L
            0x85 => { print!("ADD A, L"); self.instr.add(A, L); 2 },
            // ADD A, (HL)
            0x86 => { print!("ADD A, (HL)"); self.instr.add(A, HL); 2 },
            // ADD A, A
            0x87 => { print!("ADD A, A"); self.instr.add(A, A); 2 },
            // ADC A, B
            0x88 => { print!("ADC A, B"); self.instr.adc(B); 2 },
            // ADC A, C
            0x89 => { print!("ADC A, C"); self.instr.adc(C); 2 },
            // ADC A, D
            0x8A => { print!("ADC A, D"); self.instr.adc(D); 2 },
            // ADC A, E
            0x8B => { print!("ADC A, E"); self.instr.adc(E); 2 },
            // ADC A, H
            0x8C => { print!("ADC A, H"); self.instr.adc(H); 2 },
            // ADC A, L
            0x8D => { print!("ADC A, L"); self.instr.adc(L); 2 },
            // ADC A, (HL)
            0x8E => { print!("ADC A, (HL)"); self.instr.adc(HL); 2 },
            // ADC A, A
            0x8F => { print!("ADC A, A"); self.instr.adc(A); 2 },
            // SUB B
            0x90 => { print!("SUB B"); self.instr.sub(A, B); 1 },
            // SUB C
            0x91 => { print!("SUB C"); self.instr.sub(A, C); 1 },
            // SUB D
            0x92 => { print!("SUB D"); self.instr.sub(A, D); 1 },
            // SUB E
            0x93 => { print!("SUB E"); self.instr.sub(A, E); 1 },
            // SUB H
            0x94 => { print!("SUB H"); self.instr.sub(A, H); 1 },
            // SUB L
            0x95 => { print!("SUB L"); self.instr.sub(A, L); 1 },
            // SUB (HL)
            0x96 => { print!("SUB (HL)"); self.instr.sub(A, HL); 2 },
            // SUB A
            0x97 => { print!("SUB A"); self.instr.sub(A, A); 1 },
            // SBC A, B
            0x98 => { print!("SBC A, B"); self.instr.sbc(B); 1 },
            // SBC A, C
            0x99 => { print!("SBC A, C"); self.instr.sbc(C); 1 },
            // SBC A, D
            0x9A => { print!("SBC A, D"); self.instr.sbc(D); 1 },
            // SBC A, E
            0x9B => { print!("SBC A, E"); self.instr.sbc(E); 1 },
            // SBC A, H
            0x9C => { print!("SBC A, H"); self.instr.sbc(H); 1 },
            // SBC A, L
            0x9D => { print!("SBC A, L"); self.instr.sbc(L); 1 },
            // SBC A, (HL)
            0x9E => { print!("SBC A, (HL)"); self.instr.sbc(HL); 2 },
            // SBC A, A
            0x9F => { print!("SBC A, A"); self.instr.sbc(A); 1 },
            // AND B
            0xA0 => { print!("AND B"); self.instr.and(B); 1 },
            // AND C
            0xA1 => { print!("AND C"); self.instr.and(C); 1 },
            // AND D
            0xA2 => { print!("AND D"); self.instr.and(D); 1 },
            // AND E
            0xA3 => { print!("AND E"); self.instr.and(E); 1 },
            // AND H
            0xA4 => { print!("AND H"); self.instr.and(H); 1 },
            // AND L
            0xA5 => { print!("AND L"); self.instr.and(L); 1 },
            // AND (HL)
            0xA6 => { print!("AND (HL)"); self.instr.and(HL); 2 },
            // AND A
            0xA7 => { print!("AND A"); self.instr.and(A); 1 },
            // XOR B
            0xA8 => { print!("XOR B"); self.instr.xor(B); 1 },
            // XOR C
            0xA9 => { print!("XOR C"); self.instr.xor(C); 1 },
            // XOR D
            0xAA => { print!("XOR D"); self.instr.xor(D); 1 },
            // XOR E
            0xAB => { print!("XOR E"); self.instr.xor(E); 1 },
            // XOR H
            0xAC => { print!("XOR H"); self.instr.xor(H); 1 },
            // XOR L
            0xAD => { print!("XOR L"); self.instr.xor(L); 1 },
            // XOR (HL)
            0xAE => { print!("XOR (HL)"); self.instr.xor(HL); 2 },
            // XOR A
            0xAF => { print!("XOR A"); self.instr.xor(A); 1 },
            // OR B
            0xB0 => { print!("OR B"); self.instr.or(B); 1 },
            // OR C
            0xB1 => { print!("OR C"); self.instr.or(C); 1 },
            // OR D
            0xB2 => { print!("OR D"); self.instr.or(D); 1 },
            // OR E
            0xB3 => { print!("OR E"); self.instr.or(E); 1 },
            // OR H
            0xB4 => { print!("OR H"); self.instr.or(H); 1 },
            // OR L
            0xB5 => { print!("OR L"); self.instr.or(L); 1 },
            // OR (HL)
            0xB6 => { print!("OR (HL)"); self.instr.or(HL); 2 },
            // OR A
            0xB7 => { print!("OR A"); self.instr.or(A); 1 },
            // CP B
            0xB8 => { print!("CP B"); self.instr.cp(B); 1 },
            // CP C
            0xB9 => { print!("CP C"); self.instr.cp(C); 1 },
            // CP D
            0xBA => { print!("CP D"); self.instr.cp(D); 1 },
            // CP E
            0xBB => { print!("CP E"); self.instr.cp(E); 1 },
            // CP H
            0xBC => { print!("CP H"); self.instr.cp(H); 1 },
            // CP L
            0xBD => { print!("CP L"); self.instr.cp(L); 1 },
            // CP (HL)
            0xBE => { print!("CP (HL)"); self.instr.cp(HL); 2 },
            // CP A
            0xBF => { print!("CP A"); self.instr.cp(A); 1 },
            // RET NZ
            0xC0 => { print!("RET NZ"); self.instr.retc(!self.instr.get_flag(ALUFlag::Z)) },
            // POP BC
            0xC1 => { print!("POP BC"); self.instr.pop(BC); 3 },
            // JP NZ, a16
            0xC2 => { print!("JP NZ, a16"); self.instr.jp(!self.instr.get_flag(ALUFlag::Z)) },
            // JP a16
            0xC3 => { print!("JP a16"); self.instr.jp(true) },
            // CALL NZ, a16
            0xC4 => { print!("CALL NZ, a16"); self.instr.call(!self.instr.get_flag(ALUFlag::Z)) },
            // PUSH BC
            0xC5 => { print!("PUSH BC"); self.instr.push(BC); 4 },
            // ADD A, d8
            0xC6 => { print!("ADD A, d8"); self.instr.add_val(A); 2 },
            // RST 0
            0xC7 => { print!("RST 0"); self.instr.rst(0); 4 },
            // RET Z
            0xC8 => { print!("RET Z"); self.instr.retc(self.instr.get_flag(ALUFlag::Z)) },
            // RET
            0xC9 => { print!("RET"); self.instr.ret(); 4 },
            // JP Z, a16
            0xCA => { print!("JP Z, a16"); self.instr.jp(self.instr.get_flag(ALUFlag::Z)) },
            // CB Instruction
            0xCB => { print!("CB Instruction"); self.exec_cb() },
            // CALL Z, a16
            0xCC => { print!("CALL Z, a16"); self.instr.call(self.instr.get_flag(ALUFlag::Z)) },
            // CALL a16
            0xCD => { print!("CALL a16"); self.instr.call(true) },
            // ADC d8
            0xCE => { print!("ADC d8"); self.instr.adc_val(); 2 },
            // RST 1
            0xCF => { print!("RST 1"); self.instr.rst(1); 4 },
            // RET NC
            0xD0 => { print!("RET NC"); self.instr.retc(!self.instr.get_flag(ALUFlag::C)) },
            // POP DE
            0xD1 => { print!("POP DE"); self.instr.pop(DE); 3 },
            // JP NC, a16
            0xD2 => { print!("JP NC, a16"); self.instr.jp(!self.instr.get_flag(ALUFlag::C)) },
            // CALL NC, a16
            0xD4 => { print!("CALL NC, a16"); self.instr.call(!self.instr.get_flag(ALUFlag::C)) },
            // PUSH DE
            0xD5 => { print!("PUSH DE"); self.instr.push(DE); 4 },
            // SUB d8
            0xD6 => { print!("SUB d8"); self.instr.sub_val(A); 2 },
            // RST 2
            0xD7 => { print!("RST 2"); self.instr.rst(2); 4 },
            // RET C
            0xD8 => { print!("RET C"); self.instr.retc(self.instr.get_flag(ALUFlag::C)) },
            // RETI
            0xD9 => { print!("RETI"); self.instr.reti(); 4 },
            // JP C, a16
            0xDA => { print!("JP C, a16"); self.instr.jp(self.instr.get_flag(ALUFlag::C)) },
            // CALL C, a16
            0xDC => { print!("CALL C, a16"); self.instr.call(self.instr.get_flag(ALUFlag::C)) },
            // SBC d8
            0xDE => { print!("SBC d8"); self.instr.sbc_val(); 2 },
            // RST 3
            0xDF => { print!("RST 3"); self.instr.rst(3); 4 },
            // LD (a8), A
            0xE0 => { print!("LD (a8), A"); self.instr.ld_mbr(A); 3 },
            // POP HL
            0xE1 => { print!("POP HL"); self.instr.pop(HL); 3 },
            // LD (C), A
            0xE2 => { print!("LD (C), A"); self.instr.ld_mrc(); 2 },
            // PUSH HL
            0xE5 => { print!("PUSH HL"); self.instr.push(HL); 4 },
            // AND d8
            0xE6 => { print!("AND d8"); self.instr.and_val(); 2 },
            // RST 4
            0xE7 => { print!("RST 4"); self.instr.rst(4); 4 },
            // ADD SP, s8
            0xE8 => { print!("ADD SP, s8"); self.instr.add_stack(); 4 },
            // JP (HL)
            0xE9 => { print!("JP (HL)"); self.instr.jp_hl(); 4 },
            // LD (a16), A
            0xEA => { print!("LD (a16), A"); self.instr.ld_mwr(A); 4 },
            // XOR d8
            0xEE => { print!("XOR d8"); self.instr.xor_val(); 2 },
            // RST 5
            0xEF => { print!("RST 5"); self.instr.rst(5); 4 },
            // LD A, (a8)
            0xF0 => { print!("LD A, (a8)"); self.instr.ld_rmb(A); 3 },
            // POP AF
            0xF1 => { print!("POP AF"); self.instr.pop(AF); 3 },
            // LD A, (C)
            0xF2 => { print!("LD A, (C)"); self.instr.ld_rmc(); 2 },
            // DI
            0xF3 => { print!("DI"); self.instr.di(); 1 },
            // PUSH AF
            0xF5 => { print!("PUSH AF"); self.instr.push(AF); 4 },
            // OR d8
            0xF6 => { print!("OR d8"); self.instr.or_val(); 2 },
            // RST 6
            0xF7 => { print!("RST 6"); self.instr.rst(6); 4 },
            // LD HL, SP+s8
            0xF8 => { print!("LD HL, SP+s8"); self.instr.add_stack(); 3 },
            // LD SP, HL
            0xF9 => { print!("LD SP, HL"); self.instr.ld_rrw(SP, HL); 2 },
            // LD A, (a16)
            0xFA => { print!("LD A, (a16)"); self.instr.ld_rmw(A); 4 },
            // EI
            0xFB => { print!("EI");self.instr.ei(); 1 },
            // CP d8
            0xFE => { print!("CP d8"); self.instr.cp_val(); 2 },
            // RST 7
            0xFF => { print!("RST 7"); self.instr.rst(7); 4 },
            _ => { panic!("boop"); }
        }
        println!("");
    }

    fn exec_cb(&mut self) -> u8 {
        print!(" --- ");
        let opcode = self.instr.fetch();
        match opcode {
            // RLC B
            0x00 => { print!("RLC B");  self.instr.rlc(B); 2 },
            // RLC C
            0x01 => { print!("RLC C");  self.instr.rlc(C); 2 },
            // RLC D
            0x02 => { print!("RLC D");  self.instr.rlc(D); 2 },
            // RLC E
            0x03 => { print!("RLC E");  self.instr.rlc(E); 2 },
            // RLC H
            0x04 => { print!("RLC H");  self.instr.rlc(H); 2 },
            // RLC L
            0x05 => { print!("RLC L");  self.instr.rlc(L); 2 },
            // RLC (HL)
            0x06 => { print!("RLC (HL)");  self.instr.rlc_mem(HL); 4 },
            // RLC A
            0x07 => { print!("RLC A");  self.instr.rlc(A); 2 },
            // RRC B
            0x08 => { print!("RRC B");  self.instr.rrc(B); 2 },
            // RRC C
            0x09 => { print!("RRC C");  self.instr.rrc(C); 2 },
            // RRC D
            0x0A => { print!("RRC D");  self.instr.rrc(D); 2 },
            // RRC E
            0x0B => { print!("RRC E");  self.instr.rrc(E); 2 },
            // RRC H
            0x0C => { print!("RRC H");  self.instr.rrc(H); 2 },
            // RRC L
            0x0D => { print!("RRC L");  self.instr.rrc(L); 2 },
            // RRC (HL)
            0x0E => { print!("RRC (HL)");  self.instr.rrc_mem(HL); 4 },
            // RRC A
            0x0F => { print!("RRC A");  self.instr.rrc(A); 2 },
            // RL B
            0x10 => { print!("RL B");  self.instr.rl(B); 2 },
            // RL C
            0x11 => { print!("RL C");  self.instr.rl(C); 2 },
            // RL D
            0x12 => { print!("RL D");  self.instr.rl(D); 2 },
            // RL E
            0x13 => { print!("RL E");  self.instr.rl(E); 2 },
            // RL H
            0x14 => { print!("RL H");  self.instr.rl(H); 2 },
            // RL L
            0x15 => { print!("RL L");  self.instr.rl(L); 2 },
            // RL (HL)
            0x16 => { print!("RL (HL)");  self.instr.rl_mem(HL); 4 },
            // RL A
            0x17 => { print!("RL A");  self.instr.rl(A); 2 },
            // RR B
            0x18 => { print!("RR B");  self.instr.rr(B); 2 },
            // RR C
            0x19 => { print!("RR C");  self.instr.rr(C); 2 },
            // RR D
            0x1A => { print!("RR D");  self.instr.rr(D); 2 },
            // RR E
            0x1B => { print!("RR E");  self.instr.rr(E); 2 },
            // RR H
            0x1C => { print!("RR H");  self.instr.rr(H); 2 },
            // RR L
            0x1D => { print!("RR L");  self.instr.rr(L); 2 },
            // RR (HL)
            0x1E => { print!("RR (HL)");  self.instr.rr_mem(HL); 4 },
            // RR A
            0x1F => { print!("RR A");  self.instr.rr(A); 2 },
            // SLA B
            0x20 => { print!("SLA B");  self.instr.sla(B); 2 },
            // SLA C
            0x21 => { print!("SLA C");  self.instr.sla(C); 2 },
            // SLA D
            0x22 => { print!("SLA D");  self.instr.sla(D); 2 },
            // SLA E
            0x23 => { print!("SLA E");  self.instr.sla(E); 2 },
            // SLA H
            0x24 => { print!("SLA H");  self.instr.sla(H); 2 },
            // SLA L
            0x25 => { print!("SLA L");  self.instr.sla(L); 2 },
            // SLA (HL)
            0x26 => { print!("SLA (HL)");  self.instr.sla_mem(HL); 4 },
            // SLA A
            0x27 => { print!("SLA A");  self.instr.sla(A); 2 },
            // SRA B
            0x28 => { print!("SRA B");  self.instr.sra(B); 2 },
            // SRA C
            0x29 => { print!("SRA C");  self.instr.sra(C); 2 },
            // SRA D
            0x2A => { print!("SRA D");  self.instr.sra(D); 2 },
            // SRA E
            0x2B => { print!("SRA E");  self.instr.sra(E); 2 },
            // SRA H
            0x2C => { print!("SRA H");  self.instr.sra(H); 2 },
            // SRA L
            0x2D => { print!("SRA L");  self.instr.sra(L); 2 },
            // SRA (HL)
            0x2E => { print!("SRA (HL)");  self.instr.sra_mem(HL); 4 },
            // SRA A
            0x2F => { print!("SRA A");  self.instr.sra(A); 2 },
            // SWAP B
            0x30 => { print!("SWAP B");  self.instr.swap(B); 2 },
            // SWAP C
            0x31 => { print!("SWAP C");  self.instr.swap(C); 2 },
            // SWAP D
            0x32 => { print!("SWAP D");  self.instr.swap(D); 2 },
            // SWAP E
            0x33 => { print!("SWAP E");  self.instr.swap(E); 2 },
            // SWAP H
            0x34 => { print!("SWAP H");  self.instr.swap(H); 2 },
            // SWAP L
            0x35 => { print!("SWAP L");  self.instr.swap(L); 2 },
            // SWAP (HL)
            0x36 => { print!("SWAP (HL)");  self.instr.swap_mem(HL); 4 },
            // SWAP A
            0x37 => { print!("SWAP A");  self.instr.swap(A); 2 },
            // SRL B
            0x38 => { print!("SRL B");  self.instr.srl(B); 2 },
            // SRL C
            0x39 => { print!("SRL C");  self.instr.srl(C); 2 },
            // SRL D
            0x3A => { print!("SRL D");  self.instr.srl(D); 2 },
            // SRL E
            0x3B => { print!("SRL E");  self.instr.srl(E); 2 },
            // SRL H
            0x3C => { print!("SRL H");  self.instr.srl(H); 2 },
            // SRL L
            0x3D => { print!("SRL L");  self.instr.srl(L); 2 },
            // SRL (HL)
            0x3E => { print!("SRL (HL)");  self.instr.srl_mem(HL); 4 },
            // SRL A
            0x3F => { print!("SRL A");  self.instr.srl(A); 2 },
            // BIT 0, B
            0x40 => { print!("BIT 0, B");  self.instr.bit(0, B); 2 },
            // BIT 0, C
            0x41 => { print!("BIT 0, C");  self.instr.bit(0, C); 2 },
            // BIT 0, D
            0x42 => { print!("BIT 0, D");  self.instr.bit(0, D); 2 },
            // BIT 0, E
            0x43 => { print!("BIT 0, E");  self.instr.bit(0, E); 2 },
            // BIT 0, H
            0x44 => { print!("BIT 0, H");  self.instr.bit(0, H); 2 },
            // BIT 0, L
            0x45 => { print!("BIT 0, L");  self.instr.bit(0, L); 2 },
            // BIT 0, (HL)
            0x46 => { print!("BIT 0, (HL)");  self.instr.bit_mem(0, HL); 4 },
            // BIT 0, A
            0x47 => { print!("BIT 0, A");  self.instr.bit(0, A); 2 },
            // BIT 1, B
            0x48 => { print!("BIT 1, B");  self.instr.bit(1, B); 2 },
            // BIT 1, C
            0x49 => { print!("BIT 1, C");  self.instr.bit(1, C); 2 },
            // BIT 1, D
            0x4A => { print!("BIT 1, D");  self.instr.bit(1, D); 2 },
            // BIT 1, E
            0x4B => { print!("BIT 1, E");  self.instr.bit(1, E); 2 },
            // BIT 1, H
            0x4C => { print!("BIT 1, H");  self.instr.bit(1, H); 2 },
            // BIT 1, L
            0x4D => { print!("BIT 1, L");  self.instr.bit(1, L); 2 },
            // BIT 1, (HL)
            0x4E => { print!("BIT 1, (HL)");  self.instr.bit_mem(1, HL); 4 },
            // BIT 1, A
            0x4F => { print!("BIT 1, A");  self.instr.bit(1, A); 2 },
            // BIT 2, B
            0x50 => { print!("BIT 2, B");  self.instr.bit(2, B); 2 },
            // BIT 2, C
            0x51 => { print!("BIT 2, C");  self.instr.bit(2, C); 2 },
            // BIT 2, D
            0x52 => { print!("BIT 2, D");  self.instr.bit(2, D); 2 },
            // BIT 2, E
            0x53 => { print!("BIT 2, E");  self.instr.bit(2, E); 2 },
            // BIT 2, H
            0x54 => { print!("BIT 2, H");  self.instr.bit(2, H); 2 },
            // BIT 2, L
            0x55 => { print!("BIT 2, L");  self.instr.bit(2, L); 2 },
            // BIT 2, (HL)
            0x56 => { print!("BIT 2, (HL)");  self.instr.bit_mem(2, HL); 4 },
            // BIT 2, A
            0x57 => { print!("BIT 2, A");  self.instr.bit(2, A); 2 },
            // BIT 3, B
            0x58 => { print!("BIT 3, B");  self.instr.bit(3, B); 2 },
            // BIT 3, C
            0x59 => { print!("BIT 3, C");  self.instr.bit(3, C); 2 },
            // BIT 3, D
            0x5A => { print!("BIT 3, D");  self.instr.bit(3, D); 2 },
            // BIT 3, E
            0x5B => { print!("BIT 3, E");  self.instr.bit(3, E); 2 },
            // BIT 3, H
            0x5C => { print!("BIT 3, H");  self.instr.bit(3, H); 2 },
            // BIT 3, L
            0x5D => { print!("BIT 3, L");  self.instr.bit(3, L); 2 },
            // BIT 3, (HL)
            0x5E => { print!("BIT 3, (HL)");  self.instr.bit_mem(3, HL); 4 },
            // BIT 3, A
            0x5F => { print!("BIT 3, A");  self.instr.bit(3, A); 2 },
            // BIT 4, B
            0x60 => { print!("BIT 4, B");  self.instr.bit(4, B); 2 },
            // BIT 4, C
            0x61 => { print!("BIT 4, C");  self.instr.bit(4, C); 2 },
            // BIT 4, D
            0x62 => { print!("BIT 4, D");  self.instr.bit(4, D); 2 },
            // BIT 4, E
            0x63 => { print!("BIT 4, E");  self.instr.bit(4, E); 2 },
            // BIT 4, H
            0x64 => { print!("BIT 4, H");  self.instr.bit(4, H); 2 },
            // BIT 4, L
            0x65 => { print!("BIT 4, L");  self.instr.bit(4, L); 2 },
            // BIT 4, (HL)
            0x66 => { print!("BIT 4, (HL)");  self.instr.bit_mem(4, HL); 4 },
            // BIT 4, A
            0x67 => { print!("BIT 4, A");  self.instr.bit(4, A); 2 },
            // BIT 5, B
            0x68 => { print!("BIT 5, B");  self.instr.bit(5, B); 2 },
            // BIT 5, C
            0x69 => { print!("BIT 5, C");  self.instr.bit(5, C); 2 },
            // BIT 5, D
            0x6A => { print!("BIT 5, D");  self.instr.bit(5, D); 2 },
            // BIT 5, E
            0x6B => { print!("BIT 5, E");  self.instr.bit(5, E); 2 },
            // BIT 5, H
            0x6C => { print!("BIT 5, H");  self.instr.bit(5, H); 2 },
            // BIT 5, L
            0x6D => { print!("BIT 5, L");  self.instr.bit(5, L); 2 },
            // BIT 5, (HL)
            0x6E => { print!("BIT 5, (HL)");  self.instr.bit_mem(5, HL); 4 },
            // BIT 5, A
            0x6F => { print!("BIT 5, A");  self.instr.bit(5, A); 2 },
            // BIT 6, B
            0x70 => { print!("BIT 6, B");  self.instr.bit(6, B); 2 },
            // BIT 6, C
            0x71 => { print!("BIT 6, C");  self.instr.bit(6, C); 2 },
            // BIT 6, D
            0x72 => { print!("BIT 6, D");  self.instr.bit(6, D); 2 },
            // BIT 6, E
            0x73 => { print!("BIT 6, E");  self.instr.bit(6, E); 2 },
            // BIT 6, H
            0x74 => { print!("BIT 6, H");  self.instr.bit(6, H); 2 },
            // BIT 6, L
            0x75 => { print!("BIT 6, L");  self.instr.bit(6, L); 2 },
            // BIT 6, (HL)
            0x76 => { print!("BIT 6, (HL)");  self.instr.bit_mem(6, HL); 4 },
            // BIT 6, A
            0x77 => { print!("BIT 6, A");  self.instr.bit(6, A); 2 },
            // BIT 7, B
            0x78 => { print!("BIT 7, B");  self.instr.bit(7, B); 2 },
            // BIT 7, C
            0x79 => { print!("BIT 7, C");  self.instr.bit(7, C); 2 },
            // BIT 7, D
            0x7A => { print!("BIT 7, D");  self.instr.bit(7, D); 2 },
            // BIT 7, E
            0x7B => { print!("BIT 7, E");  self.instr.bit(7, E); 2 },
            // BIT 7, H
            0x7C => { print!("BIT 7, H");  self.instr.bit(7, H); 2 },
            // BIT 7, L
            0x7D => { print!("BIT 7, L");  self.instr.bit(7, L); 2 },
            // BIT 7, (HL)
            0x7E => { print!("BIT 7, (HL)");  self.instr.bit_mem(7, HL); 4 },
            // BIT 7, A
            0x7F => { print!("BIT 7, A");  self.instr.bit(7, A); 2 },
            // RES 0, B
            0x80 => { print!("RES 0, B");  self.instr.res(0, B); 2 },
            // RES 0, C
            0x81 => { print!("RES 0, C");  self.instr.res(0, C); 2 },
            // RES 0, D
            0x82 => { print!("RES 0, D");  self.instr.res(0, D); 2 },
            // RES 0, E
            0x83 => { print!("RES 0, E");  self.instr.res(0, E); 2 },
            // RES 0, H
            0x84 => { print!("RES 0, H");  self.instr.res(0, H); 2 },
            // RES 0, L
            0x85 => { print!("RES 0, L");  self.instr.res(0, L); 2 },
            // RES 0, (HL)
            0x86 => { print!("RES 0, (HL)");  self.instr.res_mem(0, HL); 4 },
            // RES 0, A
            0x87 => { print!("RES 0, A");  self.instr.res(0, A); 2 },
            // RES 1, B
            0x88 => { print!("RES 1, B");  self.instr.res(1, B); 2 },
            // RES 1, C
            0x89 => { print!("RES 1, C");  self.instr.res(1, C); 2 },
            // RES 1, D
            0x8A => { print!("RES 1, D");  self.instr.res(1, D); 2 },
            // RES 1, E
            0x8B => { print!("RES 1, E");  self.instr.res(1, E); 2 },
            // RES 1, H
            0x8C => { print!("RES 1, H");  self.instr.res(1, H); 2 },
            // RES 1, L
            0x8D => { print!("RES 1, L");  self.instr.res(1, L); 2 },
            // RES 1, (HL)
            0x8E => { print!("RES 1, (HL)");  self.instr.res_mem(1, HL); 4 },
            // RES 1, A
            0x8F => { print!("RES 1, A");  self.instr.res(1, A); 2 },
            // RES 2, B
            0x90 => { print!("RES 2, B");  self.instr.res(2, B); 2 },
            // RES 2, C
            0x91 => { print!("RES 2, C");  self.instr.res(2, C); 2 },
            // RES 2, D
            0x92 => { print!("RES 2, D");  self.instr.res(2, D); 2 },
            // RES 2, E
            0x93 => { print!("RES 2, E");  self.instr.res(2, E); 2 },
            // RES 2, H
            0x94 => { print!("RES 2, H");  self.instr.res(2, H); 2 },
            // RES 2, L
            0x95 => { print!("RES 2, L");  self.instr.res(2, L); 2 },
            // RES 2, (HL)
            0x96 => { print!("RES 2, (HL)");  self.instr.res_mem(2, HL); 4 },
            // RES 2, A
            0x97 => { print!("RES 2, A");  self.instr.res(2, A); 2 },
            // RES 3, B
            0x98 => { print!("RES 3, B");  self.instr.res(3, B); 2 },
            // RES 3, C
            0x99 => { print!("RES 3, C");  self.instr.res(3, C); 2 },
            // RES 3, D
            0x9A => { print!("RES 3, D");  self.instr.res(3, D); 2 },
            // RES 3, E
            0x9B => { print!("RES 3, E");  self.instr.res(3, E); 2 },
            // RES 3, H
            0x9C => { print!("RES 3, H");  self.instr.res(3, H); 2 },
            // RES 3, L
            0x9D => { print!("RES 3, L");  self.instr.res(3, L); 2 },
            // RES 3, (HL)
            0x9E => { print!("RES 3, (HL)");  self.instr.res_mem(3, HL); 4 },
            // RES 3, A
            0x9F => { print!("RES 3, A");  self.instr.res(3, A); 2 },
            // RES 4, B
            0xA0 => { print!("RES 4, B");  self.instr.res(4, B); 2 },
            // RES 4, C
            0xA1 => { print!("RES 4, C");  self.instr.res(4, C); 2 },
            // RES 4, D
            0xA2 => { print!("RES 4, D");  self.instr.res(4, D); 2 },
            // RES 4, E
            0xA3 => { print!("RES 4, E");  self.instr.res(4, E); 2 },
            // RES 4, H
            0xA4 => { print!("RES 4, H");  self.instr.res(4, H); 2 },
            // RES 4, L
            0xA5 => { print!("RES 4, L");  self.instr.res(4, L); 2 },
            // RES 4, (HL)
            0xA6 => { print!("RES 4, (HL)");  self.instr.res_mem(4, HL); 4 },
            // RES 4, A
            0xA7 => { print!("RES 4, A");  self.instr.res(4, A); 2 },
            // RES 5, B
            0xA8 => { print!("RES 5, B");  self.instr.res(5, B); 2 },
            // RES 5, C
            0xA9 => { print!("RES 5, C");  self.instr.res(5, C); 2 },
            // RES 5, D
            0xAA => { print!("RES 5, D");  self.instr.res(5, D); 2 },
            // RES 5, E
            0xAB => { print!("RES 5, E");  self.instr.res(5, E); 2 },
            // RES 5, H
            0xAC => { print!("RES 5, H");  self.instr.res(5, H); 2 },
            // RES 5, L
            0xAD => { print!("RES 5, L");  self.instr.res(5, L); 2 },
            // RES 5, (HL)
            0xAE => { print!("RES 5, (HL)");  self.instr.res_mem(5, HL); 4 },
            // RES 5, A
            0xAF => { print!("RES 5, A");  self.instr.res(5, A); 2 },
            // RES 6, B
            0xB0 => { print!("RES 6, B");  self.instr.res(6, B); 2 },
            // RES 6, C
            0xB1 => { print!("RES 6, C");  self.instr.res(6, C); 2 },
            // RES 6, D
            0xB2 => { print!("RES 6, D");  self.instr.res(6, D); 2 },
            // RES 6, E
            0xB3 => { print!("RES 6, E");  self.instr.res(6, E); 2 },
            // RES 6, H
            0xB4 => { print!("RES 6, H");  self.instr.res(6, H); 2 },
            // RES 6, L
            0xB5 => { print!("RES 6, L");  self.instr.res(6, L); 2 },
            // RES 6, (HL)
            0xB6 => { print!("RES 6, (HL)");  self.instr.res_mem(6, HL); 4 },
            // RES 6, A
            0xB7 => { print!("RES 6, A");  self.instr.res(6, A); 2 },
            // RES 7, B
            0xB8 => { print!("RES 7, B");  self.instr.res(7, B); 2 },
            // RES 7, C
            0xB9 => { print!("RES 7, C");  self.instr.res(7, C); 2 },
            // RES 7, D
            0xBA => { print!("RES 7, D");  self.instr.res(7, D); 2 },
            // RES 7, E
            0xBB => { print!("RES 7, E");  self.instr.res(7, E); 2 },
            // RES 7, H
            0xBC => { print!("RES 7, H");  self.instr.res(7, H); 2 },
            // RES 7, L
            0xBD => { print!("RES 7, L");  self.instr.res(7, L); 2 },
            // RES 7, (HL)
            0xBE => { print!("RES 7, (HL)");  self.instr.res_mem(7, HL); 4 },
            // RES 7, A
            0xBF => { print!("RES 7, A");  self.instr.res(7, A); 2 },
            // SET 0, B
            0xC0 => { print!("SET 0, B");  self.instr.set(0, B); 2 },
            // SET 0, C
            0xC1 => { print!("SET 0, C");  self.instr.set(0, C); 2 },
            // SET 0, D
            0xC2 => { print!("SET 0, D");  self.instr.set(0, D); 2 },
            // SET 0, E
            0xC3 => { print!("SET 0, E");  self.instr.set(0, E); 2 },
            // SET 0, H
            0xC4 => { print!("SET 0, H");  self.instr.set(0, H); 2 },
            // SET 0, L
            0xC5 => { print!("SET 0, L");  self.instr.set(0, L); 2 },
            // SET 0, (HL)
            0xC6 => { print!("SET 0, (HL)");  self.instr.set_mem(0, HL); 4 },
            // SET 0, A
            0xC7 => { print!("SET 0, A");  self.instr.set(0, A); 2 },
            // SET 1, B
            0xC8 => { print!("SET 1, B");  self.instr.set(1, B); 2 },
            // SET 1, C
            0xC9 => { print!("SET 1, C");  self.instr.set(1, C); 2 },
            // SET 1, D
            0xCA => { print!("SET 1, D");  self.instr.set(1, D); 2 },
            // SET 1, E
            0xCB => { print!("SET 1, E");  self.instr.set(1, E); 2 },
            // SET 1, H
            0xCC => { print!("SET 1, H");  self.instr.set(1, H); 2 },
            // SET 1, L
            0xCD => { print!("SET 1, L");  self.instr.set(1, L); 2 },
            // SET 1, (HL)
            0xCE => { print!("SET 1, (HL)");  self.instr.set_mem(1, HL); 4 },
            // SET 1, A
            0xCF => { print!("SET 1, A");  self.instr.set(1, A); 2 },
            // SET 2, B
            0xD0 => { print!("SET 2, B");  self.instr.set(2, B); 2 },
            // SET 2, C
            0xD1 => { print!("SET 2, C");  self.instr.set(2, C); 2 },
            // SET 2, D
            0xD2 => { print!("SET 2, D");  self.instr.set(2, D); 2 },
            // SET 2, E
            0xD3 => { print!("SET 2, E");  self.instr.set(2, E); 2 },
            // SET 2, H
            0xD4 => { print!("SET 2, H");  self.instr.set(2, H); 2 },
            // SET 2, L
            0xD5 => { print!("SET 2, L");  self.instr.set(2, L); 2 },
            // SET 2, (HL)
            0xD6 => { print!("SET 2, (HL)");  self.instr.set_mem(2, HL); 4 },
            // SET 2, A
            0xD7 => { print!("SET 2, A");  self.instr.set(2, A); 2 },
            // SET 3, B
            0xD8 => { print!("SET 3, B");  self.instr.set(3, B); 2 },
            // SET 3, C
            0xD9 => { print!("SET 3, C");  self.instr.set(3, C); 2 },
            // SET 3, D
            0xDA => { print!("SET 3, D");  self.instr.set(3, D); 2 },
            // SET 3, E
            0xDB => { print!("SET 3, E");  self.instr.set(3, E); 2 },
            // SET 3, H
            0xDC => { print!("SET 3, H");  self.instr.set(3, H); 2 },
            // SET 3, L
            0xDD => { print!("SET 3, L");  self.instr.set(3, L); 2 },
            // SET 3, (HL)
            0xDE => { print!("SET 3, (HL)");  self.instr.set_mem(3, HL); 4 },
            // SET 3, A
            0xDF => { print!("SET 3, A");  self.instr.set(3, A); 2 },
            // SET 4, B
            0xE0 => { print!("SET 4, B");  self.instr.set(4, B); 2 },
            // SET 4, C
            0xE1 => { print!("SET 4, C");  self.instr.set(4, C); 2 },
            // SET 4, D
            0xE2 => { print!("SET 4, D");  self.instr.set(4, D); 2 },
            // SET 4, E
            0xE3 => { print!("SET 4, E");  self.instr.set(4, E); 2 },
            // SET 4, H
            0xE4 => { print!("SET 4, H");  self.instr.set(4, H); 2 },
            // SET 4, L
            0xE5 => { print!("SET 4, L");  self.instr.set(4, L); 2 },
            // SET 4, (HL)
            0xE6 => { print!("SET 4, (HL)");  self.instr.set_mem(4, HL); 4 },
            // SET 4, A
            0xE7 => { print!("SET 4, A");  self.instr.set(4, A); 2 },
            // SET 5, B
            0xE8 => { print!("SET 5, B");  self.instr.set(5, B); 2 },
            // SET 5, C
            0xE9 => { print!("SET 5, C");  self.instr.set(5, C); 2 },
            // SET 5, D
            0xEA => { print!("SET 5, D");  self.instr.set(5, D); 2 },
            // SET 5, E
            0xEB => { print!("SET 5, E");  self.instr.set(5, E); 2 },
            // SET 5, H
            0xEC => { print!("SET 5, H");  self.instr.set(5, H); 2 },
            // SET 5, L
            0xED => { print!("SET 5, L");  self.instr.set(5, L); 2 },
            // SET 5, (HL)
            0xEE => { print!("SET 5, (HL)");  self.instr.set_mem(5, HL); 4 },
            // SET 5, A
            0xEF => { print!("SET 5, A");  self.instr.set(5, A); 2 },
            // SET 6, B
            0xF0 => { print!("SET 6, B");  self.instr.set(6, B); 2 },
            // SET 6, C
            0xF1 => { print!("SET 6, C");  self.instr.set(6, C); 2 },
            // SET 6, D
            0xF2 => { print!("SET 6, D");  self.instr.set(6, D); 2 },
            // SET 6, E
            0xF3 => { print!("SET 6, E");  self.instr.set(6, E); 2 },
            // SET 6, H
            0xF4 => { print!("SET 6, H");  self.instr.set(6, H); 2 },
            // SET 6, L
            0xF5 => { print!("SET 6, L");  self.instr.set(6, L); 2 },
            // SET 6, (HL)
            0xF6 => { print!("SET 6, (HL)");  self.instr.set_mem(6, HL); 4 },
            // SET 6, A
            0xF7 => { print!("SET 6, A");  self.instr.set(6, A); 2 },
            // SET 7, B
            0xF8 => { print!("SET 7, B");  self.instr.set(7, B); 2 },
            // SET 7, C
            0xF9 => { print!("SET 7, C");  self.instr.set(7, C); 2 },
            // SET 7, D
            0xFA => { print!("SET 7, D");  self.instr.set(7, D); 2 },
            // SET 7, E
            0xFB => { print!("SET 7, E");  self.instr.set(7, E); 2 },
            // SET 7, H
            0xFC => { print!("SET 7, H");  self.instr.set(7, H); 2 },
            // SET 7, L
            0xFD => { print!("SET 7, L");  self.instr.set(7, L); 2 },
            // SET 7, (HL)
            0xFE => { print!("SET 7, (HL)");  self.instr.set_mem(7, HL); 4 },
            // SET 7, A
            0xFF => { print!("SET 7, A");  self.instr.set(7, A); 2 },
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
    // 0xFF05 -- TIMA -- Timer Counter ------ increments at freq. of TAC. Resets at overflow to TMA value and triggers interrupt. CPU jumps to 0x50 at TIMA overflow, where the next instruction is read.
    // 0xFF06 -- TMA --- Timer Mod ---------- contains an offset value for the timer to restart with
    // 0xFF07 -- TAC --- Timer Control
    //                   0 (Stop)     00 -- 4096 Hz
    //                   1 (Start)    01 -- 262144 Hz
    //                                10 -- 65536 Hz
    //                                11 -- 16384 Hz

    //Stalls CPU until interrupt is triggered, at which point HALT is broken and CPU switches to interrupt vector. After the interrupt returns, instructions continue immediately following the halt.
    //pub fn halt() {}

    // these two are interrupt enablers
    //pub fn ei() {}

    //pub fn reti() {}
}

