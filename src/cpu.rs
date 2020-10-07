use crate::register::Register;
use crate::mem_map::Memory;
use crate::register::ALUFlag;
use crate::register::REG;
use crate::instructions::InstructionSet;
use crate::cb_instructions::CBInstructionSet;
//single core
pub struct CPU {
    i: InstructionSet,
    r: Register,
    m: Memory,
}


impl CPU {
    pub fn init(registers: Register, mem_map: Memory) -> CPU {
        CPU {
            i: InstructionSet::init(registers,  mem_map),
            r: registers,
            m: mem_map
        }
    }

    pub fn read_opcode(&self) -> u8 {
        0
    }

    pub fn scan_word(&self) -> u16 {
        1
    }

    pub fn scan_byte(&self) -> u8 {
        2
    }

    // Returns number of cycles
    pub fn exec(&mut self) -> u8 {
        let opcode = self.read_opcode();
        match opcode {
            // NOP
            0x00 => { 1 },
            // LD BC, d16
            0x01 => { self.i.ld_rw(REG::BC, self.scan_word()); 3 },
            // LD (BC), A
            0x02 => { self.i.ld_mr(REG::BC, REG::A); 2 },
            // INC BC
            0x03 => { self.r.set_word(REG::BC, self.r.get_word(REG::BC).wrapping_add(1)); 2 },
            // INC B
            0x04 => { self.i.inc(REG::B); 1 },
            // DEC B
            0x05 => { self.i.dec(REG::B); 1 },
            // LD B, u8
            0x06 => { self.i.ld_rb(REG::B, self.scan_byte()); 2 },
            // RLCA
            0x07 => { self.i.rlca(REG::A); 1 },
            // LD (u16), SP
            0x08 => { self.i.ld_mw(REG::SP, self.scan_word()); 5 },
            // ADD HL, BC
            0x09 => { self.i.add_word(REG::HL, self.r.get_word(REG::BC)); 2 },
            // LD A, (BC)
            0x0A => { self.i.ld_rm(REG::A, REG::BC); 2 },
            // DEC BC
            0x0B => { self.r.set_word(REG::BC, self.r.get_word(REG::BC).wrapping_sub(1)); 2 },
            // INC C
            0x0C => { self.i.inc(REG::C); 1 },
            // DEC C
            0x0D => { self.i.dec(REG::C); 1 },
            // LD C, u8
            0x0E => { self.i.ld_rb(REG::C, self.scan_byte()); 2 },
            // RRCA
            0x0F => { self.i.rrca(REG::A); 1 },
            // STOP
            0x10 => { self.i.stop(); 1 }, // TODO
            // LD DE, U16
            0x11 => { self.i.ld_rw(REG::DE, self.scan_word()); 3 },
            // LD (DE), A
            0x12 => { self.i.ld_mr(REG::DE, REG::A); 2 },
            // INC DE
            0x13 => { self.r.set_word(REG::DE, self.r.get_word(REG::DE).wrapping_add(1)); 2 },
            // INC D
            0x14 => { self.i.inc(REG::D); 1 },
            // DEC D
            0x15 => { self.i.dec(REG::D); 1 },
            // LD D, u8
            0x16 => { self.i.ld_rb(REG::D, self.scan_byte()); 2 },
            // RLA
            0x17 => { self.i.rla(REG::A); 1 },
            // JR s8
            0x18 => { self.i.jr(self.scan_byte() as i8, true) }, // TODO
            // ADD HL, DE
            0x19 => { self.i.add_word(REG::HL, self.r.get_word(REG::DE)); 2 },
            // LD A, (DE)
            0x1A => { self.i.ld_rm(REG::A, REG::DE); 2 },
            // DEC DE
            0x1B => { self.r.set_word(REG::DE, self.r.get_word(REG::DE).wrapping_sub(1)); 2 },
            // INC E
            0x1C => { self.i.inc(REG::E); 1 },
            // DEC E
            0x1D => { self.i.dec(REG::E); 1 },
            // LD E, u8
            0x1E => { self.i.ld_rb(REG::E, self.scan_byte()); 2 },
            // RRA
            0x1F => { self.i.rra(REG::A); 1 },
            // JR NZ, s8
            0x20 => { self.i.jr(self.scan_byte() as i8, !self.r.get_flag(ALUFlag::Z)) },
            // LD HL, U16
            0x21 => { self.i.ld_rw(REG::HL, self.scan_word()); 3 },
            // LD (HL+), A
            0x22 => { self.i.ld_mem_inc(); 2 },
            // INC HL
            0x23 => { self.r.set_word(REG::HL, self.r.get_word(REG::HL).wrapping_add(1)); 2 },
            // INC H
            0x24 => { self.i.inc(REG::H); 1 },
            // DEC H
            0x25 => { self.i.dec(REG::H); 1 },
            // LD H, u8
            0x26 => { self.i.ld_rb(REG::H, self.scan_byte()); 2 },
            // DAA
            0x27 => { self.i.daa(); 1 },
            // JR Z, s8
            0x28 => { self.i.jr(self.scan_byte() as i8, self.r.get_flag(ALUFlag::Z)) },
            // ADD HL, HL
            0x29 => { self.i.add_word(REG::HL, self.r.get_word(REG::HL)); 2 },
            // LD A, (HL+)
            0x2A => { self.i.ld_inc(); 2 },
            // DEC HL
            0x2B => { self.r.set_word(REG::HL, self.r.get_word(REG::HL).wrapping_sub(1)); 2 },
            // INC L
            0x2C => { self.i.inc(REG::L); 1 },
            // DEC L
            0x2D => { self.i.dec(REG::L); 1 },
            // LD L, u8
            0x2E => { self.i.ld_rb(REG::L, self.scan_byte()); 2 },
            // CPL
            0x2F => { self.i.cpl(); 1 },
            // JR NC, s8
            0x30 => { self.i.jr(self.scan_byte() as i8, !self.r.get_flag(ALUFlag::C)) },
            // LD SP, U16
            0x31 => { self.i.ld_rw(REG::SP, self.scan_word()); 3 },
            // LD (HL-), A
            0x32 => { self.i.ld_mem_dec(); 2 },
            // INC SP
            0x33 => { self.r.set_word(REG::SP, self.r.get_word(REG::SP).wrapping_add(1)); 2 },
            // INC (HL)
            0x34 => { self.i.inc_mem(); 3 },
            // DEC (HL)
            0x35 => { self.i.dec_mem(); 3 },
            // LD (HL), u8
            0x36 => { self.i.ld_mb(REG::HL, self.scan_byte()); 3 },
            // SCF
            0x37 => { self.i.scf(); 1 },
            // JR C, s8
            0x38 => { self.i.jr(self.scan_byte() as i8, self.r.get_flag(ALUFlag::C)) },
            // ADD HL, SP
            0x39 => { self.i.add_word(REG::HL, self.r.get_word(REG::SP)); 2 },
            // LD A, (HL-)
            0x3A => { self.i.ld_dec(); 2 },
            // DEC SP
            0x3B => { self.r.set_word(REG::SP, self.r.get_word(REG::SP).wrapping_sub(1)); 2 },
            // INC A
            0x3C => { self.i.inc(REG::A); 1 },
            // DEC A
            0x3D => { self.i.dec(REG::A); 1 },
            // LD A, u8
            0x3E => { self.i.ld_rb(REG::A, self.scan_byte()); 2 },
            // CCF
            0x3F => { self.i.ccf(); 1 },
            // LD B, B
            0x40 => { self.i.ld_rr(REG::B, REG::B); 1 },
            // LD B, C
            0x41 => { self.i.ld_rr(REG::B, REG::C); 1 },
            // LD B, D
            0x42 => { self.i.ld_rr(REG::B, REG::D); 1 },
            // LD B, E
            0x43 => { self.i.ld_rr(REG::B, REG::E); 1 },
            // LD B, H
            0x44 => { self.i.ld_rr(REG::B, REG::H); 1 },
            // LD B, L
            0x45 => { self.i.ld_rr(REG::B, REG::L); 1 },
            // LD B, (HL)
            0x46 => { self.i.ld_rm(REG::B, REG::HL); 2 },
            // LD B, A
            0x47 => { self.i.ld_rr(REG::B, REG::A); 1 },
            // LD C, B
            0x48 => { self.i.ld_rr(REG::C, REG::B); 1 },
            // LD C, C
            0x49 => { self.i.ld_rr(REG::C, REG::C); 1 },
            // LD C, D
            0x4A => { self.i.ld_rr(REG::C, REG::D); 1 },
            // LD C, E
            0x4B => { self.i.ld_rr(REG::C, REG::E); 1 },
            // LD C, H
            0x4C => { self.i.ld_rr(REG::C, REG::H); 1 },
            // LD C, L
            0x4D => { self.i.ld_rr(REG::C, REG::L); 1 },
            // LD C, (HL)
            0x4E => { self.i.ld_rm(REG::C, REG::HL); 2 },
            // LD C, A
            0x4F => { self.i.ld_rr(REG::C, REG::A); 1 },
            // LD D, B
            0x50 => { self.i.ld_rr(REG::D, REG::B); 1 },
            // LD D, C
            0x51 => { self.i.ld_rr(REG::D, REG::C); 1 },
            // LD D, D
            0x52 => { self.i.ld_rr(REG::D, REG::D); 1 },
            // LD D, E
            0x53 => { self.i.ld_rr(REG::D, REG::E); 1 },
            // LD D, H
            0x54 => { self.i.ld_rr(REG::D, REG::H); 1 },
            // LD D, L
            0x55 => { self.i.ld_rr(REG::D, REG::L); 1 },
            // LD D, (HL)
            0x56 => { self.i.ld_rm(REG::D, REG::HL); 2 },
            // LD D, A
            0x57 => { self.i.ld_rr(REG::D, REG::A); 1 },
            // LD E, B
            0x58 => { self.i.ld_rr(REG::E, REG::B); 1 },
            // LD E, C
            0x59 => { self.i.ld_rr(REG::E, REG::C); 1 },
            // LD E, D
            0x5A => { self.i.ld_rr(REG::E, REG::D); 1 },
            // LD E, E
            0x5B => { self.i.ld_rr(REG::E, REG::E); 1 },
            // LD E, H
            0x5C => { self.i.ld_rr(REG::E, REG::H); 1 },
            // LD E, L
            0x5D => { self.i.ld_rr(REG::E, REG::L); 1 },
            // LD E, (HL)
            0x5E => { self.i.ld_rm(REG::E, REG::HL); 2 },
            // LD E, A
            0x5F => { self.i.ld_rr(REG::E, REG::A); 1 },
            // LD H, B
            0x60 => { self.i.ld_rr(REG::H, REG::B); 1 },
            // LD H, C
            0x61 => { self.i.ld_rr(REG::H, REG::C); 1 },
            // LD H, D
            0x62 => { self.i.ld_rr(REG::H, REG::D); 1 },
            // LD H, E
            0x63 => { self.i.ld_rr(REG::H, REG::E); 1 },
            // LD H, H
            0x64 => { self.i.ld_rr(REG::H, REG::H); 1 },
            // LD H, L
            0x65 => { self.i.ld_rr(REG::H, REG::L); 1 },
            // LD H, (HL)
            0x66 => { self.i.ld_rm(REG::H, REG::HL); 2 },
            // LD H, A
            0x67 => { self.i.ld_rr(REG::H, REG::A); 1 },
            // LD L, B
            0x68 => { self.i.ld_rr(REG::L, REG::B); 1 },
            // LD L, C
            0x69 => { self.i.ld_rr(REG::L, REG::C); 1 },
            // LD L, D
            0x6A => { self.i.ld_rr(REG::L, REG::D); 1 },
            // LD L, E
            0x6B => { self.i.ld_rr(REG::L, REG::E); 1 },
            // LD L, H
            0x6C => { self.i.ld_rr(REG::L, REG::H); 1 },
            // LD L, L
            0x6D => { self.i.ld_rr(REG::L, REG::L); 1 },
            // LD L, (HL)
            0x6E => { self.i.ld_rm(REG::L, REG::HL); 2 },
            // LD L, A
            0x6F => { self.i.ld_rr(REG::L, REG::A); 1 },
            // LD (HL), B
            0x70 => { self.i.ld_mr(REG::HL, REG::B); 2 },
            // LD (HL), C
            0x71 => { self.i.ld_mr(REG::HL, REG::C); 2 },
            // LD (HL), D
            0x72 => { self.i.ld_mr(REG::HL, REG::D); 2 },
            // LD (HL), E
            0x73 => { self.i.ld_mr(REG::HL, REG::E); 2 },
            // LD (HL), H
            0x74 => { self.i.ld_mr(REG::HL, REG::H); 2 },
            // LD (HL), L
            0x75 => { self.i.ld_mr(REG::HL, REG::L); 2 },
            // HALT
            0x76 => { self.i.halt(); 1 }, // TODO
            // LD (HL), A
            0x77 => { self.i.ld_mr(REG::HL, REG::A); 2 },
            // LD A, B
            0x78 => { self.i.ld_rr(REG::A, REG::B); 1 },
            // LD A, C
            0x79 => { self.i.ld_rr(REG::A, REG::C); 1 },
            // LD A, D
            0x7A => { self.i.ld_rr(REG::A, REG::D); 1 },
            // LD A, E
            0x7B => { self.i.ld_rr(REG::A, REG::E); 1 },
            // LD A, H
            0x7C => { self.i.ld_rr(REG::A, REG::H); 1 },
            // LD A, L
            0x7D => { self.i.ld_rr(REG::A, REG::L); 1 },
            // LD A, (HL)
            0x7E => { self.i.ld_rm(REG::A, REG::HL); 2 },
            // LD A, A
            0x7F => { 1 },
            // ADD A, B
            0x80 => { self.i.add_byte(REG::A, self.r.get_byte(REG::B)); 2 },
            // ADD A, C
            0x81 => { self.i.add_byte(REG::A, self.r.get_byte(REG::C)); 2 },
            // ADD A, D
            0x82 => { self.i.add_byte(REG::A, self.r.get_byte(REG::D)); 2 },
            // ADD A, E
            0x83 => { self.i.add_byte(REG::A, self.r.get_byte(REG::E)); 2 },
            // ADD A, H
            0x84 => { self.i.add_byte(REG::A, self.r.get_byte(REG::H)); 2 },
            // ADD A, L
            0x85 => { self.i.add_byte(REG::A, self.r.get_byte(REG::L)); 2 },
            // ADD A, (HL)
            0x86 => { self.i.add_byte(REG::A, self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // ADD A, A
            0x87 => { self.i.add_byte(REG::A, self.r.get_byte(REG::A)); 2 },
            // ADC A, B
            0x88 => { self.i.adc(self.r.get_byte(REG::B)); 2 },
            // ADC A, C
            0x89 => { self.i.adc(self.r.get_byte(REG::C)); 2 },
            // ADC A, D
            0x8A => { self.i.adc(self.r.get_byte(REG::D)); 2 },
            // ADC A, E
            0x8B => { self.i.adc(self.r.get_byte(REG::E)); 2 },
            // ADC A, H
            0x8C => { self.i.adc(self.r.get_byte(REG::H)); 2 },
            // ADC A, L
            0x8D => { self.i.adc(self.r.get_byte(REG::L)); 2 },
            // ADC A, (HL)
            0x8E => { self.i.adc(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // ADC A, A
            0x8F => { self.i.adc(self.r.get_byte(REG::A)); 2 },
            // SUB B
            0x90 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::B)); 1 },
            // SUB C
            0x91 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::C)); 1 },
            // SUB D
            0x92 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::D)); 1 },
            // SUB E
            0x93 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::E)); 1 },
            // SUB H
            0x94 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::H)); 1 },
            // SUB L
            0x95 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::L)); 1 },
            // SUB (HL)
            0x96 => { self.i.sub_byte(REG::A, self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // SUB A
            0x97 => { self.i.sub_byte(REG::A, self.r.get_byte(REG::A)); 1 },
            // SBC A, B
            0x98 => { self.i.sbc(self.r.get_byte(REG::B)); 1 },
            // SBC A, C
            0x99 => { self.i.sbc(self.r.get_byte(REG::C)); 1 },
            // SBC A, D
            0x9A => { self.i.sbc(self.r.get_byte(REG::D)); 1 },
            // SBC A, E
            0x9B => { self.i.sbc(self.r.get_byte(REG::E)); 1 },
            // SBC A, H
            0x9C => { self.i.sbc(self.r.get_byte(REG::H)); 1 },
            // SBC A, L
            0x9D => { self.i.sbc(self.r.get_byte(REG::L)); 1 },
            // SBC A, (HL)
            0x9E => { self.i.sbc(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // SBC A, A
            0x9F => { self.i.sbc(self.r.get_byte(REG::A)); 1 },
            // AND B
            0xA0 => { self.i.and(self.r.get_byte(REG::B)); 1 },
            // AND C
            0xA1 => { self.i.and(self.r.get_byte(REG::C)); 1 },
            // AND D
            0xA2 => { self.i.and(self.r.get_byte(REG::D)); 1 },
            // AND E
            0xA3 => { self.i.and(self.r.get_byte(REG::E)); 1 },
            // AND H
            0xA4 => { self.i.and(self.r.get_byte(REG::H)); 1 },
            // AND L
            0xA5 => { self.i.and(self.r.get_byte(REG::L)); 1 },
            // AND (HL)
            0xA6 => { self.i.and(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // AND A
            0xA7 => { self.i.and(self.r.get_byte(REG::A)); 1 },
            // XOR B
            0xA8 => { self.i.xor(self.r.get_byte(REG::B)); 1 },
            // XOR C
            0xA9 => { self.i.xor(self.r.get_byte(REG::C)); 1 },
            // XOR D
            0xAA => { self.i.xor(self.r.get_byte(REG::D)); 1 },
            // XOR E
            0xAB => { self.i.xor(self.r.get_byte(REG::E)); 1 },
            // XOR H
            0xAC => { self.i.xor(self.r.get_byte(REG::H)); 1 },
            // XOR L
            0xAD => { self.i.xor(self.r.get_byte(REG::L)); 1 },
            // XOR (HL)
            0xAE => { self.i.xor(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // XOR A
            0xAF => { self.i.xor(self.r.get_byte(REG::A)); 1 },
            // OR B
            0xB0 => { self.i.or(self.r.get_byte(REG::B)); 1 },
            // OR C
            0xB1 => { self.i.or(self.r.get_byte(REG::C)); 1 },
            // OR D
            0xB2 => { self.i.or(self.r.get_byte(REG::D)); 1 },
            // OR E
            0xB3 => { self.i.or(self.r.get_byte(REG::E)); 1 },
            // OR H
            0xB4 => { self.i.or(self.r.get_byte(REG::H)); 1 },
            // OR L
            0xB5 => { self.i.or(self.r.get_byte(REG::L)); 1 },
            // OR (HL)
            0xB6 => { self.i.or(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // OR A
            0xB7 => { self.i.or(self.r.get_byte(REG::A)); 1 },
            // CP B
            0xB8 => { self.i.cp(self.r.get_byte(REG::B)); 1 },
            // CP C
            0xB9 => { self.i.cp(self.r.get_byte(REG::C)); 1 },
            // CP D
            0xBA => { self.i.cp(self.r.get_byte(REG::D)); 1 },
            // CP E
            0xBB => { self.i.cp(self.r.get_byte(REG::E)); 1 },
            // CP H
            0xBC => { self.i.cp(self.r.get_byte(REG::H)); 1 },
            // CP L
            0xBD => { self.i.cp(self.r.get_byte(REG::L)); 1 },
            // CP (HL)
            0xBE => { self.i.cp(self.m.read_byte(self.r.get_word(REG::HL))); 2 },
            // CP A
            0xBF => { self.i.cp(self.r.get_byte(REG::A)); 1 },
            // RET NZ
            0xC0 => { self.i.retc(!self.r.get_flag(ALUFlag::Z)) },
            // POP BC
            0xC1 => { self.i.pop(REG::BC); 3 },
            // JP NZ, a16
            0xC2 => { self.i.jp(self.scan_word(), !self.r.get_flag(ALUFlag::Z)) },
            // JP a16
            0xC3 => { self.i.jp(self.scan_word(), true) },
            // CALL NZ, a16
            0xC4 => { self.i.call(self.scan_word(), !self.r.get_flag(ALUFlag::Z)) },
            // PUSH BC
            0xC5 => { self.i.push(REG::BC); 4 },
            // ADD A, d8
            0xC6 => { self.i.add_byte(REG::A, self.scan_byte()); 2 },
            // RST 0
            0xC7 => { self.i.rst(0); 4 },
            // RET Z
            0xC8 => { self.i.retc(self.r.get_flag(ALUFlag::Z)) },
            // RET
            0xC9 => { self.i.ret(); 4 },
            // JP Z, a16
            0xCA => { self.i.jp(self.scan_word(), self.r.get_flag(ALUFlag::Z)) },
            // CALL Z, a16
            0xCC => { self.i.call(self.scan_word(), self.r.get_flag(ALUFlag::Z)) },
            // CALL a16
            0xCD => { self.i.call(self.scan_word(), true) },
            // ADC d8
            0xCE => { self.i.adc(self.scan_byte()); 2 },
            // RST 1
            0xCF => { self.i.rst(1); 4 },
            // RET NC
            0xD0 => { self.i.retc(!self.r.get_flag(ALUFlag::C)) },
            // POP DE
            0xD1 => { self.i.pop(REG::DE); 3 },
            // JP NC, a16
            0xD2 => { self.i.jp(self.scan_word(), !self.r.get_flag(ALUFlag::C)) },
            // CALL NC, a16
            0xD4 => { self.i.call(self.scan_word(), !self.r.get_flag(ALUFlag::C)) },
            // PUSH DE
            0xD5 => { self.i.push(REG::DE); 4 },
            // SUB d8
            0xD6 => { self.i.sub_byte(REG::A, self.scan_byte()); 2 },
            // RST 2
            0xD7 => { self.i.rst(2); 4 },
            // RET C
            0xD8 => { self.i.retc(self.r.get_flag(ALUFlag::C)) },
            // RETI
            0xD9 => { self.i.reti(); 4 },
            // JP C, a16
            0xDA => { self.i.jp(self.scan_word(), self.r.get_flag(ALUFlag::C)) },
            // CALL C, a16
            0xDC => { self.i.call(self.scan_word(), self.r.get_flag(ALUFlag::C)) },
            // SBC d8
            0xDE => { self.i.sbc(self.scan_byte()); 2 },
            // RST 3
            0xDF => { self.i.rst(3); 4 },
            // LD (a8), A
            0xE0 => { self.i.ld_mbr(self.scan_byte(), REG::A); 3 },
            // POP HL
            0xE1 => { self.i.pop(REG::HL); 3 },
            // LD (C), A
            0xE2 => { self.i.ld_mrc(); 2 },
            // PUSH HL
            0xE5 => { self.i.push(REG::HL); 4 },
            // AND d8
            0xE6 => { self.i.and(self.scan_byte()); 2 },
            // RST 4
            0xE7 => { self.i.rst(4); 4 },
            // ADD SP, s8
            0xE8 => { self.i.add_stack(self.scan_byte() as i8); 4 },
            // JP (HL)
            0xE9 => { self.i.jp(self.r.get_word(REG::HL), true) },
            // LD (a16), A
            0xEA => { self.i.ld_mwr(self.scan_word(), REG::A); 4 },
            // XOR d8
            0xEE => { self.i.xor(self.scan_byte()); 2 },
            // RST 5
            0xEF => { self.i.rst(5); 4 },
            // LD A, (a8)
            0xF0 => { self.i.ld_rmb(REG::A, self.scan_byte()); 3 },
            // POP AF
            0xF1 => { self.i.pop(REG::AF); 3 },
            // LD A, (C)
            0xF2 => { self.i.ld_rmc(); 2 },
            // DI
            0xF3 => { self.i.di(); 1 },
            // PUSH AF
            0xF5 => { self.i.push(REG::AF); 4 },
            // OR d8
            0xF6 => { self.i.or(self.scan_byte()); 2 },
            // RST 6
            0xF7 => { self.i.rst(6); 4 },
            // LD HL, SP+s8
            0xF8 => { self.i.add_stack(self.scan_byte() as i8); 3 },
            // LD SP, HL
            0xF9 => { self.i.ld_rrw(REG::SP, REG::HL); 2 },
            // LD A, (a16)
            0xFA => { self.i.ld_rmw(REG::A, self.scan_word()); 4 },
            // EI
            0xFB => {self.i.ei(); 1 },
            // CP d8
            0xFE => { self.i.cp(self.scan_byte()); 2 },
            // RST 7
            0xFF => { self.i.rst(7); 4 },
            _other => { panic!("[ERROR] invalid opcode") }

        }
    }

    pub fn exec_cb(&mut self) -> u8 {
        let opcode = self.read_opcode();
        match opcode {
            // RLC B
            0x00 => { self.i.rlc(REG::B); 2 },
            // RLC C
            0x01 => { self.i.rlc(REG::C); 2 },
            // RLC D
            0x02 => { self.i.rlc(REG::D); 2 },
            // RLC E
            0x03 => { self.i.rlc(REG::E); 2 },
            // RLC H
            0x04 => { self.i.rlc(REG::H); 2 },
            // RLC L
            0x05 => { self.i.rlc(REG::L); 2 },
            // RLC (HL)
            0x06 => { self.i.rlc_mem(REG::HL); 4 },
            // RLC A
            0x07 => { self.i.rlc(REG::A); 2 },
            // RRC B
            0x08 => { self.i.rrc(REG::B); 2 },
            // RRC C
            0x09 => { self.i.rrc(REG::C); 2 },
            // RRC D
            0x0A => { self.i.rrc(REG::D); 2 },
            // RRC E
            0x0B => { self.i.rrc(REG::E); 2 },
            // RRC H
            0x0C => { self.i.rrc(REG::H); 2 },
            // RRC L
            0x0D => { self.i.rrc(REG::L); 2 },
            // RRC (HL)
            0x0E => { self.i.rrc_mem(REG::HL); 4 },
            // RRC A
            0x0F => { self.i.rrc(REG::A); 2 },
            // RL B
            0x10 => { self.i.rl(REG::B); 2 },
            // RL C
            0x11 => { self.i.rl(REG::C); 2 },
            // RL D
            0x12 => { self.i.rl(REG::D); 2 },
            // RL E
            0x13 => { self.i.rl(REG::E); 2 },
            // RL H
            0x14 => { self.i.rl(REG::H); 2 },
            // RL L
            0x15 => { self.i.rl(REG::L); 2 },
            // RL (HL)
            0x16 => { self.i.rl_mem(REG::HL); 4 },
            // RL A
            0x17 => { self.i.rl(REG::A); 2 },
            // RR B
            0x18 => { self.i.rr(REG::B); 2 },
            // RR C
            0x19 => { self.i.rr(REG::C); 2 },
            // RR D
            0x1A => { self.i.rr(REG::D); 2 },
            // RR E
            0x1B => { self.i.rr(REG::E); 2 },
            // RR H
            0x1C => { self.i.rr(REG::H); 2 },
            // RR L
            0x1D => { self.i.rr(REG::L); 2 },
            // RR (HL)
            0x1E => { self.i.rr_mem(REG::HL); 4 },
            // RR A
            0x1F => { self.i.rr(REG::A); 2 },
            // SLA B
            0x20 => { self.i.sla(REG::B); 2 },
            // SLA C
            0x21 => { self.i.sla(REG::C); 2 },
            // SLA D
            0x22 => { self.i.sla(REG::D); 2 },
            // SLA E
            0x23 => { self.i.sla(REG::E); 2 },
            // SLA H
            0x24 => { self.i.sla(REG::H); 2 },
            // SLA L
            0x25 => { self.i.sla(REG::L); 2 },
            // SLA (HL)
            0x26 => { self.i.sla_mem(REG::HL); 4 },
            // SLA A
            0x27 => { self.i.sla(REG::A); 2 },
            // SRA B
            0x28 => { self.i.sra(REG::B); 2 },
            // SRA C
            0x29 => { self.i.sra(REG::C); 2 },
            // SRA D
            0x2A => { self.i.sra(REG::D); 2 },
            // SRA E
            0x2B => { self.i.sra(REG::E); 2 },
            // SRA H
            0x2C => { self.i.sra(REG::H); 2 },
            // SRA L
            0x2D => { self.i.sra(REG::L); 2 },
            // SRA (HL)
            0x2E => { self.i.sra_mem(REG::HL); 4 },
            // SRA A
            0x2F => { self.i.sra(REG::A); 2 },
            // SWAP B
            0x30 => { self.i.swap(REG::B); 2 },
            // SWAP C
            0x31 => { self.i.swap(REG::C); 2 },
            // SWAP D
            0x32 => { self.i.swap(REG::D); 2 },
            // SWAP E
            0x33 => { self.i.swap(REG::E); 2 },
            // SWAP H
            0x34 => { self.i.swap(REG::H); 2 },
            // SWAP L
            0x35 => { self.i.swap(REG::L); 2 },
            // SWAP (HL)
            0x36 => { self.i.swap_mem(REG::HL); 4 },
            // SWAP A
            0x37 => { self.i.swap(REG::A); 2 },
            // SRL B
            0x38 => { self.i.srl(REG::B); 2 },
            // SRL C
            0x39 => { self.i.srl(REG::C); 2 },
            // SRL D
            0x3A => { self.i.srl(REG::D); 2 },
            // SRL E
            0x3B => { self.i.srl(REG::E); 2 },
            // SRL H
            0x3C => { self.i.srl(REG::H); 2 },
            // SRL L
            0x3D => { self.i.srl(REG::L); 2 },
            // SRL (HL)
            0x3E => { self.i.srl_mem(REG::HL); 4 },
            // SRL A
            0x3F => { self.i.srl(REG::A); 2 },
            // BIT 0, B
            0x40 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 0, C
            0x41 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 0, D
            0x42 => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 0, E
            0x43 => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 0, H
            0x44 => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 0, L
            0x45 => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 0, (HL)
            0x46 => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 0, A
            0x47 => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 1, B
            0x48 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 1, C
            0x49 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 1, D
            0x4A => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 1, E
            0x4B => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 1, H
            0x4C => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 1, L
            0x4D => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 1, (HL)
            0x4E => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 1, A
            0x4F => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 2, B
            0x50 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 2, C
            0x51 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 2, D
            0x52 => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 2, E
            0x53 => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 2, H
            0x54 => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 2, L
            0x55 => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 2, (HL)
            0x56 => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 2, A
            0x57 => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 3, B
            0x58 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 3, C
            0x59 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 3, D
            0x5A => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 3, E
            0x5B => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 3, H
            0x5C => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 3, L
            0x5D => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 3, (HL)
            0x5E => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 3, A
            0x5F => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 4, B
            0x60 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 4, C
            0x61 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 4, D
            0x62 => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 4, E
            0x63 => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 4, H
            0x64 => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 4, L
            0x65 => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 4, (HL)
            0x66 => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 4, A
            0x67 => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 5, B
            0x68 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 5, C
            0x69 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 5, D
            0x6A => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 5, E
            0x6B => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 5, H
            0x6C => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 5, L
            0x6D => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 5, (HL)
            0x6E => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 5, A
            0x6F => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 6, B
            0x70 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 6, C
            0x71 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 6, D
            0x72 => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 6, E
            0x73 => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 6, H
            0x74 => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 6, L
            0x75 => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 6, (HL)
            0x76 => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 6, A
            0x77 => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // BIT 7, B
            0x78 => { self.i.bit(self.scan_byte(), REG::B); 2 },
            // BIT 7, C
            0x79 => { self.i.bit(self.scan_byte(), REG::C); 2 },
            // BIT 7, D
            0x7A => { self.i.bit(self.scan_byte(), REG::D); 2 },
            // BIT 7, E
            0x7B => { self.i.bit(self.scan_byte(), REG::E); 2 },
            // BIT 7, H
            0x7C => { self.i.bit(self.scan_byte(), REG::H); 2 },
            // BIT 7, L
            0x7D => { self.i.bit(self.scan_byte(), REG::L); 2 },
            // BIT 7, (HL)
            0x7E => { self.i.bit_mem(self.scan_byte(), REG::HL); 4 },
            // BIT 7, A
            0x7F => { self.i.bit(self.scan_byte(), REG::A); 2 },
            // RES 0, B
            0x80 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 0, C
            0x81 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 0, D
            0x82 => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 0, E
            0x83 => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 0, H
            0x84 => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 0, L
            0x85 => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 0, (HL)
            0x86 => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 0, A
            0x87 => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 1, B
            0x88 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 1, C
            0x89 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 1, D
            0x8A => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 1, E
            0x8B => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 1, H
            0x8C => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 1, L
            0x8D => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 1, (HL)
            0x8E => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 1, A
            0x8F => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 2, B
            0x90 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 2, C
            0x91 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 2, D
            0x92 => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 2, E
            0x93 => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 2, H
            0x94 => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 2, L
            0x95 => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 2, (HL)
            0x96 => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 2, A
            0x97 => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 3, B
            0x98 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 3, C
            0x99 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 3, D
            0x9A => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 3, E
            0x9B => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 3, H
            0x9C => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 3, L
            0x9D => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 3, (HL)
            0x9E => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 3, A
            0x9F => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 4, B
            0xA0 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 4, C
            0xA1 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 4, D
            0xA2 => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 4, E
            0xA3 => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 4, H
            0xA4 => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 4, L
            0xA5 => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 4, (HL)
            0xA6 => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 4, A
            0xA7 => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 5, B
            0xA8 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 5, C
            0xA9 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 5, D
            0xAA => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 5, E
            0xAB => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 5, H
            0xAC => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 5, L
            0xAD => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 5, (HL)
            0xAE => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 5, A
            0xAF => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 6, B
            0xB0 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 6, C
            0xB1 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 6, D
            0xB2 => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 6, E
            0xB3 => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 6, H
            0xB4 => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 6, L
            0xB5 => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 6, (HL)
            0xB6 => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 6, A
            0xB7 => { self.i.res(self.scan_byte(), REG::A); 2 },
            // RES 7, B
            0xB8 => { self.i.res(self.scan_byte(), REG::B); 2 },
            // RES 7, C
            0xB9 => { self.i.res(self.scan_byte(), REG::C); 2 },
            // RES 7, D
            0xBA => { self.i.res(self.scan_byte(), REG::D); 2 },
            // RES 7, E
            0xBB => { self.i.res(self.scan_byte(), REG::E); 2 },
            // RES 7, H
            0xBC => { self.i.res(self.scan_byte(), REG::H); 2 },
            // RES 7, L
            0xBD => { self.i.res(self.scan_byte(), REG::L); 2 },
            // RES 7, (HL)
            0xBE => { self.i.res_mem(self.scan_byte(), REG::HL); 4 },
            // RES 7, A
            0xBF => { self.i.res(self.scan_byte(), REG::A); 2 },
            // SET 0, B
            0xC0 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 0, C
            0xC1 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 0, D
            0xC2 => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 0, E
            0xC3 => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 0, H
            0xC4 => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 0, L
            0xC5 => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 0, (HL)
            0xC6 => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 0, A
            0xC7 => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 1, B
            0xC8 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 1, C
            0xC9 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 1, D
            0xCA => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 1, E
            0xCB => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 1, H
            0xCC => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 1, L
            0xCD => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 1, (HL)
            0xCE => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 1, A
            0xCF => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 2, B
            0xD0 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 2, C
            0xD1 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 2, D
            0xD2 => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 2, E
            0xD3 => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 2, H
            0xD4 => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 2, L
            0xD5 => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 2, (HL)
            0xD6 => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 2, A
            0xD7 => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 3, B
            0xD8 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 3, C
            0xD9 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 3, D
            0xDA => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 3, E
            0xDB => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 3, H
            0xDC => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 3, L
            0xDD => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 3, (HL)
            0xDE => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 3, A
            0xDF => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 4, B
            0xE0 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 4, C
            0xE1 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 4, D
            0xE2 => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 4, E
            0xE3 => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 4, H
            0xE4 => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 4, L
            0xE5 => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 4, (HL)
            0xE6 => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 4, A
            0xE7 => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 5, B
            0xE8 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 5, C
            0xE9 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 5, D
            0xEA => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 5, E
            0xEB => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 5, H
            0xEC => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 5, L
            0xED => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 5, (HL)
            0xEE => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 5, A
            0xEF => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 6, B
            0xF0 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 6, C
            0xF1 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 6, D
            0xF2 => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 6, E
            0xF3 => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 6, H
            0xF4 => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 6, L
            0xF5 => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 6, (HL)
            0xF6 => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 6, A
            0xF7 => { self.i.set(self.scan_byte(), REG::A); 2 },
            // SET 7, B
            0xF8 => { self.i.set(self.scan_byte(), REG::B); 2 },
            // SET 7, C
            0xF9 => { self.i.set(self.scan_byte(), REG::C); 2 },
            // SET 7, D
            0xFA => { self.i.set(self.scan_byte(), REG::D); 2 },
            // SET 7, E
            0xFB => { self.i.set(self.scan_byte(), REG::E); 2 },
            // SET 7, H
            0xFC => { self.i.set(self.scan_byte(), REG::H); 2 },
            // SET 7, L
            0xFD => { self.i.set(self.scan_byte(), REG::L); 2 },
            // SET 7, (HL)
            0xFE => { self.i.set_mem(self.scan_byte(), REG::HL); 4 },
            // SET 7, A
            0xFF => { self.i.set(self.scan_byte(), REG::A); 2 },
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

