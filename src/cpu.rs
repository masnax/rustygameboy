use crate::register::Register;
use crate::register::ALUFlag;
use crate::instructions::Instruction;
//single core
pub struct CPU {
    r: Register,
}



// 0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
// 4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
// 8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
// A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
// C000-CFFF   4KB Work RAM Bank 0 (WRAM)
// D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
// E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
// FE00-FE9F   Sprite Attribute Table (OAM)
// FEA0-FEFF   Not Usable
// FF00-FF7F   I/O Ports
// FF80-FFFE   High RAM (HRAM)
// FFFF        Interrupt Enable R

impl CPU {
    pub fn init() -> CPU {
        CPU {
            r: Register::init()
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
    pub fn execOpCode(&mut self) -> u8 {
        let opcode = self.read_opcode();
        match opcode {
            // NOP
            0x00 => { 1 },
            // LD BC, d16
            0x01 => { self.r.set_word("BC", self.scan_word()); 3 },
            // LD (BC), A
            0x02 => { self.r.write_byte(self.r.get_word("BC"), self.r.get_byte('A')); 2 },
            // INC BC
            0x03 => { self.r.set_word("BC", self.r.get_word("BC").wrapping_add(1)); 2 },
            // INC B
            0x04 => { self.r.inc('B'); 1 },
            // DEC B
            0x05 => { self.r.dec('B'); 1 },
            // LD B, u8
            0x06 => { self.r.set_byte('B', self.scan_byte()); 2 },
            // RLCA
            0x07 => { self.r.rlc('A'); 1 },
            // LD (u16), SP
            0x08 => { self.r.write_word(self.scan_word(), self.r.get_word("SP")); 5 },
            // ADD HL, BC
            0x09 => { self.r.addW("HL", self.r.get_word("BC")); 2 },
            // LD A, (BC)
            0x0A => { self.r.set_byte('A', self.r.read_byte(self.r.get_word("BC"))); 2 },
            // DEC BC
            0x0B => { self.r.set_word("BC", self.r.get_word("BC").wrapping_sub(1)); 2 },
            // INC C
            0x0C => { self.r.inc('C'); 1 },
            // DEC C
            0x0D => { self.r.dec('C'); 1 },
            // LD C, u8
            0x0E => { self.r.set_byte('C', self.scan_byte()); 2 },
            // RRCA
            0x0F => { self.r.rrc('A'); 1 },
            // STOP
            0x10 => { 1 }, // TODO
            // LD DE, U16
            0x11 => { self.r.set_word("DE", self.scan_word()); 3 },
            // LD (DE), A
            0x12 => { self.r.write_byte(self.r.get_word("DE"), self.r.get_byte('A')); 2 },
            // INC DE
            0x13 => { self.r.set_word("DE", self.r.get_word("DE").wrapping_add(1)); 2 },
            // INC D
            0x14 => { self.r.inc('D'); 1 },
            // DEC D
            0x15 => { self.r.dec('D'); 1 },
            // LD D, u8
            0x16 => { self.r.set_byte('D', self.scan_byte()); 2 },
            // RLA
            0x17 => { self.r.rl('A'); 1 },
            // JR s8
            0x18 => { self.r.jr(self.scan_byte() as i8, true) }, // TODO
            // ADD HL, DE
            0x19 => { self.r.addW("HL", self.r.get_word("DE")); 2 },
            // LD A, (DE)
            0x1A => { self.r.set_byte('A', self.r.read_byte(self.r.get_word("DE"))); 2 },
            // DEC DE
            0x1B => { self.r.set_word("DE", self.r.get_word("DE").wrapping_sub(1)); 2 },
            // INC E
            0x1C => { self.r.inc('E'); 1 },
            // DEC E
            0x1D => { self.r.dec('E'); 1 },
            // LD E, u8
            0x1E => { self.r.set_byte('E', self.scan_byte()); 2 },
            // RRA
            0x1F => { self.r.rr('A'); 1 },
            // JR NZ, s8
            0x20 => { self.r.jr(self.scan_byte() as i8, !self.r.get_flag(ALUFlag::Z)) },
            // LD HL, U16
            0x21 => { self.r.set_word("HL", self.scan_word()); 3 },
            // LD (HL+), A
            0x22 => { self.r.ldMemInc(); 2 },
            // INC HL
            0x23 => { self.r.set_word("HL", self.r.get_word("HL").wrapping_add(1)); 2 },
            // INC H
            0x24 => { self.r.inc('H'); 1 },
            // DEC H
            0x25 => { self.r.dec('H'); 1 },
            // LD H, u8
            0x26 => { self.r.set_byte('H', self.scan_byte()); 2 },
            // DAA
            0x27 => { self.r.daa(); 1 },
            // JR Z, s8
            0x28 => { self.r.jr(self.scan_byte() as i8, self.r.get_flag(ALUFlag::Z)) },
            // ADD HL, HL
            0x29 => { self.r.addW("HL", self.r.get_word("HL")); 2 },
            // LD A, (HL+)
            0x2A => { self.r.ldInc(); 2 },
            // DEC HL
            0x2B => { self.r.set_word("HL", self.r.get_word("HL").wrapping_sub(1)); 2 },
            // INC L
            0x2C => { self.r.inc('L'); 1 },
            // DEC L
            0x2D => { self.r.dec('L'); 1 },
            // LD L, u8
            0x2E => { self.r.set_byte('L', self.scan_byte()); 2 },
            // CPL
            0x2F => { self.r.cpl(); 1 },
            // JR NC, s8
            0x30 => { self.r.jr(self.scan_byte() as i8, !self.r.get_flag(ALUFlag::C)) },
            // LD SP, U16
            0x31 => { self.r.set_word("SP", self.scan_word()); 3 },
            // LD (HL-), A
            0x32 => { self.r.ldMemDec(); 2 },
            // INC SP
            0x33 => { self.r.set_word("SP", self.r.get_word("SP").wrapping_add(1)); 2 },
            // INC (HL)
            0x34 => { self.r.incMem(); 3 },
            // DEC (HL)
            0x35 => { self.r.decMem(); 3 },
            // LD (HL), u8
            0x36 => { self.r.write_byte(self.r.get_word("HL"), self.scan_byte()); 3 },
            // SCF
            0x37 => { self.r.scf(); 1 },
            // JR C, s8
            0x38 => { self.r.jr(self.scan_byte() as i8, self.r.get_flag(ALUFlag::C)) },
            // ADD HL, SP
            0x39 => { self.r.addW("HL", self.r.get_word("SP")); 2 },
            // LD A, (HL-)
            0x3A => { self.r.ldDec(); 2 },
            // DEC SP
            0x3B => { self.r.set_word("SP", self.r.get_word("SP").wrapping_sub(1)); 2 },
            // INC A
            0x3C => { self.r.inc('A'); 1 },
            // DEC A
            0x3D => { self.r.dec('A'); 1 },
            // LD A, u8
            0x3E => { self.r.set_byte('A', self.scan_byte()); 2 },
            // CCF
            0x3F => { self.r.ccf(); 1 },
            // LD B, B
            0x40 => { self.r.set_byte('B', self.r.get_byte('B')); 1 },
            // LD B, C
            0x41 => { self.r.set_byte('B', self.r.get_byte('C')); 1 },
            // LD B, D
            0x42 => { self.r.set_byte('B', self.r.get_byte('D')); 1 },
            // LD B, E
            0x43 => { self.r.set_byte('B', self.r.get_byte('E')); 1 },
            // LD B, H
            0x44 => { self.r.set_byte('B', self.r.get_byte('H')); 1 },
            // LD B, L
            0x45 => { self.r.set_byte('B', self.r.get_byte('L')); 1 },
            // LD B, (HL)
            0x46 => { self.r.set_byte('B', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD B, A
            0x47 => { self.r.set_byte('B', self.r.get_byte('A')); 1 },
            // LD C, B
            0x48 => { self.r.set_byte('C', self.r.get_byte('B')); 1 },
            // LD C, C
            0x49 => { self.r.set_byte('C', self.r.get_byte('C')); 1 },
            // LD C, D
            0x4A => { self.r.set_byte('C', self.r.get_byte('D')); 1 },
            // LD C, E
            0x4B => { self.r.set_byte('C', self.r.get_byte('E')); 1 },
            // LD C, H
            0x4C => { self.r.set_byte('C', self.r.get_byte('H')); 1 },
            // LD C, L
            0x4D => { self.r.set_byte('C', self.r.get_byte('L')); 1 },
            // LD C, (HL)
            0x4E => { self.r.set_byte('C', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD C, A
            0x4F => { self.r.set_byte('C', self.r.get_byte('A')); 1 },
            // LD D, B
            0x50 => { self.r.set_byte('D', self.r.get_byte('B')); 1 },
            // LD D, C
            0x51 => { self.r.set_byte('D', self.r.get_byte('C')); 1 },
            // LD D, D
            0x52 => { self.r.set_byte('D', self.r.get_byte('D')); 1 },
            // LD D, E
            0x53 => { self.r.set_byte('D', self.r.get_byte('E')); 1 },
            // LD D, H
            0x54 => { self.r.set_byte('D', self.r.get_byte('H')); 1 },
            // LD D, L
            0x55 => { self.r.set_byte('D', self.r.get_byte('L')); 1 },
            // LD D, (HL)
            0x56 => { self.r.set_byte('D', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD D, A
            0x57 => { self.r.set_byte('D', self.r.get_byte('A')); 1 },
            // LD E, B
            0x58 => { self.r.set_byte('E', self.r.get_byte('B')); 1 },
            // LD E, C
            0x59 => { self.r.set_byte('E', self.r.get_byte('C')); 1 },
            // LD E, D
            0x5A => { self.r.set_byte('E', self.r.get_byte('D')); 1 },
            // LD E, E
            0x5B => { self.r.set_byte('E', self.r.get_byte('E')); 1 },
            // LD E, H
            0x5C => { self.r.set_byte('E', self.r.get_byte('H')); 1 },
            // LD E, L
            0x5D => { self.r.set_byte('E', self.r.get_byte('L')); 1 },
            // LD E, (HL)
            0x5E => { self.r.set_byte('E', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD E, A
            0x5F => { self.r.set_byte('E', self.r.get_byte('A')); 1 },
            // LD H, B
            0x60 => { self.r.set_byte('H', self.r.get_byte('B')); 1 },
            // LD H, C
            0x61 => { self.r.set_byte('H', self.r.get_byte('C')); 1 },
            // LD H, D
            0x62 => { self.r.set_byte('H', self.r.get_byte('D')); 1 },
            // LD H, E
            0x63 => { self.r.set_byte('H', self.r.get_byte('E')); 1 },
            // LD H, H
            0x64 => { self.r.set_byte('H', self.r.get_byte('H')); 1 },
            // LD H, L
            0x65 => { self.r.set_byte('H', self.r.get_byte('L')); 1 },
            // LD H, (HL)
            0x66 => { self.r.set_byte('H', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD H, A
            0x67 => { self.r.set_byte('H', self.r.get_byte('A')); 1 },
            // LD L, B
            0x68 => { self.r.set_byte('L', self.r.get_byte('B')); 1 },
            // LD L, C
            0x69 => { self.r.set_byte('L', self.r.get_byte('C')); 1 },
            // LD L, D
            0x6A => { self.r.set_byte('L', self.r.get_byte('D')); 1 },
            // LD L, E
            0x6B => { self.r.set_byte('L', self.r.get_byte('E')); 1 },
            // LD L, H
            0x6C => { self.r.set_byte('L', self.r.get_byte('H')); 1 },
            // LD L, L
            0x6D => { self.r.set_byte('L', self.r.get_byte('L')); 1 },
            // LD L, (HL)
            0x6E => { self.r.set_byte('L', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD L, A
            0x6F => { self.r.set_byte('L', self.r.get_byte('A')); 1 },
            // LD (HL), B
            0x70 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('B')); 2 },
            // LD (HL), C
            0x71 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('C')); 2 },
            // LD (HL), D
            0x72 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('D')); 2 },
            // LD (HL), E
            0x73 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('E')); 2 },
            // LD (HL), H
            0x74 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('H')); 2 },
            // LD (HL), L
            0x75 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('L')); 2 },
            // HALT
            0x76 => { 1 }, // TODO
            // LD (HL), A
            0x77 => { self.r.write_byte(self.r.get_word("HL"), self.r.get_byte('A')); 2 },
            // LD A, B
            0x78 => { self.r.set_byte('A', self.r.get_byte('B')); 1 },
            // LD A, C
            0x79 => { self.r.set_byte('A', self.r.get_byte('C')); 1 },
            // LD A, D
            0x7A => { self.r.set_byte('A', self.r.get_byte('D')); 1 },
            // LD A, E
            0x7B => { self.r.set_byte('A', self.r.get_byte('E')); 1 },
            // LD A, H
            0x7C => { self.r.set_byte('A', self.r.get_byte('H')); 1 },
            // LD A, L
            0x7D => { self.r.set_byte('A', self.r.get_byte('L')); 1 },
            // LD A, (HL)
            0x7E => { self.r.set_byte('A', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // LD A, A
            0x7F => { 1 },
            // ADD A, B
            0x80 => { self.r.addB('A', self.r.get_byte('B')); 2 },
            // ADD A, C
            0x81 => { self.r.addB('A', self.r.get_byte('C')); 2 },
            // ADD A, D
            0x82 => { self.r.addB('A', self.r.get_byte('D')); 2 },
            // ADD A, E
            0x83 => { self.r.addB('A', self.r.get_byte('E')); 2 },
            // ADD A, H
            0x84 => { self.r.addB('A', self.r.get_byte('H')); 2 },
            // ADD A, L
            0x85 => { self.r.addB('A', self.r.get_byte('L')); 2 },
            // ADD A, (HL)
            0x86 => { self.r.addB('A', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // ADD A, A
            0x87 => { self.r.addB('A', self.r.get_byte('A')); 2 },
            // ADC A, B
            0x88 => { self.r.adc(self.r.get_byte('B')); 2 },
            // ADC A, C
            0x89 => { self.r.adc(self.r.get_byte('C')); 2 },
            // ADC A, D
            0x8A => { self.r.adc(self.r.get_byte('D')); 2 },
            // ADC A, E
            0x8B => { self.r.adc(self.r.get_byte('E')); 2 },
            // ADC A, H
            0x8C => { self.r.adc(self.r.get_byte('H')); 2 },
            // ADC A, L
            0x8D => { self.r.adc(self.r.get_byte('L')); 2 },
            // ADC A, (HL)
            0x8E => { self.r.adc(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // ADC A, A
            0x8F => { self.r.adc(self.r.get_byte('A')); 2 },
            // SUB B
            0x90 => { self.r.subB('A', self.r.get_byte('B')); 1 },
            // SUB C
            0x91 => { self.r.subB('A', self.r.get_byte('C')); 1 },
            // SUB D
            0x92 => { self.r.subB('A', self.r.get_byte('D')); 1 },
            // SUB E
            0x93 => { self.r.subB('A', self.r.get_byte('E')); 1 },
            // SUB H
            0x94 => { self.r.subB('A', self.r.get_byte('H')); 1 },
            // SUB L
            0x95 => { self.r.subB('A', self.r.get_byte('L')); 1 },
            // SUB (HL)
            0x96 => { self.r.subB('A', self.r.read_byte(self.r.get_word("HL"))); 2 },
            // SUB A
            0x97 => { self.r.subB('A', self.r.get_byte('A')); 1 },
            // SBC A, B
            0x98 => { self.r.sbc(self.r.get_byte('B')); 1 },
            // SBC A, C
            0x99 => { self.r.sbc(self.r.get_byte('C')); 1 },
            // SBC A, D
            0x9A => { self.r.sbc(self.r.get_byte('D')); 1 },
            // SBC A, E
            0x9B => { self.r.sbc(self.r.get_byte('E')); 1 },
            // SBC A, H
            0x9C => { self.r.sbc(self.r.get_byte('H')); 1 },
            // SBC A, L
            0x9D => { self.r.sbc(self.r.get_byte('L')); 1 },
            // SBC A, (HL)
            0x9E => { self.r.sbc(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // SBC A, A
            0x9F => { self.r.sbc(self.r.get_byte('A')); 1 },
            // AND B
            0xA0 => { self.r.and(self.r.get_byte('B')); 1 },
            // AND C
            0xA1 => { self.r.and(self.r.get_byte('C')); 1 },
            // AND D
            0xA2 => { self.r.and(self.r.get_byte('D')); 1 },
            // AND E
            0xA3 => { self.r.and(self.r.get_byte('E')); 1 },
            // AND H
            0xA4 => { self.r.and(self.r.get_byte('H')); 1 },
            // AND L
            0xA5 => { self.r.and(self.r.get_byte('L')); 1 },
            // AND (HL)
            0xA6 => { self.r.and(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // AND A
            0xA7 => { self.r.and(self.r.get_byte('A')); 1 },
            // XOR B
            0xA8 => { self.r.xor(self.r.get_byte('B')); 1 },
            // XOR C
            0xA9 => { self.r.xor(self.r.get_byte('C')); 1 },
            // XOR D
            0xAA => { self.r.xor(self.r.get_byte('D')); 1 },
            // XOR E
            0xAB => { self.r.xor(self.r.get_byte('E')); 1 },
            // XOR H
            0xAC => { self.r.xor(self.r.get_byte('H')); 1 },
            // XOR L
            0xAD => { self.r.xor(self.r.get_byte('L')); 1 },
            // XOR (HL)
            0xAE => { self.r.xor(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // XOR A
            0xAF => { self.r.xor(self.r.get_byte('A')); 1 },
            // OR B
            0xB0 => { self.r.or(self.r.get_byte('B')); 1 },
            // OR C
            0xB1 => { self.r.or(self.r.get_byte('C')); 1 },
            // OR D
            0xB2 => { self.r.or(self.r.get_byte('D')); 1 },
            // OR E
            0xB3 => { self.r.or(self.r.get_byte('E')); 1 },
            // OR H
            0xB4 => { self.r.or(self.r.get_byte('H')); 1 },
            // OR L
            0xB5 => { self.r.or(self.r.get_byte('L')); 1 },
            // OR (HL)
            0xB6 => { self.r.or(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // OR A
            0xB7 => { self.r.or(self.r.get_byte('A')); 1 },
            // CP B
            0xB8 => { self.r.cp(self.r.get_byte('B')); 1 },
            // CP C
            0xB9 => { self.r.cp(self.r.get_byte('C')); 1 },
            // CP D
            0xBA => { self.r.cp(self.r.get_byte('D')); 1 },
            // CP E
            0xBB => { self.r.cp(self.r.get_byte('E')); 1 },
            // CP H
            0xBC => { self.r.cp(self.r.get_byte('H')); 1 },
            // CP L
            0xBD => { self.r.cp(self.r.get_byte('L')); 1 },
            // CP (HL)
            0xBE => { self.r.cp(self.r.read_byte(self.r.get_word("HL"))); 2 },
            // CP A
            0xBF => { self.r.cp(self.r.get_byte('A')); 1 },
            // RET NZ
            0xC0 => { self.r.ret(!self.r.get_flag(ALUFlag::Z)) },
            // POP BC
            0xC1 => { self.r.pop("BC"); 3 },
            // JP NZ, a16
            0xC2 => { self.r.jp(self.scan_word(), !self.r.get_flag(ALUFlag::Z)) },
            // JP a16
            0xC3 => { self.r.jp(self.scan_word(), true) },
            // CALL NZ, a16
            0xC4 => { self.r.call(self.scan_word(), !self.r.get_flag(ALUFlag::Z)) },
            // PUSH BC
            0xC5 => { self.r.push("BC"); 4 },
            // ADD A, d8
            0xC6 => { self.r.addB('A', self.scan_byte()); 2 },
            // RST 0
            0xC7 => { self.r.rst(0); 4 },
            // RET Z
            0xC8 => { self.r.ret(self.r.get_flag(ALUFlag::Z)) },
            // RET
            0xC9 => { self.r.ret(true) },
            // JP Z, a16
            0xCA => { self.r.jp(self.scan_word(), self.r.get_flag(ALUFlag::Z)) },
            // CALL Z, a16
            0xCC => { self.r.call(self.scan_word(), self.r.get_flag(ALUFlag::Z)) },
            // CALL a16
            0xCD => { self.r.call(self.scan_word(), true) },
            // ADC d8
            0xCE => { self.r.adc(self.scan_byte()); 2 },
            // RST 1
            0xCF => { self.r.rst(1); 4 },
            // RET NC
            0xD0 => { self.r.ret(!self.r.get_flag(ALUFlag::C)) },
            // POP DE
            0xD1 => { self.r.pop("DE"); 3 },
            // JP NC, a16
            0xD2 => { self.r.jp(self.scan_word(), !self.r.get_flag(ALUFlag::C)) },
            // CALL NC, a16
            0xD4 => { self.r.call(self.scan_word(), !self.r.get_flag(ALUFlag::C)) },
            // PUSH DE
            0xD5 => { self.r.push("DE"); 4 },
            // SUB d8
            0xD6 => { self.r.subB('A', self.scan_byte()); 2 },
            // RST 2
            0xD7 => { self.r.rst(2); 4 },
            // RET C
            0xD8 => { self.r.ret(self.r.get_flag(ALUFlag::C)) },
            // RETI
            0xD9 => { self.r.reti(); 4 },
            // JP C, a16
            0xDA => { self.r.jp(self.scan_word(), self.r.get_flag(ALUFlag::C)) },
            // CALL C, a16
            0xDC => { self.r.call(self.scan_word(), self.r.get_flag(ALUFlag::C)) },
            // SBC d8
            0xDE => { self.r.sbc(self.scan_byte()); 2 },
            // RST 3
            0xDF => { self.r.rst(3); 4 },
            // LD (a8), A
            0xE0 => { self.r.write_byte(0xFF00 | (self.scan_byte() as u16), self.r.get_byte('A')); 3 },
            // POP HL
            0xE1 => { self.r.pop("HL"); 3 },
            // LD (C), A
            0xE2 => { self.r.write_byte(0xFF00 | (self.r.get_byte('C') as u16), self.r.get_byte('A')); 2 },
            // PUSH HL
            0xE5 => { self.r.push("HL"); 4 },
            // AND d8
            0xE6 => { self.r.and(self.scan_byte()); 2 },
            // RST 4
            0xE7 => { self.r.rst(4); 4 },
            // ADD SP, s8
            0xE8 => { self.r.addSP(self.scan_byte() as i8); 4 },
            // JP (HL)
            0xE9 => { self.r.jp(self.r.read_word(self.r.get_word("HL")), true) },
            // LD (a16), A
            0xEA => { self.r.write_byte(self.scan_word(), self.r.get_byte('A')); 4 },
            // XOR d8
            0xEE => { self.r.xor(self.scan_byte()); 2 },
            // RST 5
            0xEF => { self.r.rst(5); 4 },
            // LD A, (a8)
            0xF0 => { self.r.set_byte('A', self.r.read_byte(0xFF00 | (self.scan_byte() as u16))); 3 },
            // POP AF
            0xF1 => { self.r.pop("AF"); 3 },
            // LD A, (C)
            0xF2 => { self.r.set_byte('A', self.r.read_byte(0xFF00 | (self.r.get_byte('C') as u16))); 2 },
            // DI
            0xF3 => { self.r.di(); 1 },
            // PUSH AF
            0xF5 => { self.r.push("AF"); 4 },
            // OR d8
            0xF6 => { self.r.or(self.scan_byte()); 2 },
            // RST 6
            0xF7 => { self.r.rst(6); 4 },
            // LD HL, SP+s8
            0xF8 => { self.r.addSP(self.scan_byte() as i8); 3 },
            // LD SP, HL
            0xF9 => { self.r.set_word("SP", self.r.get_word("HL")); 2 },
            // LD A, (a16)
            0xFA => { self.r.set_byte('A', self.r.read_byte(self.scan_word())); 4 },
            // EI
            0xFB => {self.r.ei(); 1 },
            // CP d8
            0xFE => { self.r.cp(self.scan_byte()); 2 },
            // RST 7
            0xFF => { self.r.rst(7); 4 },
            _other => { panic!("[ERROR] invalid opcode") }

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

