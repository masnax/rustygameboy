use std::collections::HashMap;

pub enum ALUFlag {
    C = 0x10, // Carry flag
    H = 0x20, // Half-carry flag
    N = 0x40, // Sign flag
    Z = 0x80, // Zero flag
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum REG {
    A  = 0x00,
    F  = 0x01,
    B  = 0x02,
    C  = 0x03,
    D  = 0x04,
    E  = 0x05,
    H  = 0x06,
    L  = 0x07,
    AF = 0x08,
    BC = 0x09,
    DE = 0x0A,
    HL = 0x0B,
    SP = 0x0C,
    PC = 0x0D,
}

pub struct Register {
   registers: HashMap<REG, u16>,
}

impl Register {
    pub fn init() -> Register {
        let mut map = HashMap::new();
        map.insert(REG::AF, 0);
        map.insert(REG::BC, 0);
        map.insert(REG::DE, 0);
        map.insert(REG::HL, 0);
        map.insert(REG::SP, 0);
        map.insert(REG::PC, 0);
        Register {
            registers: map
        }
    }

    pub fn get_word(&self, reg: REG) -> u16 {
        let word = self.registers.get(&reg);
        if word != None { word.unwrap(); } ; panic!("[ERROR] invalid register id");
    }

    pub fn get_byte(&self, reg: REG) -> u8 {
        match reg {
            REG::A => { (self.registers.get(&REG::AF).unwrap() >> 8) as u8 },
            REG::F => { (self.registers.get(&REG::AF).unwrap() & 0xF0) as u8 },
            REG::B => { (self.registers.get(&REG::BC).unwrap() >> 8) as u8 },
            REG::C => { (self.registers.get(&REG::BC).unwrap() & 0xFF) as u8 },
            REG::D => { (self.registers.get(&REG::DE).unwrap() >> 8) as u8 },
            REG::E => { (self.registers.get(&REG::DE).unwrap() & 0xFF) as u8 },
            REG::H => { (self.registers.get(&REG::HL).unwrap() >> 8) as u8 },
            REG::L => { (self.registers.get(&REG::HL).unwrap() & 0xFF) as u8 },
            _other=> { panic!("Canned Message") }
        }
    }

    pub fn set_word(&mut self, reg: REG, value: u16) {
        if self.registers.get(&reg) == None {
            panic!("Canned Message")
        }

        if reg == REG::AF {
            self.registers.insert(REG::AF, value & 0xFFF0);
        } else {
            self.registers.insert(reg, value);
        }
    }

    pub fn set_byte(&mut self, reg: REG, value: u8) {

        match reg {
            REG::A => { self.registers.insert(REG::AF,((value as u16) << 8) | (self.get_word(REG::AF) & 0xF0)); },
            REG::F => { self.registers.insert(REG::AF, (self.get_word(REG::AF) & 0xFF00) | (value as u16)); },
            REG::B => { self.registers.insert(REG::BC, ((value as u16) << 8) | (self.get_word(REG::BC) & 0xFF)); },
            REG::C => { self.registers.insert(REG::BC, (self.get_word(REG::BC) & 0xFF00) | (value as u16)); },
            REG::D => { self.registers.insert(REG::DE, ((value as u16) << 8) | (self.get_word(REG::DE) & 0xFF)); },
            REG::E => { self.registers.insert(REG::DE, (self.get_word(REG::DE) & 0xFF00) | (value as u16)); },
            REG::H => { self.registers.insert(REG::HL, ((value as u16) << 8) | (self.get_word(REG::HL) & 0xFF)); },
            REG::L => { self.registers.insert(REG::HL, (self.get_word(REG::HL) & 0xFF00) | (value as u16)); },
            _other => { panic!("Canned Message") }
        }
    }

    pub fn write_word(&mut self, addr: u16, value: u16) {
        println!("{:?} {:?}", addr, value);

    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        println!("{:?} {:?}", addr, value);

    }

    pub fn read_word(&self, addr: u16) -> u16 {
        println!("{:?} {:?}", addr, addr);
        return 0;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        println!("{:?} {:?}", addr, addr);
        return 0;
    }

    pub fn get_flag(&self, flag: ALUFlag) -> bool {
        let f = self.get_byte(REG::F);
        let flag_bits = flag as u8;
        f & flag_bits == flag_bits
    }

    pub fn update_flag(&mut self, flag: ALUFlag, switch: bool) {
        let flag_bits = flag as u8;
        let f = self.get_byte(REG::F) & (0xff - flag_bits);
        self.set_byte(REG::F, f | (flag_bits * switch as u8));
    }
}
