use std::collections::HashMap;
#[derive(Default)]
pub struct Register {
   Map: HashMap<&'static str, u16>,
}

pub enum ALUFlag {
    C = 0x10, // Carry flag
    H = 0x20, // Half-carry flag
    N = 0x40, // Sign flag
    Z = 0x80, // Zero flag
}

impl Register {
    pub fn init() -> Register {
        let mut map = HashMap::new();
        map.insert("AF", 0);
        map.insert("BC", 0);
        map.insert("DE", 0);
        map.insert("HL", 0);
        map.insert("SP", 0);
        map.insert("PC", 0);
        Register {
            Map: map
        }
    }

    pub fn get_word(&self, reg: &str) -> u16 {
        let word = self.Map.get(reg);
        if word != None { word.unwrap(); } ; panic!("[ERROR] invalid register id");
    }

    pub fn get_byte(&self, reg: char) -> u8 {
        match reg {
            'A' => { (self.Map.get("AF").unwrap() >> 8) as u8 },
            'F' => { (self.Map.get("AF").unwrap() & 0xF0) as u8 },
            'B' => { (self.Map.get("BC").unwrap() >> 8) as u8 },
            'C' => { (self.Map.get("BC").unwrap() & 0xFF) as u8 },
            'D' => { (self.Map.get("DE").unwrap() >> 8) as u8 },
            'E' => { (self.Map.get("DE").unwrap() & 0xFF) as u8 },
            'H' => { (self.Map.get("HL").unwrap() >> 8) as u8 },
            'L' => { (self.Map.get("HL").unwrap() & 0xFF) as u8 },
            _other=> { panic!("Canned Message") }
        }
    }

    pub fn set_word(&mut self, reg: &str, value: u16) {
        if self.Map.get(reg) == None {
            panic!("Canned Message")
        }

        if reg == "AF" {
            self.Map.insert("AF", value & 0xFFF0);
        } else {
            *self.Map.get_mut(reg).unwrap() = value;
        }
    }

    pub fn set_byte(&mut self, reg: char, value: u8) {

        match reg {
            'A' => { self.Map.insert("AF",((value as u16) << 8) | (self.get_word("AF") & 0xF0)); },
            'F' => { self.Map.insert("AF", (self.get_word("AF") & 0xFF00) | (value as u16)); },
            'B' => { self.Map.insert("BC", ((value as u16) << 8) | (self.get_word("BC") & 0xFF)); },
            'C' => { self.Map.insert("BC", (self.get_word("BC") & 0xFF00) | (value as u16)); },
            'D' => { self.Map.insert("DE", ((value as u16) << 8) | (self.get_word("DE") & 0xFF)); },
            'E' => { self.Map.insert("DE", (self.get_word("DE") & 0xFF00) | (value as u16)); },
            'H' => { self.Map.insert("HL", ((value as u16) << 8) | (self.get_word("HL") & 0xFF)); },
            'L' => { self.Map.insert("HL", (self.get_word("HL") & 0xFF00) | (value as u16)); },
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
}
