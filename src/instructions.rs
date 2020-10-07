use crate::register::Register;
use crate::register::ALUFlag;
use crate::register::REG;
use crate::mem_map::Memory;

pub struct InstructionSet {
    pub r: Register,
    pub m: Memory
}


impl InstructionSet {
    pub fn init(registers: Register, mem_map: Memory) -> InstructionSet {
        InstructionSet {
            r: registers,
            m: mem_map
        }
    }

    pub fn inc(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let add: u8 = value.wrapping_add(1);
        match id {
            REG::A => {
                let inc: u16 = ((add as u16) << 8) | (self.r.get_word(REG::AF) & 0xF0);
                self.r.set_word(REG::AF, inc);
            },
            REG::B => {
                let inc: u16 = ((add as u16) << 8) | (self.r.get_word(REG::BC) & 0xFF);
                self.r.set_word(REG::BC, inc);
            },
            REG::C => {
                let inc: u16 = (self.r.get_word(REG::BC) & 0xFF00) | (add as u16);
                self.r.set_word(REG::BC, inc);
            },
            REG::D => {
                let inc: u16 = ((add as u16) << 8) | (self.r.get_word(REG::DE) & 0xFF);
                self.r.set_word(REG::DE, inc);
            },
            REG::E => {
                let inc: u16 = (self.r.get_word(REG::DE) & 0xFF00) | (add as u16);
                self.r.set_word(REG::DE, inc);
            },
            REG::H => {
                let inc: u16 = ((add as u16) << 8) | (self.r.get_word(REG::BC) & 0xFF);
                self.r.set_word(REG::BC, inc);
            },
            REG::L => {
                let inc: u16 = (self.r.get_word(REG::HL) & 0xFF00) | (add as u16);
                self.r.set_word(REG::HL, inc);
            },
            _other => {
                panic!("[ERROR] invalid register");
            }
        }
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    pub fn dec(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let sub: u8 = value.wrapping_sub(1);
        match id {
            REG::A => {
                let inc: u16 = ((sub as u16) << 8) | (self.r.get_word(REG::AF) & 0xF0);
                self.r.set_word(REG::AF, inc);
            },
            REG::B => {
                let inc: u16 = ((sub as u16) << 8) | (self.r.get_word(REG::BC) & 0xFF);
                self.r.set_word(REG::BC, inc);
            },
            REG::C => {
                let inc: u16 = (self.r.get_word(REG::BC) & 0xFF00) | (sub as u16);
                self.r.set_word(REG::BC, inc);
            },
            REG::D => {
                let inc: u16 = ((sub as u16) << 8) | (self.r.get_word(REG::DE) & 0xFF);
                self.r.set_word(REG::DE, inc);
            },
            REG::E => {
                let inc: u16 = (self.r.get_word(REG::DE) & 0xFF00) | (sub as u16);
                self.r.set_word(REG::DE, inc);
            },
            REG::H => {
                let inc: u16 = ((sub as u16) << 8) | (self.r.get_word(REG::BC) & 0xFF);
                self.r.set_word(REG::BC, inc);
            },
            REG::L => {
                let inc: u16 = (self.r.get_word(REG::HL) & 0xFF00) | (sub as u16);
                self.r.set_word(REG::HL, inc);
            },
            _other => {
                panic!("[ERROR] invalid register");

            }
        }
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    pub fn inc_mem(&mut self) {
        let addr: u16 = self.r.get_word(REG::HL);
        let value: u8 = self.m.read_byte(addr);
        let add: u8 = value.wrapping_add(1);
        self.m.write_byte(addr, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    pub fn dec_mem(&mut self) {
        let addr: u16 = self.r.get_word(REG::HL);
        let value: u8 = self.m.read_byte(addr);
        let sub: u8 = value.wrapping_sub(1);
        self.m.write_byte(addr, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    pub fn add_word(&mut self, id: REG, value: u16) {
        let previous: u16 = self.r.get_word(id);
        let add: u16 = previous.wrapping_add(value);
        self.r.set_word(id, add);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (previous & 0xFF) + (value & 0xFF) > 0xFF);
        self.r.update_flag(ALUFlag::C, previous > 0xFFFF - value);
    }

    pub fn add_byte(&mut self, id: REG, value: u8) {
        let previous: u8 = self.r.get_byte(id);
        let add: u8 = previous.wrapping_add(value);
        self.r.set_byte(id, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (previous & 0xF) + (value & 0xF) > 0xF);
        self.r.update_flag(ALUFlag::C, previous > 0xFF - value);
    }

    pub fn add_stack(&mut self, value: i8) {
        let sp: u16 = self.r.get_word(REG::SP);
        let value_u16: u16 = value as u16;
        let add: u16 = sp.wrapping_add(value_u16);
        self.r.set_word(REG::SP, add);
        self.r.update_flag(ALUFlag::Z, false);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (sp & 0xFF) + (value_u16 & 0xFF) > 0xFF);
        self.r.update_flag(ALUFlag::C, sp > 0xFFFF - value_u16);
    }

    pub fn adc(&mut self, value: u8) {
        let a: u8 = self.r.get_byte(REG::A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let add: u8 = a.wrapping_add(value).wrapping_add(c);
        self.r.set_byte(REG::A, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (a & 0xF) + (value & 0xF) + c > 0xF);
        self.r.update_flag(ALUFlag::C, (a as u16) > (0xFF as u16) - (value as u16) - (c as u16));


    }

    pub fn sub_word(&mut self, id: REG, value: u16) {
        let previous: u16 = self.r.get_word(id);
        let sub: u16 = previous.wrapping_sub(value);
        self.r.set_word(id, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (previous & 0xFF) < (value & 0xFF));
        self.r.update_flag(ALUFlag::C, previous < value);
    }

    pub fn sub_byte(&mut self, id: REG, value: u8) {
        let previous: u8 = self.r.get_byte(id);
        let sub: u8 = previous.wrapping_sub(value);
        self.r.set_byte(id, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (previous & 0xF) < (value & 0xF));
        self.r.update_flag(ALUFlag::C, previous < value);
    }

    pub fn sbc(&mut self, value: u8) {
        let a: u8 = self.r.get_byte(REG::A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let sub: u8 = a.wrapping_sub(value).wrapping_sub(c);
        self.r.set_byte(REG::A, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (value & 0xF) + c);
        self.r.update_flag(ALUFlag::C, (a as u16) < (value as u16) + (c as u16));
    }

    pub fn rlca(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (carry as u8);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    pub fn rla(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (self.r.get_flag(ALUFlag::C) as u8);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    pub fn rrca(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((carry as u8) << 7);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    pub fn rra(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((self.r.get_flag(ALUFlag::C) as u8) << 7);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    pub fn daa(&mut self) {
        let a = self.r.get_byte(REG::A);
        let c = self.r.get_flag(ALUFlag::C);
        let h = self.r.get_flag(ALUFlag::H);
        let n = self.r.get_flag(ALUFlag::N);
        let mut bcd = 0;
        if c { bcd |= 0x60; }
        if h { bcd |= 0x06; }
        if n {
            self.r.set_byte(REG::A, a.wrapping_sub(bcd));
        } else {
            if a & 0xF > 0x9 { bcd |= 0x6; }
            if a > 0x99 { bcd |= 0x60; }
            self.r.set_byte(REG::A, a.wrapping_add(bcd));
        }
        self.r.update_flag(ALUFlag::C, bcd >= 0x60);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::Z, self.r.get_byte(REG::A) == 0);
    }

    pub fn cpl(&mut self) {
        self.r.set_byte(REG::A, 0xFF ^ self.r.get_byte(REG::A));
        self.r.update_flag(ALUFlag::H, true);
        self.r.update_flag(ALUFlag::N, true);
    }

    pub fn scf(&mut self) {
        self.r.update_flag(ALUFlag::C, true);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::N, false);
    }

    pub fn ccf(&mut self) {
        self.r.update_flag(ALUFlag::C, !self.r.get_flag(ALUFlag::C));
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::N, false);
    }


    pub fn and(&mut self, val: u8) {
        let and_a = self.r.get_byte(REG::A) & val;
        self.r.set_byte(REG::A, and_a);
        self.r.update_flag(ALUFlag::Z, and_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, true);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn xor(&mut self, val: u8) {
        let xor_a = self.r.get_byte(REG::A) ^ val;
        self.r.set_byte(REG::A, xor_a);
        self.r.update_flag(ALUFlag::Z, xor_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn or(&mut self, val: u8) {
        let or_a = self.r.get_byte(REG::A) | val;
        self.r.set_byte(REG::A, or_a);
        self.r.update_flag(ALUFlag::Z, or_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);

    }
    pub fn cp(&mut self, val: u8) {
        let a = self.r.get_byte(REG::A);
        let cp_a = a.wrapping_sub(val);
        self.r.update_flag(ALUFlag::Z, cp_a == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (val & 0xF));
        self.r.update_flag(ALUFlag::C, a < val);
    }

    pub fn jp(&mut self, value: u16, flag: bool) -> u8 {
        if flag {
            self.r.set_word(REG::PC, value);
            4
        } else {
            //self.r.set_word("PC", self.r.get_word("PC") + 2);
            // TODO
            3
        }
    }

    pub fn jr(&mut self, steps: i8, flag: bool) -> u8 {
        if flag {
            self.r.set_word(REG::PC, self.r.get_word(REG::PC).wrapping_add(steps as u16));
            3
        } else {
            // TODO
            // update PC
            2
        }
    }

    pub fn push(&mut self, id: REG) {
        let value: u16 = self.r.get_word(id);
        let value_high: u8 = ((value & 0xff00) >> 8) as u8;
        let value_low: u8 = (value & 0xff) as u8;
        let sp: u16 = self.r.get_word(REG::SP);
        self.m.write_byte(sp - 1, value_high);
        self.m.write_byte(sp - 2, value_low);
        self.r.set_word(REG::SP, sp - 2);
    }

    pub fn pop(&mut self, id: REG) {
        let sp: u16 = self.r.get_word(REG::SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(id, stack_total);
        self.r.set_word(REG::SP, sp + 2);
    }

    pub fn call(&mut self, value: u16, flag: bool) -> u8 {
        if flag {
            let pc: u16 = self.r.get_word(REG::PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.r.get_word(REG::SP);
            self.m.write_byte(sp - 1, pc_high);
            self.m.write_byte(sp - 2, pc_low);
            self.r.set_word(REG::SP, sp - 2);
            self.r.set_word(REG::PC, value);
            6
        } else {
            3
        }
    }
    pub fn rst(&mut self, value: u8) {
            let pc: u16 = self.r.get_word(REG::PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.r.get_word(REG::SP);
            self.m.write_byte(sp - 1, pc_high);
            self.m.write_byte(sp - 2, pc_low);
            self.r.set_word(REG::SP, sp - 2);
            self.r.set_word(REG::PC, (value * 8) as u16);
    }
    pub fn retc(&mut self, flag: bool) -> u8 {
        if flag {
            let sp: u16 = self.r.get_word(REG::SP);
            let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
            let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
            let stack_total: u16 = (stack_high << 8) & stack_low;
            self.r.set_word(REG::PC, stack_total);
            self.r.set_word(REG::SP, sp + 2);
            5
        } else {
            2
        }
    }

    pub fn ret(&mut self) {
        let sp: u16 = self.r.get_word(REG::SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(REG::PC, stack_total);
        self.r.set_word(REG::SP, sp + 2);
    }

    pub fn reti(&mut self) {
        let sp: u16 = self.r.get_word(REG::SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(REG::PC, stack_total);
        self.r.set_word(REG::SP, sp + 2);

    }
    pub fn ei(&mut self) {
        // Interrupt Master Enable
        // IME = 1
        // TODO

    }
    pub fn di(&mut self) {
        // Interrupt Master Disable
        // IME = 0
        // TODO

    }

    pub fn halt(&mut self) {
        // HALT MODE ; 1
        // TODO

    }

    pub fn stop(&mut self) {
        // STOP MODE ; 1
        // TODO

    }

    pub fn ld_rr(&mut self, r1: REG, r2: REG) {
        self.r.set_byte(r1, self.r.get_byte(r2));

    }

    pub fn ld_rb(&mut self, r: REG, b: u8) {
        self.r.set_byte(r, b);
    }

    pub fn ld_rw(&mut self, r: REG, w: u16) {
        self.r.set_word(r, w);
    }

    pub fn ld_mr(&mut self, m: REG, r: REG) {
        self.m.write_byte(self.r.get_word(m), self.r.get_byte(r));
    }

    pub fn ld_rm(&mut self, r: REG, m: REG) {
        self.r.set_byte(r, self.m.read_byte(self.r.get_word(m)));
    }

    pub fn ld_mbr(&mut self, mb: u8, r: REG) {
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.m.write_byte(full_addr, self.r.get_byte(r));
    }

    pub fn ld_mwr(&mut self, mw: u16, r: REG) {
        self.m.write_byte(mw, self.r.get_byte(r));
    }

    pub fn ld_rmb(&mut self, r: REG, mb: u8) {
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.r.set_byte(r, self.m.read_byte(full_addr));
    }

    pub fn ld_rmw(&mut self, r: REG, mw: u16) {
        self.r.set_byte(r, self.m.read_byte(mw));
    }

    pub fn ld_mw(&mut self, m: REG, w: u16) {
       let sp: u16 = self.r.get_word(m);
       let sp_low: u8 = (sp & 0xFF) as u8;
       let sp_high: u8 = ((sp & 0xFF00) >> 8) as u8;
       self.m.write_byte(w, sp_low);
       self.m.write_byte(w+1, sp_high);
    }

    pub fn ld_mb(&mut self, m: REG, b: u8) {
        self.m.write_byte(self.r.get_word(m), b);
    }

    pub fn ld_rrw(&mut self, r1: REG, r2: REG) {
        self.r.set_word(r1, self.r.get_word(r2));
    }

    pub fn ld_mrc(&mut self) {
        let a: u8 = self.r.get_byte(REG::A);
        let c: u8 = self.r.get_byte(REG::C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.m.write_byte(addr, a);
    }

    pub fn ld_rmc(&mut self) {
        let c: u8 = self.r.get_byte(REG::C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.r.set_byte(REG::A, self.m.read_byte(addr));
    }

    pub fn ld_inc(&mut self) {
        let hl: u16 = self.r.get_word(REG::HL);
        let mem_hl: u8 = self.m.read_byte(hl);
        self.r.set_byte(REG::A, mem_hl);
        self.r.set_word(REG::HL, hl.wrapping_add(1));
    }
    pub fn ld_dec(&mut self ) {
        let hl: u16 = self.r.get_word(REG::HL);
        let mem_hl: u8 = self.m.read_byte(hl);
        self.r.set_byte(REG::A, mem_hl);
        self.r.set_word(REG::HL, hl.wrapping_sub(1));
    }

    pub fn ld_mem_inc(&mut self) {
        let hl: u16 = self.r.get_word(REG::HL);
        let a: u8 = self.r.get_byte(REG::A);
        self.m.write_byte(hl, a);
        self.r.set_word(REG::HL, hl.wrapping_add(1));
    }

    pub fn ld_mem_dec(&mut self) {
        let hl: u16 = self.r.get_word(REG::HL);
        let a: u8 = self.r.get_byte(REG::A);
        self.m.write_byte(hl, a);
        self.r.set_word(REG::HL, hl.wrapping_sub(1));
    }
}

