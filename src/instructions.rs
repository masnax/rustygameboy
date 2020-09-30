use crate::register::Register;
use crate::register::ALUFlag;
use crate::register::REG;

pub trait Instruction {
    // Arithmetic
    fn inc(&mut self, id: REG);
    fn dec(&mut self, id: REG);
    fn inc_mem(&mut self);
    fn dec_mem(&mut self);
    fn add_word(&mut self, id: REG, val: u16);
    fn add_byte(&mut self, id: REG, val: u8);
    fn add_stack(&mut self, val: i8);
    fn adc(&mut self, val: u8);
    fn sub_word(&mut self, id: REG, val: u16);
    fn sub_byte(&mut self, id: REG, val: u8);
    fn sbc(&mut self, val: u8);
    fn daa(&mut self);
    fn cpl(&mut self);

    fn and(&mut self, val: u8);
    fn xor(&mut self, val: u8);
    fn or(&mut self, val: u8);
    fn cp(&mut self, val: u8);

    fn rlca(&mut self, id: REG);
    fn rla(&mut self, id: REG);
    fn rrca(&mut self, id: REG);
    fn rra(&mut self, id: REG);

    // Moves
    fn jr(&mut self, steps: i8, flag: bool) -> u8;
    fn jp(&mut self, value: u16, flag: bool) -> u8;
    fn call(&mut self, value: u16, flag: bool) -> u8;
    fn rst(&mut self, value: u8);
    fn ret(&mut self);
    fn retc(&mut self, flag: bool) -> u8;

    // Flags
    fn scf(&mut self);
    fn ccf(&mut self);

    // Stack
    fn push(&mut self, id: REG);
    fn pop(&mut self, id: REG);

    // Interrupts
    fn reti(&mut self);
    fn di(&mut self);
    fn ei(&mut self);
    fn halt(&mut self);
    fn stop(&mut self);

    // Load
    fn ld_rr(&mut self, r1: REG, r2: REG);
    fn ld_rrw(&mut self, r1: REG, r2: REG);
    fn ld_rb(&mut self, r: REG, b: u8);
    fn ld_rw(&mut self, r: REG, w: u16);
    fn ld_mr(&mut self, m: REG, r: REG);
    fn ld_rm(&mut self, r: REG, m: REG);
    
    fn ld_mbr(&mut self, mb: u8, r: REG);
    fn ld_mwr(&mut self, mw: u16, r: REG);
    fn ld_rmb(&mut self, r: REG, mb: u8);
    fn ld_rmw(&mut self, r: REG, mw: u16);

    fn ld_mw(&mut self, m: REG, w: u16);
    fn ld_mb(&mut self, m: REG, b: u8);
    fn ld_mrc(&mut self);
    fn ld_rmc(&mut self);

    fn ld_inc(&mut self);
    fn ld_dec(&mut self);
    fn ld_mem_inc(&mut self);
    fn ld_mem_dec(&mut self);
}


impl Instruction for Register {
    fn inc(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let add: u8 = value.wrapping_add(1);
        match id {
            REG::A => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word(REG::AF) & 0xF0);
                self.set_word(REG::AF, inc);
            },
            REG::B => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word(REG::BC) & 0xFF);
                self.set_word(REG::BC, inc);
            },
            REG::C => {
                let inc: u16 = (self.get_word(REG::BC) & 0xFF00) | (add as u16);
                self.set_word(REG::BC, inc);
            },
            REG::D => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word(REG::DE) & 0xFF);
                self.set_word(REG::DE, inc);
            },
            REG::E => {
                let inc: u16 = (self.get_word(REG::DE) & 0xFF00) | (add as u16);
                self.set_word(REG::DE, inc);
            },
            REG::H => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word(REG::BC) & 0xFF);
                self.set_word(REG::BC, inc);
            },
            REG::L => {
                let inc: u16 = (self.get_word(REG::HL) & 0xFF00) | (add as u16);
                self.set_word(REG::HL, inc);
            },
            _other => {
                panic!("[ERROR] invalid register");
            }
        }
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    fn dec(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let sub: u8 = value.wrapping_sub(1);
        match id {
            REG::A => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word(REG::AF) & 0xF0);
                self.set_word(REG::AF, inc);
            },
            REG::B => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word(REG::BC) & 0xFF);
                self.set_word(REG::BC, inc);
            },
            REG::C => {
                let inc: u16 = (self.get_word(REG::BC) & 0xFF00) | (sub as u16);
                self.set_word(REG::BC, inc);
            },
            REG::D => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word(REG::DE) & 0xFF);
                self.set_word(REG::DE, inc);
            },
            REG::E => {
                let inc: u16 = (self.get_word(REG::DE) & 0xFF00) | (sub as u16);
                self.set_word(REG::DE, inc);
            },
            REG::H => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word(REG::BC) & 0xFF);
                self.set_word(REG::BC, inc);
            },
            REG::L => {
                let inc: u16 = (self.get_word(REG::HL) & 0xFF00) | (sub as u16);
                self.set_word(REG::HL, inc);
            },
            _other => {
                panic!("[ERROR] invalid register");

            }
        }
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    fn inc_mem(&mut self) {
        let addr: u16 = self.get_word(REG::HL);
        let value: u8 = self.read_byte(addr);
        let add: u8 = value.wrapping_add(1);
        self.write_byte(addr, add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    fn dec_mem(&mut self) {
        let addr: u16 = self.get_word(REG::HL);
        let value: u8 = self.read_byte(addr);
        let sub: u8 = value.wrapping_sub(1);
        self.write_byte(addr, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    fn add_word(&mut self, id: REG, value: u16) {
        let previous: u16 = self.get_word(id);
        let add: u16 = previous.wrapping_add(value);
        self.set_word(id, add);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (previous & 0xFF) + (value & 0xFF) > 0xFF);
        self.update_flag(ALUFlag::C, previous > 0xFFFF - value);
    }

    fn add_byte(&mut self, id: REG, value: u8) {
        let previous: u8 = self.get_byte(id);
        let add: u8 = previous.wrapping_add(value);
        self.set_byte(id, add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (previous & 0xF) + (value & 0xF) > 0xF);
        self.update_flag(ALUFlag::C, previous > 0xFF - value);
    }

    fn add_stack(&mut self, value: i8) {
        let sp: u16 = self.get_word(REG::SP);
        let value_u16: u16 = value as u16;
        let add: u16 = sp.wrapping_add(value_u16);
        self.set_word(REG::SP, add);
        self.update_flag(ALUFlag::Z, false);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (sp & 0xFF) + (value_u16 & 0xFF) > 0xFF);
        self.update_flag(ALUFlag::C, sp > 0xFFFF - value_u16);
    }

    fn adc(&mut self, value: u8) {
        let a: u8 = self.get_byte(REG::A);
        let c: u8 = self.get_flag(ALUFlag::C) as u8;
        let add: u8 = a.wrapping_add(value).wrapping_add(c);
        self.set_byte(REG::A, add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (a & 0xF) + (value & 0xF) + c > 0xF);
        self.update_flag(ALUFlag::C, (a as u16) > (0xFF as u16) - (value as u16) - (c as u16));


    }

    fn sub_word(&mut self, id: REG, value: u16) {
        let previous: u16 = self.get_word(id);
        let sub: u16 = previous.wrapping_sub(value);
        self.set_word(id, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (previous & 0xFF) < (value & 0xFF));
        self.update_flag(ALUFlag::C, previous < value);
    }

    fn sub_byte(&mut self, id: REG, value: u8) {
        let previous: u8 = self.get_byte(id);
        let sub: u8 = previous.wrapping_sub(value);
        self.set_byte(id, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (previous & 0xF) < (value & 0xF));
        self.update_flag(ALUFlag::C, previous < value);
    }

    fn sbc(&mut self, value: u8) {
        let a: u8 = self.get_byte(REG::A);
        let c: u8 = self.get_flag(ALUFlag::C) as u8;
        let sub: u8 = a.wrapping_sub(value).wrapping_sub(c);
        self.set_byte(REG::A, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (a & 0xF) < (value & 0xF) + c);
        self.update_flag(ALUFlag::C, (a as u16) < (value as u16) + (c as u16));
    }

    fn rlca(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (carry as u8);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rla(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (self.get_flag(ALUFlag::C) as u8);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rrca(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((carry as u8) << 7);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rra(&mut self, id: REG) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((self.get_flag(ALUFlag::C) as u8) << 7);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn daa(&mut self) {
        let a = self.get_byte(REG::A);
        let c = self.get_flag(ALUFlag::C);
        let h = self.get_flag(ALUFlag::H);
        let n = self.get_flag(ALUFlag::N);
        let mut bcd = 0;
        if c { bcd |= 0x60; }
        if h { bcd |= 0x06; }
        if n {
            self.set_byte(REG::A, a.wrapping_sub(bcd));
        } else {
            if a & 0xF > 0x9 { bcd |= 0x6; }
            if a > 0x99 { bcd |= 0x60; }
            self.set_byte(REG::A, a.wrapping_add(bcd));
        }
        self.update_flag(ALUFlag::C, bcd >= 0x60);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::Z, self.get_byte(REG::A) == 0);
    }

    fn cpl(&mut self) {
        self.set_byte(REG::A, 0xFF ^ self.get_byte(REG::A));
        self.update_flag(ALUFlag::H, true);
        self.update_flag(ALUFlag::N, true);
    }

    fn scf(&mut self) {
        self.update_flag(ALUFlag::C, true);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::N, false);
    }

    fn ccf(&mut self) {
        self.update_flag(ALUFlag::C, !self.get_flag(ALUFlag::C));
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::N, false);
    }


    fn and(&mut self, val: u8) {
        let and_a = self.get_byte(REG::A) & val;
        self.set_byte(REG::A, and_a);
        self.update_flag(ALUFlag::Z, and_a == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, true);
        self.update_flag(ALUFlag::C, false);
    }

    fn xor(&mut self, val: u8) {
        let xor_a = self.get_byte(REG::A) ^ val;
        self.set_byte(REG::A, xor_a);
        self.update_flag(ALUFlag::Z, xor_a == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, false);
    }

    fn or(&mut self, val: u8) {
        let or_a = self.get_byte(REG::A) | val;
        self.set_byte(REG::A, or_a);
        self.update_flag(ALUFlag::Z, or_a == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, false);

    }
    fn cp(&mut self, val: u8) {
        let a = self.get_byte(REG::A);
        let cp_a = a.wrapping_sub(val);
        self.update_flag(ALUFlag::Z, cp_a == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (a & 0xF) < (val & 0xF));
        self.update_flag(ALUFlag::C, a < val);
    }

    fn jp(&mut self, value: u16, flag: bool) -> u8 {
        if flag {
            self.set_word(REG::PC, value);
            4
        } else {
            //self.set_word("PC", self.get_word("PC") + 2);
            // TODO
            3
        }
    }

    fn jr(&mut self, steps: i8, flag: bool) -> u8 {
        if flag {
            self.set_word(REG::PC, self.get_word(REG::PC).wrapping_add(steps as u16));
            3
        } else {
            // TODO
            // update PC
            2
        }
    }

    fn push(&mut self, id: REG) {
        let value: u16 = self.get_word(id);
        let value_high: u8 = ((value & 0xff00) >> 8) as u8;
        let value_low: u8 = (value & 0xff) as u8;
        let sp: u16 = self.get_word(REG::SP);
        self.write_byte(sp - 1, value_high);
        self.write_byte(sp - 2, value_low);
        self.set_word(REG::SP, sp - 2);
    }

    fn pop(&mut self, id: REG) {
        let sp: u16 = self.get_word(REG::SP);
        let stack_low: u16 = (self.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.set_word(id, stack_total);
        self.set_word(REG::SP, sp + 2);
    }

    fn call(&mut self, value: u16, flag: bool) -> u8 {
        if flag {
            let pc: u16 = self.get_word(REG::PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.get_word(REG::SP);
            self.write_byte(sp - 1, pc_high);
            self.write_byte(sp - 2, pc_low);
            self.set_word(REG::SP, sp - 2);
            self.set_word(REG::PC, value);
            6
        } else {
            3
        }
    }
    fn rst(&mut self, value: u8) {
            let pc: u16 = self.get_word(REG::PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.get_word(REG::SP);
            self.write_byte(sp - 1, pc_high);
            self.write_byte(sp - 2, pc_low);
            self.set_word(REG::SP, sp - 2);
            self.set_word(REG::PC, (value * 8) as u16);
    }
    fn retc(&mut self, flag: bool) -> u8 {
        if flag {
            let sp: u16 = self.get_word(REG::SP);
            let stack_low: u16 = (self.read_byte(sp) as u16) & 0xff;
            let stack_high: u16 = (self.read_byte(sp + 1) as u16) & 0xff;
            let stack_total: u16 = (stack_high << 8) & stack_low;
            self.set_word(REG::PC, stack_total);
            self.set_word(REG::SP, sp + 2);
            5
        } else {
            2
        }
    }

    fn ret(&mut self) {
        let sp: u16 = self.get_word(REG::SP);
        let stack_low: u16 = (self.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.set_word(REG::PC, stack_total);
        self.set_word(REG::SP, sp + 2);
    }

    fn reti(&mut self) {
        let sp: u16 = self.get_word(REG::SP);
        let stack_low: u16 = (self.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.set_word(REG::PC, stack_total);
        self.set_word(REG::SP, sp + 2);

    }
    fn ei(&mut self) {
        // Interrupt Master Enable
        // IME = 1

    }
    fn di(&mut self) {
        // Interrupt Master Disable
        // IME = 0

    }

    fn halt(&mut self) {
        // HALT MODE ; 1

    }

    fn stop(&mut self) {
        // STOP MODE ; 1

    }

    fn ld_rr(&mut self, r1: REG, r2: REG) {
        self.set_byte(r1, self.get_byte(r2));

    }

    fn ld_rb(&mut self, r: REG, b: u8) {
        self.set_byte(r, b);
    }

    fn ld_rw(&mut self, r: REG, w: u16) {
        self.set_word(r, w);
    }

    fn ld_mr(&mut self, m: REG, r: REG) {
        self.write_byte(self.get_word(m), self.get_byte(r));
    }

    fn ld_rm(&mut self, r: REG, m: REG) {
        self.set_byte(r, self.read_byte(self.get_word(m)));
    }

    fn ld_mbr(&mut self, mb: u8, r: REG) {
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.write_byte(full_addr, self.get_byte(r));
    }

    fn ld_mwr(&mut self, mw: u16, r: REG) {
        self.write_byte(mw, self.get_byte(r));
    }

    fn ld_rmb(&mut self, r: REG, mb: u8) {
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.set_byte(r, self.read_byte(full_addr));
    }

    fn ld_rmw(&mut self, r: REG, mw: u16) {
        self.set_byte(r, self.read_byte(mw));
    }

    fn ld_mw(&mut self, m: REG, w: u16) {
       let sp: u16 = self.get_word(m);
       let sp_low: u8 = (sp & 0xFF) as u8;
       let sp_high: u8 = ((sp & 0xFF00) >> 8) as u8;
       self.write_byte(w, sp_low);
       self.write_byte(w+1, sp_high);
    }

    fn ld_mb(&mut self, m: REG, b: u8) {
        self.write_byte(self.get_word(m), b);
    }

    fn ld_rrw(&mut self, r1: REG, r2: REG) {
        self.set_word(r1, self.get_word(r2));
    }

    fn ld_mrc(&mut self) {
        let a: u8 = self.get_byte(REG::A);
        let c: u8 = self.get_byte(REG::C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.write_byte(addr, a);
    }

    fn ld_rmc(&mut self) {
        let c: u8 = self.get_byte(REG::C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.set_byte(REG::A, self.read_byte(addr));
    }

    fn ld_inc(&mut self) {
        let hl: u16 = self.get_word(REG::HL);
        let mem_hl: u8 = self.read_byte(hl);
        self.set_byte(REG::A, mem_hl);
        self.set_word(REG::HL, hl.wrapping_add(1));
    }
    fn ld_dec(&mut self ) {
        let hl: u16 = self.get_word(REG::HL);
        let mem_hl: u8 = self.read_byte(hl);
        self.set_byte(REG::A, mem_hl);
        self.set_word(REG::HL, hl.wrapping_sub(1));
    }

    fn ld_mem_inc(&mut self) {
        let hl: u16 = self.get_word(REG::HL);
        let a: u8 = self.get_byte(REG::A);
        self.write_byte(hl, a);
        self.set_word(REG::HL, hl.wrapping_add(1));
    }

    fn ld_mem_dec(&mut self) {
        let hl: u16 = self.get_word(REG::HL);
        let a: u8 = self.get_byte(REG::A);
        self.write_byte(hl, a);
        self.set_word(REG::HL, hl.wrapping_sub(1));
    }
}

