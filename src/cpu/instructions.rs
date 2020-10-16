use crate::register::{REG, REG::*, ALUFlag, Register};
use crate::mem_map::Memory;

pub struct InstructionSet<'a> {
    pub r: Register,
    pub m: &'a mut Memory,
    pub halted: bool,
}


impl<'a> InstructionSet<'a> {
    pub fn init(registers: Register, mem_map: &'a  mut Memory) -> InstructionSet<'a> {
        InstructionSet {
            r: registers,
            m: mem_map,
            halted: false,
        }
    }

    pub fn inc(&mut self, id: REG) {
        if id as u8 > L as u8 {
            self.r.set_word(id, self.r.get_word(id).wrapping_add(1));
        } else {
            let value: u8 = self.r.get_byte(id);
            let add: u8 = value.wrapping_add(1);
            self.r.set_byte(id, add);
            self.r.update_flag(ALUFlag::Z, add == 0);
            self.r.update_flag(ALUFlag::N, false);
            self.r.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
        }
    }

    pub fn dec(&mut self, id: REG) {
        if id as u8 > L as u8 {
            self.r.set_word(id, self.r.get_word(id).wrapping_sub(1));
        } else {
            let value: u8 = self.r.get_byte(id);
            let sub: u8 = value.wrapping_sub(1);
            self.r.set_byte(id, sub);
            self.r.update_flag(ALUFlag::Z, sub == 0);
            self.r.update_flag(ALUFlag::N, true);
            self.r.update_flag(ALUFlag::H, (value & 0xF) == 0);
        }
    }

    pub fn inc_mem(&mut self) {
        let addr: u16 = self.r.get_word(HL);
        let value: u8 = self.m.read_byte(addr);
        let add: u8 = value.wrapping_add(1);
        self.m.write_byte(addr, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    pub fn dec_mem(&mut self) {
        let addr: u16 = self.r.get_word(HL);
        let value: u8 = self.m.read_byte(addr);
        let sub: u8 = value.wrapping_sub(1);
        self.m.write_byte(addr, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    pub fn add_val(&mut self, id: REG) {
        let value: u8 = self.fetch();
        let previous: u8 = self.r.get_byte(id);
        let add: u8 = previous.wrapping_add(value);
        self.r.set_byte(id, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (previous & 0xF) + (value & 0xF) > 0xF);
        self.r.update_flag(ALUFlag::C, previous > 0xFF - value);
    }

    pub fn add(&mut self, id1: REG, id2: REG) {
        if id1 as u8 > L as u8 {
            let previous: u16 = self.r.get_word(id1);
            let value: u16 = self.r.get_word(id2);
            let add: u16 = previous.wrapping_add(value);
            self.r.set_word(id1, add);
            self.r.update_flag(ALUFlag::N, false);
            self.r.update_flag(ALUFlag::H, (previous & 0xFF) + (value & 0xFF) > 0xFF);
            self.r.update_flag(ALUFlag::C, previous > 0xFFFF - value);
        } else {
            let previous: u8 = self.r.get_byte(id1);
            let value: u8 = if id2 == HL { self.m.read_byte(self.r.get_word(id2)) } else { self.r.get_byte(id2) };
            let add: u8 = previous.wrapping_add(value);
            self.r.set_byte(id1, add);
            self.r.update_flag(ALUFlag::Z, add == 0);
            self.r.update_flag(ALUFlag::N, false);
            self.r.update_flag(ALUFlag::H, (previous & 0xF) + (value & 0xF) > 0xF);
            self.r.update_flag(ALUFlag::C, previous > 0xFF - value);
        }
    }

    pub fn add_stack(&mut self) {
        let value: i8 = self.fetch() as i8;
        let sp: u16 = self.r.get_word(SP);
        let value_u16: u16 = value as u16;
        let add: u16 = sp.wrapping_add(value_u16);
        self.r.set_word(SP, add);
        self.r.update_flag(ALUFlag::Z, false);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (sp & 0xFF) + (value_u16 & 0xFF) > 0xFF);
        self.r.update_flag(ALUFlag::C, sp > 0xFFFF - value_u16);
    }

    pub fn sub(&mut self, id: REG, id2: REG) {
        let previous: u8 = self.r.get_byte(id);
        let value: u8 = if id2 == HL { self.m.read_byte(self.r.get_word(id2)) } else { self.r.get_byte(id2) };
        let sub: u8 = previous.wrapping_sub(value);
        self.r.set_byte(id, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (previous & 0xF) < (value & 0xF));
        self.r.update_flag(ALUFlag::C, previous < value);
    }
    
    pub fn sub_val(&mut self, id: REG) {
        let value: u8 = self.fetch();
        let previous: u8 = self.r.get_byte(id);
        let sub: u8 = previous.wrapping_sub(value);
        self.r.set_byte(id, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (previous & 0xF) < (value & 0xF));
        self.r.update_flag(ALUFlag::C, previous < value);
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
        let a = self.r.get_byte(A);
        let c = self.r.get_flag(ALUFlag::C);
        let h = self.r.get_flag(ALUFlag::H);
        let n = self.r.get_flag(ALUFlag::N);
        let mut bcd = 0;
        if c { bcd |= 0x60; }
        if h { bcd |= 0x06; }
        if n {
            self.r.set_byte(A, a.wrapping_sub(bcd));
        } else {
            if a & 0xF > 0x9 { bcd |= 0x6; }
            if a > 0x99 { bcd |= 0x60; }
            self.r.set_byte(A, a.wrapping_add(bcd));
        }
        self.r.update_flag(ALUFlag::C, bcd >= 0x60);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::Z, self.r.get_byte(A) == 0);
    }

    pub fn cpl(&mut self) {
        self.r.set_byte(A, 0xFF ^ self.r.get_byte(A));
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

    pub fn adc(&mut self, id: REG) {
        let a: u8 = self.r.get_byte(A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let value: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let add: u8 = a.wrapping_add(value).wrapping_add(c);
        self.r.set_byte(A, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (a & 0xF) + (value & 0xF) + c > 0xF);
        self.r.update_flag(ALUFlag::C, (a as u16) > (0xFF as u16) - (value as u16) - (c as u16));
    }

    pub fn sbc(&mut self, id: REG) {
        let a: u8 = self.r.get_byte(A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let value: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let sub: u8 = a.wrapping_sub(value).wrapping_sub(c);
        self.r.set_byte(A, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (value & 0xF) + c);
        self.r.update_flag(ALUFlag::C, (a as u16) < (value as u16) + (c as u16));
    }

    pub fn and(&mut self, id: REG) {
        let val: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let and_a = self.r.get_byte(A) & val;
        self.r.set_byte(A, and_a);
        self.r.update_flag(ALUFlag::Z, and_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, true);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn xor(&mut self, id: REG) {
        let val: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let xor_a = self.r.get_byte(A) ^ val;
        self.r.set_byte(A, xor_a);
        self.r.update_flag(ALUFlag::Z, xor_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn or(&mut self, id: REG) {
        let val: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let or_a = self.r.get_byte(A) | val;
        self.r.set_byte(A, or_a);
        self.r.update_flag(ALUFlag::Z, or_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);

    }
    pub fn cp(&mut self, id: REG) {
        let val: u8 = if id == HL { self.m.read_byte(self.r.get_word(id)) } else { self.r.get_byte(id) };
        let a = self.r.get_byte(A);
        let cp_a = a.wrapping_sub(val);
        self.r.update_flag(ALUFlag::Z, cp_a == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (val & 0xF));
        self.r.update_flag(ALUFlag::C, a < val);
    }

    pub fn adc_val(&mut self) {
        let val: u8 = self.fetch();
        let a: u8 = self.r.get_byte(A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let add: u8 = a.wrapping_add(val).wrapping_add(c);
        self.r.set_byte(A, add);
        self.r.update_flag(ALUFlag::Z, add == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, (a & 0xF) + (val & 0xF) + c > 0xF);
        self.r.update_flag(ALUFlag::C, (a as u16) > (0xFF as u16) - (val as u16) - (c as u16));
    }

    pub fn sbc_val(&mut self) {
        let val: u8 = self.fetch();
        let a: u8 = self.r.get_byte(A);
        let c: u8 = self.r.get_flag(ALUFlag::C) as u8;
        let sub: u8 = a.wrapping_sub(val).wrapping_sub(c);
        self.r.set_byte(A, sub);
        self.r.update_flag(ALUFlag::Z, sub == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (val & 0xF) + c);
        self.r.update_flag(ALUFlag::C, (a as u16) < (val as u16) + (c as u16));
    }

    pub fn and_val(&mut self) {
        let val: u8 = self.fetch();
        let and_a = self.r.get_byte(A) & val;
        self.r.set_byte(A, and_a);
        self.r.update_flag(ALUFlag::Z, and_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, true);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn xor_val(&mut self) {
        let val: u8 = self.fetch();
        let xor_a = self.r.get_byte(A) ^ val;
        self.r.set_byte(A, xor_a);
        self.r.update_flag(ALUFlag::Z, xor_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);
    }

    pub fn or_val(&mut self) {
        let val: u8 = self.fetch();
        let or_a = self.r.get_byte(A) | val;
        self.r.set_byte(A, or_a);
        self.r.update_flag(ALUFlag::Z, or_a == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);

    }
    pub fn cp_val(&mut self) {
        let val: u8 = self.fetch();
        let a = self.r.get_byte(A);
        let cp_a = a.wrapping_sub(val);
        self.r.update_flag(ALUFlag::Z, cp_a == 0);
        self.r.update_flag(ALUFlag::N, true);
        self.r.update_flag(ALUFlag::H, (a & 0xF) < (val & 0xF));
        self.r.update_flag(ALUFlag::C, a < val);
    }

    pub fn jp_hl(&mut self) {
        self.r.set_word(PC, self.r.get_word(HL));
    }

    pub fn jp(&mut self, flag: bool) -> u8 {
        let value: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
        if flag {
            self.r.set_word(PC, value);
            4
        } else {
            3
        }
    }

    pub fn jr(&mut self, flag: bool) -> u8 {
        let steps: i8 = self.fetch() as i8;
        if flag {
            self.r.set_word(PC, self.r.get_word(PC).wrapping_add(steps as u16));
            3
        } else {
            2
        }
    }

    pub fn push(&mut self, id: REG) {
        let value: u16 = self.r.get_word(id);
        let value_high: u8 = ((value & 0xff00) >> 8) as u8;
        let value_low: u8 = (value & 0xff) as u8;
        let sp: u16 = self.r.get_word(SP);
        self.m.write_byte(sp - 1, value_high);
        self.m.write_byte(sp - 2, value_low);
        self.r.set_word(SP, sp - 2);
    }

    pub fn pop(&mut self, id: REG) {
        let sp: u16 = self.r.get_word(SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(id, stack_total);
        self.r.set_word(SP, sp + 2);
    }

    pub fn call(&mut self, flag: bool) -> u8 {
        let value: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
        if flag {
            let pc: u16 = self.r.get_word(PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.r.get_word(SP);
            self.m.write_byte(sp - 1, pc_high);
            self.m.write_byte(sp - 2, pc_low);
            self.r.set_word(SP, sp - 2);
            self.r.set_word(PC, value);
            6
        } else {
            3
        }
    }
    pub fn rst(&mut self, value: u8) {
            let pc: u16 = self.r.get_word(PC);
            let pc_high: u8 = ((pc & 0xff00) >> 8) as u8;
            let pc_low: u8 = (pc & 0xff) as u8;
            let sp: u16 = self.r.get_word(SP);
            self.m.write_byte(sp - 1, pc_high);
            self.m.write_byte(sp - 2, pc_low);
            self.r.set_word(SP, sp - 2);
            self.r.set_word(PC, (value * 8) as u16);
    }
    pub fn retc(&mut self, flag: bool) -> u8 {
        if flag {
            let sp: u16 = self.r.get_word(SP);
            let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
            let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
            let stack_total: u16 = (stack_high << 8) & stack_low;
            self.r.set_word(PC, stack_total);
            self.r.set_word(SP, sp + 2);
            5
        } else {
            2
        }
    }

    pub fn ret(&mut self) {
        let sp: u16 = self.r.get_word(SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(PC, stack_total);
        self.r.set_word(SP, sp + 2);
    }

    pub fn reti(&mut self) {
        let sp: u16 = self.r.get_word(SP);
        let stack_low: u16 = (self.m.read_byte(sp) as u16) & 0xff;
        let stack_high: u16 = (self.m.read_byte(sp + 1) as u16) & 0xff;
        let stack_total: u16 = (stack_high << 8) & stack_low;
        self.r.set_word(PC, stack_total);
        self.r.set_word(SP, sp + 2);

    }
    pub fn ei(&mut self) {
        // Interrupt Master Enable
        // IME = 1
        println!("EI: TODO");
    }
    pub fn di(&mut self) {
        // Interrupt Master Disable
        // IME = 0
        println!("DI: TODO");
    }

    pub fn halt(&mut self) {
        self.halted = true;
    }

    pub fn stop(&mut self) {
        // STOP MODE ; 1
        println!("STOP: TODO");
    }

    pub fn ld_rr(&mut self, r1: REG, r2: REG) {
        self.r.set_byte(r1, self.r.get_byte(r2));

    }

    pub fn ld_rb(&mut self, r: REG) {
        let b: u8 = self.fetch();
        self.r.set_byte(r, b);
    }

    pub fn ld_rw(&mut self, r: REG) {
        let w: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
        self.r.set_word(r, w);
    }

    pub fn ld_mr(&mut self, m: REG, r: REG) {
        self.m.write_byte(self.r.get_word(m), self.r.get_byte(r));
    }

    pub fn ld_rm(&mut self, r: REG, m: REG) {
        self.r.set_byte(r, self.m.read_byte(self.r.get_word(m)));
    }

    pub fn ld_mbr(&mut self, r: REG) {
        let mb: u8 = self.fetch();
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.m.write_byte(full_addr, self.r.get_byte(r));
    }

    pub fn ld_mwr(&mut self, r: REG) {
        let mw: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
        self.m.write_byte(mw, self.r.get_byte(r));
    }

    pub fn ld_rmb(&mut self, r: REG) {
        let mb: u8 = self.fetch();
        let full_addr: u16 = 0xFF00 | (mb as u16);
        self.r.set_byte(r, self.m.read_byte(full_addr));
    }

    pub fn ld_rmw(&mut self, r: REG) {
        let mw: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
        self.r.set_byte(r, self.m.read_byte(mw));
    }

    pub fn ld_mw(&mut self, m: REG) {
       let w: u16 = ((self.fetch() as u16) << 8) | self.fetch() as u16;
       let sp: u16 = self.r.get_word(m);
       let sp_low: u8 = (sp & 0xFF) as u8;
       let sp_high: u8 = ((sp & 0xFF00) >> 8) as u8;
       self.m.write_byte(w, sp_low);
       self.m.write_byte(w+1, sp_high);
    }

    pub fn ld_mb(&mut self, m: REG) {
        let b: u8 = self.fetch();
        self.m.write_byte(self.r.get_word(m), b);
    }

    pub fn ld_rrw(&mut self, r1: REG, r2: REG) {
        self.r.set_word(r1, self.r.get_word(r2));
    }

    pub fn ld_mrc(&mut self) {
        let a: u8 = self.r.get_byte(A);
        let c: u8 = self.r.get_byte(C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.m.write_byte(addr, a);
    }

    pub fn ld_rmc(&mut self) {
        let c: u8 = self.r.get_byte(C);
        let addr: u16 = 0xFF00 | (c as u16);
        self.r.set_byte(A, self.m.read_byte(addr));
    }

    pub fn ld_inc(&mut self) {
        let hl: u16 = self.r.get_word(HL);
        let mem_hl: u8 = self.m.read_byte(hl);
        self.r.set_byte(A, mem_hl);
        self.r.set_word(HL, hl.wrapping_add(1));
    }
    pub fn ld_dec(&mut self ) {
        let hl: u16 = self.r.get_word(HL);
        let mem_hl: u8 = self.m.read_byte(hl);
        self.r.set_byte(A, mem_hl);
        self.r.set_word(HL, hl.wrapping_sub(1));
    }

    pub fn ld_mem_inc(&mut self) {
        let hl: u16 = self.r.get_word(HL);
        let a: u8 = self.r.get_byte(A);
        self.m.write_byte(hl, a);
        self.r.set_word(HL, hl.wrapping_add(1));
    }

    pub fn ld_mem_dec(&mut self) {
        let hl: u16 = self.r.get_word(HL);
        let a: u8 = self.r.get_byte(A);
        self.m.write_byte(hl, a);
        self.r.set_word(HL, hl.wrapping_sub(1));
    }

    pub fn get_flag(&self, flag: ALUFlag) -> bool {
        self.r.get_flag(flag)
    }

    pub fn fetch(&mut self) -> u8 {
        let byte: u8 = self.m.read_byte(self.r.get_word(PC));
        self.inc(PC);
        return byte;
    }

    pub fn get_vram(&self) -> [u8; 0x2000] {
        self.m.get_vram()
    }
}

