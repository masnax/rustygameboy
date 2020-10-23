use crate::cpu::instructions::InstructionSet;
use crate::register::{ALUFlag, REG};
use crate::memory::Memory;

pub trait CBInstructionSet<'a> {
    fn rlc(&mut self, id: REG);
    fn rl(&mut self, id: REG);
    fn rrc(&mut self, id: REG);
    fn rr(&mut self, id: REG);

    fn sla(&mut self, id: REG);
    fn sra(&mut self, id: REG);
    fn srl(&mut self, id: REG);

    fn swap(&mut self, id: REG);
    fn bit(&mut self, val: u8, id: REG);
    fn res(&mut self, val: u8, id: REG);
    fn set(&mut self, val: u8, id: REG);

    fn rlc_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn rl_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn rrc_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn rr_mem(&mut self, mem: &'a mut Memory, id: REG);

    fn sla_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn sra_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn srl_mem(&mut self, mem: &'a mut Memory, id: REG);

    fn swap_mem(&mut self, mem: &'a mut Memory, id: REG);
    fn bit_mem(&mut self, mem: &'a mut Memory, val: u8, id: REG);
    fn res_mem(&mut self, mem: &'a mut Memory, val: u8, id: REG);
    fn set_mem(&mut self, mem: &'a mut Memory, val: u8, id: REG);
}


impl<'a> CBInstructionSet<'a> for InstructionSet {
    fn rlc(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (carry as u8);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);}

    fn rl(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (self.r.get_flag(ALUFlag::C) as u8);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn rrc(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((carry as u8) << 7);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);

    }

    fn rr(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((self.r.get_flag(ALUFlag::C) as u8) << 7);
        self.r.set_byte(id, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn sla(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let shift: u8 = (value << 1) & 0xFE;
        self.r.set_byte(id, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn sra(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let last_bit: u8 = value & 0x80;
        let shift: u8 = (value >> 1) | last_bit;
        self.r.set_byte(id, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn srl(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let shift: u8 = (value >> 1) & 0x7F;
        self.r.set_byte(id, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn swap(&mut self, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let val_high: u8 = (value & 0xF0) >> 4;
        let val_low: u8 = (value & 0xF) << 4;
        let swap: u8 = val_low | val_high;
        self.r.set_byte(id, swap);
        self.r.update_flag(ALUFlag::Z, swap == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);
    }

    fn bit(&mut self, bit: u8, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let comp: bool = ((value >> bit) & 0x1) == 0;
        self.r.update_flag(ALUFlag::Z, comp);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, true);

    }

    fn res(&mut self, bit: u8, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let shift: u8 = 0xFF - (0x1 << bit);
        let set: u8 = value & shift;
        self.r.set_byte(id, set);

    }

    fn set(&mut self, bit: u8, id: REG) {
        let value: u8 = self.r.get_byte(id);
        let shift: u8 = 0x1 << bit;
        let set: u8 = value | shift;
        self.r.set_byte(id, set);
    }

    fn rlc_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (carry as u8);
        mem.write_byte(addr, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);}

    fn rl_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (self.r.get_flag(ALUFlag::C) as u8);
        mem.write_byte(addr, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn rrc_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((carry as u8) << 7);
        mem.write_byte(addr, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);

    }

    fn rr_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((self.r.get_flag(ALUFlag::C) as u8) << 7);
        mem.write_byte(addr, rotate);
        self.r.update_flag(ALUFlag::Z, rotate == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn sla_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x80) > 0;
        let shift: u8 = (value << 1) & 0xFE;
        mem.write_byte(addr, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn sra_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x1) > 0;
        let last_bit: u8 = value & 0x80;
        let shift: u8 = (value >> 1) | last_bit;
        mem.write_byte(addr, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn srl_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let carry: bool = (value & 0x1) > 0;
        let shift: u8 = (value >> 1) & 0x7F;
        mem.write_byte(addr, shift);
        self.r.update_flag(ALUFlag::Z, shift == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, carry);
    }

    fn swap_mem(&mut self, mem: &'a mut Memory, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let val_high: u8 = (value & 0xF0) >> 4;
        let val_low: u8 = (value & 0xF) << 4;
        let swap: u8 = val_low | val_high;
        mem.write_byte(addr, swap);
        self.r.update_flag(ALUFlag::Z, swap == 0);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, false);
        self.r.update_flag(ALUFlag::C, false);
    }

    fn bit_mem(&mut self, mem: &'a mut Memory, bit: u8, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let comp: bool = ((value >> bit) & 0x1) == 0;
        self.r.update_flag(ALUFlag::Z, comp);
        self.r.update_flag(ALUFlag::N, false);
        self.r.update_flag(ALUFlag::H, true);

    }

    fn res_mem(&mut self, mem: &'a mut Memory, bit: u8, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let shift: u8 = 0xFF - (0x1 << bit);
        let set: u8 = value & shift;
        mem.write_byte(addr, set);

    }

    fn set_mem(&mut self, mem: &'a mut Memory, bit: u8, id: REG) {
        let addr: u16 = self.r.get_word(id);
        let value: u8 = mem.read_byte(addr);
        let shift: u8 = 0x1 << bit;
        let set: u8 = value | shift;
        mem.write_byte(addr, set);
    }

}

