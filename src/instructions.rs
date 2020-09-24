use crate::register::Register;
use crate::register::ALUFlag;

pub trait Instruction {
    // Base register access
    fn get_flag(&self, flag: ALUFlag) -> bool;
    fn update_flag(&mut self, flag: ALUFlag, value:bool);

    // Arithmetic
    fn inc(&mut self, id: char);
    fn dec(&mut self, id: char);
    fn incMem(&mut self);
    fn decMem(&mut self);
    fn addW(&mut self, id: &str, val: u16);
    fn addB(&mut self, id: char, val: u8);
    fn addSP(&mut self, val: i8);
    fn adc(&mut self, val: u8);
    fn subW(&mut self, id: &str, val: u16);
    fn subB(&mut self, id: char, val: u8);
    fn sbc(&mut self, val: u8);
    fn daa(&mut self);
    fn cpl(&mut self);

    fn and(&mut self, val: u8);
    fn xor(&mut self, val: u8);
    fn or(&mut self, val: u8);
    fn cp(&mut self, val: u8);

    fn rlc(&mut self, id: char);
    fn rl(&mut self, id: char);
    fn rrc(&mut self, id: char);
    fn rr(&mut self, id: char);

    // Moves
    fn jr(&mut self, steps: i8, flag: bool) -> u8;
    fn jp(&mut self, value: u16, flag: bool) -> u8;
    fn call(&mut self, value: u16, flag: bool) -> u8;
    fn rst(&mut self, value: u8);
    fn ret(&mut self, flag: bool) -> u8;

    // Flags
    fn scf(&mut self);
    fn ccf(&mut self);

    // Stack
    fn push(&mut self, id: &str);
    fn pop(&mut self, id: &str);

    // Interrupts
    fn reti(&mut self);
    fn di(&mut self);
    fn ei(&mut self);

    // Load HL
    fn ldInc(&mut self);
    fn ldDec(&mut self);
    fn ldMemInc(&mut self);
    fn ldMemDec(&mut self);
}


impl Instruction for Register {
    fn inc(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let add: u8 = value.wrapping_add(1);
        match id {
            'A' => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word("AF") & 0xF0);
                self.set_word("AF", inc);
            },
            'B' => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word("BC") & 0xFF);
                self.set_word("BC", inc);
            },
            'C' => {
                let inc: u16 = (self.get_word("BC") & 0xFF00) | (add as u16);
                self.set_word("BC", inc);
            },
            'D' => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word("DE") & 0xFF);
                self.set_word("DE", inc);
            },
            'E' => {
                let inc: u16 = (self.get_word("DE") & 0xFF00) | (add as u16);
                self.set_word("DE", inc);
            },
            'H' => {
                let inc: u16 = ((add as u16) << 8) | (self.get_word("BC") & 0xFF);
                self.set_word("BC", inc);
            },
            'L' => {
                let inc: u16 = (self.get_word("HL") & 0xFF00) | (add as u16);
                self.set_word("HL", inc);
            },
            _other => {
                panic!("[ERROR] invalid register");

            }
        }
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    fn dec(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let sub: u8 = value.wrapping_sub(1);
        match id {
            'A' => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word("AF") & 0xF0);
                self.set_word("AF", inc);
            },
            'B' => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word("BC") & 0xFF);
                self.set_word("BC", inc);
            },
            'C' => {
                let inc: u16 = (self.get_word("BC") & 0xFF00) | (sub as u16);
                self.set_word("BC", inc);
            },
            'D' => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word("DE") & 0xFF);
                self.set_word("DE", inc);
            },
            'E' => {
                let inc: u16 = (self.get_word("DE") & 0xFF00) | (sub as u16);
                self.set_word("DE", inc);
            },
            'H' => {
                let inc: u16 = ((sub as u16) << 8) | (self.get_word("BC") & 0xFF);
                self.set_word("BC", inc);
            },
            'L' => {
                let inc: u16 = (self.get_word("HL") & 0xFF00) | (sub as u16);
                self.set_word("HL", inc);
            },
            _other => {
                panic!("[ERROR] invalid register");

            }
        }
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    fn incMem(&mut self) {
        let addr: u16 = self.get_word("HL");
        let value: u8 = self.read_byte(addr);
        let add: u8 = value.wrapping_add(1);
        self.write_byte(addr, add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (value & 0xF) + 1 > 0xF);
    }

    fn decMem(&mut self) {
        let addr: u16 = self.get_word("HL");
        let value: u8 = self.read_byte(addr);
        let sub: u8 = value.wrapping_sub(1);
        self.write_byte(addr, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (value & 0xF) == 0);
    }

    fn addW(&mut self, id: &str, value: u16) {
        let previous: u16 = self.get_word(id);
        let add: u16 = previous.wrapping_add(value);
        self.set_word(id, add);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (previous & 0xFF) + (value & 0xFF) > 0xFF);
        self.update_flag(ALUFlag::C, previous > 0xFFFF - value);
    }

    fn addB(&mut self, id: char, value: u8) {
        let previous: u8 = self.get_byte(id);
        let add: u8 = previous.wrapping_add(value);
        self.set_byte(id, add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (previous & 0xF) + (value & 0xF) > 0xF);
        self.update_flag(ALUFlag::C, previous > 0xFF - value);
    }

    fn addSP(&mut self, value: i8) {
        let SP: u16 = self.get_word("SP");
        let bigValue: u16 = value as u16;
        let add: u16 = SP.wrapping_add(bigValue);
        self.set_word("SP", add);
        self.update_flag(ALUFlag::Z, false);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (SP & 0xFF) + (bigValue & 0xFF) > 0xFF);
        self.update_flag(ALUFlag::C, SP > 0xFFFF - bigValue);
    }

    fn adc(&mut self, value: u8) {
        let A: u8 = self.get_byte('A');
        let C: u8 = self.get_flag(ALUFlag::C) as u8;
        let add: u8 = A.wrapping_add(value).wrapping_add(C);
        self.set_byte('A', add);
        self.update_flag(ALUFlag::Z, add == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, (A & 0xF) + (value & 0xF) + C > 0xFF);
        self.update_flag(ALUFlag::C, (A as u16) > (0xFF as u16) - (value as u16) - (C as u16));


    }

    fn subW(&mut self, id: &str, value: u16) {
        let previous: u16 = self.get_word(id);
        let sub: u16 = previous.wrapping_sub(value);
        self.set_word(id, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (previous & 0xFF) < (value & 0xFF));
        self.update_flag(ALUFlag::C, previous < value);
    }

    fn subB(&mut self, id: char, value: u8) {
        let previous: u8 = self.get_byte(id);
        let sub: u8 = previous.wrapping_sub(value);
        self.set_byte(id, sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (previous & 0xF) < (value & 0xF));
        self.update_flag(ALUFlag::C, previous < value);
    }

    fn sbc(&mut self, value: u8) {
        let A: u8 = self.get_byte('A');
        let C: u8 = self.get_flag(ALUFlag::C) as u8;
        let sub: u8 = A.wrapping_sub(value).wrapping_sub(C);
        self.set_byte('A', sub);
        self.update_flag(ALUFlag::Z, sub == 0);
        self.update_flag(ALUFlag::N, true);
        self.update_flag(ALUFlag::H, (A & 0xF) < (value & 0xF) + C);
        self.update_flag(ALUFlag::C, (A as u16) < (value as u16) + (C as u16));
    }

    fn rlc(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (carry as u8);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rl(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x80) > 0;
        let rotate: u8 = (value << 1) | (self.get_flag(ALUFlag::C) as u8);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rrc(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((carry as u8) << 7);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn rr(&mut self, id: char) {
        let value: u8 = self.get_byte(id);
        let carry: bool = (value & 0x1) > 0;
        let rotate: u8 = (value >> 1) | ((self.get_flag(ALUFlag::C) as u8) << 7);
        self.set_byte(id, rotate);
        self.update_flag(ALUFlag::Z, rotate == 0);
        self.update_flag(ALUFlag::N, false);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::C, carry);
    }

    fn jr(&mut self, steps: i8, flag: bool) -> u8 {
        if flag {
            self.set_word("PC", ((self.get_word("PC") as i32) + (steps as i32)) as u16);
            3
        } else {
            // update PC
            2
        }
    }

    fn daa(&mut self) {
        let A = self.get_byte('A');
        let C = self.get_flag(ALUFlag::C);
        let H = self.get_flag(ALUFlag::H);
        let N = self.get_flag(ALUFlag::N);
        let mut bcd = 0;
        if C { bcd |= 0x60; }
        if H { bcd |= 0x06; }
        if N {
            self.set_byte('A', A.wrapping_sub(bcd));
        } else {
            if A & 0xF > 0x9 { bcd |= 0x6; }
            if A > 0x99 { bcd |= 0x60; }
            self.set_byte('A', A.wrapping_add(bcd));
        }
        self.update_flag(ALUFlag::C, bcd >= 0x60);
        self.update_flag(ALUFlag::H, false);
        self.update_flag(ALUFlag::Z, self.get_byte('A') == 0);
    }

    fn cpl(&mut self) {
        self.set_byte('A', 0xFF ^ self.get_byte('A'));
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
        println!("{}", val);
    }

    fn xor(&mut self, val: u8) {
        println!("{}", val);

    }
    fn or(&mut self, val: u8) {
        println!("{}", val);

    }
    fn cp(&mut self, val: u8) {
        println!("{}", val);

    }

    fn jp(&mut self, value: u16, flag: bool) -> u8 { println!("{} {}", value, flag); 0 }
    fn call(&mut self, value: u16, flag: bool) -> u8 { println!("{} {}", value, flag); 0 }
    fn rst(&mut self, value: u8) {         println!("{}", value); }
    fn ret(&mut self, flag: bool) -> u8 { println!("{}", flag); 0 }
    fn push(&mut self, id: &str) { println!("{}", id); }
    fn pop(&mut self, id: &str) { println!("{}", id);}
    fn reti(&mut self) {}
    fn ei(&mut self) {}
    fn di(&mut self) {}

    fn ldInc(&mut self) {
        let HL: u16 = self.get_word("HL");
        let memHL: u8 = self.read_byte(HL);
        self.set_byte('A', memHL);
        self.set_word("HL", HL.wrapping_add(1));
    }
    fn ldDec(&mut self ) {
        let HL: u16 = self.get_word("HL");
        let memHL: u8 = self.read_byte(HL);
        self.set_byte('A', memHL);
        self.set_word("HL", HL.wrapping_sub(1));
    }

    fn ldMemInc(&mut self) {
        let HL: u16 = self.get_word("HL");
        let A: u8 = self.get_byte('A');
        self.write_byte(HL, A);
        self.set_word("HL", HL.wrapping_add(1));
    }

    fn ldMemDec(&mut self) {
        let HL: u16 = self.get_word("HL");
        let A: u8 = self.get_byte('A');
        self.write_byte(HL, A);
        self.set_word("HL", HL.wrapping_sub(1));
    }

    fn get_flag(&self, flag: ALUFlag) -> bool {
        let F = self.get_byte('F');
        let flagValue = flag as u8;
        F & flagValue == flagValue
    }

    fn update_flag(&mut self, flag: ALUFlag, switch: bool) {
        let flagValue = flag as u8;
        let F = self.get_byte('F') ^ flagValue;
        self.set_byte('F', F | (flagValue * switch as u8));
    }


    // load from SP to u8 and u8+1 represented as u16
//    fn LD_RR(r1: u8, r2: u8) {
//
//    }
//
//    fn LD_RN(r: u8, n: u16) {
//
//    }
//
//    fn ln_rm() {}
//
//    // weird, use carry for loops
//    fn jmp() {
//
//    }
//
//    //ret addrs pushed to stack with CALL
//    fn call() {}
//
//    //ret addrs popped from stack with RET
//    fn ret() {}

}


    //C   Reg
    //0   0111 1011
    //     ___>___|
    //    |
    //1---1011 1101
