mod cpu;
mod register;
mod instructions;
mod cb_instructions;
mod mem_map;
mod cartridge;

//struct MotherBoard {
//    cpu: cpu::CPU
//}

fn main() {
    let max = std::u16::MAX;
    println!("{:?}", max);
    let mut c: cpu::CPU = boot_sequence();
    c.exec();
    c.exec_cb();
//    cpu::run();
}

fn boot_sequence() -> cpu::CPU {
    let register: register::Register = register::Register::init();
    let mem = mem_map::Memory::init([0;0x100], "some file");
    return cpu::CPU::init(register, mem);
}

pub fn run() {
    let reg: [u8; 4] = [0; 4];
    println!("{:?}",reg);

}

