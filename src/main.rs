mod cpu;
mod register;
mod instructions;
mod instructions_cb;

struct MotherBoard {
    cpu: cpu::CPU
}

fn main() {
    let max = std::u16::MAX;
    println!("{:?}", max);
//    cpu::run();
}

pub fn run() {
    let reg: [u8; 4] = [0; 4];
    println!("{:?}",reg);

}

