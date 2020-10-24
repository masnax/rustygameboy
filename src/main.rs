extern crate minifb;
use minifb::{Window, WindowOptions, Scale};

mod cpu;
mod register;
mod memory;
mod display;
mod cartridge;

//struct MotherBoard {
//    cpu: cpu::Cpu
//}

fn main() {
    //let mut c: cpu::Cpu<'static> = boot_sequence();
    let register: register::Register = register::Register::init();
    let boot_sequence: Vec<u8> = std::fs::read("boot/boot.bin").expect("Missing Boot ROM");
    let mut mem = memory::Memory::init(boot_sequence, "boot/red.gb");
  //  let tile_set = tile::TileSet::init(&mut mem);
    let mut c: cpu::Cpu = cpu::Cpu::init(register, &mut mem);

    let mut window = Window::new("Maw Boy", 160, 144,
        WindowOptions { scale: Scale::X2, ..Default::default()}
        ).expect("Error Creating Window");

    let mut cycles: u16 = 0;
    while window.is_open() {
        cycles = c.cycle(cycles);
        let frame: &[u32] = c.get_frame();
        let _ = window.update_with_buffer(frame, 256, 256);
    }
//    cpu::run();
}

pub fn run() {
    let reg: [u8; 4] = [0; 4];
    println!("WOOOOOO {:?}",reg);

}

