extern crate minifb;
use minifb::{Window, WindowOptions, Scale};

mod cpu;
mod register;
mod mem_map;
mod tile;
mod cartridge;

//struct MotherBoard {
//    cpu: cpu::CPU
//}

fn main() {
    //let mut c: cpu::CPU<'static> = boot_sequence();
    let register: register::Register = register::Register::init();
    let boot_sequence: Vec<u8> = std::fs::read("boot/boot.bin").expect("Missing Boot ROM");
    let mut mem = mem_map::Memory::init(boot_sequence, "boot/red.gb");
  //  let tile_set = tile::TileSet::init(&mut mem);
    let mut c: cpu::CPU = cpu::CPU::init(register, &mut mem);

    let mut window = Window::new("Maw Boy", 160, 144,
        WindowOptions { scale: Scale::X2, ..Default::default()}
        ).expect("Error Creating Window");

    while window.is_open() {
        c.exec();
        window.update();
    }
//    cpu::run();
}


pub fn run() {
    let reg: [u8; 4] = [0; 4];
    println!("{:?}",reg);

}

