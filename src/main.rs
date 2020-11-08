extern crate minifb;
use minifb::{Window, WindowOptions, Scale};

mod cpu;
mod register;
mod memory;
mod display;
mod cartridge;
mod joypad;

//struct MotherBoard {
//    cpu: cpu::Cpu
//}
//
const HEIGHT: usize = 144;
const  WIDTH: usize = 160;

fn main() {
    //let mut c: cpu::Cpu<'static> = boot_sequence();
    let register: register::Register = register::Register::init();
    let boot_sequence: Vec<u8> = std::fs::read("boot/boot.bin").expect("Missing Boot ROM");
    let mut mem = memory::Memory::init(boot_sequence, &std::env::args().nth(1).unwrap());
  //  let tile_set = tile::TileSet::init(&mut mem);
    let mut c: cpu::Cpu = cpu::Cpu::init(register, &mut mem);

    let mut window = Window::new("Maw Boy", WIDTH, HEIGHT,
        WindowOptions { scale: Scale::X2, ..Default::default()}
        ).expect("Error Creating Window");

    let sleep_time = std::time::Duration::from_millis(16);
    window.limit_update_rate(Some(sleep_time));
    while window.is_open() {
        match c.cycle() {
            Some(frame) => {
                let _ = window.update_with_buffer(frame, WIDTH, HEIGHT);
            },
            None => { },
        };
    }
//    cpu::run();
}

pub fn run() {
    let reg: [u8; 4] = [0; 4];
    println!("WOOOOOO {:?}",reg);

}

