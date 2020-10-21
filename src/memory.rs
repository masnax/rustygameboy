use std::path::PathBuf;
use crate::cartridge::Cartridge;

const TILE_SET_SIZE: usize = 0x1800;
const BG_SIZE: usize = 0x800;
const SRAM_SIZE: usize = 0xA0;
const RAM_SIZE: usize = 0x2000;
const HEADER_SIZE: usize = 0x100;
const ZRAM_SIZE: usize= 0x80;
pub type Header = [u8; HEADER_SIZE];

pub struct Memory {
    header: Header,
    cartridge: Cartridge,
    tile_ram: [u8; TILE_SET_SIZE],
    bg_ram: [u8; BG_SIZE],
    sprite_ram: [u8; SRAM_SIZE],
    ram: [u8; RAM_SIZE],
    zram: [u8; ZRAM_SIZE],
    io_ram: [u8; 0x80],
    interrupt: u8,
}



impl Memory {
    pub fn init(boot_rom: Vec<u8>, filename: &str) -> Memory {
        let mut header: Header = [0; HEADER_SIZE];
        header.copy_from_slice(&boot_rom[..HEADER_SIZE]);
        let cartridge: Cartridge = Cartridge::load(PathBuf::from(filename));
        let ram = [0; RAM_SIZE];
        let zram = [0; ZRAM_SIZE];
        let tile_ram = [0; TILE_SET_SIZE];
        let bg_ram = [0; BG_SIZE];
        let sprite_ram = [0; SRAM_SIZE];
        let io_ram = [0; 0x80];
        let interrupt = 0;
        Memory { header, cartridge, tile_ram, bg_ram, sprite_ram, ram, zram, io_ram, interrupt, }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.header[addr as usize] },
            // Cartridge Header Area
            0x0100 ..= 0x014F => { self.cartridge.read(addr) },
            // Cartridge ROM - Bank 0 (fixed)
            0x0150 ..= 0x3FFF => { self.cartridge.read(addr) },
            // Cartridge ROM - Switchable Banks 1-xx
            0x4000 ..= 0x7FFF => { self.cartridge.read(addr) },
            // Character RAM
            0x8000 ..= 0x97FF => { self.read_tile(addr) },
            // BG  Data 1
            0x9800 ..= 0x9BFF => { self.read_bg(addr) },
            // BG  Data 2
            0x9C00 ..= 0x9FFF => { self.read_bg(addr) },
            // Cartridge RAM (If Available)
            0xA000 ..= 0xBFFF => { self.cartridge.read(addr) },
            // Internal RAM - Bank 0 (fixed)
            0xC000 ..= 0xCFFF => { self.read_ram(addr) },
            // Internal RAM - Bank 1-7 (switchable - CGB only)
            0xD000 ..= 0xDFFF => { self.read_ram(addr) },
            // Echo RAM - Reserved, Do Not Use
            0xE000 ..= 0xFDFF => { 0 },
            // OAM - Object Attribute Memory
            0xFE00 ..= 0xFE9F => { self.read_sprite(addr) },
            // Unusable Memory
            0xFEA0 ..= 0xFEFF => { 0 },
            // Hardware I/O Registers
            0xFF00 ..= 0xFF7F => { self.read_io(addr) },
            // Zero Page - 127 bytes
            0xFF80 ..= 0xFFFE => { self.zram[(addr & 0x7F) as usize] },
            // Interrupt Enable Flag
            0xFFFF => { self.interrupt }
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.header[addr as usize] = value; },
            // Cartridge Header Area
            0x0100 ..= 0x014F => { self.cartridge.write(addr, value); },
            // Cartridge ROM - Bank 0 (fixed)
            0x0150 ..= 0x3FFF => { self.cartridge.write(addr, value); },
            // Cartridge ROM - Switchable Banks 1-xx
            0x4000 ..= 0x7FFF => { self.cartridge.write(addr, value); },
            // Character RAM
            0x8000 ..= 0x97FF => { self.write_tile(addr, value); },
            // BG  Data 1
            0x9800 ..= 0x9BFF => { self.write_bg(addr, value); },
            // BG  Data 2
            0x9C00 ..= 0x9FFF => { self.write_bg(addr, value); },
            // Cartridge RAM (If Available)
            0xA000 ..= 0xBFFF => { self.cartridge.write(addr, value); },
            // Internal RAM - Bank 0 (fixed)
            0xC000 ..= 0xCFFF => { self.write_ram(addr, value); },
            // Internal RAM - Bank 1-7 (switchable - CGB only)
            0xD000 ..= 0xDFFF => { self.write_ram(addr, value); },
            // Echo RAM - Reserved, Do Not Use
            0xE000 ..= 0xFDFF => { },
            // OAM - Object Attribute Memory
            0xFE00 ..= 0xFE9F => { self.write_sprite(addr, value); },
            // Unusable Memory
            0xFEA0 ..= 0xFEFF => { },
            // Hardware I/O Registers
            0xFF00 ..= 0xFF7F => { self.write_io(addr, value); },
            // Zero Page - 127 bytes
            0xFF80 ..= 0xFFFE => { self.zram[(addr & 0x7F) as usize] = value; },
            // Interrupt Enable Flag
            0xFFFF => { self.interrupt = value; }
        }
    }
    
    fn read_ram(&self, addr: u16) -> u8 {
        self.ram[(addr & 0x1FFF) as usize]
    }

    fn write_ram(&mut self, addr: u16, value: u8) {
        self.ram[(addr & 0x1FFF) as usize] = value;
    }
 
    fn read_tile(&self, addr: u16) -> u8 {
        self.tile_ram[(addr & 0x1FFF) as usize]
    }

    fn write_tile(&mut self, addr: u16, value: u8) {
        self.tile_ram[(addr & 0x1FFF) as usize] = value;
    }
    
    fn read_bg(&self, addr: u16) -> u8 {
        self.bg_ram[(addr & 0x7FF) as usize]
    }

    fn write_bg(&mut self, addr: u16, value: u8) {
        self.bg_ram[(addr & 0x7FF) as usize] = value;
    }

    fn read_sprite(&self, addr: u16) -> u8 {
        self.sprite_ram[(addr & 0xFF) as usize]
    }

    fn write_sprite(&mut self, addr: u16, value: u8) {
        self.sprite_ram[(addr & 0xFF) as usize] = value;
    }
    
    fn read_io(&self, addr: u16) -> u8 {
        self.io_ram[(addr & 0x7F) as usize]
    }

    fn write_io(&mut self, addr: u16, value: u8) {
        if addr == 0xFF44 {
            if value < 0x9A {
                self.io_ram[(addr & 0x7F) as usize] = value;
            } else {
                self.io_ram[(addr & 0x7F) as usize] = 0;
            }
        } else {
            self.io_ram[(addr & 0x7F) as usize] = value;
        }
    }

    pub fn get_frame_info(&self) -> ([u8; 0x800], usize, [u8; 0x1800], u8) {
        return (self.bg_ram, 0, self.tile_ram, self.read_io(0xFF47));
    }
}

