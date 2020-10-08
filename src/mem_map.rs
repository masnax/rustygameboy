use std::path::PathBuf;
use crate::cartridge::Cartridge;

pub struct Memory {
    header: [u8; 0x150],
    cartridge: Cartridge,
    ram: [u8; 0xFFFF],
}



impl Memory {
    pub fn init(boot_rom: [u8; 0x100], filename: &str) -> Memory {
        let mut header: [u8; 0x150] = [0; 0x150];
        header[..0x100].copy_from_slice(&boot_rom);
        let cartridge: Cartridge = Cartridge::load(PathBuf::from(filename));
        header[0x100..0x150].copy_from_slice(&cartridge.get_header()[..]);
        let ram = [0; 0xFFFF];
        Memory { header, cartridge, ram, }
    }


    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.header[addr as usize] },
            // Cartridge Header Area
            0x0100 ..= 0x014F => { self.header[addr as usize] },
            // Cartridge ROM - Bank 0 (fixed)
            0x0150 ..= 0x3FFF => { self.cartridge.read(addr) },
            // Cartridge ROM - Switchable Banks 1-xx
            0x4000 ..= 0x7FFF => { self.cartridge.read(addr) },
            // Character RAM
            0x8000 ..= 0x97FF => { self.ram[addr as usize] },
            // BG  Data 1
            0x9800 ..= 0x9BFF => { self.ram[addr as usize] },
            // BG  Data 2
            0x9C00 ..= 0x9FFF => { self.ram[addr as usize] },
            // Cartridge RAM (If Available)
            0xA000 ..= 0xBFFF => { self.cartridge.read(addr) },
            // Internal RAM - Bank 0 (fixed)
            0xC000 ..= 0xCFFF => { self.ram[addr as usize] },
            // Internal RAM - Bank 1-7 (switchable - CGB only)
            0xD000 ..= 0xDFFF => { self.ram[addr as usize] },
            // Echo RAM - Reserved, Do Not Use
            0xE000 ..= 0xFDFF => { 0 },
            // OAM - Object Attribute Memory
            0xFE00 ..= 0xFE9F => { self.ram[addr as usize] },
            // Unusable Memory
            0xFEA0 ..= 0xFEFF => { 0 },
            // Hardware I/O Registers
            0xFF00 ..= 0xFF7F => { self.ram[addr as usize] },
            // Zero Page - 127 bytes
            0xFF80 ..= 0xFFFE => { self.ram[addr as usize] },
            // Interrupt Enable Flag
            0xFFFF => { self.ram[addr as usize] }
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.header[addr as usize]; },
            // Cartridge Header Area
            0x0100 ..= 0x014F => { self.header[addr as usize]; },
            // Cartridge ROM - Bank 0 (fixed)
            0x0150 ..= 0x3FFF => { self.cartridge.write(addr, value); },
            // Cartridge ROM - Switchable Banks 1-xx
            0x4000 ..= 0x7FFF => { self.cartridge.write(addr, value); },
            // Character RAM
            0x8000 ..= 0x97FF => { self.ram[addr as usize]; },
            // BG  Data 1
            0x9800 ..= 0x9BFF => { self.ram[addr as usize]; },
            // BG  Data 2
            0x9C00 ..= 0x9FFF => { self.ram[addr as usize]; },
            // Cartridge RAM (If Available)
            0xA000 ..= 0xBFFF => { self.cartridge.write(addr, value); },
            // Internal RAM - Bank 0 (fixed)
            0xC000 ..= 0xCFFF => { self.ram[addr as usize]; },
            // Internal RAM - Bank 1-7 (switchable - CGB only)
            0xD000 ..= 0xDFFF => { self.ram[addr as usize]; },
            // Echo RAM - Reserved, Do Not Use
            0xE000 ..= 0xFDFF => { 0; },
            // OAM - Object Attribute Memory
            0xFE00 ..= 0xFE9F => { self.ram[addr as usize]; },
            // Unusable Memory
            0xFEA0 ..= 0xFEFF => { 0; },
            // Hardware I/O Registers
            0xFF00 ..= 0xFF7F => { self.ram[addr as usize]; },
            // Zero Page - 127 bytes
            0xFF80 ..= 0xFFFE => { self.ram[addr as usize]; },
            // Interrupt Enable Flag
            0xFFFF => { self.ram[addr as usize]; }
        }
    }

}

