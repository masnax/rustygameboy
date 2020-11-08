use std::path::PathBuf;
use crate::cartridge::Cartridge;
use crate::display::LcdController;
use crate::joypad::Joypad;

pub const TILE_SET_SIZE: usize = 0x1800;
pub const BG_SIZE: usize = 0x800;
pub const SRAM_SIZE: usize = 0xA0;
pub const RAM_SIZE: usize = 0x2000;
const HEADER_SIZE: usize = 0x100;
const ZRAM_SIZE: usize= 0x80;
pub type Header = [u8; HEADER_SIZE];

pub struct Memory {
    pub lcdc: LcdController,
    header: Header,
    cartridge: Cartridge,
    dma: u8,
    tile_ram: [u8; TILE_SET_SIZE],
    bg_ram: [u8; BG_SIZE],
    sprite_ram: [u8; SRAM_SIZE],
    ram: [u8; RAM_SIZE],
    zram: [u8; ZRAM_SIZE],
    boot_mode: bool,
    interrupt_enable: u8,
    interrupt_flag: u8,
    joypad: Joypad,
}



impl Memory {
    pub fn init(boot_rom: Vec<u8>, filename: &str) -> Memory {
        let mut header: Header = [0; HEADER_SIZE];
        header.copy_from_slice(&boot_rom[..HEADER_SIZE]);
        let cartridge: Cartridge = Cartridge::load(PathBuf::from(filename));
        let lcdc: LcdController = LcdController::init();
        let dma: u8 = 0;
        let ram = [0; RAM_SIZE];
        let zram = [0; ZRAM_SIZE];
        let tile_ram = [0; TILE_SET_SIZE];
        let bg_ram = [0; BG_SIZE];
        let sprite_ram = [0; SRAM_SIZE];
        let interrupt_enable = 0;
        let interrupt_flag = 0;
        let boot_mode = true;
        let joypad = Joypad::init();
        Memory { header, cartridge, lcdc, dma, tile_ram, bg_ram, sprite_ram, ram, zram, joypad, interrupt_enable, interrupt_flag, boot_mode, }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.read_boot(addr) },
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
            0xFFFF => { self.interrupt_enable }
        }
    }

    pub fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            // Boot ROM
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { self.write_boot(addr, value); },
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
            0xFFFF => { self.interrupt_enable = value; }
        }
    }

    fn read_boot(&self, addr: u16) -> u8 {
        if self.boot_mode {
            return self.header[addr as usize];
        } else {
            return self.cartridge.read(addr);
        }
    }

    fn write_boot(&mut self, addr: u16, value: u8) {
        if !self.boot_mode {
            self.cartridge.write(addr, value);
        }
    }

    fn read_ram(&self, addr: u16) -> u8 {
        self.ram[(addr & 0x1FFF) as usize]
    }

    fn write_ram(&mut self, addr: u16, value: u8) {
        self.ram[(addr & 0x1FFF) as usize] = value;
    }

    fn read_tile(&self, addr: u16) -> u8 {
        if self.lcdc.vram_locked() {
            return 0;
        }
        self.tile_ram[(addr & 0x1FFF) as usize]
    }

    fn write_tile(&mut self, addr: u16, value: u8) {
        if !self.lcdc.vram_locked() {
            let offset_addr: u16 = addr & 0x1FFF;
            self.tile_ram[offset_addr as usize] = value;
            let tile_addr: u16 = offset_addr & 0xFFFE;
            let byte_1: u8 = self.read_tile(tile_addr);
            let byte_2: u8 = self.read_tile(tile_addr + 1);
            self.lcdc.update_tiles(tile_addr, byte_1, byte_2);
        }
    }

    fn read_bg(&self, addr: u16) -> u8 {
        if self.lcdc.vram_locked() {
            return 0;
        }
            self.bg_ram[(addr & 0x7FF) as usize]
    }

    fn write_bg(&mut self, addr: u16, value: u8) {
        if !self.lcdc.vram_locked() {
            let offset_addr: usize = (addr & 0x7FF) as usize;
            self.bg_ram[offset_addr] = value;
            self.lcdc.map_background(offset_addr, value as usize);
        }
    }

    fn read_sprite(&self, addr: u16) -> u8 {
        if self.lcdc.oam_locked() {
            return 0;
        }
        self.sprite_ram[(addr & 0xFF) as usize]
    }

    fn write_sprite(&mut self, addr: u16, value: u8) {
        if !self.lcdc.oam_locked() {
            let offset_addr: usize = (addr & 0xFF) as usize;
            self.sprite_ram[offset_addr] = value;
            let start_addr: usize = offset_addr & 0xFC;
            let byte_1: u8 = self.sprite_ram[start_addr];
            let byte_2: u8 = self.sprite_ram[start_addr + 1];
            let byte_3: u8 = self.sprite_ram[start_addr + 2];
            let byte_4: u8 = self.sprite_ram[start_addr + 3];
            let order: usize = (addr & 0xFFFC) as usize;
            self.lcdc.map_sprite(order, byte_1, byte_2, byte_3, byte_4);
        }
    }

    fn read_io(&self, addr: u16) -> u8 {
        match addr {
            0xFF00 => { self.joypad.read() },
            0xFF01 ..= 0xFF02 => { 0 }, // Serial Transfer
            0xFF03 => { 0 }, // Unused
            0xFF04 ..= 0xFF07 => { 0 }, //timer
            0xFF08 ..= 0xFF0E => { 0 }, // Unused
            0xFF0F => { self.interrupt_flag },
            0xFF10 ..= 0xFF3F => { 0 }, //sound
            0xFF40 ..= 0xFF45 => { self.lcdc.read(addr) },
            0xFF46 => { self.dma },
            0xFF47 ..= 0xFF4B => { self.lcdc.read(addr) },
            0xFF4C ..= 0xFF4F => { 0 }, // Unused
            0xFF50 => { self.boot_mode as u8 },
            0xFF51 ..= 0xFF7F => { 0 }, // Unused
            _ => { 0 }
        }
    }

    fn write_io(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF00 => { self.joypad.write(value); },
            0xFF0F => { self.interrupt_flag = value; },
            0xFF46 => { self.dma_transfer(value); }
            0xFF40 ..= 0xFF4B => { self.lcdc.write(addr, value) },
            0xFF50 => { self.exit_boot_mode(); },
            _ => { }
        }
    }

    fn exit_boot_mode(&mut self) {
        self.boot_mode = false;
    }

    fn dma_transfer(&mut self, value: u8) {
        self.dma = value;
        let dma_start: u16 = (self.dma as u16) << 8;

        for i in 0..0x9F {
            let src_addr: u16 = dma_start | i;
            let dst_addr: u16 = 0xFE00 | i;
            let value: u8 = self.read_byte(src_addr);
            self.write_byte(dst_addr, value);
        }
    }

    pub fn reset_interrupt(&mut self, interrupt: u8) {
        if self.interrupt_flag & interrupt == interrupt {
            self.interrupt_flag -= interrupt;
        }
    }

    pub fn set_interrupt(&mut self, interrupt: u8) {
        self.interrupt_flag |= interrupt;
    }

    pub fn get_interrupt_info(&self) -> (u8, u8) {
        return (self.interrupt_flag, self.interrupt_enable);
    }
}

