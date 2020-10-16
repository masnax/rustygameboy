use std::path::PathBuf;
use crate::cartridge::{MBC, ram::RAM};

pub struct MBC5 {
    active_rom_bank: usize,
    ram: RAM,
    swap: Vec<Vec<u8>>,
    _header_checksum: u8,
}

impl MBC5 {
    pub fn init(filename: PathBuf, data: Vec<u8>, bank_size: u8, ram_size: u8, _header_checksum: u8) -> MBC5 {
        let mut swap: Vec<Vec<u8>> = Vec::with_capacity(bank_size as usize);
        swap.push(data[..0x4000].to_vec());
        swap.push(data[0x4000..0x8000].to_vec());
        for _ in 2..bank_size {
           swap.push(vec![0; 0x4000]);
        }
        let active_rom_bank = 1;
        let ram: RAM = RAM::init(&filename, ram_size);

        MBC5 { active_rom_bank, ram, swap, _header_checksum }
    }
}


impl MBC for MBC5 {
    fn read_byte(&self, addr: u16) -> u8 {
        if addr <= 0xBFFF && addr >= 0xA000 {
            self.ram.read(addr)
        }
        else if addr < 0x4000 {
            self.swap[0][addr as usize]
        } else {
            self.swap[self.active_rom_bank][(addr & 0x3FFF) as usize]
        }
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000 ..= 0x1FFF => { self.ram.ram_enabled = value == 0xA; },
            0x2000 ..= 0x2FFF => { self.active_rom_bank = (self.active_rom_bank & 0x100) | (value as usize); },
            0x3000 ..= 0x3FFF => { self.active_rom_bank = (((value & 0x1) as usize) << 8) | (self.active_rom_bank & 0xFF); },
            0x4000 ..= 0x5FFF => { self.ram.active_ram_bank = (value & 0xF) as usize; },
            _ => { panic!("Boop"); }
        }
    }
}

