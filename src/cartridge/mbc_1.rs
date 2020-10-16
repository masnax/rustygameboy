use std::path::PathBuf;
use crate::cartridge::{MBC, ram::RAM};

pub struct MBC1 {
    active_rom_bank: usize,
    ram_bank_mode: bool,
    ram: RAM,
    swap: Vec<Vec<u8>>,
    _header_checksum: u8,
}

impl MBC1 {
    pub fn init(filename: PathBuf, data: Vec<u8>, bank_size: u8, ram_size: u8, _header_checksum: u8) -> MBC1 {
        let mut swap: Vec<Vec<u8>> = Vec::with_capacity(bank_size as usize);
        swap.push(data[..0x4000].to_vec());
        swap.push(data[0x4000..0x8000].to_vec());
        for _ in 2..bank_size {
           swap.push(vec![0; 0x4000]);
        }
        let active_rom_bank = 1;
        let ram_bank_mode = false;
        let ram: RAM = RAM::init(&filename, ram_size);

        MBC1 { active_rom_bank, ram_bank_mode, ram, swap, _header_checksum }
    }
}

impl MBC for MBC1 {
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
            0x2000 ..= 0x3FFF => {
                let low_bits: usize = (value & 0x1F) as usize;
                if low_bits == 0 {
                    self.active_rom_bank = (self.active_rom_bank & 0x60) | 0x1;
                } else {
                    self.active_rom_bank = (self.active_rom_bank & 0x60) | low_bits;
                }
            }
            0x4000 ..= 0x5FFF => {
                if self.ram_bank_mode {
                    self.ram.active_ram_bank = (value & 0x3) as usize;
                } else {
                    self.active_rom_bank = (((value & 0x3) << 5) as usize) | (self.active_rom_bank & 0x1F);
                }
            },
            0x6000 ..= 0x7FFF => { self.ram_bank_mode = (value & 0x1) == 1; },
            0xA000 ..= 0xBFFF => { self.ram.write(addr, value); },
            _ => { }
        }
    }
}

