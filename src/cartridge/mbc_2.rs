use std::path::PathBuf;
use crate::cartridge::{MBC, ram::RAM};

pub struct MBC2 {
    active_rom_bank: usize,
    ram: RAM,
    swap: Vec<Vec<u8>>,
    _header_checksum: u8,
}

impl MBC2 {
    pub fn init(filename: PathBuf, data: Vec<u8>, bank_size: u8, _header_checksum: u8) -> MBC2 {
        let mut swap: Vec<Vec<u8>> = Vec::with_capacity(bank_size as usize);
        if bank_size < 2 {
            swap.push(data[..0x4000].to_vec());
            swap.push(data[0x4000..0x8000].to_vec());
        } else {
            for i in 0..bank_size {
                let lo: usize = (i as usize) * 0x4000;
                let hi: usize = ((i as usize)+1) * 0x4000;
                if data.len() >= hi {
                    swap.push(data[lo..hi].to_vec());
                } else {
                    swap.push(vec![0; 0x4000]);
                }
            }
        }
        let active_rom_bank = 1;
        let ram: RAM = RAM::init(&filename, 1);

        MBC2 { active_rom_bank, ram, swap, _header_checksum }
    }
}


impl MBC for MBC2 {
    fn read_byte(&self, addr: u16) -> u8 {
        if addr <= 0xA1FF && addr >= 0xA000 {
            self.ram.read(addr) & 0xF
        }
        else if addr < 0x4000 {
            self.swap[0][addr as usize]
        } else {
            self.swap[self.active_rom_bank][addr as usize]
        }
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000 ..= 0x1FFF => {
                if addr & 0x100 == 0 {
                    self.ram.ram_enabled = !self.ram.ram_enabled;
                }
            },
            0x2000 ..= 0x3FFF => {
                if addr & 0x100 == 1 {
                    let bank: u8 = value & 0xF;
                    if bank == 0 {
                        self.active_rom_bank = 1;
                    } else {
                        self.active_rom_bank = bank as usize;
                    }
                }
            }
            0xA000 ..= 0xA1FF => { self.ram.write(addr, value & 0xF); },
            _ => { }
        }
    }

    fn get_ram(&self) -> Option<Vec<u8>> {
        return self.ram.get_ram();
    }
}

