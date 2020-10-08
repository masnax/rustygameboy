use std::path::PathBuf;
use crate::cartridge::{MBC, ram::RAM, rtc::RTC};

pub struct MBC3 {
    active_rom_bank: usize,
    rtc_bank_mode: bool,
    ram: RAM,
    rtc: RTC,
    swap: Vec<Vec<u8>>,
    _header_checksum: u8,
}

impl MBC3 {
    pub fn init(filename: PathBuf, data: Vec<u8>, bank_size: u8, ram_size: u8, _header_checksum: u8) -> MBC3 {
        let mut swap: Vec<Vec<u8>> = Vec::with_capacity(bank_size as usize);
        swap.push(data[..0x4000].to_vec());
        swap.push(data[0x4000..0x8000].to_vec());
        for _ in 2..bank_size {
           swap.push(vec![0; 0x4000]);
        }
        let active_rom_bank = 1;
        let rtc_bank_mode = false;
        let ram: RAM = RAM::init(&filename, ram_size);
        let rtc: RTC = RTC::init(&filename);
        MBC3 { active_rom_bank, rtc_bank_mode, ram, rtc, swap, _header_checksum }
    }
}


impl MBC for MBC3 {
    fn read_byte(&self, addr: u16) -> u8 {
        if addr <= 0xBFFF && addr >= 0xA000 {
            if self.rtc_bank_mode {
                self.rtc.read()
            } else {
                self.ram.read(addr)
            }
        }
        else if addr < 0x4000 {
            self.swap[0][addr as usize]
        } else {
            self.swap[self.active_rom_bank][(addr & 0x3FFF) as usize]
        }
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000 ..= 0x1FFF => {
                self.ram.ram_enabled = value == 0xA;
                self.rtc.rtc_enabled = value == 0xA;
            },
            0x2000 ..= 0x3FFF => {
                let bank_number: usize = (value & 0x7F) as usize;
                if bank_number == 0 {
                    self.active_rom_bank = 0x1;
                } else {
                    self.active_rom_bank = bank_number;
                }
            }
            0x4000 ..= 0x5FFF => {
                if value <= 0x3 {
                    self.ram.active_ram_bank = (value & 0x3) as usize;
                } else if value >= 0x8 && value <= 0xC {
                    self.rtc.active_rtc_register = (value & 0x7) as usize;
                }
            },
            0x6000 ..= 0x7FFF => {
                match value {
                    0x0 => { self.rtc.latch_buffer = true; },
                    0x1 => { self.rtc.latch(); },
                    _ => { }
                }
            },
            0xA000 ..= 0xBFFF => {
                if self.rtc_bank_mode {
                    self.rtc.write(value);
                } else {
                    self.ram.write(addr, value);
                }
            },
            _ => { }
        }
    }

    fn get_header(&self) -> Vec<u8> { self.swap[0][..0x150].to_vec() }
}

