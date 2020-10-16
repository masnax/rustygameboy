use std::path::PathBuf;
pub mod ram;
pub mod rtc;
pub mod mbc_0;
pub mod mbc_1;
pub mod mbc_2;
pub mod mbc_3;
pub mod mbc_5;


pub struct Cartridge {
    mbc: Box<dyn MBC>,
}


impl Cartridge {
    pub fn load(filename: PathBuf) -> Cartridge {
        let buf: Vec<u8> = std::fs::read(&filename).expect("MAGIC!!");
        let mbc_type_flag: u8 = buf[0x147];
        let bank_size_flag: u8 = buf[0x148];
        let ram_size_flag: u8 = buf[0x149];
        let header_checksum: u8 = buf[0x14D];
        let bank_size: u8;
        let ram_size: u8;
        let save_file: PathBuf = filename.with_extension("gbsave");

        match bank_size_flag {
            0x00 => { bank_size = 0; }, // No ROM
            0x01 => { bank_size = 4; },
            0x02 => { bank_size = 8; },
            0x03 => { bank_size = 16; },
            0x04 => { bank_size = 32; },
            0x05 => { bank_size = 64; },
            0x06 => { bank_size = 128; },
            0x07 => { bank_size = 255; }, // as usize
            0x52 => { bank_size = 72; },
            0x53 => { bank_size = 80; },
            0x54 => { bank_size = 96; },
            _ => { panic!("hi");}
        }
        match ram_size_flag {
            0x00 => { ram_size = 0; }, // No RAM
            0x01 => { ram_size = 1; }, // 1 Bank,  2KB
            0x02 => { ram_size = 1; }, // 1 Bank,  8KB
            0x03 => { ram_size = 4; }, // 4 Banks, 32KB
            _ => { panic!("hi");}
        }

        match mbc_type_flag {
            0x00 => { Cartridge {
                mbc: Box::new(mbc_0::MBC0::init(buf, header_checksum))
            } },
            0x01 ..= 0x03 => { Cartridge {
                mbc: Box::new(mbc_1::MBC1::init(save_file, buf, bank_size, ram_size, header_checksum))
            } },
            0x05 ..= 0x06 => { Cartridge {
                mbc: Box::new(mbc_2::MBC2::init(save_file, buf, bank_size, header_checksum))
            } },
            0x0F ..= 0x13 => { Cartridge {
                mbc: Box::new(mbc_3::MBC3::init(save_file, buf, bank_size, ram_size, header_checksum))
            } },
            0x19 ..= 0x1E => { Cartridge {
                mbc: Box::new(mbc_5::MBC5::init(save_file, buf, bank_size, ram_size, header_checksum))
            } },
            _ => { panic!("hi");}
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.mbc.read_byte(addr)
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.mbc.write_byte(addr, value);
    }
}


pub trait MBC {
    fn read_byte(&self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, value: u8);
}
