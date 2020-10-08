use crate::cartridge::MBC;

pub struct MBC0 {
    data: Vec<u8>,
    bank_size: u8,
    ram_size: u8,
    header_checksum: u8,
}

impl MBC0 {
    pub fn init(data: Vec<u8>, bank_size: u8, ram_size: u8, header_checksum: u8) -> Self {
        MBC0 { data, bank_size, ram_size, header_checksum, }
    }
}


impl MBC for MBC0 {
    fn read_byte(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    // ROM type zero never writes to ROM
    fn write_byte(&mut self, _addr: u16, _value: u8) { }

    fn get_header(&self) -> Vec<u8> { self.data[..0x150].to_vec() }
}
