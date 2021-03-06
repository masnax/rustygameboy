use crate::cartridge::MBC;

pub struct MBC0 {
    data: Vec<u8>,
    _header_checksum: u8,
}

impl MBC0 {
    pub fn init(data: Vec<u8>, _header_checksum: u8) -> Self {
        MBC0 { data, _header_checksum, }
    }
}


impl MBC for MBC0 {
    fn read_byte(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    // ROM type zero never writes to ROM
    fn write_byte(&mut self, _addr: u16, _value: u8) { }

    fn get_ram(&self) -> Option<Vec<u8>> {
        return None;
    }
}
