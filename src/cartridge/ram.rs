use std::path::PathBuf;
pub struct RAM {
    pub ram_enabled: bool,
    pub active_ram_bank: usize,
    pub ram_size: u8,
    swap: Vec<Vec<u8>>,
}

impl RAM {
    pub fn init(filename: &PathBuf, ram_size: u8) -> RAM {
        let ram_enabled: bool = false;
        let active_ram_bank: usize = 0;
        let swap: Vec<Vec<u8>> = vec![vec![0; 0x2000]; ram_size as usize];
        let mut ram:RAM = RAM { ram_enabled, active_ram_bank, ram_size, swap };
        ram.load_from_save(filename, ram_size);
        ram
    }

    fn load_from_save(&mut self, filename: &PathBuf, ram_size: u8) {
        let buf: Vec<u8> = std::fs::read(filename).expect("Canned Error");
        for offset in 0..ram_size {
            let start_index: usize = (offset as u16 * 0x2000) as usize;
            let end_index: usize = ((offset + 1) as u16 * 0x2000) as usize;
            self.swap[offset as usize] = buf[start_index..end_index].to_vec();
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        if self.ram_enabled{
            self.swap[self.active_ram_bank][(addr & 0x1FFF) as usize]
        } else {
            0x0
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        if self.ram_enabled {
            self.swap[self.active_ram_bank][(addr & 0x1FFF) as usize] = value;
        }
    }
}
