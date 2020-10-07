pub struct RAM {
    pub ram_enabled: bool,
    pub active_ram_bank: usize,
    pub ram_size: u8,
    swap: Vec<Vec<u8>>,
}

impl RAM {
    pub fn init(ram_size: u8) -> RAM {
        let ram_enabled: bool = false;
        let active_ram_bank: usize = 0;
        let mut swap: Vec<Vec<u8>> = vec![vec![0; 0x2000]; ram_size as usize];
        RAM { ram_enabled, active_ram_bank, ram_size, swap }
    }

    fn load_from_save() -> Vec<Vec<u8>> {
        // TODO
        vec![vec![5]]
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
