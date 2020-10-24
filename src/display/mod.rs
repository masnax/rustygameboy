pub mod tile;
use tile::*;

const FRAME_FREQ: u16 = 0x1C8;
const LY_MAX: u8 = 0x9B;

pub struct LcdController {
    tile_set: TileSet,
    pub background: Vec<u32>,
    bg_map: Vec<u8>,

    display_enable: bool,
    window_bank_offset: u16,
    window_enable: bool,
    tile_bank_offset: u16,
    bg_bank_offset: u16,
    sprite_size: u8,
    sprite_enable: bool,
    blank: bool,

    coincidence_interrupt_enable: bool,
    oam_interrupt: bool,
    vblank_interrupt: bool,
    hblank_interrupt: bool,
    coincidence_interrupt_mode: bool,
    lcd_mode: u8,

    scy: u8,
    scx: u8,
    ly: u8,
    lyc: u8,
    bg_palette: u8,
    object_palette_0: u8,
    object_palette_1: u8,
    wy: u8,
    wx: u8,
}


impl<'a> LcdController {
    pub fn init() -> LcdController {
        LcdController {
            tile_set: TileSet::init(),
            background: vec![pixel_to_u32(WHITE); 0x10000],
            bg_map: vec![0;0x400],

            display_enable: false,
            window_bank_offset: 0,
            window_enable: false,
            tile_bank_offset: 0,
            bg_bank_offset: 0,
            sprite_size: 0,
            sprite_enable: false,
            blank: false,

            coincidence_interrupt_enable: false,
            oam_interrupt: false,
            vblank_interrupt: false,
            hblank_interrupt: false,
            coincidence_interrupt_mode: false,
            lcd_mode: 0,

            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bg_palette: 0,
            object_palette_0: 0,
            object_palette_1: 0,
            wy: 0,
            wx: 0,
        }
    }

    pub fn cycle(&mut self, cycles: u16) -> u16 {
        if cycles >= FRAME_FREQ - 1 {
            self.lcd_mode = (self.lcd_mode + 1) % 4;
            self.ly = (self.ly + 1) % LY_MAX;
        }
        return cycles % FRAME_FREQ;
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF40 => {
                ((self.display_enable as u8) << 7) |
                    (((self.window_bank_offset / 0x400) as u8) << 6) |
                    ((self.window_enable as u8) << 5) |
                    (((self.tile_bank_offset / 0x800) as u8) << 4) |
                    (((self.bg_bank_offset / 0x400) as u8) << 3) |
                    ((self.sprite_size / 0xA) << 2) |
                    ((self.sprite_enable as u8) << 1) |
                    (self.blank as u8)
            },
            0xFF41 => {
                ((self.coincidence_interrupt_enable as u8) << 6) |
                    ((self.oam_interrupt as u8) << 5) |
                    ((self.vblank_interrupt as u8) << 4) |
                    ((self.hblank_interrupt as u8) << 3) |
                    ((self.coincidence_interrupt_mode as u8) << 2) |
                    self.lcd_mode
            },
            0xFF42 => self.scy,
            0xFF43 => self.scx,
            0xFF44 => self.ly,
            0xFF45 => self.lyc,
            0xFF47 => self.bg_palette,
            0xFF48 => self.object_palette_0,
            0xFF49 => self.object_palette_1,
            0xFF4A => self.wy,
            0xFF4B => self.wx,
            _ => panic!("LCDC: Invalid Address")
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF40 => { self.set_lcdc(value); },
            0xFF41 => { self.set_stat(value); },
            0xFF42 => { self.scy = value; },
            0xFF43 => { self.scx = value; },
            0xFF45 => { self.cycle_compare(value); },
            0xFF47 => { self.bg_palette = value; },
            0xFF48 => { self.object_palette_0 = value; },
            0xFF49 => { self.object_palette_1 = value; },
            0xFF4A => { self.wy = value; },
            0xFF4B => { self.wx = value; },
            _ => panic!("LCDC: Invalid Address")
        }
    }


    fn set_lcdc(&mut self, lcdc: u8) {
            self.display_enable = (lcdc & 0x80) == 0x80;
            self.window_bank_offset = 0x400 * (((lcdc & 0x40) == 0x40) as u16);
            self.window_enable = (lcdc & 0x20) == 0x20;
            self.tile_bank_offset = 0x800 * (((lcdc & 0x10) == 0x10) as u16);
            self.bg_bank_offset = 0x400 * (((lcdc & 0x8) == 0x8) as u16);
            self.sprite_size = 0x8 * (1 + (((lcdc & 0x4) == 0x4) as u8));
            self.sprite_enable = (lcdc & 0x2) == 0x2;
            self.blank = (lcdc & 0x1) == 0x1;
    }

    fn set_stat(&mut self, stat: u8) {
            self.coincidence_interrupt_enable = (stat & 0x40) == 0x40;
            self.oam_interrupt = (stat & 0x20) == 0x20;
            self.vblank_interrupt = (stat & 0x10) == 0x10;
            self.hblank_interrupt = (stat & 0x8) == 0x8;
    }


    fn cycle_compare(&mut self, value: u8) {
        self.lyc = value;
        self.coincidence_interrupt_mode = self.lyc == self.ly;
    }

    pub fn update_tiles(&mut self, addr: u16, byte_1: u8, byte_2: u8) {
        let tile_index: usize = self.tile_set.update(addr, byte_1, byte_2, self.bg_palette);
        match self.bg_map.iter().position(|&r| r == tile_index as u8) {
            None => {  },
            Some(index) => {
                self.populate_bg(index, self.bg_map[index]);
            },
        }
    }

    pub fn map_background(&mut self, addr: usize, value: u8) {
        self.bg_map[addr] = value;
        self.populate_bg(addr, value);
    }

    pub fn populate_bg(&mut self, addr: usize, value: u8) {
        for row in 0..0x40 {
            let row_offset = (0x8 * addr) + ((addr / 0x20) * 0x100 * 0x7);
            let pixel_index: usize = (row % 0x8) + ((row / 0x8) * 0x100) + row_offset;
            let pixel: Pixel = self.tile_set.get_pixel(value as usize, row / 0x8, row % 0x8);
            self.background[pixel_index] = pixel_to_u32(pixel);
        }
    }

    pub fn get_background(&self) -> &[u32] {
        &self.background
    }

}
