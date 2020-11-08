mod tile;
mod sprite;
use tile::*;
use sprite::*;

const HBLANK_INTERRUPT_CYCLES: u32 = 204;
const VBLANK_INTERRUPT_CYCLES: u32 = 456;
const OAM_INTERRUPT_CYCLES: u32 = 80;
const VRAM_INTERRUPT_CYCLES: u32 = 172;
const LCD_INTERRUPT: u8 = 0x2;
const VBLANK_INTERRUPT: u8 = 0x1;

pub struct LcdController {
    tile_set: TileSet,
    sprites: Sprites,
    background: Vec<u32>,
    viewport: Vec<u32>,

    display_enable: bool,
    window_bank: usize,
    window_enable: bool,
    signed_tiles: bool,
    bg_bank: usize,
    large_sprites: bool,
    sprite_enable: bool,
    blank: bool,

    coincidence_interrupt_enable: bool,
    mode_2_int_enabled: bool,
    mode_1_int_enabled: bool,
    mode_0_int_enabled: bool,
    coincidence_interrupt_mode: bool,
    lcd_mode: u8,

    scy: u8,
    scx: u8,
    ly: u8,
    lyc: u8,
    bg_palette: u8,
    sprite_palette_0: u8,
    sprite_palette_1: u8,
    wy: u8,
    wx: u8,
    pub drawing_display: bool,
}


impl<'a> LcdController {
    pub fn init() -> LcdController {
        LcdController {
            tile_set: TileSet::init(),
            sprites: Sprites::init(),
            background: vec![pixel_to_u32(WHITE); 0x20000],
            viewport: vec![pixel_to_u32(WHITE); 0x5A00],

            display_enable: false,
            window_bank: 0,
            window_enable: false,
            signed_tiles: false,
            bg_bank: 0,
            large_sprites: false,
            sprite_enable: false,
            blank: false,

            coincidence_interrupt_enable: false,
            mode_2_int_enabled: false,
            mode_1_int_enabled: false,
            mode_0_int_enabled: false,
            coincidence_interrupt_mode: false,
            lcd_mode: 0,

            scy: 0,
            scx: 0,
            ly: 0,
            lyc: 0,
            bg_palette: 0,
            sprite_palette_0: 0,
            sprite_palette_1: 0,
            wy: 0,
            wx: 0,
            drawing_display: false,
        }
    }

    fn compare_coincidence(&mut self) -> bool {
        self.coincidence_interrupt_mode = self.lyc == self.ly;
        return self.coincidence_interrupt_enable && self.coincidence_interrupt_mode;
    }

    fn advance_lcd_mode(&mut self) {
        self.lcd_mode = match self.lcd_mode {
            0 => if self.ly == 144 { 1 } else { 2 },
            1 => if self.ly == 0   { 2 } else { 1 },
            2 => 3,
            3 => 0,
            _ => panic!("Invalid LCD Mode"),
        }
    }

    pub fn cycle(&mut self, cycles: u32) -> (u8, u32) {
        let mut interrupts: u8 = 0;
        if self.display_enable {
            if cycles >= VBLANK_INTERRUPT_CYCLES && self.lcd_mode == 1 {
                if self.ly == 144 {
                    interrupts |= VBLANK_INTERRUPT;
                    self.drawing_display = false;
                    if self.mode_1_int_enabled || self.compare_coincidence() {
                        interrupts |= LCD_INTERRUPT;
                    }
                }
                self.ly = (self.ly + 1) % 154;
                self.advance_lcd_mode();
                return (interrupts, VBLANK_INTERRUPT_CYCLES);
            }
            if cycles >= HBLANK_INTERRUPT_CYCLES && self.lcd_mode == 0 {
                self.draw_one_line();
                if self.mode_0_int_enabled || self.compare_coincidence() {
                    interrupts |= LCD_INTERRUPT;
                }
                self.ly = (self.ly + 1) % 154;
                self.advance_lcd_mode();
                return (interrupts, HBLANK_INTERRUPT_CYCLES);
            }
            if cycles >= VRAM_INTERRUPT_CYCLES && self.lcd_mode == 3 {
                self.advance_lcd_mode();
                return (interrupts, VRAM_INTERRUPT_CYCLES);
            }
            if cycles >= OAM_INTERRUPT_CYCLES && self.lcd_mode == 2 {
                if self.mode_2_int_enabled {
                    interrupts |= LCD_INTERRUPT;
                }
                self.advance_lcd_mode();
                return (interrupts, OAM_INTERRUPT_CYCLES);
            }
        }
        return (interrupts, 0);
    }

    pub fn vram_locked(&self) -> bool {
        return self.lcd_mode == 3;
    }

    pub fn oam_locked(&self) -> bool {
        return self.lcd_mode == 3 || self.lcd_mode == 2;
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF40 => {
                ((self.display_enable as u8) << 7) |
                    ((self.window_bank as u8) << 6) |
                    ((self.window_enable as u8) << 5) |
                    ((self.signed_tiles as u8) << 4) |
                    ((self.bg_bank as u8) << 3) |
                    ((self.large_sprites as u8) << 2) |
                    ((self.sprite_enable as u8) << 1) |
                    (self.blank as u8)
            },
            0xFF41 => {
                ((self.coincidence_interrupt_enable as u8) << 6) |
                    ((self.mode_2_int_enabled as u8) << 5) |
                    ((self.mode_1_int_enabled as u8) << 4) |
                    ((self.mode_0_int_enabled as u8) << 3) |
                    ((self.coincidence_interrupt_mode as u8) << 2) |
                    self.lcd_mode
            },
            0xFF42 => self.scy,
            0xFF43 => self.scx,
            0xFF44 => self.ly,
            0xFF45 => self.lyc,
            0xFF47 => self.bg_palette,
            0xFF48 => self.sprite_palette_0,
            0xFF49 => self.sprite_palette_1,
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
            0xFF44 => { if !self.display_enable { self.ly = 0}; },
            0xFF45 => { self.cycle_compare(value); },
            0xFF47 => { self.bg_palette = value; },
            0xFF48 => { self.sprite_palette_0 = value; },
            0xFF49 => { self.sprite_palette_1 = value; },
            0xFF4A => { self.wy = value; },
            0xFF4B => { self.wx = value; },
            _ => panic!("LCDC: Invalid Address")
        }
    }


    fn set_lcdc(&mut self, lcdc: u8) {
        self.display_enable = (lcdc & 0x80) == 0x80;
        self.window_bank = ((lcdc & 0x40) == 0x40) as usize;
        self.window_enable = (lcdc & 0x20) == 0x20;
        self.signed_tiles = (lcdc & 0x10) == 0;
        self.bg_bank = ((lcdc & 0x8) == 0x8) as usize;
        self.large_sprites = (lcdc & 0x4) == 0x4;
        self.sprite_enable = (lcdc & 0x2) == 0x2;
        self.blank = (lcdc & 0x1) == 0x1;
    }

    fn set_stat(&mut self, stat: u8) {
        self.coincidence_interrupt_enable = (stat & 0x40) == 0x40;
        self.mode_2_int_enabled = (stat & 0x20) == 0x20;
        self.mode_1_int_enabled = (stat & 0x10) == 0x10;
        self.mode_0_int_enabled = (stat & 0x8) == 0x8;
    }

    fn cycle_compare(&mut self, value: u8) {
        self.lyc = value;
        self.coincidence_interrupt_mode = self.lyc == self.ly;
    }

    fn draw_one_line(&mut self) {
        if self.display_enable {
            if self.blank {
                self.draw_bg_line();
            } else {
                self.viewport = vec![pixel_to_u32(WHITE); 0x5A00];
            }

            if self.sprite_enable {
                self.draw_sprite_line();
            }
        }
    }

    fn draw_sprite_line(&mut self) {
        for sprite in self.sprites.get_sprite_line(self.ly) {
            self.set_sprite(sprite);
        }
    }

    fn set_sprite(&mut self, sprite: Sprite) {
        let viewport_row: usize = (self.ly as usize) * 0xA0;
        for col in 0..8 {
            let viewport_pos = viewport_row + (sprite.x as usize) + col;
            let sprite_pos = (((self.ly - sprite.y) as usize) * 0x8) + col;
            let bits = sprite.data[sprite_pos];
            if viewport_pos < self.viewport.len() && bits != 0{
                let pixel = pixel_to_u32(bits_to_pixel(bits, sprite.palette));
                if !sprite.hidden || self.viewport[viewport_pos] == pixel_to_u32(WHITE) {
                    self.viewport[viewport_pos] = pixel;
                }
            }
        }
    }

    pub fn map_sprite(&mut self, addr: usize, b1: u8, b2: u8, b3: u8, b4: u8) {
        let y: u8 = b1.wrapping_sub(16);
        let x: u8 = b2.wrapping_sub(8);
        let tile_index: usize = b3 as usize;
        let hidden: bool = b4 & 0x80 == 0x80;
        let mirror_y: bool = b4 & 0x40 == 0x40;
        let mirror_x: bool = b4 & 0x20 == 0x20;
        let palette: u8 = if b4 & 0x10 == 0x10 { self.sprite_palette_1 } else { self.sprite_palette_0 };

        if self.large_sprites {
            let top: usize = tile_index & 0xFE;
            let bot: usize = tile_index | 0x01;
            let data_top: SpriteTile = self.tile_set.get_sprite_tile(top, mirror_x, mirror_y);
            let data_bot: SpriteTile = self.tile_set.get_sprite_tile(bot, mirror_x, mirror_y);
            let sprite_top = Sprite::init(addr, x, y, hidden, palette, data_top);
            let sprite_bot = Sprite::init(addr, x, y+1, hidden, palette, data_bot);
            self.sprites.insert(sprite_top, Some(sprite_bot));

        } else {
            let data: SpriteTile = self.tile_set.get_sprite_tile(tile_index, mirror_x, mirror_y);
            let sprite = Sprite::init(addr, x, y, hidden, palette, data);
            self.sprites.insert(sprite, None);

        }
    }

    fn draw_bg_line(&mut self) {
        let window: bool = self.window_enable && self.wy <= self.ly && self.wx < 160;
        let bank: usize = (if window { self.window_bank } else { self.bg_bank }) * 0x10000;
        let y_offset: usize = if window {
            self.ly.wrapping_sub(self.wy)
        } else {
            self.ly.wrapping_add(self.scy)
        } as usize * 0x100;

        let wx: u8 = self.wx.wrapping_sub(7);
        let viewport_row: usize = (self.ly as usize) * 0xA0;
        for col in 0..0xA0 {
            let scx: u8 = if window { wx } else { self.scx };
            let x: u8 = (col as u8).wrapping_add(scx);
            let x_offset: usize = if !self.window_enable && x >= 0xA0 {
                x.wrapping_sub(0xA0) as usize
            } else {
                x as usize
            };
            self.viewport[viewport_row + col] = self.background[y_offset + x_offset + bank];
        }
    }


    pub fn update_tiles(&mut self, addr: u16, byte_1: u8, byte_2: u8) {
        self.tile_set.update(addr, byte_1, byte_2);
    }

    pub fn map_background(&mut self, map_index: usize, tile_index: usize) {
        for i in 0..(TILE_SIZE * TILE_SIZE) {
            let row_offset = (TILE_SIZE * map_index) + ((map_index / 0x20) * 0x100 * 0x7);
            let col: usize = i % TILE_SIZE;
            let row: usize = i / TILE_SIZE;
            let pixel_index: usize = col + (row * 0x100) + row_offset;
            let pixel: u32 = self.tile_set.get_pixel(self.signed_tiles, tile_index, row, col, self.bg_palette);
            self.background[pixel_index] = pixel;
        }
    }

    pub fn get_viewport(&mut self) -> Option<&[u32]> {
        self.drawing_display = true;
        if self.display_enable {
            if !self.blank {
                self.viewport = vec![pixel_to_u32(WHITE); 0x5A00];
            }
            return Some(&self.viewport);
        } else {
            return None;
        }
    }

}
