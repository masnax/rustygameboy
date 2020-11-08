#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub struct Pixel {
     r: u8,
     g: u8,
     b: u8,
}

pub const WHITE: Pixel = Pixel {
    r: 224,
    g: 248,
    b: 208,
};

pub const LIGHT_GRAY: Pixel = Pixel {
    r: 136,
    g: 192,
    b: 112,
};
pub const DARK_GRAY: Pixel = Pixel {
    r: 39,
    g: 80,
    b: 70,
};
pub const BLACK: Pixel = Pixel {
    r: 8,
    g: 24,
    b: 32,
};

pub const TILE_SIZE: usize = 0x8;
pub const TILE_COUNT: usize = 0x180;
pub type Tile = [[u8; TILE_SIZE]; TILE_SIZE];
pub type SpriteTile = [u8; TILE_SIZE * TILE_SIZE];


pub struct TileSet {
    tile_set: [Tile; TILE_COUNT]
}

// Screen buffer: 256 x 256 pixels (32 x 32 tiles)


impl TileSet {
    pub fn init() -> TileSet {
        TileSet {
            tile_set: [[[0;TILE_SIZE];TILE_SIZE];TILE_COUNT]
        }
    }

    pub fn update(&mut self, start_addr: u16, byte_1: u8, byte_2: u8) {
        let tile_index: usize = (start_addr >> 4) as usize;
        let row_index: usize = ((start_addr & 0xF) / 2) as usize;
        for col_index in 0..8 {
            let bit_1: u8 = (byte_1 >> (7 - col_index)) & 0x1;
            let bit_2: u8 = (byte_2 >> (7 - col_index)) & 0x1;
            self.tile_set[tile_index][row_index][col_index] = (bit_2 << 1) | bit_1;
        }
    }

    pub fn get_pixel(&self, signed: bool, tile_index: usize, row_index: usize, col_index: usize, palette: u8) -> u32 {
        let tile_index: usize = if signed && tile_index < 0x80 {
            tile_index + 0x100
        } else {
            tile_index
        };
        let tile_value = self.tile_set[tile_index][row_index][col_index];
        let pixel = bits_to_pixel(tile_value, palette);
        return pixel_to_u32(pixel);
    }

    pub fn get_sprite_tile(&self, tile_index: usize, mirror_x: bool, mirror_y: bool) -> SpriteTile {
        let mut pixels: SpriteTile = [0; TILE_SIZE*TILE_SIZE];
        for row in 0..TILE_SIZE {
            for col in 0..TILE_SIZE {
                let mirror_col = if mirror_x { TILE_SIZE - col - 1 } else { col };
                let mirror_row = if mirror_y { TILE_SIZE - row - 1 } else { row };
                let bits = self.tile_set[tile_index][mirror_row][mirror_col];
                pixels[(row*8) + col] = bits;
            }
        }
        return pixels;
    }
}

pub fn pixel_to_u32(p: Pixel) -> u32 {
    let (r, g, b) = (p.r as u32, p.g as u32, p.b as u32);
    (r << 16) | (g << 8) | b
}


pub fn bits_to_pixel(bits: u8, palette: u8) -> Pixel {
    let col_3: u8 = (palette & 0xC0) >> 6;
    let col_2: u8 = (palette & 0x30) >> 4;
    let col_1: u8 = (palette & 0xC) >> 2;
    let col_0: u8 = palette & 0x3;

    let bit_color: u8 = match bits {
        0 => col_0,
        1 => col_1,
        2 => col_2,
        3 => col_3,
        _ => panic!("Invalid Color Code")
    };

    return match bit_color {
        0 => WHITE,
        1 => LIGHT_GRAY,
        2 => DARK_GRAY,
        3 => BLACK,
        _ => panic!("Invalid Color Code")
    };
}

