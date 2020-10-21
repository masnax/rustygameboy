#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub struct Pixel {
    r: u8,
    g: u8,
    b: u8,
}

const WHITE: Pixel = Pixel {
    r: 224,
    g: 248,
    b: 208,
};
const LIGHT_GRAY: Pixel = Pixel {
    r: 136,
    g: 192,
    b: 112,
};
const DARK_GRAY: Pixel = Pixel {
    r: 39,
    g: 80,
    b: 70,
};
const BLACK: Pixel = Pixel {
    r: 8,
    g: 24,
    b: 32,
};

pub const TILE_SET_SIZE: usize = 0x1800;
pub const ROW_LEN: usize = 0x8;
pub const TILE_COUNT: usize = 0x180;
pub type Tile = [[Pixel; ROW_LEN]; ROW_LEN];

// Screen buffer: 256 x 256 pixels (32 x 32 tiles)


pub fn get_tile_set(vram: [u8; TILE_SET_SIZE], palette: u8) -> [Tile; TILE_COUNT] {
    let mut tiles: [Tile; TILE_COUNT] = [[[WHITE;ROW_LEN];ROW_LEN];TILE_COUNT];
    for i in 0..TILE_COUNT {
        let mut tile:Tile = [[WHITE;ROW_LEN]; ROW_LEN];
        for row in 0..ROW_LEN {
            let addr: usize = ((i * 0x10) + (2 * row)) & 0xFFFE;
            let mut tile_row: [Pixel; ROW_LEN] = [WHITE; ROW_LEN];
            let byte_1: u8 = vram[addr];
            let byte_2: u8 = vram[addr + 1];
            for col in 0..ROW_LEN {
                let bit_1: u8 = (byte_1 >> (7 - col)) & 0x1;
                let bit_2: u8 = (byte_2 >> (7 - col)) & 0x1;
                tile_row[col] = match (bit_1 == 0, bit_2 == 0) {
                    (true, true)   => BLACK,
                    (false, true)  => DARK_GRAY,
                    (true, false)  => LIGHT_GRAY,
                    (false, false) => WHITE,
                };
                tile_row[col] = palette_to_pixel(bit_1 << 1 | bit_2, palette);
            }
            tile[row] = tile_row;
        }
        tiles[i] = tile;
    }
    tiles
}

pub fn pixel_to_u32(p: Pixel) -> u32 {
    let (r, g, b) = (p.r as u32, p.g as u32, p.b as u32);
    (r << 16) | (g << 8) | b
}

fn palette_to_pixel(bits: u8, palette: u8) -> Pixel {
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
