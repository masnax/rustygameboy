#[derive(Copy,Clone)]
pub enum ColorCode {
     B = 0x3,
    DG = 0x2,
    LG = 0x1,
     W = 0x0,
}
#[derive(Debug,PartialEq,Eq)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

const WHITE: Color = Color {
    r: 224,
    g: 248,
    b: 208,
    a: 255,
};
const LIGHT_GRAY: Color = Color {
    r: 136,
    g: 192,
    b: 112,
    a: 255,
};
const DARK_GRAY: Color = Color {
    r: 39,
    g: 80,
    b: 70,
    a: 255,
};
const BLACK: Color = Color {
    r: 8,
    g: 24,
    b: 32,
    a: 255,
};

const TILE_SET_SIZE: usize = 0x1800;
const ROW_LEN: usize = 0x8;
const TILE_COUNT: usize = 0x180;
pub type Tile = [[ColorCode; ROW_LEN]; ROW_LEN];

pub struct TileSet {
    _mem: [u8; 5],
}

impl TileSet {
    pub fn init() -> TileSet {
        TileSet { _mem: [0; 5] }
    }

    pub fn get_tile_set(&self, vram: [u8; TILE_SET_SIZE]) -> [Tile; TILE_COUNT] {
        let mut tiles: [Tile; TILE_COUNT] = [[[ColorCode::W;ROW_LEN];ROW_LEN];TILE_COUNT];
        for i in 0..TILE_COUNT {
            let mut tile:Tile = [[ColorCode::W;ROW_LEN]; ROW_LEN];
            for row in 0..ROW_LEN {
                let addr: usize = ((i * 0x10) + (2 * row)) & 0xFFFE;
                let mut tile_row: [ColorCode; ROW_LEN] = [ColorCode::W; ROW_LEN];
                let byte_1: u8 = vram[addr];
                let byte_2: u8 = vram[addr + 1];
                for col in 0..ROW_LEN {
                    let bit_1: u8 = (byte_1 >> (7 - col)) & 0x1;
                    let bit_2: u8 = (byte_2 >> (7 - col)) & 0x1;
                    tile_row[col] = match (bit_1 == 0, bit_2 == 0) {
                        (true, true)   => ColorCode::B,
                        (false, true)  => ColorCode::DG,
                        (true, false)  => ColorCode::LG,
                        (false, false) => ColorCode::W,
                    }
                }
                tile[row] = tile_row;
            }
            tiles[i] = tile;
        }
        tiles
    }
}
