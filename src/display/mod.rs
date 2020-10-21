pub mod tile;
use tile::*;


pub fn get_frame(bg_ram: [u8; 0x800], bg_bank: usize, vram: [u8; TILE_SET_SIZE], palette: u8) -> Vec<u32> {
    let tile_set: [Tile; 0x180] = get_tile_set(vram, palette);
    let mut buffer: Vec<u32> = vec![0; 0x10000];

    for or in 0..0x20 {
        for ir in 0..0x8 {
            for oc in 0..0x20 {
                for ic in 0..0x8 {
                    let pixel_index: usize = (0x8*0x20*0x8*or)+(0x20*0x8*ir)+(0x8*oc) + ic;
                    let tile_index: usize = bg_ram[bg_bank + (0x20*or) + oc] as usize;
                    let pixel: Pixel = tile_set[tile_index][ir][ic];
                    buffer[pixel_index] = pixel_to_u32(pixel);
                }
            }
        }
    }
    return buffer;
}
