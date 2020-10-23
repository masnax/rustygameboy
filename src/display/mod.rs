pub mod tile;
use tile::*;
use crate::memory::Memory;


pub fn get_frame<'a>(mem: &'a mut Memory) -> Vec<u32> {
    let bg_bank: usize = 0; // TODO
    let tile_set: [Tile; 0x180] = get_tile_set(mem.tile_ram, mem.read_byte(0xFF47));
    let mut buffer: Vec<u32> = vec![0; 0x10000];

    for or in 0..0x20 {
        for ir in 0..0x8 {
            for oc in 0..0x20 {
                for ic in 0..0x8 {
                    let pixel_index: usize = (0x8*0x20*0x8*or)+(0x20*0x8*ir)+(0x8*oc) + ic;
                    let tile_index: usize = mem.bg_ram[bg_bank + (0x20*or) + oc] as usize;
                    let pixel: Pixel = tile_set[tile_index][ir][ic];
                    buffer[pixel_index] = pixel_to_u32(pixel);
                }
            }
        }
    }
    return buffer;
}
