use crate::display::tile::SpriteTile;
#[derive(Copy,Clone)]
pub struct Sprite {
    pub addr: usize,
    pub x: u8,
    pub y: u8,
    pub hidden: bool,
    pub palette: u8,
    pub data: SpriteTile,
}

pub type SpritePair = (Sprite, Option<Sprite>);
pub const NUM_SPRITES: usize = 40;

impl Sprite {
    pub fn init(addr: usize, x: u8, y: u8, hidden: bool, palette: u8, data: SpriteTile) -> Sprite {
        Sprite { addr, x, y, hidden, palette, data }
    }

}
pub struct Sprites(Vec<SpritePair>);

impl Sprites {
    pub fn init() -> Sprites {
        Sprites(Vec::new())
    }

    pub fn insert(&mut self, sprite_top: Sprite, sprite_bot: Option<Sprite>) {
        match self.0.iter().position(|s| s.0.addr == sprite_top.addr) {
            None => {
                if self.0.len() < NUM_SPRITES {
                    self.0.push((sprite_top, sprite_bot));
                }
            },
            Some(index) => {
                    self.0[index] = (sprite_top, sprite_bot);
            }
        }
    }

    pub fn get_sprite_line(&mut self, ly: u8) -> Vec<Sprite> {
        let mut sprites: Vec<Sprite> = Vec::new();
        for (st, sb) in &self.0 {
            match sb {
                None => {
                    if ly - st.y < 8 {
                        sprites.push(*st);
                    }
                },
                Some(sprite) => {
                    if ly - sprite.y < 8 {
                        sprites.push(*st);
                        sprites.push(*sprite);
                    }
                }
            }
        }
        return sprites;
    }

    pub fn _check_priority(&self, addr: usize, x: u8, y: u8) -> bool {
        if y < 160 {
            let y_space: u8 = 8;
            let y_space_wrap = 0xFF - y_space + 1;
            for (st, _) in &self.0 {
                let y_diff = st.y.wrapping_sub(y);
                if y_diff < y_space || y_diff > y_space_wrap {
                    let x_space = 8;
                    let x_space_wrap = 0xFF - x_space + 1;
                    let x_diff = st.x.wrapping_sub(x);
                    if x_diff == 0 {
                        if addr < st.addr {
                            continue;
                        } else {
                            return false;
                        }
                    } else if x_diff < x_space || x_diff > x_space_wrap {
                            print!(" |||| {:?} {:?} {:?} {:?}", x, st.x, y, st.y);
                        return false;
                    }
                }
            }
        }
        return true;
    }
}
