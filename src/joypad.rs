use minifb::Key;

pub struct Joypad {
    function_active: bool,
    d_pad_active: bool,
    pressed: u8,
    mode: u8,
}

enum Button {
    Down   = 0x08,
    Up     = 0x04,
    Left   = 0x02,
    Right  = 0x01,
    Start  = 0x80,
    Select = 0x40,
    B      = 0x20,
    A      = 0x10,
}

impl Joypad {
    pub fn init() -> Joypad {
        Joypad {
            function_active: false,
            d_pad_active: false,
            pressed: 0xF,
            mode: 0x30,
        }
    }

    pub fn read(&self) -> u8 {
        return self.pressed | (!self.function_active as u8 * 0x20) | (!self.d_pad_active as u8 * 0x10);
    }

    pub fn write(&mut self, value: u8) {
        self.function_active = ((value >> 5) & 0x1) == 0;
        self.d_pad_active = ((value >> 4) & 0x1) == 0;
    }

    pub fn check_keypress(&mut self, keys: Option<Vec<Key>>) {
        self.pressed = 0xF;
        match keys {
            None => { },
            Some(keys) => {
                for key in keys {
                    use Button::*;
                    let button: Option<Button> = match key {
                        Key::Space     => Some(A),
                        Key::LeftShift => Some(B),
                        Key::Enter     => Some(Start),
                        Key::Backspace => Some(Select),
                        Key::Up        => Some(Up),
                        Key::Down      => Some(Down),
                        Key::Left      => Some(Left),
                        Key::Right     => Some(Right),
                        _ => { None },
                    };
                    match button {
                        None => { return; },
                        Some(button) => {
                            match button  {
                                A | B | Select | Start => {
                                    if self.function_active {
                                        self.pressed ^= (button as u8) >> 4;
                                        return;
                                    }
                                },
                                Up | Down | Left | Right => {
                                    if self.d_pad_active {
                                        self.pressed ^= button as u8;
                                        return;
                                    }
                                },
                            }
                        }
                    }
                }
            }
        }
    }
}
