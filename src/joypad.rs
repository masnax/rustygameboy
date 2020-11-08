pub struct Joypad {
    function_active: bool,
    d_pad_active: bool,
    pressed: u8
}

impl Joypad {
    pub fn init() -> Joypad {
        Joypad {
            function_active: false,
            d_pad_active: false,
            pressed: 0xF,
        }
    }

    pub fn read(&self) -> u8 {
        return self.pressed | (self.function_active as u8 * 0x20) | (self.d_pad_active as u8 * 0x10);
    }

    pub fn write(&mut self, value: u8) {
        self.function_active = ((value >> 5) & 0x1) == 0;
        self.d_pad_active = ((value >> 4) & 0x1) == 0;
    }
}
