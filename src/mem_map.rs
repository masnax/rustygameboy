// $FFFF	Interrupt Enable Flag
// $FF80-$FFFE	Zero Page - 127 bytes
// $FF00-$FF7F	Hardware I/O Registers
// $FEA0-$FEFF	Unusable Memory
// $FE00-$FE9F	OAM - Object Attribute Memory
// $E000-$FDFF	Echo RAM - Reserved, Do Not Use
// $D000-$DFFF	Internal RAM - Bank 1-7 (switchable - CGB only)
// $C000-$CFFF	Internal RAM - Bank 0 (fixed)
// $A000-$BFFF	Cartridge RAM (If Available)
// $9C00-$9FFF	BG Map Data 2
// $9800-$9BFF	BG Map Data 1
// $8000-$97FF	Character RAM
// $4000-$7FFF	Cartridge ROM - Switchable Banks 1-xx
// $0150-$3FFF	Cartridge ROM - Bank 0 (fixed)
// $0100-$014F	Cartridge Header Area
// $0000-$00FF	Restart and Interrupt Vectors


pub struct MemoryMap {
    token: u8,
}


impl MemoryMap {
    pub fn init() -> MemoryMap {
        MemoryMap {
            token: 0
        }
    }

    pub fn read_b(&self, addr: u16) -> u8 {
        match addr {
            // Restart and Interrupt Vectors
            0x0000 ..= 0x00FF => { 0 },
            // Cartridge Header Area
            0x0100 ..= 0x014F => { 0 },
            // Cartridge ROM - Bank 0 (fixed)
            0x0150 ..= 0x3FFF => { 0 },
            // Cartridge ROM - Switchable Banks 1-xx
            0x4000 ..= 0x7FFF => { 0 },
            // Character RAM
            0x8000 ..= 0x97FF => { 0 },
            // BG Map Data 1
            0x9800 ..= 0x9BFF => { 0 },
            // BG Map Data 2
            0x9C00 ..= 0x9FFF => { 0 },
            // Cartridge RAM (If Available)
            0xA000 ..= 0xBFFF => { 0 },
            // Internal RAM - Bank 0 (fixed)
            0xC000 ..= 0xCFFF => { 0 },
            // Internal RAM - Bank 1-7 (switchable - CGB only)
            0xD000 ..= 0xDFFF => { 0 },
            // Echo RAM - Reserved, Do Not Use
            0xE000 ..= 0xFDFF => { 0 },
            // OAM - Object Attribute Memory
            0xFE00 ..= 0xFE9F => { 0 },
            // Unusable Memory
            0xFEA0 ..= 0xFEFF => { 0 },
            // Hardware I/O Registers
            0xFF00 ..= 0xFF7F => { 0 },
            // Zero Page - 127 bytes
            0xFF80 ..= 0xFFFE => { 0 },
            // Interrupt Enable Flag
            0xFFFF => { 0 },
            _ => { panic!("boo"); }
        }
    }

}
