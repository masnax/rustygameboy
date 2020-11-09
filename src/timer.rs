    // **Interrupts**
    // Only 1 active at a time
    // PC pushed to stack
    // Interrupts -- in race condition first in order is activated, then action is undefined
    // (1) VBLANK -- LCD has drawn full frame --- Call 0x40 ---- Edit VRAM in between frames
    // (2) LCDC ---- LCD controller changed ----- Call 0x48 ---- Cause modifiable by STAT reg ; Can wait for screen to draw specific line via LYC reg
    // (3) SERIAL -- Serial Transfer completed -- Call 0x50 ---- Use of serial port for multiplayer
    // (4) TIMER --- Serial Transfer completed -- Call 0x58 ---- Activates once the timer register overflows, then the timer resets to a predefined value, and starts moving.
    // (5) HiToLo -- User pressed a button ------ Call 0x60 ---- Interrupt to update action based on input. Input is still not reflected until next frame
    // All interrupts are enabled/disabled via register at address 0xFFFF
    // Timer Registers
    // 0xFF04 -- DIV --- Divider Register --- increments at freq. 16384 Hz. Resets if written to.
    // 0xFF05 -- TIMA -- Timer Counter ------ increments at freq. of TAC. Resets at overflow to TMA value and triggers interrupt. Cpu jumps to 0x50 at TIMA overflow, where the next instruction is read.
    // 0xFF06 -- TMA --- Timer Mod ---------- contains an offset value for the timer to restart with
    // 0xFF07 -- TAC --- Timer Control
    //                   0 (Stop)     00 -- 4096 Hz
    //                   1 (Start)    01 -- 262144 Hz
    //                                10 -- 65536 Hz
    //                                11 -- 16384 Hz



const CPU_CYCLE_FREQ: u32 = 0x400000;
const CYCLE_THRESHOLD: u32 = 5;
const CLOCK_0: u32 = 0x01000;
const CLOCK_1: u32 = 0x40000;
const CLOCK_2: u32 = 0x10000;
const CLOCK_3: u32 = 0x04000;
const TIMER_INTERRUPT: u8 = 0x4;


pub struct Timer {
    div: u8,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn init() -> Timer {
        Timer {
            div: 0,
            tima: 0,
            tma: 0,
            tac: 0,
        }
    }

    pub fn cycle(&mut self, cpu_cycles: u32) -> u8 {
        let mut interrupt: u8 = 0;
        let clock_num: u32 = match self.tac * 0x3 {
            0 => CLOCK_0,
            1 => CLOCK_1,
            2 => CLOCK_2,
            3 => CLOCK_3,
            _ => panic!("Invalid TAC Value"),
        };
        let tick_cycles = CPU_CYCLE_FREQ / clock_num;
        if self.should_tick(tick_cycles, cpu_cycles) {
            if self.tima == 0xFF {
                self.tima = self.tma;
                interrupt |= TIMER_INTERRUPT;
            } else {
                self.tima += 1;
            }
        }

        let tick_cycles = CPU_CYCLE_FREQ / CLOCK_3;
        if self.should_tick(tick_cycles, cpu_cycles) {
            self.div = self.div.wrapping_add(1);
        }

        return interrupt;
    }

    fn should_tick(&mut self, tick_cycles: u32, cpu_cycles: u32) -> bool {
        if cpu_cycles >= tick_cycles {
            return cpu_cycles % tick_cycles <= CYCLE_THRESHOLD;
        }
        return false;

    }


    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => self.div,
            0xFF05 => self.tima,
            0xFF06 => self.tma,
            0xFF07 => self.tac,
            _ => panic!("TIMER: Invalid address"),
        }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF04 => self.div = 0,
            0xFF05 => self.tima = value,
            0xFF06 => self.tma = value,
            0xFF07 => self.tac = value,
            _ => panic!("TIMER: Invalid address"),
        }
    }
}
