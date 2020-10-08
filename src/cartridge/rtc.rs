use std::path::PathBuf;
use std::time::{SystemTime, Duration, UNIX_EPOCH};

pub struct RTC {
  pub rtc_enabled: bool,
  pub latch_buffer: bool,
  pub active_rtc_register: usize,
  clock: [u8; 5],
}

impl RTC {
    pub fn init(filename: &PathBuf) -> RTC {
        let rtc_enabled: bool = false;
        let active_rtc_register: usize = 0;
        let latch_buffer: bool = false;
        let clock: [u8; 5] = [0; 5];
        let mut rtc: RTC = RTC { rtc_enabled, latch_buffer, active_rtc_register, clock };
        rtc.load_from_save(filename);
        rtc
    }

    fn load_from_save(&mut self, filename: &PathBuf) {
        let buf: Vec<u8> = std::fs::read(filename).expect("Canned Error");
        for i in 0..5 {
            self.clock[i] = buf[i];
        }
    }

    pub fn read(&self) -> u8 {
        if self.rtc_enabled {
            self.clock[self.active_rtc_register]
        } else {
            0x0
        }
    }

    pub fn write(&mut self, value: u8) {
        if self.rtc_enabled {
            if (self.clock[4] & 0x40) > 0x0 {
                self.clock[self.active_rtc_register] = value;
            } else if self.active_rtc_register == 0x4 && value == 0x1 {
                self.clock[self.active_rtc_register] = value;
            }
        }
    }

    pub fn latch(&mut self) {
        if self.latch_buffer && (self.clock[4] & 0x40) > 0x0 {
            let secs: u64 = self.clock[0] as u64;
            let mins: u64 = self.clock[1] as u64 * 60;
            let hours: u64 = self.clock[2] as u64 * 3600;
            let days_l: u64 = self.clock[3] as u64;
            let days_h: u64 = ((self.clock[4] & 0x1) as u64) << 8;
            let days: u64 =  (days_h | days_l) * 3600 * 24;
            let offset = UNIX_EPOCH + Duration::from_secs(secs + mins + hours + days);
            let offset_time = SystemTime::now().duration_since(offset).expect("Canned error").as_secs();

            self.clock[0] = (offset_time % 60) as u8;
            self.clock[1] = ((offset_time / 60) % 60) as u8;
            self.clock[2] = ((offset_time / 3600) % 24) as u8;
            let new_days: u64 = offset_time / (3600 * 24);
            let carry: u8 = if new_days > 0x1FF { 0x80 } else { 0x00 };
            let new_days_h: u8 = ((new_days >> 8) & 0x1) as u8;
            self.clock[3] = new_days as u8;
            self.clock[4] = carry | new_days_h;
            self.latch_buffer = false;
        }
    }
}
