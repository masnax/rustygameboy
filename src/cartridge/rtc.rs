use std::time::{SystemTime, Duration, UNIX_EPOCH};

pub struct RTC {
  pub rtc_enabled: bool,
  pub latch_buffer: bool,
  pub active_rtc_register: usize,
  rtc: [u8; 5],
}

impl RTC {
    pub fn init() -> RTC {
        let rtc_enabled: bool = false;
        let active_rtc_register: usize = 0;
        let latch_buffer: bool = false;
        let rtc: [u8; 5] = [0; 5];
        RTC { rtc_enabled, latch_buffer, active_rtc_register, rtc }
    }

    fn load_from_save() {
        // TODO
    }

    pub fn read(&self) -> u8 {
        if self.rtc_enabled {
            self.rtc[self.active_rtc_register]
        } else {
            0x0
        }
    }

    pub fn write(&self, value: u8) {
        if self.rtc_enabled {
            if (self.rtc[4] & 0x40) > 0x0 {
                self.rtc[self.active_rtc_register] = value;
            } else if self.active_rtc_register == 0x4 && value == 0x1 {
                self.rtc[self.active_rtc_register] = value;
            }
        }
    }

    pub fn latch(&mut self) {
        if self.latch_buffer && (self.rtc[4] & 0x40) > 0x0 {
            let secs: u64 = self.rtc[0] as u64;
            let mins: u64 = self.rtc[1] as u64 * 60;
            let hours: u64 = self.rtc[2] as u64 * 3600;
            let days_l: u64 = self.rtc[3] as u64;
            let days_h: u64 = ((self.rtc[4] & 0x1) << 8) as u64;
            let days: u64 =  (days_h | days_l) * 3600 * 24;
            let offset = UNIX_EPOCH + Duration::from_secs(secs + mins + hours + days);
            let offset_time = SystemTime::now().duration_since(offset).expect("Canned error").as_secs();

            self.rtc[0] = (offset_time % 60) as u8;
            self.rtc[1] = ((offset_time / 60) % 60) as u8;
            self.rtc[2] = ((offset_time / 3600) % 24) as u8;
            let new_days: u64 = offset_time / (3600 * 24);
            let carry: u8 = if new_days > 0x1FF { 0x80 } else { 0x00 };
            let new_days_h: u8 = ((new_days >> 8) & 0x1) as u8;
            self.rtc[3] = new_days as u8;
            self.rtc[4] = carry | new_days_h;
            self.latch_buffer = false;
        }
    }

    fn get_(&self) -> u64 {
        let secs = self.rtc[0];
        let mins_as_secs = self.rtc[1] * 60;
        let hrs_as_secs = self.rtc[2] * 3600;
        let days_l = self.rtc[3] as u16;
        let days_u = ((self.rtc[4] & 0x1) << 8) as u16;
        let days_as_secs =  (days_u | days_l) * 3600 * 24;


        return 0;
    }
}
