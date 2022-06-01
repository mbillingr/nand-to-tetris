use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

pub struct Computer {
    stop: Arc<AtomicBool>,
    rom: Vec<u16>,
    ram: Vec<u16>,
}

impl Computer {
    pub fn new(rom: Vec<u16>) -> Self {
        Computer {
            stop: Arc::new(AtomicBool::new(false)),
            rom,
            ram: vec![0; 65536],
        }
    }

    pub fn get_ram(&self, addr: u16) -> u16 {
        self.ram[addr as usize]
    }

    pub fn run(&mut self) {
        let mut pc = 0;
        let mut a = 0;
        let mut d = 0;

        loop {
            if self.stop.load(Ordering::Relaxed) {
                return;
            }

            if pc >= self.rom.len() {
                return;
            }

            let op = self.rom[pc];

            if is_a_instruction(op) {
                a = op;
                pc += 1;
            } else {
                let mut x = d;
                let mut y = if use_mem(op) { self.ram[a as usize] } else { a };

                if zero_x(op) {
                    x = 0;
                }

                if negate_x(op) {
                    x = !x;
                }

                if zero_y(op) {
                    y = 0;
                }

                if negate_y(op) {
                    y = !y;
                }

                let mut z = if do_add(op) {
                    u16::wrapping_add(x, y)
                } else {
                    x & y
                };

                if negate_z(op) {
                    z = !z
                }

                if set_m(op) {
                    self.ram[a as usize] = z;
                }

                if set_d(op) {
                    d = z;
                }

                if set_a(op) {
                    a = z;
                }

                let z_signed = z as i16;

                if jump_gt(op) && z_signed > 0
                    || jump_eq(op) && z_signed == 0
                    || jump_lt(op) && z_signed < 0
                {
                    pc = a as usize;
                } else {
                    pc += 1;
                }
            }
        }
    }
}

fn is_a_instruction(op: u16) -> bool {
    (op & 0b1000000000000000) == 0
}

fn use_mem(op: u16) -> bool {
    (op & 0b0001000000000000) != 0
}

fn zero_x(op: u16) -> bool {
    (op & 0b0000100000000000) != 0
}

fn negate_x(op: u16) -> bool {
    (op & 0b0000010000000000) != 0
}

fn zero_y(op: u16) -> bool {
    (op & 0b0000001000000000) != 0
}

fn negate_y(op: u16) -> bool {
    (op & 0b0000000100000000) != 0
}

fn do_add(op: u16) -> bool {
    (op & 0b0000000010000000) != 0
}

fn negate_z(op: u16) -> bool {
    (op & 0b0000000001000000) != 0
}

fn set_a(op: u16) -> bool {
    (op & 0b0000000000100000) != 0
}

fn set_d(op: u16) -> bool {
    (op & 0b0000000000010000) != 0
}

fn set_m(op: u16) -> bool {
    (op & 0b0000000000001000) != 0
}

fn jump_lt(op: u16) -> bool {
    (op & 0b0000000000000100) != 0
}

fn jump_eq(op: u16) -> bool {
    (op & 0b0000000000000010) != 0
}

fn jump_gt(op: u16) -> bool {
    (op & 0b0000000000000001) != 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chapter01_boolean_logic::Bit::{self, I, O};
    use crate::chapter02_boolean_arithmetic::bus_as_number;
    use crate::op;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn can_stop_computer() {
        let mut emu = Computer::new(vec![bus_as_number(&op!(@0)), bus_as_number(&op!((0);JMP))]);

        let stop = emu.stop.clone();
        thread::spawn(move || {
            thread::sleep(Duration::from_millis(10));
            stop.store(true, Ordering::Relaxed);
        });

        emu.run();
    }

    #[test]
    fn computer_stops_when_running_out_of_instructions() {
        let mut emu = Computer::new(vec![]);
        emu.run();
    }

    #[test]
    fn set_memory() {
        let mut emu = Computer::new(vec![bus_as_number(&op!(@0)), bus_as_number(&op!(M = (1)))]);
        emu.run();
        assert_eq!(emu.ram[0], 1);
    }

    #[test]
    fn set_d_register() {
        let mut emu = Computer::new(vec![
            bus_as_number(&op!(D = (1))),
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(M = (D))),
        ]);
        emu.run();
        assert_eq!(emu.ram[0], 1);
    }

    #[test]
    fn set_a_register() {
        let mut emu = Computer::new(vec![
            bus_as_number(&op!(A = (1))),
            bus_as_number(&op!(D = (A))),
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(M = (D))),
        ]);
        emu.run();
        assert_eq!(emu.ram[0], 1);
    }

    #[test]
    fn set_m_with_all_targets() {
        let mut emu = Computer::new(vec![
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(ADM = (1))),
        ]);
        emu.run();
        assert_eq!(emu.ram[0], 1);
    }

    #[test]
    fn set_d_with_all_targets() {
        let mut emu = Computer::new(vec![
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(ADM = (1))),
            bus_as_number(&op!(@10)),
            bus_as_number(&op!(M = (D))),
        ]);
        emu.run();
        assert_eq!(emu.ram[10], 1);
    }

    #[test]
    fn set_a_with_all_targets() {
        let mut emu = Computer::new(vec![
            bus_as_number(&op!(@0)),
            bus_as_number(&op!(ADM = (1))),
            bus_as_number(&op!(D = (A))),
            bus_as_number(&op!(@10)),
            bus_as_number(&op!(M = (D))),
        ]);
        emu.run();
        assert_eq!(emu.ram[10], 1);
    }
}
