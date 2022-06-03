use crate::chapter01_boolean_logic::Bit::{I, O};
use crate::chapter02_boolean_arithmetic::{bus_as_number, number_to_bus};
use minifb::{Window, WindowOptions};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};
use std::thread;
use std::thread::JoinHandle;

pub trait IoCallback {
    fn write_screen(&self, addr: u16, value: u16);
    fn read_screen(&self, addr: u16) -> u16;
    fn read_keyboard(&self) -> u16;
    fn is_disconnected(&self) -> bool;
}

pub struct Computer {
    stop: Arc<AtomicBool>,
    rom: Vec<u16>,
    pub ram: Vec<u16>,
    io: Arc<dyn IoCallback>,
}

const SCREEN_START_ADDR: u16 = 0x4000;
const SCREEN_END_ADDR: u16 = 0x6000;
const KEYBOARD_ADDR: u16 = 0x6000;

impl Computer {
    pub fn new(rom: Vec<u16>) -> Self {
        Computer {
            stop: Arc::new(AtomicBool::new(false)),
            rom,
            ram: vec![0; 65536],
            io: Arc::new(()),
        }
    }

    pub fn set_io(&mut self, device: Arc<dyn IoCallback>) {
        self.io = device;
    }

    pub fn get_ram(&self, addr: u16) -> u16 {
        self.ram[addr as usize]
    }

    fn read_memory(&self, addr: u16) -> u16 {
        if (SCREEN_START_ADDR..SCREEN_END_ADDR).contains(&addr) {
            return self.io.read_screen(addr - SCREEN_START_ADDR);
        }

        if addr == KEYBOARD_ADDR {
            return self.io.read_keyboard();
        }

        self.ram[addr as usize]
    }

    fn write_memory(&mut self, addr: u16, value: u16) {
        if (SCREEN_START_ADDR..SCREEN_END_ADDR).contains(&addr) {
            return self.io.write_screen(addr - SCREEN_START_ADDR, value);
        }

        self.ram[addr as usize] = value
    }

    pub fn run(&mut self) {
        let mut pc = 0;
        let mut a = 0;
        let mut d = 0;

        loop {
            if self.io.is_disconnected() {
                return;
            }

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
                let mut y = if use_mem(op) { self.read_memory(a) } else { a };

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
                    self.write_memory(a, z);
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

impl IoCallback for () {
    fn write_screen(&self, _: u16, _: u16) {}

    fn read_screen(&self, _: u16) -> u16 {
        0
    }

    fn read_keyboard(&self) -> u16 {
        0
    }

    fn is_disconnected(&self) -> bool {
        false
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

pub struct IoDevice {
    join_handle: JoinHandle<()>,
    command: mpsc::Sender<IoCommand>,
    response: mpsc::Receiver<u16>,
}

#[derive(Debug)]
enum IoCommand {
    SetScreen(u16, u16),
    GetScreen(u16),
    GetKeyboard,
}

impl IoDevice {
    pub fn new() -> Self {
        const WIDTH: usize = 512;
        const HEIGHT: usize = 256;
        const PIXELS_PER_WORD: usize = 16;
        const BLACK: u32 = 0x00000000;
        const WHITE: u32 = 0xFFFFFFFF;

        let (command_tx, command_rx) = mpsc::channel();
        let (response_tx, response_rx) = mpsc::channel();

        let join_handle = thread::spawn(move || {
            let mut buffer: Vec<u32> = vec![WHITE; WIDTH * HEIGHT];

            let mut window =
                Window::new("Hack Emulator", WIDTH, HEIGHT, WindowOptions::default()).unwrap();

            let mut pixel_block_buffer = Vec::with_capacity(PIXELS_PER_WORD);

            while window.is_open() {
                while let Ok(cmd) = command_rx.try_recv() {
                    match cmd {
                        IoCommand::SetScreen(addr, value) => {
                            pixel_block_buffer.clear();
                            number_to_bus(value, PIXELS_PER_WORD, &mut pixel_block_buffer);

                            let pixel_offset = addr as usize * PIXELS_PER_WORD;
                            let pixels = &mut buffer[pixel_offset..pixel_offset + PIXELS_PER_WORD];

                            for (px, &bit) in pixels.iter_mut().zip(&pixel_block_buffer) {
                                match bit {
                                    I => *px = BLACK,
                                    O => *px = WHITE,
                                }
                            }
                        }

                        IoCommand::GetScreen(addr) => {
                            let pixel_offset = addr as usize / PIXELS_PER_WORD;
                            let pixels = &mut buffer[pixel_offset..pixel_offset + PIXELS_PER_WORD];

                            for (&px, bit) in pixels.iter().zip(&mut pixel_block_buffer) {
                                match px {
                                    WHITE => *bit = O,
                                    _ => *bit = I,
                                }
                            }

                            let value = bus_as_number(&pixel_block_buffer);
                            response_tx.send(value).unwrap();
                        }

                        IoCommand::GetKeyboard => {
                            let mut key = 0;
                            for k in window.get_keys() {
                                match k {
                                    _ => key = 128,
                                }
                            }
                            response_tx.send(key).unwrap();
                        }
                    }
                }
                // Limit to max ~60 fps update rate
                window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

                window.update_with_buffer(&buffer, WIDTH, HEIGHT).unwrap();
            }
        });

        IoDevice {
            join_handle,
            command: command_tx,
            response: response_rx,
        }
    }

    pub fn join(self) -> Result<(), String> {
        self.join_handle.join().map_err(|e| format!("{:?}", e))
    }
}

impl IoCallback for IoDevice {
    fn write_screen(&self, addr: u16, value: u16) {
        self.command
            .send(IoCommand::SetScreen(addr, value))
            .unwrap()
    }

    fn read_screen(&self, addr: u16) -> u16 {
        self.command.send(IoCommand::GetScreen(addr)).unwrap();
        self.response.recv().unwrap()
    }

    fn read_keyboard(&self) -> u16 {
        self.command.send(IoCommand::GetKeyboard).unwrap();
        self.response.recv().unwrap()
    }

    fn is_disconnected(&self) -> bool {
        self.join_handle.is_finished()
    }
}

impl Default for IoDevice {
    fn default() -> Self {
        Self::new()
    }
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
