//imports
use crate::opcodes;
use std::collections::HashMap;

// map bitflags as const masks to set easily
bitflags! {
    pub struct Flags: u8 {
        const CARRY =       0b0000_0001;
        const ZERO =        0b0000_0010;
        const INTERRUPT =   0b0000_0100;
        const DECIMAL =     0b0000_1000;
        const BREAK =       0b0001_0000;
        const BREAK2 =      0b0010_0000;
        const OVERFLOW =    0b0100_0000;
        const NEGATIVE =    0b1000_0000;
    }
}

pub struct CPU {
    // accumulator
    pub reg_a: u8,
    // registers x and y
    pub reg_x: u8,
    pub reg_y: u8,
    // status bits are in the following order on the 6502
    // NV--DIZC
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF]
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
// enum of all of the 6502 CPU adressing modes.
pub enum AddressingMode {
    IMM, 
    ZP0, 
    ZPX,
    ZPY,
    ABS,
    ABX,
    ABY,
    IND,
    IZX,
    IZY,
    NoneAddressing,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF]
        }
    }

    // Adressing Mode handling==============================================
    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            // Immediate
            AddressingMode::IMM => {
                self.program_counter
            }
            // Zero-Page Adressing using our endian memread function
            AddressingMode::ZP0 => {
                self.mem_read(self.program_counter) as u16
            }
            // Zero-Page with X offset
            AddressingMode::ZPX => {
                let position = self.mem_read(self.program_counter);
                let ret = position.wrapping_add(self.reg_x) as u16;
                ret
            }
            // Zero-Page with Y offset
            AddressingMode::ZPY => {
                let position = self.mem_read(self.program_counter);
                let ret = position.wrapping_add(self.reg_y) as u16;
                ret
            }
            // Absolute
            AddressingMode::ABS => {
                self.mem_read_u16(self.program_counter)
            }
            // Absolute with X offset
            AddressingMode::ABX => {
                let position = self.mem_read_u16(self.program_counter);
                let ret = position.wrapping_add(self.reg_x as u16);
                ret
            }
            // Absolute with Y offset
            AddressingMode::ABY => {
                let position = self.mem_read_u16(self.program_counter);
                let ret = position.wrapping_add(self.reg_y as u16);
                ret
            }
            // Indirect
            AddressingMode::IND => {
                /*
                let base = self.mem_read_u16(self.program_counter);
                let ptr: u8 = (base as u8);

                let low_order = self.mem_read(ptr as u16);
                let high_order = self.mem_read(ptr.wrapping_add(1) as u16);

                (high_order as u16) << 8 | (low_order as u16)
                */
                todo!();
            }
            // Indirect off zero page with X offset
            AddressingMode::IZX => {
                let base = self.mem_read(self.program_counter);
                let ptr: u8 = (base as u8).wrapping_add(self.reg_x);

                let low_order = self.mem_read(ptr as u16);
                let high_order = self.mem_read(ptr.wrapping_add(1) as u16);

                (high_order as u16) << 8 | (low_order as u16)
            }
            // Indirect off zero page with Y offset
            AddressingMode::IZY => {
                let base = self.mem_read(self.program_counter);
                let ptr: u8 = (base as u8).wrapping_add(self.reg_y);

                let low_order = self.mem_read(ptr as u16);
                let high_order = self.mem_read(ptr.wrapping_add(1) as u16);

                (high_order as u16) << 8 | (low_order as u16)
            }
            // catch invalid addressing
            AddressingMode::NoneAddressing => {
                panic!("Addressing mode {:?} is not supported.", mode)
            }
        }
    }

    // opcode implementations
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        self.reg_a = val;
        self.set_flags(self.reg_a);
    }

    fn tax(&mut self) {
        self.reg_x = self.reg_a;
        self.set_flags(self.reg_x);
    }

    fn inx(&mut self) {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.set_flags(self.reg_x)
    }

    fn set_flags(&mut self, result:u8) {
        // set the 0 flag
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        // set the n flag
        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
        }
    }

    // memory related functions
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    // little endian support ==========================================
    fn mem_read_u16(&self, pos: u16) -> u16 {
        let low_order = self.mem_read(pos) as u16;
        let high_order = self.mem_read(pos + 1) as u16;
        // chain them together
        (high_order << 8) | (low_order as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let high_order = (data >> 8) as u8;
        let low_order = (data & 0xFF) as u8; // using masking
        self.mem_write(pos, low_order);
        self.mem_write(pos+1, high_order);
    }
    //==================================================================

    pub fn load_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    // load the program vector to mem location 0x8000 and set the program counter to this location
    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    // reset function to reset registers and status, and point the program counter to 0xFFFC. Used by roms
    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        // hashmap of opcodes
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            // read from memory
            let code = self.mem_read(self.program_counter);
            // increment program counter
            self.program_counter = self.program_counter + 1;
            let pc_state = self.program_counter;
            let opcode = opcodes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            match code {
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }
                // BRK (0x00)
                0x00 => {
                    return;
                }
                // TAX (0xAA)
                0xAA => {
                    self.tax();
                }
                //INX (0xe8)
                0xe8 => {
                    self.inx();
                }
                // placeholder for further instructions
                _ => todo!()
            }

            // handling of additional cycles needed
            if pc_state == self.program_counter {
                self.program_counter += (opcode.length - 1) as u16;
            }
        }
    }
}

// TESTS
#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_lda_immediate_load() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xA9, 0x05, 0x00]);
        // LDA 0x05
        // BRK
        assert_eq!(cpu.reg_a, 0x05);                // accumulator is set to 0x05
        assert!(cpu.status & 0b0000_0010 == 0);     // zero flag is not set
        assert!(cpu.status & 0b1000_0000 == 0);     // neg flag is not set
    }

    #[test]
    fn test_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xA9, 0x00, 0x00]);
        // LDA 0x00
        // BRK
        assert_eq!(cpu.reg_a, 0x00);                // accumulator set to 0x00
        assert!(cpu.status & 0b0000_0010 == 0b10)   // zero flag is set
    }

    #[test]
    fn test_tax_immediate_load() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xA9, 0x05, 0xAA, 0x00]);
        // LDA 0x0A
        // TAX
        // BRK
        assert_eq!(cpu.reg_x, 0x05);                // reg x set to 0x05
        assert!(cpu.status & 0b0000_0010 == 0);     // zero flag is not set
        assert!(cpu.status & 0b1000_0000 == 0);     // neg flag is not set
    }

    #[test]
    fn test_tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xAA, 0x00]);
        // TAX
        // BRK
        assert_eq!(cpu.reg_x, 0x00);                // reg x set to 0x00
        assert!(cpu.status & 0b0000_0010 == 0b10)   // zero flag is set
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xA9, 0xFF, 0xAA, 0xe8, 0xe8, 0x00]);
        // LDA 0xFF
        // TAX
        // INX
        // INX
        // BRK
        assert_eq!(cpu.reg_x, 0x01);
        // if it equals 1, that implies that overflow worked.
    }

    
    #[test]
    fn test_mini_program() {
        let mut cpu = CPU::new();
        cpu.load_run(vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00]);
        // LDA 0xC0
        // TAX
        // INX
        // BRk
        assert_eq!(cpu.reg_x, 0xC1);
    }

    #[test]
    fn test_lda_imm() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        // load program
        cpu.load_run(vec![0xA5, 0x10, 0x00]);
        // LDA 0x10 (zero page)
        assert_eq!(cpu.reg_a, 0x55);
    }
}
