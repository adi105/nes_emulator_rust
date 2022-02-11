//imports
use crate::opcodes;
use std::collections::HashMap;
use crate::bus::Bus;

// map bitflags as const masks to set easily
bitflags! {
    pub struct Flags: u8 {
        const CARRY =       0b00000001;
        const ZERO =        0b00000010;
        const INTERRUPT =   0b00000100;
        const DECIMAL =     0b00001000;
        const BREAK =       0b00010000;
        const BREAK2 =      0b00100000;
        const OVERFLOW =    0b01000000;
        const NEGATIVE =    0b10000000;
    }
}

// const to refer to when resetting stack pointer
const STACK: u16 = 0x0100;
const STACK_RST: u8 = 0xFD;

pub struct CPU {
    // accumulator
    pub reg_a: u8,
    // registers x and y
    pub reg_x: u8,
    pub reg_y: u8,
    // status bits are in the following order on the 6502
    // NV--DIZC
    pub status: Flags,
    pub program_counter: u16,
    pub stack_ptr: u8,
    // add bus
    pub bus: Bus,
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
    IZX,
    IZY,
    NoneAddressing,
}

// implement memory functions as trait rather than directly into CPU class
pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

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
}

impl Mem for CPU {
    // memory related functions
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
        //self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
        //self.memory[addr as usize] = data;
    }

    
    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data)
    }
    
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        CPU {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            status: Flags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_ptr: STACK_RST,
            bus: bus,   
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

    // opcode implementations========================================
    // memory
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        self.reg_a = val;
        self.set_flags(self.reg_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        self.reg_x = val;
        self.set_flags(self.reg_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let val = self.mem_read(addr);

        self.reg_y = val;
        self.set_flags(self.reg_y);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.reg_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.reg_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.reg_y);
    }

    // helper function to set value of reg a
    fn set_reg_a(&mut self, val: u8) {
        self.reg_a = val;
        self.set_flags(self.reg_a);
    }

    // arithmetic
    // to implement arithmetic operations, most use the same steps. We will export these steps to a helper function
    fn addition_reg_a(&mut self, data: u8) {
        let temp = self.reg_a as u16 + data as u16 + (if self.status.contains(Flags::CARRY) { 1 } else { 0 }) as u16;
        // set carry flag if necessary
        let carryflag = temp > 0xFF;
        if carryflag {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        let val = temp as u8;

        // test for overflow, niche way to determine if V flag must be set using bitwise XOR.
        if (data ^ val) & (val ^ self.reg_a) & 0x80 != 0 {
            self.status.insert(Flags::OVERFLOW);
        } else {
            self.status.remove(Flags::OVERFLOW);
        }

        self.set_reg_a(val);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.addition_reg_a(data);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        // subtraction can utilize addition function
        // a - b is the same as a + (-b), where -b = !b + 1
        self.addition_reg_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
        //          cast to 8b int, use wrapping neg to invert, add 1, cast to u8
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_reg_a(data & self.reg_a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_reg_a(data ^ self.reg_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_reg_a(data | self.reg_a);
    }
    

    fn tax(&mut self) {
        self.reg_x = self.reg_a;
        self.set_flags(self.reg_x);
    }

    fn tay(&mut self) {
        self.reg_y = self.reg_a;
        self.set_flags(self.reg_y);
    }

    fn inx(&mut self) {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.set_flags(self.reg_x)
    }

    // bitshifting
    fn asl(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        // check for carry
        if data >> 7 == 1 {
            // set carry flag
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        // perform the shift
        data = data << 1;
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    // shifting regarding accumulator (no data fetches)
    fn asl_reg_a(&mut self) {
        let mut data = self.reg_a;
        // check for carry
        if data >> 7 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data << 1;
        self.set_reg_a(data);
    }

    fn lsr(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        // check for carry
        if data & 1 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data >> 1;
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    fn lsr_reg_a(&mut self) {
        let mut data = self.reg_a;
        // carry check
        if data & 1 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data >> 1;
        self.set_reg_a(data);
    }

    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let carry_set = self.status.contains(Flags::CARRY);
        
        // check for carry bit
        if data >> 7 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data << 1;
        if carry_set {
            // set bit 0 to 1
            data = data | 1;
        }
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    fn rol_reg_a(&mut self) {
        let mut data = self.reg_a;
        let carry_set = self.status.contains(Flags::CARRY);

        //check for carry bit
        if data >> 7 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data << 1;
        if carry_set {
            data = data | 1;
        }
        self.set_reg_a(data);
    }

    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let carry_set = self.status.contains(Flags::CARRY);

        // check for carry bit
        if data & 1 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data >> 1;
        if carry_set {
            data = data | 0b1000_0000;
        }
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    fn ror_reg_a(&mut self) {
        let mut data = self.reg_a;
        let carry_set = self.status.contains(Flags::CARRY);

        // check for carry bit
        if data & 1 == 1 {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        data = data >> 1;
        if carry_set {
            data = data | 0b1000_0000;
        }
        self.set_reg_a(data);
    }

    fn inc(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        // add 1 using wrapping add
        data = data.wrapping_add(1);
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    fn iny(&mut self) {
        self.reg_y = self.reg_y.wrapping_add(1);
        self.set_flags(self.reg_y);
    }

    fn dec(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        //subtract 1 using wrapping sub
        data = data.wrapping_sub(1);
        self.mem_write(addr, data);
        self.set_flags(data);
        data
    }

    fn dex(&mut self) {
        self.reg_x = self.reg_x.wrapping_sub(1);
        self.set_flags(self.reg_x);
    }

    fn dey(&mut self) {
        self.reg_y = self.reg_y.wrapping_sub(1);
        self.set_flags(self.reg_y);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let val = self.reg_a.wrapping_sub(data);

        // set carry flag
        if self.reg_a >= data {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        self.set_flags(val);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        let val = self.reg_x.wrapping_sub(data);

        // set carry flag
        if self.reg_x >= data {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        self.set_flags(val);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let val = self.reg_y.wrapping_sub(data);

        // set carry flag
        if self.reg_y >= data {
            self.status.insert(Flags::CARRY);
        } else {
            self.status.remove(Flags::CARRY);
        }

        self.set_flags(val);
    }

    // branch with condition to be used for all branch opcodes
    fn branch(&mut self, cond: bool) {
        if cond {
            // calculate location to jump to
            let jump:i8 = self.mem_read(self.program_counter) as i8;
            let jmp_addr = self.program_counter.wrapping_add(1).wrapping_add(jump as u16);
            // set pc to location
            self.program_counter = jmp_addr;
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        // apply mask
        let masked = self.reg_a & data;
        // set Z flag
        if masked == 0 {
            self.status.insert(Flags::ZERO);
        } else {
            self.status.remove(Flags::ZERO);
        }

        // BIT opcode sets the value of negative and overflow bits to bit 7 and 6 of mask, respectively
        self.status.set(Flags::NEGATIVE, data & 0b1000_0000 > 0);
        self.status.set(Flags::OVERFLOW, data & 0b0100_0000 > 0);
    }

    // stack helper functions
    fn stack_push(&mut self, data: u8) {
        // write to the stack using STACK constant + offset
        self.mem_write((STACK as u16) + self.stack_ptr as u16, data);
        self.stack_ptr = self.stack_ptr.wrapping_sub(1);
    }

    fn stack_pop(&mut self) -> u8 {
        // read from stack using STACK const + offset
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        self.mem_read((STACK as u16) + self.stack_ptr as u16)
    }

    fn stack_push_u16(&mut self, data: u16) {
        // separate high order and low order bytes
        let high_order = (data >> 8) as u8;
        let low_order = (data & 0xFF) as u8;
        // push high order fist
        self.stack_push(high_order);
        self.stack_push(low_order);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let low_order = self.stack_pop() as u16;
        let high_order = self.stack_pop() as u16;
        // combine
        let val = high_order << 8 | low_order;
        val
    }

    // stack opcodes
    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_reg_a(data);
    }

    fn plp(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(Flags::BREAK);
        self.status.insert(Flags::BREAK2);
    }

    fn php(&mut self) {
        let mut flags_to_push = self.status.clone();
        // php opcode sets B flags to 1
        flags_to_push.insert(Flags::BREAK);
        flags_to_push.insert(Flags::BREAK2);
        self.stack_push(flags_to_push.bits);
    }
    //===============================================================

    fn set_flags(&mut self, result:u8) {
        // set the 0 flag
        if result == 0 {
            self.status.insert(Flags::ZERO);
        } else {
            self.status.remove(Flags::ZERO);
        }

        // set the n flag
        if result & 0b1000_0000 != 0 {
            self.status.insert(Flags::NEGATIVE);
        } else {
            self.status.remove(Flags::NEGATIVE);
        }
    }
    //==================================================================

    pub fn load_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    // load the program vector to mem location 0x8000 and set the program counter to this location
    pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(0x600 + i, program[i as usize]);
        }
        self.mem_write_u16(0xFFFC, 0x0600);
    }

    // reset function to reset registers and status, and point the program counter to 0xFFFC. Used by roms
    pub fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.status = Flags::from_bits_truncate(0b100100);
        self.program_counter = self.mem_read_u16(0xFFFC);
        self.stack_ptr = STACK_RST;
        // self.memory = [0; 0xFFFF];
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where 
        F: FnMut(&mut CPU) 
    {
        // hashmap of opcodes
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            // read from memory
            let code = self.mem_read(self.program_counter);
            // increment program counter
            self.program_counter = self.program_counter + 1;
            let pc_state = self.program_counter;
            let opcode = opcodes.get(&code).unwrap();

            match code {
                // LDA
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }
                // BRK
                0x00 => {
                    return;
                }
                //NOP
                0xea => {
                    // nothing
                }
                // LDX
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&opcode.mode);
                }
                // LDY
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&opcode.mode);
                }
                // STA
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }
                // STX
                0x86 | 0x96 | 0x8e => {
                    self.stx(&opcode.mode);
                }
                // STY
                0x84 | 0x94 | 0x8c => {
                    self.sty(&opcode.mode);
                }
                // ADC
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.mode);
                }
                // SBC
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&opcode.mode);
                }
                // AND
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.mode);
                }
                // EOR
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&opcode.mode);
                }
                // ORA
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&opcode.mode);
                }
                // TAX
                0xAA => {
                    self.tax();
                }
                // TAY
                0xa8 => {
                    self.tay();
                }
                //INX
                0xe8 => {
                    self.inx();
                }
                // ASL
                0x0a => {
                    self.asl_reg_a();
                }
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&opcode.mode);
                }
                // LSR
                0x4a => {
                    self.lsr_reg_a();
                }
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&opcode.mode);
                }
                // ROL
                0x2a => {
                    self.rol_reg_a();
                }
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.mode);
                }
                // ROR
                0x6a => {
                    self.ror_reg_a();
                }
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.mode);
                }
                // INC
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&opcode.mode);
                }
                // INY
                0xc8 => {
                    self.iny();
                }
                // DEC
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&opcode.mode);
                }
                // DEX
                0xca => {
                    self.dex();
                }
                // DEY
                0x88 => {
                    self.dey();
                }
                // CMP
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.cmp(&opcode.mode);
                }
                // CPX
                0xe0 | 0xe4 | 0xec => {
                    self.cpx(&opcode.mode);
                }
                // CPY
                0xc0 | 0xc4 | 0xcc => {
                    self.cpy(&opcode.mode);
                }
                // (two types of JMP instructions, 6502 bug emulated)
                // JMP ABS
                0x4c => {
                    let new_addr = self.mem_read_u16(self.program_counter);
                    self.program_counter = new_addr;
                }
                // JMP Indirect
                // page bug: if JMP crosses page boundary, it jumps to an unexpected location
                0x6c => {
                    let new_addr = self.mem_read_u16(self.program_counter);
                    //------------------------------------------------------
                    // if the page boundary bug takes place, we manually read the correct reference
                    // if it does not take place, we can let mem_read_u16 work as expected
                    let ind_reference = if new_addr & 0x00FF == 0x00FF {
                        let low_order = self.mem_read(new_addr);
                        let high_order = self.mem_read(new_addr & 0xFF00);
                        (high_order as u16) << 8 | (low_order as u16)
                    } else {
                        self.mem_read_u16(new_addr)
                    };

                    self.program_counter = ind_reference;
                }
                // JSR
                0x20 => {
                    self.stack_push_u16(self.program_counter + 2 - 1);
                    let target = self.mem_read_u16(self.program_counter);
                    self.program_counter = target;
                }
                // RTS
                0x60 => {
                    self.program_counter = self.stack_pop_u16() + 1;
                }
                // RTI
                // pull status from stack, followed by PC
                0x40 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(Flags::BREAK);
                    self.status.remove(Flags::BREAK2);

                    self.program_counter = self.stack_pop_u16();
                }
                // BNE
                0xd0 => {
                    self.branch(!self.status.contains(Flags::ZERO));
                }
                // BVS
                0x70 => {
                    self.branch(self.status.contains(Flags::OVERFLOW));
                }
                // BVC
                0x50 => {
                    self.branch(!self.status.contains(Flags::OVERFLOW));
                }
                // BMI
                0x30 => {
                    self.branch(self.status.contains(Flags::NEGATIVE));
                }
                // BEQ
                0xF0 => {
                    self.branch(self.status.contains(Flags::ZERO));
                }
                // BCS
                0xB0 => {
                    self.branch(self.status.contains(Flags::CARRY));
                }
                // BCC
                0x90 => {
                    self.branch(!self.status.contains(Flags::CARRY));
                }
                // BPL
                0x10 => {
                    self.branch(!self.status.contains(Flags::NEGATIVE));
                }
                // BIT
                0x24 | 0x2c => {
                    self.bit(&opcode.mode);
                }
                // TSX
                0xba => {
                    self.reg_x = self.stack_ptr;
                    self.set_flags(self.reg_x);
                }
                // TXA
                0x8a => {
                    self.reg_a = self.reg_x;
                    self.set_flags(self.reg_a);
                }
                // TXS
                0x9a => {
                    self.stack_ptr = self.reg_x;
                }
                // TYA
                0x98 => {
                    self.reg_a = self.reg_y;
                    self.set_flags(self.reg_a);
                }
                // CLD
                0xd8 => {
                    self.status.remove(Flags::DECIMAL);
                }
                // CLI
                0x58 => {
                    self.status.remove(Flags::INTERRUPT);
                }
                // CLV
                0xb8 => {
                    self.status.remove(Flags::OVERFLOW);
                }
                // CLC
                0x18 => {
                    self.status.remove(Flags::CARRY);
                }
                // SEC
                0x38 => {
                    self.status.insert(Flags::CARRY);
                }
                // SEI
                0x78 => {
                    self.status.insert(Flags::INTERRUPT);
                }
                // SED
                0xf8 => {
                    self.status.insert(Flags::DECIMAL);
                }
                // PHA
                0x48 => {
                    self.stack_push(self.reg_a);
                }
                // PLA
                0x68 => {
                    self.pla();
                }
                // PHP
                0x08 => {
                    self.php();
                }
                // PLP
                0x28 => {
                    self.plp();
                }


                // placeholder for further instructions
                _ => todo!()
            }

            // handling of additional cycles needed
            if pc_state == self.program_counter {
                self.program_counter += (opcode.length - 1) as u16;
            }
            callback(self);
        }
    }
}

// TESTS
#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn test_lda_immediate_load() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.load_run(vec![0xa9, 0x05, 0x00]);
        // LDA 0x05
        // BRK
        assert_eq!(cpu.reg_a, 0x05);                // accumulator is set to 0x05
        assert!(cpu.status.bits() & 0b0000_0010 == 0);     // zero flag is not set
        assert!(cpu.status.bits() & 0b1000_0000 == 0);     // neg flag is not set
    }

    #[test]
    fn test_lda_zero_flag() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.load_run(vec![0xA9, 0x00, 0x00]);
        // LDA 0x00
        // BRK
        assert_eq!(cpu.reg_a, 0x00);                // accumulator set to 0x00
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10)   // zero flag is set
    }

    #[test]
    fn test_tax_immediate_load() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.load_run(vec![0xA9, 0x05, 0xAA, 0x00]);
        // LDA 0x0A
        // TAX
        // BRK
        assert_eq!(cpu.reg_x, 0x05);                // reg x set to 0x05
        assert!(cpu.status.bits() & 0b0000_0010 == 0);     // zero flag is not set
        assert!(cpu.status.bits() & 0b1000_0000 == 0);     // neg flag is not set
    }

    #[test]
    fn test_tax_zero_flag() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.load_run(vec![0xAA, 0x00]);
        // TAX
        // BRK
        assert_eq!(cpu.reg_x, 0x00);                // reg x set to 0x00
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10)   // zero flag is set
    }

    #[test]
    fn test_inx_overflow() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
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
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.load_run(vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00]);
        // LDA 0xC0
        // TAX
        // INX
        // BRk
        assert_eq!(cpu.reg_x, 0xC1);
    }

    #[test]
    fn test_lda_imm() {
        let bus = Bus::new();
        let mut cpu = CPU::new(bus);
        cpu.mem_write(0x10, 0x55);
        // load program
        cpu.load_run(vec![0xA5, 0x10, 0x00]);
        // LDA 0x10 (zero page)
        assert_eq!(cpu.reg_a, 0x55);
    }
}
