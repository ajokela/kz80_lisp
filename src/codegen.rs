//! Z80 code generator for kz80_lisp
//!
//! This module generates Z80 machine code for a minimal LISP interpreter
//! that can run on the RetroShield Z80.

use std::collections::HashMap;

/// Z80 code generator
pub struct CodeGen {
    /// Generated code bytes
    code: Vec<u8>,
    /// Current code address
    pc: u16,
    /// Symbol table for labels
    labels: HashMap<String, u16>,
    /// Forward references to resolve
    forward_refs: Vec<(u16, String, RefType)>,
}

/// Type of reference for forward resolution
#[derive(Clone)]
enum RefType {
    Absolute16,  // 16-bit absolute address
    Relative8,   // 8-bit relative jump
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            pc: 0x0100, // Start after RST vectors
            labels: HashMap::new(),
            forward_refs: Vec::new(),
        }
    }

    /// Emit a byte
    fn emit(&mut self, byte: u8) {
        self.code.push(byte);
        self.pc += 1;
    }

    /// Emit a 16-bit word (little-endian)
    fn emit16(&mut self, word: u16) {
        self.emit((word & 0xFF) as u8);
        self.emit((word >> 8) as u8);
    }

    /// Define a label at current position
    fn label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.pc);
    }

    /// Reference a label (forward or backward)
    fn ref_label(&mut self, name: &str, ref_type: RefType) {
        if let Some(&addr) = self.labels.get(name) {
            match ref_type {
                RefType::Absolute16 => self.emit16(addr),
                RefType::Relative8 => {
                    let rel = (addr as i32 - self.pc as i32 - 1) as i8;
                    self.emit(rel as u8);
                }
            }
        } else {
            // Forward reference
            self.forward_refs.push((self.pc, name.to_string(), ref_type.clone()));
            match ref_type {
                RefType::Absolute16 => self.emit16(0),
                RefType::Relative8 => self.emit(0),
            }
        }
    }

    /// Resolve forward references
    fn resolve_refs(&mut self) {
        for (addr, name, ref_type) in &self.forward_refs {
            if let Some(&target) = self.labels.get(name) {
                let offset = (*addr - 0x0100) as usize;
                match ref_type {
                    RefType::Absolute16 => {
                        self.code[offset] = (target & 0xFF) as u8;
                        self.code[offset + 1] = (target >> 8) as u8;
                    }
                    RefType::Relative8 => {
                        let rel = target as i32 - *addr as i32 - 1;
                        if rel < -128 || rel > 127 {
                            eprintln!("ERROR: JR offset out of range at 0x{:04X} -> {} (offset {})", addr, name, rel);
                        }
                        self.code[offset] = rel as u8;
                    }
                }
            }
        }
    }

    // Z80 instructions

    /// NOP
    fn nop(&mut self) { self.emit(0x00); }

    /// LD r, n (8-bit immediate)
    fn ld_r_n(&mut self, r: u8, n: u8) {
        self.emit(0x06 | (r << 3));
        self.emit(n);
    }

    /// LD r, r' (register to register)
    fn ld_r_r(&mut self, rd: u8, rs: u8) {
        self.emit(0x40 | (rd << 3) | rs);
    }

    /// LD rr, nn (16-bit immediate)
    fn ld_rr_nn(&mut self, rr: u8, nn: u16) {
        self.emit(0x01 | (rr << 4));
        self.emit16(nn);
    }

    /// LD A, (nn)
    fn ld_a_mem(&mut self, addr: u16) {
        self.emit(0x3A);
        self.emit16(addr);
    }

    /// LD (nn), A
    fn ld_mem_a(&mut self, addr: u16) {
        self.emit(0x32);
        self.emit16(addr);
    }

    /// LD (HL), n
    fn ld_hl_ind_n(&mut self, n: u8) {
        self.emit(0x36);
        self.emit(n);
    }

    /// LD A, (HL)
    fn ld_a_hl_ind(&mut self) { self.emit(0x7E); }

    /// LD (HL), A
    fn ld_hl_ind_a(&mut self) { self.emit(0x77); }

    /// PUSH rr
    fn push(&mut self, rr: u8) {
        self.emit(0xC5 | (rr << 4));
    }

    /// POP rr
    fn pop(&mut self, rr: u8) {
        self.emit(0xC1 | (rr << 4));
    }

    /// JP nn
    fn jp(&mut self, addr: u16) {
        self.emit(0xC3);
        self.emit16(addr);
    }

    /// JP label
    fn jp_label(&mut self, label: &str) {
        self.emit(0xC3);
        self.ref_label(label, RefType::Absolute16);
    }

    /// JP cc, nn
    fn jp_cc(&mut self, cc: u8, addr: u16) {
        self.emit(0xC2 | (cc << 3));
        self.emit16(addr);
    }

    /// JP cc, label
    fn jp_cc_label(&mut self, cc: u8, label: &str) {
        self.emit(0xC2 | (cc << 3));
        self.ref_label(label, RefType::Absolute16);
    }

    /// JR e (relative jump)
    fn jr(&mut self, offset: i8) {
        self.emit(0x18);
        self.emit(offset as u8);
    }

    /// JR label
    fn jr_label(&mut self, label: &str) {
        self.emit(0x18);
        self.ref_label(label, RefType::Relative8);
    }

    /// JR cc, e
    fn jr_cc(&mut self, cc: u8, offset: i8) {
        self.emit(0x20 | (cc << 3));
        self.emit(offset as u8);
    }

    /// CALL nn
    fn call(&mut self, addr: u16) {
        self.emit(0xCD);
        self.emit16(addr);
    }

    /// CALL label
    fn call_label(&mut self, label: &str) {
        self.emit(0xCD);
        self.ref_label(label, RefType::Absolute16);
    }

    /// RET
    fn ret(&mut self) { self.emit(0xC9); }

    /// RET cc
    fn ret_cc(&mut self, cc: u8) {
        self.emit(0xC0 | (cc << 3));
    }

    /// INC r
    fn inc_r(&mut self, r: u8) {
        self.emit(0x04 | (r << 3));
    }

    /// DEC r
    fn dec_r(&mut self, r: u8) {
        self.emit(0x05 | (r << 3));
    }

    /// INC rr
    fn inc_rr(&mut self, rr: u8) {
        self.emit(0x03 | (rr << 4));
    }

    /// DEC rr
    fn dec_rr(&mut self, rr: u8) {
        self.emit(0x0B | (rr << 4));
    }

    /// ADD A, r
    fn add_a_r(&mut self, r: u8) {
        self.emit(0x80 | r);
    }

    /// ADD A, n
    fn add_a_n(&mut self, n: u8) {
        self.emit(0xC6);
        self.emit(n);
    }

    /// ADD HL, rr
    fn add_hl_rr(&mut self, rr: u8) {
        self.emit(0x09 | (rr << 4));
    }

    /// ADD HL, HL (double HL)
    fn add_hl_hl(&mut self) {
        self.emit(0x29);  // ADD HL, HL
    }

    /// ADC HL, HL (with carry)
    fn adc_hl_hl(&mut self) {
        self.emit(0xED);
        self.emit(0x6A);  // ADC HL, HL
    }

    /// ADD A, n
    fn add_a_imm(&mut self, n: u8) {
        self.emit(0xC6);
        self.emit(n);
    }

    /// SBC A, n (subtract with carry immediate)
    fn sbc_a_n(&mut self, n: u8) {
        self.emit(0xDE);
        self.emit(n);
    }

    /// DJNZ label (decrement B and jump if not zero)
    fn djnz_label(&mut self, label: &str) {
        self.emit(0x10);
        self.ref_label(label, RefType::Relative8);
    }

    /// SUB r
    fn sub_r(&mut self, r: u8) {
        self.emit(0x90 | r);
    }

    /// ADC A, r (add with carry)
    fn adc_a_r(&mut self, r: u8) {
        self.emit(0x88 | r);
    }

    /// SBC A, r (subtract with carry)
    fn sbc_a_r(&mut self, r: u8) {
        self.emit(0x98 | r);
    }

    /// CPL (complement A, i.e., A = ~A)
    fn cpl(&mut self) {
        self.emit(0x2F);
    }

    /// SUB n
    fn sub_n(&mut self, n: u8) {
        self.emit(0xD6);
        self.emit(n);
    }

    /// AND r
    fn and_r(&mut self, r: u8) {
        self.emit(0xA0 | r);
    }

    /// AND n
    fn and_n(&mut self, n: u8) {
        self.emit(0xE6);
        self.emit(n);
    }

    /// OR r
    fn or_r(&mut self, r: u8) {
        self.emit(0xB0 | r);
    }

    /// OR n
    fn or_n(&mut self, n: u8) {
        self.emit(0xF6);
        self.emit(n);
    }

    /// XOR r
    fn xor_r(&mut self, r: u8) {
        self.emit(0xA8 | r);
    }

    /// CP r
    fn cp_r(&mut self, r: u8) {
        self.emit(0xB8 | r);
    }

    /// CP n
    fn cp_n(&mut self, n: u8) {
        self.emit(0xFE);
        self.emit(n);
    }

    /// IN A, (n)
    fn in_a_n(&mut self, port: u8) {
        self.emit(0xDB);
        self.emit(port);
    }

    /// OUT (n), A
    fn out_n_a(&mut self, port: u8) {
        self.emit(0xD3);
        self.emit(port);
    }

    /// DI
    fn di(&mut self) { self.emit(0xF3); }

    /// EI
    fn ei(&mut self) { self.emit(0xFB); }

    /// HALT
    fn halt(&mut self) { self.emit(0x76); }

    /// RST n
    fn rst(&mut self, n: u8) {
        self.emit(0xC7 | n);
    }

    /// EX DE,HL
    fn ex_de_hl(&mut self) { self.emit(0xEB); }

    /// LD (nn), HL
    fn ld_mem_hl(&mut self, addr: u16) { self.emit(0x22); self.emit16(addr); }

    /// LD HL, (nn)
    fn ld_hl_mem(&mut self, addr: u16) { self.emit(0x2A); self.emit16(addr); }

    /// LD DE, (nn)
    fn ld_de_mem(&mut self, addr: u16) { self.emit(0xED); self.emit(0x5B); self.emit16(addr); }

    /// LD A, (DE)
    fn ld_a_de(&mut self) { self.emit(0x1A); }

    /// LD (DE), A
    fn ld_de_a(&mut self) { self.emit(0x12); }

    /// SBC HL, rr
    fn sbc_hl_rr(&mut self, rr: u8) { self.emit(0xED); self.emit(0x42 | (rr << 4)); }

    /// BIT n, r
    fn bit_n_r(&mut self, bit: u8, r: u8) { self.emit(0xCB); self.emit(0x40 | (bit << 3) | r); }

    /// SRL r
    fn srl_r(&mut self, r: u8) { self.emit(0xCB); self.emit(0x38 | r); }

    /// RR r
    fn rr_r(&mut self, r: u8) { self.emit(0xCB); self.emit(0x18 | r); }

    /// SRA r
    fn sra_r(&mut self, r: u8) { self.emit(0xCB); self.emit(0x28 | r); }

    /// RLA
    fn rla(&mut self) { self.emit(0x17); }

    /// RRCA - Rotate A right circular
    fn rrca(&mut self) { self.emit(0x0F); }

    /// JP (HL)
    fn jp_hl(&mut self) { self.emit(0xE9); }

    /// LD rr, nn with label
    fn ld_rr_nn_label(&mut self, rr: u8, label: &str) {
        self.emit(0x01 | (rr << 4));
        self.ref_label(label, RefType::Absolute16);
    }

    /// JR cc, label
    fn jr_cc_label(&mut self, cc: u8, label: &str) {
        self.emit(0x20 | (cc << 3));
        self.ref_label(label, RefType::Relative8);
    }

    // Register encoding
    const B: u8 = 0;
    const C: u8 = 1;
    const D: u8 = 2;
    const E: u8 = 3;
    const H: u8 = 4;
    const L: u8 = 5;
    const HL_IND: u8 = 6;
    const A: u8 = 7;

    // Register pair encoding
    const BC: u8 = 0;
    const DE: u8 = 1;
    const HL: u8 = 2;
    const SP: u8 = 3;
    const AF: u8 = 3; // For PUSH/POP

    // Condition codes
    const NZ: u8 = 0;
    const Z: u8 = 1;
    const NC: u8 = 2;
    const CC: u8 = 3; // Carry

    // Memory layout constants
    const HEAP_START: u16 = 0x5000;
    const HEAP_PTR: u16 = 0x4000;
    const ENV_PTR: u16 = 0x4004;      // Environment (alist of bindings)
    const SYM_HASH: u16 = 0x4006;     // Scratch for symbol hash calculation
    const INPUT_BUF: u16 = 0x4100;
    const INPUT_POS: u16 = 0x4002;
    const INPUT_LEN: u16 = 0x4003;

    // LISP constants
    const LISP_NIL: u16 = 0x0002;
    const LISP_T: u16 = 0x0006;
    const LISP_EOF: u16 = 0x000A;
    const TAG_FIXNUM: u8 = 0x01;
    const TAG_SPECIAL: u8 = 0x02;
    const TAG_SYMBOL: u8 = 0x03;

    // Multi-character symbol constants
    // These use high byte to distinguish from single-char symbols
    // Format: (id << 8) | TAG_SYMBOL
    const SYM_QUOTE: u16 = 0x0103;    // 'QUOTE' keyword (reader generates this for ')
    const SYM_IF: u16 = 0x0203;       // 'IF' keyword
    const SYM_LAMBDA: u16 = 0x0303;   // 'LAMBDA' keyword
    const SYM_DEFINE: u16 = 0x0403;   // 'DEFINE' keyword
    const SYM_SETQ: u16 = 0x0503;     // 'SETQ' keyword
    const SYM_LET: u16 = 0x0603;      // 'LET' keyword
    const SYM_PROGN: u16 = 0x0703;    // 'PROGN' keyword
    const SYM_COND: u16 = 0x0803;     // 'COND' keyword
    // List operations
    const SYM_CAR: u16 = 0x0903;      // 'CAR'
    const SYM_CDR: u16 = 0x0A03;      // 'CDR'
    const SYM_CONS: u16 = 0x0B03;     // 'CONS'
    const SYM_LIST: u16 = 0x0C03;     // 'LIST'
    // Predicates
    const SYM_NULL: u16 = 0x0D03;     // 'NULL'
    const SYM_ATOM: u16 = 0x0E03;     // 'ATOM'
    const SYM_EQ: u16 = 0x0F03;       // 'EQ'
    const SYM_NUMBERP: u16 = 0x1003;  // 'NUMBERP'
    const SYM_SYMBOLP: u16 = 0x1103;  // 'SYMBOLP'
    const SYM_CONSP: u16 = 0x1203;    // 'CONSP'
    // I/O
    const SYM_PRINT: u16 = 0x1303;    // 'PRINT'
    const SYM_READ: u16 = 0x1403;     // 'READ'
    // Logic
    const SYM_NOT: u16 = 0x1503;      // 'NOT'
    const SYM_AND: u16 = 0x1603;      // 'AND'
    const SYM_OR: u16 = 0x1703;       // 'OR'
    // Memory operations
    const SYM_PEEK: u16 = 0x1803;     // 'PEEK'
    const SYM_POKE: u16 = 0x1903;     // 'POKE'
    const SYM_DUMP: u16 = 0x1A03;     // 'DUMP'
    // Floating point operations
    const SYM_FLOATP: u16 = 0x1B03;   // 'FLOATP' - float predicate
    const SYM_FMAKE: u16 = 0x1C03;    // 'FMAKE' - make float from mantissa/exp
    const SYM_FADD: u16 = 0x1D03;     // 'FADD' - float add
    const SYM_FSUB: u16 = 0x1E03;     // 'FSUB' - float subtract
    const SYM_FMUL: u16 = 0x1F03;     // 'FMUL' - float multiply
    const SYM_FDIV: u16 = 0x2003;     // 'FDIV' - float divide
    const SYM_FTOI: u16 = 0x2103;     // 'FTOI' - float to integer
    const SYM_ITOF: u16 = 0x2203;     // 'ITOF' - integer to float

    // Float tag (uses low 2 bits like other types)
    const TAG_FLOAT: u8 = 0x04;       // Boxed BCD float

    /// Generate the complete Z80 LISP interpreter binary
    pub fn generate(&mut self) -> Vec<u8> {
        // Memory map:
        // 0x0000-0x00FF: RST vectors
        // 0x0100-0x3FFF: Code
        // 0x4000-0x4FFF: Data/globals
        // 0x5000-0x7DFF: Heap
        // 0x7E00-0x7FFF: Stack

        // Generate RST vectors (at start of ROM)
        let mut rom = vec![0u8; 0x100];

        // RST 0 (reset): JP init
        rom[0x00] = 0xC3;
        rom[0x01] = 0x00;
        rom[0x02] = 0x01;

        // RST 8, 10, 18, 20, 28, 30: RET
        for addr in [0x08, 0x10, 0x18, 0x20, 0x28, 0x30].iter() {
            rom[*addr] = 0xC9;
        }

        // RST 38 (IM 1 interrupt): RETI
        rom[0x38] = 0xED;
        rom[0x39] = 0x4D;

        // NMI at 0x66: RETN
        rom[0x66] = 0xED;
        rom[0x67] = 0x45;

        // Generate main code
        self.generate_init();
        self.generate_acia();
        self.generate_memory();
        self.generate_reader();
        self.generate_printer();
        self.generate_interpreter();
        self.generate_builtins();
        self.generate_repl();
        self.generate_banner();

        // Resolve forward references
        self.resolve_refs();

        // Combine RST vectors and code
        rom.extend_from_slice(&self.code);

        // Pad to 32KB
        rom.resize(32768, 0xFF);

        rom
    }

    /// Generate initialization code
    fn generate_init(&mut self) {
        self.label("init");

        // DI - disable interrupts
        self.di();

        // LD SP, 0x8000 - set stack pointer
        self.ld_rr_nn(Self::SP, 0x8000);

        // Initialize ACIA
        self.call_label("acia_init");

        // Initialize heap
        self.call_label("heap_init");

        // Initialize environment to NIL
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ld_mem_hl(Self::ENV_PTR);

        // Print banner
        self.call_label("print_banner");

        // Jump to REPL
        self.jp_label("repl");
    }

    /// Generate ACIA (serial) driver
    fn generate_acia(&mut self) {
        const ACIA_CTRL: u8 = 0x80;
        const ACIA_DATA: u8 = 0x81;
        const ACIA_RDRF: u8 = 0x01;
        const ACIA_TDRE: u8 = 0x02;

        // acia_init: Initialize ACIA
        self.label("acia_init");
        self.ld_r_n(Self::A, 0x03);  // Master reset
        self.out_n_a(ACIA_CTRL);
        self.ld_r_n(Self::A, 0x15);  // 8N1, /16
        self.out_n_a(ACIA_CTRL);
        self.ret();

        // acia_tx_ready: Check if TX ready (Z flag set if ready)
        self.label("acia_tx_ready");
        self.in_a_n(ACIA_CTRL);
        self.and_n(ACIA_TDRE);
        self.ret();

        // acia_rx_ready: Check if RX ready (Z flag clear if ready)
        self.label("acia_rx_ready");
        self.in_a_n(ACIA_CTRL);
        self.and_n(ACIA_RDRF);
        self.ret();

        // acia_putc: Output character in A
        self.label("acia_putc");
        self.push(Self::AF);
        self.label("acia_putc_wait");
        self.in_a_n(ACIA_CTRL);
        self.and_n(ACIA_TDRE);
        self.jr_cc(Self::Z, -6); // Loop until ready
        self.pop(Self::AF);
        self.out_n_a(ACIA_DATA);
        self.ret();

        // acia_getc: Get character into A (blocking)
        self.label("acia_getc");
        self.in_a_n(ACIA_CTRL);
        self.and_n(ACIA_RDRF);
        self.jr_cc(Self::Z, -6); // Loop until ready
        self.in_a_n(ACIA_DATA);
        self.ret();

        // acia_puts: Print string at HL
        self.label("acia_puts");
        self.ld_a_hl_ind();
        self.or_r(Self::A);      // Check for null terminator
        self.ret_cc(Self::Z);
        self.call_label("acia_putc");
        self.inc_rr(Self::HL);
        self.jr_label("acia_puts");

        // acia_newline: Print CR+LF
        self.label("acia_newline");
        self.ld_r_n(Self::A, 13);
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, 10);
        self.jp_label("acia_putc");

        // print_hex_nibble: Print nibble in A (0-15) as hex char
        // (Must come first so print_hex_byte can call it)
        self.label("print_hex_nibble");
        self.and_n(0x0F);
        self.cp_n(10);
        self.jr_cc_label(Self::CC, "hex_digit");  // CC = Carry condition
        self.add_a_n(b'A' - 10);
        self.jp_label("acia_putc");
        self.label("hex_digit");
        self.add_a_n(b'0');
        self.jp_label("acia_putc");

        // print_hex_byte: Print byte in A as two hex digits
        self.label("print_hex_byte");
        self.push(Self::AF);
        // Print high nibble
        self.rrca();
        self.rrca();
        self.rrca();
        self.rrca();
        self.and_n(0x0F);
        // Convert nibble to hex char
        self.cp_n(10);
        self.jr_cc_label(Self::CC, "hex_byte_hi_num"); // if < 10, jump to add '0' (CC = Carry)
        self.add_a_n(b'A' - 10);  // >= 10: A-F
        self.jr_label("hex_byte_hi_out");
        self.label("hex_byte_hi_num");
        self.add_a_n(b'0');  // < 10: 0-9
        self.label("hex_byte_hi_out");
        self.call_label("acia_putc");
        // Print low nibble
        self.pop(Self::AF);
        self.and_n(0x0F);
        self.cp_n(10);
        self.jr_cc_label(Self::CC, "hex_byte_lo_num");  // CC = Carry condition
        self.add_a_n(b'A' - 10);
        self.jp_label("acia_putc");
        self.label("hex_byte_lo_num");
        self.add_a_n(b'0');
        self.jp_label("acia_putc");

        // acia_getline: Read line into INPUT_BUF
        self.label("acia_getline");
        self.ld_rr_nn(Self::HL, Self::INPUT_BUF);
        self.ld_r_n(Self::B, 0);

        self.label("getline_loop");
        self.call_label("acia_getc");
        // Check for CR/LF
        self.cp_n(13);
        self.jr_cc_label(Self::Z, "getline_done");
        self.cp_n(10);
        self.jr_cc_label(Self::Z, "getline_done");
        // Check for backspace
        self.cp_n(8);
        self.jr_cc_label(Self::Z, "getline_bs");
        self.cp_n(127);
        self.jr_cc_label(Self::Z, "getline_bs");
        // Check buffer full
        self.ld_r_r(Self::C, Self::A);
        self.ld_r_r(Self::A, Self::B);
        self.cp_n(250);
        self.jr_cc_label(Self::NC, "getline_loop");
        // Store char
        self.ld_r_r(Self::A, Self::C);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.inc_rr(Self::HL);
        self.inc_r(Self::B);
        // Echo
        self.call_label("acia_putc");
        self.jr_label("getline_loop");

        self.label("getline_bs");
        self.ld_r_r(Self::A, Self::B);
        self.or_r(Self::A);
        self.jr_cc_label(Self::Z, "getline_loop");
        self.dec_r(Self::B);
        self.dec_rr(Self::HL);
        self.ld_r_n(Self::A, 8);
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b' ');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, 8);
        self.call_label("acia_putc");
        self.jr_label("getline_loop");

        self.label("getline_done");
        self.ld_r_n(Self::A, 0);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.ld_r_r(Self::A, Self::B);
        self.ld_mem_a(Self::INPUT_LEN);
        self.xor_r(Self::A);
        self.ld_mem_a(Self::INPUT_POS);
        self.call_label("acia_newline");
        self.ret();
    }

    /// Generate memory management
    fn generate_memory(&mut self) {
        // heap_init: Initialize heap pointer
        self.label("heap_init");
        self.ld_rr_nn(Self::HL, Self::HEAP_START);
        self.ld_mem_hl(Self::HEAP_PTR);
        self.ret();

        // alloc_cons: Allocate a cons cell, returns address in HL
        // Preserves DE and BC
        self.label("alloc_cons");
        self.push(Self::DE);  // Save DE
        self.ld_hl_mem(Self::HEAP_PTR);
        self.push(Self::HL);
        self.ld_rr_nn(Self::DE, 4);
        self.add_hl_rr(Self::DE);
        self.ld_mem_hl(Self::HEAP_PTR);
        self.pop(Self::HL);
        self.pop(Self::DE);  // Restore DE
        self.ret();

        // cons: Create cons cell, DE=car, BC=cdr, returns HL=cell
        self.label("cons");
        self.call_label("alloc_cons");
        self.ld_r_r(Self::A, Self::E);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::A, Self::D);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::A, Self::C);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::A, Self::B);
        self.ld_r_r(Self::HL_IND, Self::A);
        self.dec_rr(Self::HL);
        self.dec_rr(Self::HL);
        self.dec_rr(Self::HL);
        self.ret();

        // alloc_float: Allocate a 6-byte float, returns address in HL (with TAG_FLOAT)
        // BCD Float format (6 bytes):
        //   Byte 0: Sign (0x00 = positive, 0x80 = negative)
        //   Byte 1: Exponent (biased by 128: exp 128 = 10^0)
        //   Bytes 2-5: BCD mantissa (8 digits, big-endian, normalized)
        // Returns pointer with TAG_FLOAT in low bits
        self.label("alloc_float");
        self.push(Self::DE);
        self.ld_hl_mem(Self::HEAP_PTR);
        self.push(Self::HL);
        self.ld_rr_nn(Self::DE, 6);  // 6 bytes for float
        self.add_hl_rr(Self::DE);
        self.ld_mem_hl(Self::HEAP_PTR);
        self.pop(Self::HL);
        self.ld_r_r(Self::A, Self::L);
        self.or_n(Self::TAG_FLOAT);  // Add float tag
        self.ld_r_r(Self::L, Self::A);
        self.pop(Self::DE);
        self.ret();

        // float_ptr: Strip tag from float pointer in HL, returns raw address in HL
        self.label("float_ptr");
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0xFC);  // Clear low 2 bits (tag)
        self.ld_r_r(Self::L, Self::A);
        self.ret();

        // is_float: Check if HL is a float, sets Z flag if true
        self.label("is_float");
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x07);  // Check low 3 bits
        self.cp_n(Self::TAG_FLOAT);
        self.ret();

        // car: Get car of cons in HL, result in HL
        self.label("car");
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::L);
        self.ret_cc(Self::Z); // NIL
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.ret_cc(Self::NZ); // Not a cons
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);
        self.ex_de_hl();
        self.ret();

        // cdr: Get cdr of cons in HL, result in HL
        self.label("cdr");
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::L);
        self.ret_cc(Self::Z);
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.ret_cc(Self::NZ);
        self.inc_rr(Self::HL);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);
        self.ex_de_hl();
        self.ret();

        // is_nil: Z flag set if HL == NIL
        self.label("is_nil");
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::L);
        self.ret_cc(Self::Z);
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_NIL as u8);
        self.ret_cc(Self::NZ);
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_NIL >> 8) as u8);
        self.ret();

        // make_fixnum: Create fixnum from DE, result in HL
        self.label("make_fixnum");
        self.ld_r_r(Self::H, Self::D);
        self.ld_r_r(Self::L, Self::E);
        self.add_hl_rr(Self::HL);
        self.add_hl_rr(Self::HL);
        self.ld_r_r(Self::A, Self::L);
        self.or_n(Self::TAG_FIXNUM);
        self.ld_r_r(Self::L, Self::A);
        self.ret();

        // fixnum_val: Get value from fixnum in HL, result in DE
        self.label("fixnum_val");
        self.ld_r_r(Self::D, Self::H);
        self.ld_r_r(Self::E, Self::L);
        self.sra_r(Self::D);
        self.rr_r(Self::E);
        self.sra_r(Self::D);
        self.rr_r(Self::E);
        self.ret();
    }

    /// Generate S-expression reader
    fn generate_reader(&mut self) {
        // read_expr: Read S-expression, returns value in HL
        // Uses INPUT_BUF and INPUT_POS for input buffering
        self.label("read_expr");
        self.call_label("skip_whitespace");
        self.call_label("peek_char");
        self.cp_n(0);  // End of input?
        self.jr_cc_label(Self::NZ, "read_not_eof");
        // Return EOF marker
        self.ld_rr_nn(Self::HL, Self::LISP_EOF);
        self.ret();

        self.label("read_not_eof");
        self.cp_n(b'(');
        self.jp_cc_label(Self::Z, "read_list");
        self.cp_n(b')');
        self.jp_cc_label(Self::Z, "read_error");
        self.cp_n(b'\'');
        self.jp_cc_label(Self::Z, "read_quote");
        self.cp_n(b'-');
        self.jp_cc_label(Self::Z, "read_maybe_number");
        self.cp_n(b'0');
        self.jp_cc_label(Self::CC, "read_symbol"); // < '0' (use JP since read_symbol is too far for JR)
        self.cp_n(b'9' + 1);
        self.jr_cc_label(Self::CC, "read_number"); // <= '9'
        self.jp_label("read_symbol");

        // read_maybe_number: Could be -123 or -foo
        self.label("read_maybe_number");
        self.call_label("get_char");  // consume -
        self.call_label("peek_char");
        self.cp_n(b'0');
        self.jr_cc_label(Self::CC, "read_neg_sym");
        self.cp_n(b'9' + 1);
        self.jr_cc_label(Self::NC, "read_neg_sym");
        // It's a negative number
        self.call_label("read_uint");  // DE = positive value
        // Negate DE using two's complement: -DE = ~DE + 1
        self.ld_r_r(Self::A, Self::E);
        self.cpl();  // A = ~E
        self.ld_r_r(Self::E, Self::A);
        self.ld_r_r(Self::A, Self::D);
        self.cpl();  // A = ~D
        self.ld_r_r(Self::D, Self::A);
        self.inc_rr(Self::DE);  // DE = ~DE + 1 = -original
        self.jp_label("make_fixnum");

        self.label("read_neg_sym");
        // Put - back and read as symbol
        self.ld_a_mem(Self::INPUT_POS);
        self.dec_r(Self::A);
        self.ld_mem_a(Self::INPUT_POS);
        self.jp_label("read_symbol");

        // read_number: Read positive integer
        self.label("read_number");
        self.call_label("read_uint");
        self.jp_label("make_fixnum");

        // read_uint: Read unsigned integer into DE
        // Simple version using repeated addition
        self.label("read_uint");
        self.ld_rr_nn(Self::DE, 0);

        self.label("read_uint_loop");
        self.push(Self::DE);  // save current value
        self.call_label("peek_char");
        self.pop(Self::DE);   // restore current value
        self.cp_n(b'0');
        self.ret_cc(Self::CC);  // < '0', done
        self.cp_n(b'9' + 1);
        self.ret_cc(Self::NC); // > '9', done

        self.push(Self::DE);  // save current value
        self.call_label("get_char");
        self.sub_n(b'0');
        self.ld_r_r(Self::C, Self::A);  // C = digit
        self.pop(Self::DE);   // DE = current value

        // DE = DE * 10 + C
        // Use simple method: DE*10 = (DE*2)*5 = (DE*2)*4 + (DE*2)
        self.push(Self::BC);  // save digit
        self.ex_de_hl();      // HL = value
        self.add_hl_rr(Self::HL); // HL = value * 2
        self.ld_r_r(Self::D, Self::H);
        self.ld_r_r(Self::E, Self::L); // DE = value * 2
        self.add_hl_rr(Self::HL); // HL = value * 4
        self.add_hl_rr(Self::HL); // HL = value * 8
        self.add_hl_rr(Self::DE); // HL = value * 10
        // Add digit
        self.pop(Self::BC);  // restore digit in C
        self.ld_r_n(Self::D, 0);
        self.ld_r_r(Self::E, Self::C);
        self.add_hl_rr(Self::DE); // HL = value * 10 + digit
        self.ex_de_hl();  // DE = new value
        self.jr_label("read_uint_loop");

        // read_list: Read a list starting with (
        self.label("read_list");
        self.call_label("get_char");  // consume (
        self.call_label("skip_whitespace");
        self.call_label("peek_char");
        self.cp_n(b')');
        self.jr_cc_label(Self::NZ, "read_list_items");
        // Empty list
        self.call_label("get_char");  // consume )
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        self.label("read_list_items");
        // Read first item
        self.call_label("read_expr");
        self.push(Self::HL);  // save first item

        // Read rest of list
        self.call_label("read_list_tail");
        // HL = rest of list
        self.pop(Self::DE);  // DE = first item
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = cdr
        // cons(DE, BC) -> HL
        self.jp_label("cons");

        // read_list_tail: Read remaining items until )
        self.label("read_list_tail");
        self.call_label("skip_whitespace");
        self.call_label("peek_char");
        self.cp_n(b')');
        self.jr_cc_label(Self::NZ, "read_list_more");
        self.call_label("get_char");  // consume )
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        self.label("read_list_more");
        // Check for dotted pair
        self.cp_n(b'.');
        self.jr_cc_label(Self::Z, "read_dotted");
        // Regular list item
        self.call_label("read_expr");
        self.push(Self::HL);
        self.call_label("read_list_tail");
        self.pop(Self::DE);
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);
        self.jp_label("cons");

        // read_dotted: Read dotted pair (a . b)
        self.label("read_dotted");
        self.call_label("get_char");  // consume .
        self.call_label("skip_whitespace");
        self.call_label("read_expr");  // read cdr
        self.push(Self::HL);
        self.call_label("skip_whitespace");
        self.call_label("get_char");  // consume )
        self.pop(Self::HL);
        self.ret();

        // read_quote: 'x -> (QUOTE x)
        self.label("read_quote");
        self.call_label("get_char");  // consume '
        self.call_label("read_expr");
        // Build (QUOTE expr)
        self.push(Self::HL);  // save expr
        // Make (expr . NIL)
        self.ex_de_hl();  // DE = expr
        self.ld_rr_nn(Self::BC, Self::LISP_NIL);
        self.call_label("cons");
        // HL = (expr . NIL)
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = (expr)
        // Make QUOTE symbol - use SYM_QUOTE constant
        self.ld_rr_nn(Self::DE, Self::SYM_QUOTE);  // QUOTE symbol marker
        self.call_label("cons");
        self.pop(Self::DE);  // clean up (not needed actually)
        self.ret();

        // read_symbol: Read a symbol
        self.label("read_symbol");
        // For now, handle T and NIL specially, return symbol marker for others
        self.call_label("get_char");
        self.push(Self::AF);  // save first char
        // Check if single char followed by delimiter
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jr_cc_label(Self::NZ, "read_sym_multi");
        // Single char symbol
        self.pop(Self::AF);
        self.cp_n(b'T');
        self.jr_cc_label(Self::NZ, "read_sym_not_t");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        self.label("read_sym_not_t");
        // Return as symbol (simplified - just return marker + char)
        self.ld_r_r(Self::L, Self::A);
        self.ld_r_n(Self::H, 0);
        self.add_hl_rr(Self::HL);
        self.add_hl_rr(Self::HL);
        self.ld_r_r(Self::A, Self::L);
        self.or_n(Self::TAG_SYMBOL);
        self.ld_r_r(Self::L, Self::A);
        self.ret();

        self.label("read_sym_multi");
        self.pop(Self::AF);  // first char in A
        // Save start position in SYM_HASH for potential rollback
        // Current position is at second char; we need position of first char
        self.push(Self::AF);          // Save first char
        self.ld_a_mem(Self::INPUT_POS);
        self.dec_r(Self::A);          // Position of first char
        self.ld_mem_a(Self::SYM_HASH);  // Save start position
        self.pop(Self::AF);           // Restore first char in A
        // Check for N-words: NIL, NOT, NULL
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_not_n");
        self.call_label("peek_char");
        self.cp_n(b'I');
        self.jr_cc_label(Self::NZ, "read_sym_n_not_i");
        // NIL
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        self.label("read_sym_n_not_i");
        // Check for NOT or NULL
        self.cp_n(b'O');
        self.jr_cc_label(Self::NZ, "read_sym_n_not_o");
        // NOT
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_NOT);
        self.ret();

        self.label("read_sym_n_not_o");
        // NULL or NUMBERP
        self.cp_n(b'U');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // U
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jr_cc_label(Self::NZ, "read_sym_nu_not_l");
        // NULL
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_NULL);
        self.ret();

        self.label("read_sym_nu_not_l");
        // NUMBERP
        self.cp_n(b'M');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.cp_n(b'B');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // B
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'R');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // R
        self.call_label("peek_char");
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // P
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_NUMBERP);
        self.ret();

        self.label("read_sym_not_n");
        // Check for IF
        self.cp_n(b'I');
        self.jr_cc_label(Self::NZ, "read_sym_not_if");
        self.call_label("peek_char");
        self.cp_n(b'F');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_IF);
        self.ret();

        self.label("read_sym_not_if");
        // Check for LAMBDA, LIST, LET (all start with L)
        self.cp_n(b'L');
        self.jp_cc_label(Self::NZ, "read_sym_not_l");
        self.call_label("peek_char");
        self.cp_n(b'A');
        self.jr_cc_label(Self::NZ, "read_sym_l_not_a");
        // LA... -> LAMBDA
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.cp_n(b'M');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.cp_n(b'B');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // B
        self.call_label("peek_char");
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.cp_n(b'A');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_LAMBDA);
        self.ret();

        self.label("read_sym_l_not_a");
        // LI... -> LIST, LE... -> LET
        self.cp_n(b'I');
        self.jr_cc_label(Self::NZ, "read_sym_l_not_i");
        // LIST
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.cp_n(b'S');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // S
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_LIST);
        self.ret();

        self.label("read_sym_l_not_i");
        // LE... -> LET
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_LET);
        self.ret();

        self.label("read_sym_not_l");
        // Check for DEFINE, DUMP (start with D)
        self.cp_n(b'D');
        self.jr_cc_label(Self::NZ, "read_sym_not_d");
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jr_cc_label(Self::NZ, "read_sym_d_not_e");
        // DEFINE
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'F');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // F
        self.call_label("peek_char");
        self.cp_n(b'I');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // N
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_DEFINE);
        self.ret();

        self.label("read_sym_d_not_e");
        // DUMP
        self.cp_n(b'U');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // U
        self.call_label("peek_char");
        self.cp_n(b'M');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // P
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_DUMP);
        self.ret();

        self.label("read_sym_not_d");
        // Check for CAR, CDR, CONS, COND, CONSP (all start with C)
        self.cp_n(b'C');
        self.jp_cc_label(Self::NZ, "read_sym_not_c");
        self.call_label("peek_char");
        self.cp_n(b'A');
        self.jr_cc_label(Self::NZ, "read_sym_c_not_a");
        // CA... -> CAR
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.cp_n(b'R');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // R
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_CAR);
        self.ret();

        self.label("read_sym_c_not_a");
        self.cp_n(b'D');
        self.jr_cc_label(Self::NZ, "read_sym_c_not_d");
        // CD... -> CDR
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.cp_n(b'R');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // R
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_CDR);
        self.ret();

        self.label("read_sym_c_not_d");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        // CO... -> CONS or COND or CONSP
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // N
        self.call_label("peek_char");
        self.cp_n(b'S');
        self.jr_cc_label(Self::NZ, "read_sym_con_not_s");
        // CONS or CONSP
        self.call_label("get_char");  // S
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jr_cc_label(Self::NZ, "read_sym_consp");
        self.ld_rr_nn(Self::HL, Self::SYM_CONS);
        self.ret();
        self.label("read_sym_consp");
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // P
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_CONSP);
        self.ret();

        self.label("read_sym_con_not_s");
        // COND
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_COND);
        self.ret();

        self.label("read_sym_not_c");
        // Check for ATOM, AND
        self.cp_n(b'A');
        self.jr_cc_label(Self::NZ, "read_sym_not_a");
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jr_cc_label(Self::NZ, "read_sym_a_not_t");
        // ATOM
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'M');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_ATOM);
        self.ret();

        self.label("read_sym_a_not_t");
        // AND
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // N
        self.call_label("peek_char");
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_AND);
        self.ret();

        self.label("read_sym_not_a");
        // Check for EQ
        self.cp_n(b'E');
        self.jr_cc_label(Self::NZ, "read_sym_not_e");
        self.call_label("peek_char");
        self.cp_n(b'Q');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // Q
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_EQ);
        self.ret();

        self.label("read_sym_not_e");
        // Check for F-words: FLOATP, FADD, FSUB, FMUL, FDIV, FTOI, ITOF
        self.cp_n(b'F');
        self.jp_cc_label(Self::NZ, "read_sym_not_f");
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_l");
        // FLOATP
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'A');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // P
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FLOATP);
        self.ret();

        self.label("read_sym_f_not_l");
        self.cp_n(b'A');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_a");
        // FADD
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FADD);
        self.ret();

        self.label("read_sym_f_not_a");
        self.cp_n(b'S');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_s");
        // FSUB
        self.call_label("get_char");  // S
        self.call_label("peek_char");
        self.cp_n(b'U');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // U
        self.call_label("peek_char");
        self.cp_n(b'B');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // B
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FSUB);
        self.ret();

        self.label("read_sym_f_not_s");
        self.cp_n(b'M');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_m");
        // FMUL
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.cp_n(b'U');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // U
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FMUL);
        self.ret();

        self.label("read_sym_f_not_m");
        self.cp_n(b'D');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_d");
        // FDIV
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.cp_n(b'I');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.cp_n(b'V');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // V
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FDIV);
        self.ret();

        self.label("read_sym_f_not_d");
        self.cp_n(b'T');
        self.jr_cc_label(Self::NZ, "read_sym_f_not_t");
        // FTOI
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'I');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_FTOI);
        self.ret();

        self.label("read_sym_f_not_t");
        // Not an F-word we recognize
        self.jp_label("read_sym_intern");

        self.label("read_sym_not_f");
        // Check for ITOF (I-word)
        self.cp_n(b'I');
        self.jp_cc_label(Self::NZ, "read_sym_not_i");
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jr_cc_label(Self::NZ, "read_sym_i_not_t");
        // ITOF
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'F');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // F
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_ITOF);
        self.ret();

        self.label("read_sym_i_not_t");
        // IF is handled elsewhere, so fall through
        self.jp_label("read_sym_intern");

        self.label("read_sym_not_i");

        // Check for OR
        self.cp_n(b'O');
        self.jr_cc_label(Self::NZ, "read_sym_not_o");
        self.call_label("peek_char");
        self.cp_n(b'R');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // R
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_OR);
        self.ret();

        self.label("read_sym_not_o");
        // Check for P-words: PRINT, PROGN, PEEK, POKE
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_not_p");
        self.call_label("peek_char");
        self.cp_n(b'R');
        self.jr_cc_label(Self::NZ, "read_sym_p_not_r");
        // PR... -> PRINT or PROGN
        self.call_label("get_char");  // R
        self.call_label("peek_char");
        self.cp_n(b'I');
        self.jr_cc_label(Self::NZ, "read_sym_pr_not_i");
        // PRINT
        self.call_label("get_char");  // I
        self.call_label("peek_char");
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // N
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_PRINT);
        self.ret();

        self.label("read_sym_pr_not_i");
        // PROGN
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'G');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // G
        self.call_label("peek_char");
        self.cp_n(b'N');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // N
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_PROGN);
        self.ret();

        self.label("read_sym_p_not_r");
        // PE... -> PEEK, PO... -> POKE
        self.cp_n(b'E');
        self.jr_cc_label(Self::NZ, "read_sym_p_not_e");
        // PEEK
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'K');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // K
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_PEEK);
        self.ret();

        self.label("read_sym_p_not_e");
        // POKE
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'K');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // K
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_POKE);
        self.ret();

        self.label("read_sym_not_p");
        // Check for QUOTE (starts with Q)
        self.cp_n(b'Q');
        self.jp_cc_label(Self::NZ, "read_sym_not_q");
        self.call_label("peek_char");
        self.cp_n(b'U');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // U
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_QUOTE);
        self.ret();

        self.label("read_sym_not_q");
        // Check for SETQ, SYMBOLP (all start with S)
        self.cp_n(b'S');
        self.jp_cc_label(Self::NZ, "read_sym_not_s");
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jr_cc_label(Self::NZ, "read_sym_s_not_e");
        // SETQ
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'T');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // T
        self.call_label("peek_char");
        self.cp_n(b'Q');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // Q
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_SETQ);
        self.ret();

        self.label("read_sym_s_not_e");
        // SYMBOLP
        self.cp_n(b'Y');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // Y
        self.call_label("peek_char");
        self.cp_n(b'M');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // M
        self.call_label("peek_char");
        self.cp_n(b'B');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // B
        self.call_label("peek_char");
        self.cp_n(b'O');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // O
        self.call_label("peek_char");
        self.cp_n(b'L');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // L
        self.call_label("peek_char");
        self.cp_n(b'P');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // P
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_SYMBOLP);
        self.ret();

        self.label("read_sym_not_s");
        // Check for READ (starts with R)
        self.cp_n(b'R');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("peek_char");
        self.cp_n(b'E');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // E
        self.call_label("peek_char");
        self.cp_n(b'A');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // A
        self.call_label("peek_char");
        self.cp_n(b'D');
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.call_label("get_char");  // D
        self.call_label("peek_char");
        self.call_label("is_delimiter");
        self.jp_cc_label(Self::NZ, "read_sym_intern");
        self.ld_rr_nn(Self::HL, Self::SYM_READ);
        self.ret();

        self.label("read_sym_intern");
        // Compute hash of symbol string
        // Restore input position to start of symbol (saved at SYM_HASH)
        self.ld_a_mem(Self::SYM_HASH);
        self.ld_mem_a(Self::INPUT_POS);
        // Now read and hash all chars
        self.call_label("get_char");  // Get first char
        self.ld_r_r(Self::L, Self::A);
        self.ld_r_n(Self::H, 0x1F);  // Start with 0x1F00 | first_char

        self.label("read_sym_hash_loop");
        self.push(Self::HL);              // Save hash value
        self.call_label("peek_char");     // HL destroyed, A = next char
        self.call_label("is_delimiter");
        self.pop(Self::HL);               // Restore hash value
        self.jr_cc_label(Self::Z, "read_sym_hash_done");
        self.push(Self::HL);              // Save hash value again
        self.call_label("get_char");      // HL destroyed, A = char consumed
        self.ld_r_r(Self::C, Self::A);    // save char in C
        self.pop(Self::HL);               // Restore hash value
        // Simple hash: rotate HL left and XOR with new char
        // HL = (HL << 1) ^ A
        self.add_hl_rr(Self::HL);   // HL = HL << 1 (carry gets bit 15)
        self.ld_r_n(Self::A, 0);
        self.adc_a_r(Self::A);      // A = carry bit (from bit 15)
        self.or_r(Self::L);         // A = L | carry
        self.xor_r(Self::C);        // XOR with char
        self.ld_r_r(Self::L, Self::A);  // L = result
        self.jr_label("read_sym_hash_loop");

        self.label("read_sym_hash_done");
        // Make sure high byte is in valid symbol range (0x20-0x9F to avoid collisions with builtins)
        // Builtins use high bytes 0x01-0x1A, so we want 0x20+
        self.ld_r_r(Self::A, Self::H);
        self.and_n(0x7F);   // Clear bit 7 (range 0x00-0x7F)
        self.add_a_n(0x20);   // Offset to 0x20-0x9F range
        self.ld_r_r(Self::H, Self::A);
        // Set symbol tag in low byte (keep some hash bits)
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0xFC);   // Clear tag bits
        self.or_n(Self::TAG_SYMBOL);  // Set symbol tag
        self.ld_r_r(Self::L, Self::A);
        self.ret();

        // read_error: Unexpected )
        self.label("read_error");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // Helper: skip_whitespace
        self.label("skip_whitespace");
        self.label("skip_ws_loop");
        self.call_label("peek_char");
        self.cp_n(b' ');
        self.jr_cc_label(Self::Z, "skip_ws_next");
        self.cp_n(b'\t');
        self.jr_cc_label(Self::Z, "skip_ws_next");
        self.cp_n(b'\n');
        self.jr_cc_label(Self::Z, "skip_ws_next");
        self.cp_n(b'\r');
        self.jr_cc_label(Self::Z, "skip_ws_next");
        self.cp_n(b';');  // Comment
        self.jr_cc_label(Self::Z, "skip_comment");
        self.ret();

        self.label("skip_ws_next");
        self.call_label("get_char");
        self.jr_label("skip_ws_loop");

        self.label("skip_comment");
        self.call_label("get_char");
        self.cp_n(b'\n');
        self.jr_cc_label(Self::Z, "skip_ws_loop");
        self.cp_n(0);
        self.jr_cc_label(Self::NZ, "skip_comment");
        self.ret();

        // Helper: peek_char - get next char without consuming
        self.label("peek_char");
        self.ld_a_mem(Self::INPUT_POS);
        self.ld_r_r(Self::L, Self::A);
        self.ld_r_n(Self::H, (Self::INPUT_BUF >> 8) as u8);
        self.ld_r_r(Self::A, Self::HL_IND);
        self.ret();

        // Helper: get_char - get next char and advance
        self.label("get_char");
        self.ld_a_mem(Self::INPUT_POS);
        self.ld_r_r(Self::L, Self::A);
        self.ld_r_n(Self::H, (Self::INPUT_BUF >> 8) as u8);
        self.ld_r_r(Self::C, Self::HL_IND);
        self.ld_r_r(Self::A, Self::C);
        self.or_r(Self::A);
        self.ret_cc(Self::Z);  // Don't advance past null
        self.ld_a_mem(Self::INPUT_POS);
        self.inc_r(Self::A);
        self.ld_mem_a(Self::INPUT_POS);
        self.ld_r_r(Self::A, Self::C);
        self.ret();

        // Helper: is_delimiter - Z set if A is delimiter
        self.label("is_delimiter");
        self.cp_n(0);
        self.ret_cc(Self::Z);
        self.cp_n(b' ');
        self.ret_cc(Self::Z);
        self.cp_n(b'\t');
        self.ret_cc(Self::Z);
        self.cp_n(b'\n');
        self.ret_cc(Self::Z);
        self.cp_n(b'\r');
        self.ret_cc(Self::Z);
        self.cp_n(b'(');
        self.ret_cc(Self::Z);
        self.cp_n(b')');
        self.ret_cc(Self::Z);
        self.cp_n(b'\'');
        self.ret_cc(Self::Z);
        self.cp_n(b';');
        self.ret();
    }

    /// XOR n instruction
    fn xor_n(&mut self, n: u8) {
        self.emit(0xEE);
        self.emit(n);
    }

    /// Generate printer
    fn generate_printer(&mut self) {
        // print_expr: Print S-expression in HL
        self.label("print_expr");
        // Check type tag
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        // Check for cons (tag = 00)
        self.jp_cc_label(Self::NZ, "print_not_cons");
        // Check for special NIL (HL = 0x0000 or 0x0002)
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::L);
        self.jp_cc_label(Self::Z, "print_nil");
        // Check for LISP_NIL constant (0x0002)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_NIL as u8);
        self.jp_cc_label(Self::NZ, "print_cons");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_NIL >> 8) as u8);
        self.jp_cc_label(Self::Z, "print_nil");
        self.jp_label("print_cons");

        self.label("print_not_cons");
        // Check for fixnum (tag = 01)
        self.cp_n(Self::TAG_FIXNUM);
        self.jp_cc_label(Self::Z, "print_fixnum");
        // Check for special (tag = 10)
        self.cp_n(Self::TAG_SPECIAL);
        self.jp_cc_label(Self::Z, "print_special");
        // Symbol (tag = 11)
        self.jp_label("print_symbol");

        // print_nil
        self.label("print_nil");
        self.ld_r_n(Self::A, b'N');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'I');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'L');
        self.jp_label("acia_putc");

        // print_t: Print T
        self.label("print_t");
        self.ld_r_n(Self::A, b'T');
        self.jp_label("acia_putc");

        // print_special: Check for T, NIL, EOF
        self.label("print_special");
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_T as u8);
        self.jr_cc_label(Self::NZ, "print_special_not_t");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_T >> 8) as u8);
        self.jr_cc_label(Self::Z, "print_t");
        self.label("print_special_not_t");
        // Could be EOF or NIL or other
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_EOF as u8);
        self.jr_cc_label(Self::NZ, "print_nil");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_EOF >> 8) as u8);
        self.jr_cc_label(Self::NZ, "print_nil");
        // Print EOF indicator
        self.ld_r_n(Self::A, b'#');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'E');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'O');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'F');
        self.jp_label("acia_putc");

        // print_fixnum: Print number in HL (value is HL >> 2)
        self.label("print_fixnum");
        self.call_label("fixnum_val");  // Get value in DE
        // Check for negative
        self.bit_n_r(7, Self::D);
        self.jr_cc_label(Self::Z, "print_pos");
        // Negative: print - and negate
        self.ld_r_n(Self::A, b'-');
        self.call_label("acia_putc");
        // Negate DE using two's complement: -DE = ~DE + 1
        self.ld_r_r(Self::A, Self::E);
        self.cpl();
        self.ld_r_r(Self::E, Self::A);
        self.ld_r_r(Self::A, Self::D);
        self.cpl();
        self.ld_r_r(Self::D, Self::A);
        self.inc_rr(Self::DE);

        self.label("print_pos");
        // Print number in DE using simple recursive approach
        // Check for 0 special case
        self.ld_r_r(Self::A, Self::D);
        self.or_r(Self::E);
        self.jr_cc_label(Self::NZ, "print_nonzero");
        // DE is 0, just print '0'
        self.ld_r_n(Self::A, b'0');
        self.jp_label("acia_putc");

        self.label("print_nonzero");
        // Push a marker
        self.ld_r_n(Self::A, 0xFF);
        self.push(Self::AF);

        self.label("print_num_loop");
        // Check if DE is 0
        self.ld_r_r(Self::A, Self::D);
        self.or_r(Self::E);
        self.jr_cc_label(Self::Z, "print_num_digits");
        // DE / 10 -> DE quotient, A remainder
        self.call_label("div10");
        self.push(Self::AF);  // save remainder
        self.jr_label("print_num_loop");

        // Print digits in reverse
        self.label("print_num_digits");
        self.pop(Self::AF);
        self.cp_n(0xFF);  // marker?
        self.ret_cc(Self::Z);
        self.add_a_n(b'0');
        self.call_label("acia_putc");
        self.jr_label("print_num_digits");

        // div10: DE / 10 -> DE quotient, A remainder
        // Uses repeated subtraction (simple but slow)
        self.label("div10");
        self.push(Self::BC);
        self.push(Self::HL);
        self.ld_rr_nn(Self::HL, 0);  // quotient in HL

        self.label("div10_loop");
        // Check if DE >= 10
        self.ld_r_r(Self::A, Self::D);
        self.or_r(Self::A);
        self.jr_cc_label(Self::NZ, "div10_sub");  // D != 0, definitely >= 10
        self.ld_r_r(Self::A, Self::E);
        self.cp_n(10);
        self.jr_cc_label(Self::CC, "div10_done");  // E < 10, done

        self.label("div10_sub");
        // Subtract 10 from DE
        self.ld_r_r(Self::A, Self::E);
        self.sub_n(10);
        self.ld_r_r(Self::E, Self::A);
        self.jr_cc_label(Self::NC, "div10_noborrow");
        self.dec_r(Self::D);
        self.label("div10_noborrow");
        self.inc_rr(Self::HL);  // quotient++
        self.jr_label("div10_loop");

        self.label("div10_done");
        // E = remainder, HL = quotient
        self.ld_r_r(Self::A, Self::E);  // remainder in A
        self.ld_r_r(Self::D, Self::H);
        self.ld_r_r(Self::E, Self::L);  // quotient in DE
        self.pop(Self::HL);
        self.pop(Self::BC);
        self.ret();

        // print_symbol: Print symbol
        self.label("print_symbol");
        // For now just print ?SYMBOL
        self.ld_r_n(Self::A, b'?');
        self.jp_label("acia_putc");

        // print_cons: Print list
        self.label("print_cons");
        self.ld_r_n(Self::A, b'(');
        self.call_label("acia_putc");
        self.push(Self::HL);

        // Print car
        self.call_label("car");
        self.call_label("print_expr");

        // Print cdr elements
        self.pop(Self::HL);
        self.call_label("cdr");

        self.label("print_list_tail");
        // Check if cdr is NIL
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "print_list_end");
        // Check if cdr is a cons
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.jr_cc_label(Self::NZ, "print_dotted_cdr");
        // It's a cons - print space and continue
        self.ld_r_n(Self::A, b' ');
        self.call_label("acia_putc");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("print_expr");
        self.pop(Self::HL);
        self.call_label("cdr");
        self.jr_label("print_list_tail");

        self.label("print_dotted_cdr");
        // Print " . <cdr>"
        self.ld_r_n(Self::A, b' ');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'.');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b' ');
        self.call_label("acia_putc");
        self.call_label("print_expr");

        self.label("print_list_end");
        self.ld_r_n(Self::A, b')');
        self.jp_label("acia_putc");
    }

    /// Generate interpreter core
    fn generate_interpreter(&mut self) {
        // interp: Interpret expression in HL, result in HL
        // Environment is stored at ENV_PTR (0x4004)
        self.label("interp");
        // Check type tag
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);

        // Self-evaluating: fixnum (01), special (10)
        self.cp_n(Self::TAG_FIXNUM);
        self.ret_cc(Self::Z);  // Fixnum evaluates to itself
        self.cp_n(Self::TAG_SPECIAL);
        self.ret_cc(Self::Z);  // T, NIL, EOF evaluate to themselves

        // Symbol (11) - look up in environment
        self.cp_n(Self::TAG_SYMBOL);
        self.jp_cc_label(Self::Z, "interp_symbol");

        // Cons (00) - could be NIL or function application
        // Check for NIL first
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::L);
        self.ret_cc(Self::Z);  // NIL evaluates to itself

        // Check for LISP_NIL constant
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_NIL as u8);
        self.jr_cc_label(Self::NZ, "interp_apply");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_NIL >> 8) as u8);
        self.ret_cc(Self::Z);

        // It's a cons - function application
        self.label("interp_apply");
        self.push(Self::HL);  // save the whole expression

        // Get the car (function position)
        self.call_label("car");

        // Check for special forms
        // For now, check if car is a known symbol
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.cp_n(Self::TAG_SYMBOL);
        self.jr_cc_label(Self::NZ, "interp_call");

        // Check for QUOTE (symbol value SYM_QUOTE)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_QUOTE as u8);  // Low byte
        self.jr_cc_label(Self::NZ, "interp_not_quote");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_QUOTE >> 8) as u8);  // High byte
        self.jr_cc_label(Self::NZ, "interp_not_quote");
        // It's QUOTE - return unevaluated second element
        self.pop(Self::HL);  // get original expr
        self.call_label("cdr");  // (QUOTE x) -> (x)
        self.call_label("car");  // -> x
        self.ret();

        self.label("interp_not_quote");
        // Check for IF (symbol value SYM_IF = 0x0203)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_IF as u8);  // Low byte 0x03
        self.jr_cc_label(Self::NZ, "interp_not_if");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_IF >> 8) as u8);  // High byte 0x01
        self.jr_cc_label(Self::NZ, "interp_not_if");
        // It's IF - handle (IF test then else)
        self.pop(Self::HL);  // get original expr
        self.jp_label("special_if");

        self.label("interp_not_if");
        // Check for DEFINE (symbol value SYM_DEFINE = 0x0403)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_DEFINE as u8);  // Low byte 0x03
        self.jr_cc_label(Self::NZ, "interp_not_define");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_DEFINE >> 8) as u8);  // High byte 0x04
        self.jr_cc_label(Self::NZ, "interp_not_define");
        // It's DEFINE - handle (DEFINE name value)
        self.pop(Self::HL);  // get original expr
        self.jp_label("special_define");

        self.label("interp_not_define");
        // SETQ
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_SETQ as u8);
        self.jr_cc_label(Self::NZ, "interp_not_setq");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_SETQ >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_setq");
        self.pop(Self::HL);
        self.jp_label("special_setq");

        self.label("interp_not_setq");
        // Check for LAMBDA (symbol value SYM_LAMBDA = 0x0303)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_LAMBDA as u8);  // Low byte 0x03
        self.jr_cc_label(Self::NZ, "interp_not_lambda");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_LAMBDA >> 8) as u8);  // High byte 0x03
        self.jr_cc_label(Self::NZ, "interp_not_lambda");
        // It's LAMBDA - return lambda object
        self.pop(Self::HL);  // get original expr
        self.jp_label("special_lambda");

        self.label("interp_not_lambda");

        self.label("interp_call");
        // Regular function call
        // For now, we evaluate the car to get function
        // Then evaluate args and apply
        self.pop(Self::HL);  // restore expression

        // For a minimal interpreter, just return the expression
        // Full implementation would:
        // 1. Eval car to get function
        // 2. Eval each cdr element (the args)
        // 3. Apply function to args

        // Simplified: just evaluate arithmetic on 2-element lists
        // Check if car is +, -, *, /
        self.push(Self::HL);
        self.call_label("car");
        // HL = operator symbol
        // Single-char symbols are encoded as (char << 2) | 3 in low byte, 0 in high byte
        // Multi-char symbols have H != 0 (they use upper byte for symbol id)
        // Check H == 0 for single-char, otherwise go to multi-char check
        self.ld_r_r(Self::A, Self::H);
        self.or_r(Self::A);
        self.jr_cc_label(Self::NZ, "interp_multichar");
        // Check for '+' (ASCII 43): L = (43 << 2) | 3 = 175
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'+' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_add");
        // It's addition
        self.pop(Self::HL);  // expression
        self.jp_label("builtin_add");

        self.label("interp_not_add");
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'-' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_sub");
        self.pop(Self::HL);
        self.jp_label("builtin_sub");

        self.label("interp_not_sub");
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'*' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_mul");
        self.pop(Self::HL);
        self.jp_label("builtin_mul");

        self.label("interp_not_mul");
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'/' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_div");
        self.pop(Self::HL);
        self.jp_label("builtin_div");

        self.label("interp_not_div");
        // Check for '%' (ASCII 37): L = (37 << 2) | 3 = 151
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'%' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_mod");
        self.pop(Self::HL);
        self.jp_label("builtin_mod");

        self.label("interp_not_mod");
        // Check for '=' (ASCII 61): L = (61 << 2) | 3 = 247
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'=' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_eq");
        self.pop(Self::HL);
        self.jp_label("builtin_eq");

        self.label("interp_not_eq");
        // Check for '<' (ASCII 60): L = (60 << 2) | 3 = 243
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'<' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_lt");
        self.pop(Self::HL);
        self.jp_label("builtin_lt");

        self.label("interp_not_lt");
        // Check for '>' (ASCII 62): L = (62 << 2) | 3 = 251
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(((b'>' as u16) << 2 | 3) as u8);
        self.jr_cc_label(Self::NZ, "interp_multichar");
        self.pop(Self::HL);
        self.jp_label("builtin_gt");

        // Multi-character symbol builtins
        self.label("interp_multichar");
        // HL = car symbol, check against multi-char builtins
        // Stack still has expression

        // CAR
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_CAR as u8);
        self.jr_cc_label(Self::NZ, "interp_not_car");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_CAR >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_car");
        self.pop(Self::HL);
        self.jp_label("builtin_car_fn");

        self.label("interp_not_car");
        // CDR
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_CDR as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cdr");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_CDR >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cdr");
        self.pop(Self::HL);
        self.jp_label("builtin_cdr_fn");

        self.label("interp_not_cdr");
        // CONS
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_CONS as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cons");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_CONS >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cons");
        self.pop(Self::HL);
        self.jp_label("builtin_cons_fn");

        self.label("interp_not_cons");
        // LIST
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_LIST as u8);
        self.jr_cc_label(Self::NZ, "interp_not_list");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_LIST >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_list");
        self.pop(Self::HL);
        self.jp_label("builtin_list");

        self.label("interp_not_list");
        // ATOM
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_ATOM as u8);
        self.jr_cc_label(Self::NZ, "interp_not_atom");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_ATOM >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_atom");
        self.pop(Self::HL);
        self.jp_label("builtin_atom");

        self.label("interp_not_atom");
        // EQ
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_EQ as u8);
        self.jr_cc_label(Self::NZ, "interp_not_eq_sym");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_EQ >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_eq_sym");
        self.pop(Self::HL);
        self.jp_label("builtin_eq_sym");

        self.label("interp_not_eq_sym");
        // CONSP
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_CONSP as u8);
        self.jr_cc_label(Self::NZ, "interp_not_consp");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_CONSP >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_consp");
        self.pop(Self::HL);
        self.jp_label("builtin_consp");

        self.label("interp_not_consp");
        // COND
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_COND as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cond");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_COND >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_cond");
        self.pop(Self::HL);
        self.jp_label("special_cond");

        self.label("interp_not_cond");
        // LET
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_LET as u8);
        self.jr_cc_label(Self::NZ, "interp_not_let");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_LET >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_let");
        self.pop(Self::HL);
        self.jp_label("special_let");

        self.label("interp_not_let");
        // PROGN
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_PROGN as u8);
        self.jr_cc_label(Self::NZ, "interp_not_progn");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_PROGN >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_progn");
        self.pop(Self::HL);
        self.jp_label("special_progn");

        self.label("interp_not_progn");
        // AND
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_AND as u8);
        self.jr_cc_label(Self::NZ, "interp_not_and");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_AND >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_and");
        self.pop(Self::HL);
        self.jp_label("special_and");

        self.label("interp_not_and");
        // OR
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_OR as u8);
        self.jr_cc_label(Self::NZ, "interp_not_or");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_OR >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_or");
        self.pop(Self::HL);
        self.jp_label("special_or");

        self.label("interp_not_or");
        // NOT
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_NOT as u8);
        self.jr_cc_label(Self::NZ, "interp_not_not");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_NOT >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_not");
        self.pop(Self::HL);
        self.jp_label("builtin_not");

        self.label("interp_not_not");
        // PRINT
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_PRINT as u8);
        self.jr_cc_label(Self::NZ, "interp_not_print");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_PRINT >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_print");
        self.pop(Self::HL);
        self.jp_label("builtin_print");

        self.label("interp_not_print");
        // NULL
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_NULL as u8);
        self.jr_cc_label(Self::NZ, "interp_not_null");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_NULL >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_null");
        self.pop(Self::HL);
        self.jp_label("builtin_null");

        self.label("interp_not_null");
        // NUMBERP
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_NUMBERP as u8);
        self.jr_cc_label(Self::NZ, "interp_not_numberp");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_NUMBERP >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_numberp");
        self.pop(Self::HL);
        self.jp_label("builtin_numberp");

        self.label("interp_not_numberp");
        // SYMBOLP
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_SYMBOLP as u8);
        self.jr_cc_label(Self::NZ, "interp_not_symbolp");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_SYMBOLP >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_symbolp");
        self.pop(Self::HL);
        self.jp_label("builtin_symbolp");

        self.label("interp_not_symbolp");
        // READ
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_READ as u8);
        self.jr_cc_label(Self::NZ, "interp_not_read");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_READ >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_read");
        self.pop(Self::HL);
        self.jp_label("builtin_read");

        self.label("interp_not_read");
        // PEEK
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_PEEK as u8);
        self.jr_cc_label(Self::NZ, "interp_not_peek");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_PEEK >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_peek");
        self.pop(Self::HL);
        self.jp_label("builtin_peek");

        self.label("interp_not_peek");
        // POKE
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_POKE as u8);
        self.jr_cc_label(Self::NZ, "interp_not_poke");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_POKE >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_poke");
        self.pop(Self::HL);
        self.jp_label("builtin_poke");

        self.label("interp_not_poke");
        // DUMP
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_DUMP as u8);
        self.jr_cc_label(Self::NZ, "interp_not_dump");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_DUMP >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_dump");
        self.pop(Self::HL);
        self.jp_label("builtin_dump");

        self.label("interp_not_dump");

        // FLOATP
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FLOATP as u8);
        self.jr_cc_label(Self::NZ, "interp_not_floatp");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FLOATP >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_floatp");
        self.pop(Self::HL);
        self.jp_label("builtin_floatp");

        self.label("interp_not_floatp");

        // ITOF - integer to float
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_ITOF as u8);
        self.jr_cc_label(Self::NZ, "interp_not_itof");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_ITOF >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_itof");
        self.pop(Self::HL);
        self.jp_label("builtin_itof");

        self.label("interp_not_itof");

        // FTOI - float to integer
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FTOI as u8);
        self.jr_cc_label(Self::NZ, "interp_not_ftoi");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FTOI >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_ftoi");
        self.pop(Self::HL);
        self.jp_label("builtin_ftoi");

        self.label("interp_not_ftoi");

        // FADD
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FADD as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fadd");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FADD >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fadd");
        self.pop(Self::HL);
        self.jp_label("builtin_fadd");

        self.label("interp_not_fadd");

        // FSUB
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FSUB as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fsub");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FSUB >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fsub");
        self.pop(Self::HL);
        self.jp_label("builtin_fsub");

        self.label("interp_not_fsub");

        // FMUL
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FMUL as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fmul");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FMUL >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fmul");
        self.pop(Self::HL);
        self.jp_label("builtin_fmul");

        self.label("interp_not_fmul");

        // FDIV
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_FDIV as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fdiv");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_FDIV >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_not_fdiv");
        self.pop(Self::HL);
        self.jp_label("builtin_fdiv");

        self.label("interp_not_fdiv");

        self.label("interp_unknown");
        // Unknown builtin - try user-defined function
        // Stack has original expression, HL has car (function symbol)
        // Look up function in environment
        self.push(Self::HL);     // save function symbol
        self.call_label("interp_symbol");  // HL = looked up value
        // Check if it's a lambda (car is SYM_LAMBDA)
        self.push(Self::HL);     // save function value
        self.call_label("car");  // HL = car of function
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::SYM_LAMBDA as u8);
        self.jp_cc_label(Self::NZ, "interp_not_lambda_call");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::SYM_LAMBDA >> 8) as u8);
        self.jp_cc_label(Self::NZ, "interp_not_lambda_call");
        // It's a lambda! Apply it
        // Entry: Stack = [expr, func_sym, lambda]
        //
        // CLEAN REWRITE - Simple approach:
        // 1. Create frame = (body . old_env) on heap
        // 2. For each param/arg pair: eval arg, bind param
        // 3. Eval body, restore env from frame
        //
        // Key: We only need the frame on heap. Stack can hold loop state
        // since we only do ONE interp call at a time.

        self.pop(Self::HL);      // HL = lambda
        self.pop(Self::DE);      // discard func_sym
        self.pop(Self::DE);      // DE = expr
        // STATE: HL = lambda, DE = expr, stack = []

        // Extract params and body from lambda = (LAMBDA (params...) body)
        self.push(Self::DE);     // save expr: [expr]
        self.call_label("cdr");  // HL = ((params...) body)
        self.push(Self::HL);     // [rest, expr]
        self.call_label("car");  // HL = (params...)
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = params
        self.pop(Self::HL);      // HL = rest = ((params...) body)
        self.call_label("cdr");  // HL = (body)
        self.call_label("car");  // HL = body
        // STATE: HL = body, BC = params, stack = [expr]

        // Create frame = cons(body, old_env) - this survives nested calls
        // cons takes DE=car, BC=cdr, so DE = body, BC = old_env
        self.push(Self::BC);     // save params: [params, expr]
        self.push(Self::HL);     // save body: [body, params, expr]
        self.ld_hl_mem(Self::ENV_PTR);
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = old_env
        self.pop(Self::DE);      // DE = body
        self.call_label("cons"); // HL = (body . old_env) = frame
        // STATE: HL = frame, stack = [params, expr]

        // Get args = cdr(expr)
        self.pop(Self::BC);      // BC = params
        self.pop(Self::DE);      // DE = expr
        self.push(Self::HL);     // save frame: [frame]
        self.push(Self::BC);     // save params: [params, frame]
        self.ex_de_hl();         // HL = expr
        self.call_label("cdr");  // HL = args
        // STATE: HL = args, stack = [params, frame]

        // Now we have: HL = args, stack = [params, frame]
        // Enter binding loop

        self.label("lambda_bind_loop");
        // Entry: HL = args, stack = [params, frame]
        //
        // Check if params is nil (done binding)
        self.push(Self::HL);     // save args: [args, params, frame]
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = params
        self.ex_de_hl();         // HL = params
        self.call_label("is_nil");
        self.pop(Self::HL);      // HL = args
        self.jr_cc_label(Self::Z, "lambda_bind_done");

        // Not done - process one param/arg pair
        // Stack: [params, frame], HL = args
        // We need: param_name, rest_params, current_arg, rest_args
        // Then call interp(current_arg), make binding, extend env, loop

        // Step 1: Get param_name and rest_params from params
        self.push(Self::HL);     // save args: [args, params, frame]
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = params
        self.ex_de_hl();         // HL = params
        self.push(Self::HL);     // [params, args, params, frame]
        self.call_label("car");  // HL = param_name
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = param_name
        self.pop(Self::HL);      // HL = params
        self.push(Self::BC);     // [param_name, args, params, frame]
        self.call_label("cdr");  // HL = rest_params
        self.push(Self::HL);     // [rest_params, param_name, args, params, frame]

        // Step 2: Get current_arg and rest_args from args
        self.ld_rr_nn(Self::HL, 4);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = args
        self.ex_de_hl();         // HL = args
        self.push(Self::HL);     // [args, rest_params, param_name, args, params, frame]
        self.call_label("cdr");  // HL = rest_args
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = rest_args
        self.pop(Self::HL);      // HL = args
        self.push(Self::BC);     // [rest_args, rest_params, param_name, args, params, frame]
        self.call_label("car");  // HL = current_arg (unevaluated)

        // Evaluate the argument
        // Stack: [rest_args, rest_params, param_name, args, params, frame]
        self.call_label("interp");  // HL = value

        // Create binding = cons(param_name, value)
        // cons takes DE=car, BC=cdr, so DE = param_name, BC = value
        // param_name is at SP+4
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = value
        self.ld_rr_nn(Self::HL, 4);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = param_name
        self.call_label("cons"); // HL = (param_name . value)

        // Extend env: env = cons(binding, env)
        // cons takes DE=car, BC=cdr, so DE = binding, BC = env
        self.ex_de_hl();         // DE = binding
        self.ld_hl_mem(Self::ENV_PTR);
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = env
        self.call_label("cons"); // HL = new_env
        self.ld_mem_hl(Self::ENV_PTR);  // store new env

        // Pop loop state: [rest_args, rest_params, param_name, args, params, frame]
        self.pop(Self::HL);      // HL = rest_args
        self.pop(Self::DE);      // DE = rest_params
        self.pop(Self::BC);      // discard param_name
        self.pop(Self::BC);      // discard args
        self.pop(Self::BC);      // discard params
        // Stack: [frame]

        // Set up for next iteration: HL = rest_args (new args), DE = rest_params (new params)
        self.push(Self::DE);     // [rest_params, frame]
        // HL already = rest_args
        self.jp_label("lambda_bind_loop");

        self.label("lambda_bind_done");
        // Stack: [params, frame] (params is nil)
        self.pop(Self::DE);      // discard params
        self.pop(Self::HL);      // HL = frame = (body . old_env)

        // Eval body
        self.push(Self::HL);     // save frame: [frame]
        self.call_label("car");  // HL = body
        self.call_label("interp");  // HL = result

        // Restore old env
        // Result is in HL, but cdr destroys DE, so save result on stack
        self.push(Self::HL);     // save result
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);  // point to frame on stack
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = frame
        self.ex_de_hl();         // HL = frame
        self.call_label("cdr");  // HL = old_env (cdr destroys DE)
        self.ld_mem_hl(Self::ENV_PTR);
        self.pop(Self::HL);      // HL = result
        self.pop(Self::DE);      // discard frame from stack
        self.ret();

        self.label("interp_not_lambda_call");
        // Not a lambda - return NIL
        self.pop(Self::HL);      // clean up stack (lambda)
        self.pop(Self::HL);      // func_sym
        self.pop(Self::HL);      // expr
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // interp_symbol: Look up symbol in environment
        // HL = symbol to look up
        self.label("interp_symbol");
        // First check for T (self-evaluating)
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_T as u8);
        self.jr_cc_label(Self::NZ, "interp_sym_lookup");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_T >> 8) as u8);
        self.jr_cc_label(Self::NZ, "interp_sym_lookup");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        self.label("interp_sym_lookup");
        // Look up in environment
        // DE = symbol to find, HL = env list
        self.ex_de_hl();  // DE = symbol
        self.ld_hl_mem(Self::ENV_PTR);  // HL = env

        self.label("env_lookup_loop");
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "env_not_found");
        // Get car (which is (name . value) pair)
        self.push(Self::HL);
        self.push(Self::DE);
        self.call_label("car");  // HL = (name . value)
        self.push(Self::HL);
        self.call_label("car");  // HL = name
        // Compare with DE (symbol we're looking for)
        self.pop(Self::BC);  // BC = (name . value) pair
        self.pop(Self::DE);  // DE = symbol to find
        // Compare HL with DE
        self.and_n(0);  // clear carry
        self.sbc_hl_rr(Self::DE);
        self.jr_cc_label(Self::NZ, "env_next");
        // Found it! Get the value from BC (the pair)
        self.pop(Self::HL);  // clean stack (was env tail)
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);  // HL = (name . value)
        self.jp_label("cdr");  // return value

        self.label("env_next");
        self.pop(Self::HL);  // HL = env tail position
        self.push(Self::DE);  // save symbol
        self.call_label("cdr");  // advance to next pair
        self.pop(Self::DE);
        self.jr_label("env_lookup_loop");

        self.label("env_not_found");
        // Symbol not found, return the symbol itself
        self.ex_de_hl();  // HL = symbol
        self.ret();

        // interp_list: Evaluate each element of a list
        self.label("interp_list");
        self.call_label("is_nil");
        self.ret_cc(Self::Z);  // Empty list -> NIL
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.ex_de_hl();  // DE = evaluated car
        self.pop(Self::HL);
        self.push(Self::DE);  // save evaluated car
        self.call_label("cdr");
        self.call_label("interp_list");  // HL = evaluated cdr
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);
        self.pop(Self::DE);  // DE = evaluated car
        self.jp_label("cons");
    }

    /// Generate built-in functions
    fn generate_builtins(&mut self) {
        // special_if: (IF test then else) - HL = expression
        // Evaluates test, if non-NIL evaluates then, else evaluates else
        self.label("special_if");
        self.call_label("cdr");  // skip IF, HL = (test then else)
        self.push(Self::HL);     // save args list
        self.call_label("car");  // HL = test
        self.call_label("interp");  // HL = evaluated test
        // Check if test result is NIL
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "if_else");
        // Test was non-NIL, evaluate then clause
        self.pop(Self::HL);      // restore (test then else)
        self.call_label("cdr");  // HL = (then else)
        self.call_label("car");  // HL = then
        self.jp_label("interp"); // tail call to interpret then

        self.label("if_else");
        // Test was NIL, evaluate else clause (or return NIL if no else)
        self.pop(Self::HL);      // restore (test then else)
        self.call_label("cdr");  // HL = (then else)
        self.call_label("cdr");  // HL = (else) or NIL
        self.call_label("is_nil");
        self.ret_cc(Self::Z);    // no else clause, return NIL
        self.call_label("car");  // HL = else
        self.jp_label("interp"); // tail call to interpret else

        // special_define: (DEFINE name value) - HL = expression
        // Adds name->value binding to the environment
        self.label("special_define");
        self.call_label("cdr");  // skip DEFINE, HL = (name value)
        self.push(Self::HL);
        self.call_label("car");  // HL = name (symbol)
        self.ex_de_hl();         // DE = name
        self.pop(Self::HL);
        self.push(Self::DE);     // save name
        self.call_label("cdr");  // HL = (value)
        self.call_label("car");  // HL = value expr
        self.call_label("interp");  // HL = evaluated value
        // Now create (name . value) pair
        self.pop(Self::DE);      // DE = name
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = value
        self.call_label("cons"); // HL = (name . value)
        // Add to front of environment: ((name . value) . old_env)
        self.ex_de_hl();         // DE = (name . value)
        self.ld_hl_mem(Self::ENV_PTR);  // HL = old env
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = old env
        self.call_label("cons"); // HL = ((name . value) . old_env)
        self.ld_mem_hl(Self::ENV_PTR);  // save new env
        // Return the name as result
        self.call_label("car");  // HL = (name . value)
        self.jp_label("car");    // return name

        // special_setq: (SETQ name value) - HL = expression
        // Modifies existing binding in environment
        self.label("special_setq");
        self.call_label("cdr");  // skip SETQ, HL = (name value)
        self.push(Self::HL);
        self.call_label("car");  // HL = name (symbol)
        self.ex_de_hl();         // DE = name to find
        self.pop(Self::HL);
        self.push(Self::DE);     // save name
        self.call_label("cdr");  // HL = (value)
        self.call_label("car");  // HL = value expr
        self.call_label("interp");  // HL = evaluated value
        self.push(Self::HL);     // save value
        // Now find the binding in environment
        self.ld_hl_mem(Self::ENV_PTR);  // HL = env
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = name to find
        self.ld_hl_mem(Self::ENV_PTR);  // HL = env

        self.label("setq_loop");
        // HL = env, DE = name to find
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "setq_not_found");
        self.push(Self::HL);     // save env
        self.push(Self::DE);     // save name
        self.call_label("car");  // HL = (binding-name . binding-value)
        self.push(Self::HL);     // save binding
        self.call_label("car");  // HL = binding-name
        // Compare with name we're looking for (DE on stack)
        self.ld_rr_nn(Self::BC, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);  // DE = name to find
        self.pop(Self::HL);      // HL = binding
        self.push(Self::HL);     // save binding again
        self.call_label("car");  // HL = binding-name
        // Compare HL with DE (name vs binding-name)
        self.or_r(Self::A);      // OR A,A to clear carry
        self.sbc_hl_rr(Self::DE);
        self.pop(Self::HL);      // HL = binding
        self.pop(Self::DE);      // DE = name
        self.pop(Self::BC);      // BC = env (was on stack)
        self.jr_cc_label(Self::NZ, "setq_next");
        // Found! HL = binding cons cell, update cdr with new value
        // The binding is (name . value) - we need to update the cdr
        // Get value from stack
        self.pop(Self::DE);      // DE = new value (pop from earlier save)
        // We need to update the cons cell's cdr
        // HL is the binding cons, we need to store DE in its cdr
        // Cons cells are stored at address in HL >> 2
        // CDR is at offset 2 from the cell
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0xFC);        // clear tag bits
        self.ld_r_r(Self::L, Self::A);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);      // HL = pointer to cons cell
        self.inc_rr(Self::HL);
        self.inc_rr(Self::HL);   // point to cdr
        self.ld_r_r(Self::HL_IND, Self::E);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::HL_IND, Self::D);
        // Return the new value
        self.ex_de_hl();
        self.pop(Self::DE);      // discard saved name
        self.ret();

        self.label("setq_next");
        self.push(Self::DE);     // save name
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);  // HL = env
        self.call_label("cdr");  // HL = rest of env
        self.pop(Self::DE);      // DE = name
        self.jr_label("setq_loop");

        self.label("setq_not_found");
        // Name not found - return NIL
        self.pop(Self::HL);      // discard value
        self.pop(Self::HL);      // discard name
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // special_lambda: (LAMBDA (params) body) - HL = expression
        // Returns a lambda object which is the expression itself
        // (we use the original s-expr as the lambda representation)
        self.label("special_lambda");
        // Just return the whole expression - it's self-quoting
        // The expression is (LAMBDA (params) body)
        self.ret();

        // builtin_add: (+ a b ...) - HL = expression
        // Uses stack for accumulator to support nested calls
        self.label("builtin_add");
        self.call_label("cdr");  // skip operator, HL = args list
        // Initialize accumulator to 0 on stack
        self.ld_rr_nn(Self::BC, 0);  // BC = accumulator
        self.push(Self::BC);         // stack: [acc=0]

        self.label("add_loop");
        // Check if HL is NIL
        self.push(Self::HL);         // stack: [list, acc]
        self.call_label("is_nil");
        self.pop(Self::HL);          // restore list
        self.jr_cc_label(Self::Z, "add_done");
        // Get car and evaluate
        self.push(Self::HL);         // save list: [list, acc]
        self.call_label("car");
        self.call_label("interp");  // HL = evaluated car
        self.call_label("fixnum_val");  // DE = numeric value
        // Add to accumulator: acc = acc + DE
        self.pop(Self::HL);          // HL = list
        self.pop(Self::BC);          // BC = accumulator
        self.push(Self::HL);         // save list: [list]
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);  // HL = acc
        self.add_hl_rr(Self::DE);    // HL = acc + val
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = new acc
        self.pop(Self::HL);          // HL = list
        self.push(Self::BC);         // save acc: [acc]
        // Advance to next element
        self.call_label("cdr");
        self.jr_label("add_loop");

        self.label("add_done");
        // Load final sum from stack into DE
        self.pop(Self::DE);  // DE = accumulator
        self.jp_label("make_fixnum");

        // builtin_sub: (- a b ...) - HL = expression
        // Uses stack for accumulator to support nested calls
        self.label("builtin_sub");
        self.call_label("cdr");  // skip operator, HL = args
        // Get first argument
        self.push(Self::HL);     // save args list
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = first arg
        self.pop(Self::HL);      // HL = args list
        self.push(Self::DE);     // stack: [acc = first arg]
        self.call_label("cdr");  // HL = rest args
        // Check if more args
        self.push(Self::HL);
        self.call_label("is_nil");
        self.pop(Self::HL);
        self.jr_cc_label(Self::NZ, "sub_loop");
        // Single arg: negate
        self.pop(Self::HL);      // HL = first arg
        // Negate HL
        self.ld_r_r(Self::A, Self::L);
        self.xor_n(0xFF);
        self.ld_r_r(Self::L, Self::A);
        self.ld_r_r(Self::A, Self::H);
        self.xor_n(0xFF);
        self.ld_r_r(Self::H, Self::A);
        self.inc_rr(Self::HL);
        self.ex_de_hl();
        self.jp_label("make_fixnum");

        self.label("sub_loop");
        // stack: [acc], HL = rest args
        self.push(Self::HL);
        self.call_label("is_nil");
        self.pop(Self::HL);
        self.jr_cc_label(Self::Z, "sub_done");
        // Get and evaluate next arg
        self.push(Self::HL);     // save rest args: [rest, acc]
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = value to subtract
        // Subtract from accumulator: acc = acc - DE
        self.pop(Self::HL);      // HL = rest args
        self.pop(Self::BC);      // BC = acc
        self.push(Self::HL);     // save rest: [rest]
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);  // HL = acc
        self.and_n(0);           // clear carry
        self.sbc_hl_rr(Self::DE);  // HL = acc - val
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = new acc
        self.pop(Self::HL);      // HL = rest args
        self.push(Self::BC);     // save acc: [acc]
        // Advance to next arg
        self.call_label("cdr");
        self.jr_label("sub_loop");

        self.label("sub_done");
        self.pop(Self::DE);      // DE = accumulator
        self.jp_label("make_fixnum");

        // builtin_mul: (* a b ...) - HL = expression
        // Uses stack for accumulator to support nested calls
        self.label("builtin_mul");
        self.call_label("cdr");  // HL = args
        // Initialize accumulator to 1 on stack
        self.ld_rr_nn(Self::BC, 1);
        self.push(Self::BC);     // stack: [acc=1]

        self.label("mul_loop");
        // stack: [acc], HL = args
        self.push(Self::HL);
        self.call_label("is_nil");
        self.pop(Self::HL);
        self.jr_cc_label(Self::Z, "mul_done");
        // Get and evaluate next arg
        self.push(Self::HL);     // save args: [args, acc]
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = value
        // Multiply: acc = acc * DE
        self.pop(Self::HL);      // HL = args
        self.pop(Self::BC);      // BC = acc
        self.push(Self::HL);     // save args: [args]
        self.push(Self::DE);     // save value: [val, args]
        // Call mul16: BC * DE -> HL
        self.call_label("mul16");  // HL = acc * val
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = new acc
        self.pop(Self::DE);      // discard saved value
        self.pop(Self::HL);      // HL = args
        self.push(Self::BC);     // save acc: [acc]
        // Advance to next arg
        self.call_label("cdr");
        self.jr_label("mul_loop");

        self.label("mul_done");
        self.pop(Self::DE);      // DE = accumulator
        self.jp_label("make_fixnum");

        // mul16: BC * DE -> HL (16-bit multiply)
        self.label("mul16");
        self.ld_rr_nn(Self::HL, 0);
        self.ld_r_n(Self::A, 16);  // 16 bits

        self.label("mul16_loop");
        self.srl_r(Self::D);
        self.rr_r(Self::E);  // shift DE right, low bit into carry
        self.jr_cc_label(Self::NC, "mul16_noadd");
        self.add_hl_rr(Self::BC);

        self.label("mul16_noadd");
        self.sla_r(Self::C);  // shift BC left
        self.rl_r(Self::B);
        self.dec_r(Self::A);
        self.jr_cc_label(Self::NZ, "mul16_loop");
        self.ret();

        // builtin_div: (/ a b) - HL = expression
        self.label("builtin_div");
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = dividend
        self.pop(Self::HL);
        self.push(Self::DE);  // save dividend BEFORE cdr destroys it
        self.call_label("cdr");
        // DE is now clobbered, but dividend is safe on stack
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = divisor
        self.ld_r_r(Self::B, Self::D);
        self.ld_r_r(Self::C, Self::E);  // BC = divisor
        self.pop(Self::DE);  // DE = dividend
        // DE / BC -> DE quotient
        self.call_label("div16");
        self.jp_label("make_fixnum");

        // div16: DE / BC -> DE (quotient), HL (remainder)
        // Simple repeated subtraction for small numbers
        // More complex bit-by-bit algorithm can be added later
        self.label("div16");
        self.ld_rr_nn(Self::HL, 0);  // quotient = 0

        self.label("div16_loop");
        // Check if dividend (DE) >= divisor (BC)
        self.ld_r_r(Self::A, Self::D);
        self.cp_r(Self::B);
        self.jr_cc_label(Self::CC, "div16_done");  // D < B, definitely DE < BC
        self.jr_cc_label(Self::NZ, "div16_subtract");  // D > B, definitely DE > BC
        // D == B, check E vs C
        self.ld_r_r(Self::A, Self::E);
        self.cp_r(Self::C);
        self.jr_cc_label(Self::CC, "div16_done");  // DE < BC, done

        self.label("div16_subtract");
        // DE >= BC, subtract and increment quotient
        self.ld_r_r(Self::A, Self::E);
        self.sub_r(Self::C);
        self.ld_r_r(Self::E, Self::A);
        self.ld_r_r(Self::A, Self::D);
        self.sbc_a_r(Self::B);
        self.ld_r_r(Self::D, Self::A);
        self.inc_rr(Self::HL);  // quotient++
        self.jr_label("div16_loop");

        self.label("div16_done");
        // HL = quotient, DE = remainder
        self.ex_de_hl();  // swap so DE = quotient
        self.ret();

        // builtin_mod: (% a b) - modulo, returns remainder
        self.label("builtin_mod");
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = dividend
        self.pop(Self::HL);
        self.push(Self::DE);  // save dividend BEFORE cdr destroys it
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = divisor
        self.ld_r_r(Self::B, Self::D);
        self.ld_r_r(Self::C, Self::E);  // BC = divisor
        self.pop(Self::DE);  // DE = dividend
        // DE / BC -> HL quotient, DE remainder
        self.call_label("div16");
        // After div16: DE = quotient (swapped), but we want remainder
        // Actually div16 does ex_de_hl at end, so DE=quotient, HL=remainder
        // Wait, let me trace: before ex_de_hl, HL=quotient, DE=remainder
        // After ex_de_hl: DE=quotient, HL=remainder
        // For mod we need remainder, which is now in HL after div16 returns
        // But div16 puts quotient in DE. We need the remainder which was in DE before swap.
        // Hmm, we need a different approach - call div16 but skip the ex_de_hl
        // Actually, HL contains the remainder after the swap! Wait no...
        // Let me think: div16 computes: HL=quotient accumulator, DE gets subtracted
        // At div16_done: HL = quotient, DE = remainder (what's left of dividend)
        // Then ex_de_hl: DE = quotient, HL = remainder
        // So after calling div16, HL has remainder! But we need it in DE for make_fixnum.
        self.ex_de_hl();  // DE = remainder (HL had remainder after div16)
        self.jp_label("make_fixnum");

        // builtin_car: CAR function - expects list in args
        self.label("builtin_car_fn");
        self.call_label("cdr");  // skip CAR symbol
        self.call_label("car");  // get first arg
        self.call_label("interp");  // evaluate it
        self.jp_label("car");

        // builtin_cdr: CDR function
        self.label("builtin_cdr_fn");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.jp_label("cdr");

        // builtin_cons: CONS function
        self.label("builtin_cons_fn");
        self.call_label("cdr");  // skip CONS symbol
        self.push(Self::HL);
        self.call_label("car");  // first arg
        self.call_label("interp");
        self.ex_de_hl();  // DE = car value
        self.pop(Self::HL);
        self.call_label("cdr");
        self.push(Self::DE);
        self.call_label("car");  // second arg
        self.call_label("interp");
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = cdr value
        self.pop(Self::DE);  // DE = car value
        self.jp_label("cons");

        // builtin_eq: (= a b) - numeric equality
        // Returns T if a == b, NIL otherwise
        self.label("builtin_eq");
        self.call_label("cdr");  // skip operator
        self.push(Self::HL);
        self.call_label("car");  // first arg
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = first value
        self.pop(Self::HL);
        self.push(Self::DE);  // save first value
        self.call_label("cdr");
        self.call_label("car");  // second arg
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = second value
        self.pop(Self::HL);  // HL = first value
        // Compare HL and DE
        self.and_n(0);  // clear carry
        self.sbc_hl_rr(Self::DE);  // HL = first - second
        self.jr_cc_label(Self::NZ, "eq_false");
        // Equal: return T
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("eq_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_lt: (< a b) - numeric less than
        // Returns T if a < b, NIL otherwise
        self.label("builtin_lt");
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = first value
        self.pop(Self::HL);
        self.push(Self::DE);
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = second value
        self.pop(Self::HL);  // HL = first value
        // Compare: is HL < DE?  (first < second)
        // Do DE - HL, if carry set then HL > DE (false)
        // We want: HL - DE, if carry then HL < DE (true)
        self.and_n(0);
        self.sbc_hl_rr(Self::DE);  // HL = first - second
        // If carry is set, first < second
        self.jr_cc_label(Self::CC, "lt_true");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();
        self.label("lt_true");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        // builtin_gt: (> a b) - numeric greater than
        // Returns T if a > b, NIL otherwise
        self.label("builtin_gt");
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = first value
        self.pop(Self::HL);
        self.push(Self::DE);
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("fixnum_val");  // DE = second value
        self.pop(Self::HL);  // HL = first value
        // Compare: is HL > DE?  (first > second)
        // If first > second, then second - first has carry
        self.ex_de_hl();  // DE = first, HL = second
        self.and_n(0);
        self.sbc_hl_rr(Self::DE);  // HL = second - first
        // If carry is set, second < first, so first > second
        self.jr_cc_label(Self::CC, "gt_true");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();
        self.label("gt_true");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        // builtin_atom: (ATOM x) - returns T if x is not a cons, NIL otherwise
        self.label("builtin_atom");
        self.call_label("cdr");  // skip ATOM
        self.call_label("car");  // get arg
        self.call_label("interp");  // evaluate it
        // Check if it's a cons (tag 00)
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.jr_cc_label(Self::NZ, "atom_true");  // not 00, so it's an atom
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);  // it's a cons
        self.ret();
        self.label("atom_true");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        // builtin_eq_sym: (EQ a b) - pointer equality
        self.label("builtin_eq_sym");
        self.call_label("cdr");  // skip EQ
        self.push(Self::HL);
        self.call_label("car");  // first arg
        self.call_label("interp");
        self.push(Self::HL);  // save first result
        self.pop(Self::DE);   // DE = first result
        self.pop(Self::HL);   // HL = args list
        self.push(Self::DE);  // save first result again
        self.call_label("cdr");
        self.call_label("car");  // second arg
        self.call_label("interp");  // HL = second
        self.pop(Self::DE);  // DE = first
        // Compare HL and DE
        self.and_n(0);  // clear carry
        self.sbc_hl_rr(Self::DE);
        self.jr_cc_label(Self::NZ, "eq_sym_false");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("eq_sym_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_consp: (CONSP x) - returns T if x is a cons
        self.label("builtin_consp");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        // Check tag == 00
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);
        self.jr_cc_label(Self::NZ, "consp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("consp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_list: (LIST x y z ...) - create list from args
        // Recursive approach: (LIST a b c) = (cons (eval a) (LIST b c))
        self.label("builtin_list");
        self.call_label("cdr");  // skip LIST symbol, HL = args

        self.label("list_helper");
        // HL = remaining args to process
        self.call_label("is_nil");
        self.ret_cc(Self::Z);  // no more args, return NIL

        // Save rest of args
        self.push(Self::HL);
        // Eval first arg
        self.call_label("car");
        self.call_label("interp");  // HL = evaluated first arg
        self.push(Self::HL);  // save evaluated arg
        // Get rest of args
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);
        self.ex_de_hl();  // HL = rest of args
        self.call_label("cdr");  // HL = remaining args
        // Recurse
        self.call_label("list_helper");  // HL = list of remaining evaluated args
        // HL is now the tail
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = tail
        self.pop(Self::DE);  // DE = evaluated first arg
        self.pop(Self::HL);  // discard saved args (already used)
        // cons(DE, BC)
        self.jp_label("cons");

        // builtin_not: (NOT x) - returns T if x is NIL, NIL otherwise
        // builtin_null: (NULL x) - same as NOT
        self.label("builtin_not");
        self.label("builtin_null");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_nil");
        self.jr_cc_label(Self::NZ, "not_false");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("not_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_numberp: (NUMBERP x) - returns T if x is a fixnum, NIL otherwise
        self.label("builtin_numberp");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = evaluated arg
        // Check if tag is 01 (fixnum)
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);  // mask to get tag
        self.cp_n(0x01);   // TAG_FIXNUM = 01
        self.jr_cc_label(Self::NZ, "numberp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("numberp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_symbolp: (SYMBOLP x) - returns T if x is a symbol, NIL otherwise
        self.label("builtin_symbolp");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = evaluated arg
        // Check if tag is 11 (symbol)
        self.ld_r_r(Self::A, Self::L);
        self.and_n(0x03);  // mask to get tag
        self.cp_n(0x03);   // TAG_SYMBOL = 11
        self.jr_cc_label(Self::NZ, "symbolp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();
        self.label("symbolp_false");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_read: (READ) - read an S-expression from input
        // For now, this reads from the standard input buffer
        self.label("builtin_read");
        // READ takes no arguments - just calls the reader
        // The main loop already fills the input buffer, so we just need to
        // call the reader on any remaining input in the buffer
        // For simple implementation: prompt for new input and read it
        self.call_label("read_line");  // Read a new line into buffer
        self.jp_label("read_expr");     // Parse and return expression

        // builtin_peek: (PEEK addr) - read byte from memory address
        self.label("builtin_peek");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = address as fixnum
        // Convert fixnum to address: shift right 2 bits
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        // HL = memory address, read byte
        self.ld_r_r(Self::A, Self::HL_IND);  // A = (HL)
        // Convert byte to fixnum: shift left 2 and set tag
        self.ld_r_n(Self::H, 0);
        self.ld_r_r(Self::L, Self::A);
        self.sla_r(Self::L);
        self.rl_r(Self::H);
        self.sla_r(Self::L);
        self.rl_r(Self::H);
        self.set_r(0, Self::L);  // Set tag bit 0 = fixnum
        self.ret();

        // builtin_poke: (POKE addr value) - write byte to memory address
        self.label("builtin_poke");
        self.call_label("cdr");
        self.push(Self::HL);       // save args
        self.call_label("car");
        self.call_label("interp");  // HL = address as fixnum
        // Convert fixnum to address
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.push(Self::HL);       // save address
        // Get value
        self.ld_rr_nn(Self::HL, 2);
        self.add_hl_rr(Self::SP);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);
        self.ex_de_hl();          // HL = args
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = value as fixnum
        // Convert fixnum to byte (shift right 2)
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.ld_r_r(Self::A, Self::L);  // A = value byte
        // Get address and write
        self.pop(Self::HL);        // HL = address
        self.ld_r_r(Self::HL_IND, Self::A);  // (HL) = A
        self.pop(Self::HL);        // discard args
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        // builtin_dump: (DUMP start length) - dump memory to file via I/O port
        self.label("builtin_dump");
        self.call_label("cdr");
        self.push(Self::HL);       // save args
        self.call_label("car");
        self.call_label("interp");  // HL = start address as fixnum
        // Convert fixnum to address
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        // Output start address to ports 0x82/0x83
        self.ld_r_r(Self::A, Self::L);
        self.out_n_a(0x82);  // low byte
        self.ld_r_r(Self::A, Self::H);
        self.out_n_a(0x83);  // high byte
        // Get length
        self.pop(Self::HL);
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = length as fixnum
        // Convert fixnum to value
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        // Output length to ports 0x84/0x85
        self.ld_r_r(Self::A, Self::L);
        self.out_n_a(0x84);  // low byte
        self.ld_r_r(Self::A, Self::H);
        self.out_n_a(0x85);  // high byte
        // Trigger dump by writing to port 0x86
        self.ld_r_n(Self::A, 1);
        self.out_n_a(0x86);
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret();

        // ========================================
        // BCD Floating Point Builtins
        // ========================================
        // Float format (6 bytes at heap address):
        //   Byte 0: Sign (0x00 = positive, 0x80 = negative)
        //   Byte 1: Exponent (biased by 128: 128 = 10^0)
        //   Bytes 2-5: BCD mantissa (8 digits, normalized)
        // Pointer has TAG_FLOAT (0x04) in low bits

        // builtin_floatp: (FLOATP x) - returns T if x is a float
        self.label("builtin_floatp");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_float");
        self.ld_rr_nn(Self::HL, Self::LISP_T);
        self.ret_cc(Self::Z);
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        // builtin_itof: (ITOF n) - convert integer to float
        // Input: fixnum n
        // Output: BCD float representation
        self.label("builtin_itof");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = fixnum
        // Convert fixnum to integer
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);  // HL = integer value
        self.push(Self::HL);  // save integer
        // Allocate float
        self.call_label("alloc_float");
        self.push(Self::HL);  // save float pointer (with tag)
        self.call_label("float_ptr");  // HL = raw address
        self.ex_de_hl();  // DE = float address
        self.pop(Self::BC);  // BC = float pointer (with tag)
        self.pop(Self::HL);  // HL = integer value
        // Check sign
        self.bit_n_r(7, Self::H);  // check sign bit
        self.jr_cc_label(Self::Z, "itof_positive");
        // Negative: negate and set sign
        self.push(Self::DE);
        self.ex_de_hl();
        self.ld_rr_nn(Self::HL, 0);
        self.or_r(Self::A);  // clear carry
        self.sbc_hl_rr(Self::DE);  // HL = -value
        self.ex_de_hl();  // DE = positive value
        self.pop(Self::HL);  // HL = float address
        self.ld_r_n(Self::HL_IND, 0x80);  // set sign byte
        self.jr_label("itof_convert");

        self.label("itof_positive");
        self.ex_de_hl();  // HL = float address, DE = value
        self.ld_r_n(Self::HL_IND, 0x00);  // clear sign byte

        self.label("itof_convert");
        // HL = float address, DE = integer value (positive)
        // Convert to BCD: we need to find digits and exponent
        self.inc_rr(Self::HL);  // point to exponent
        self.push(Self::HL);
        self.push(Self::BC);  // save float pointer
        // For simplicity, we'll convert up to 5 digits (max 65535)
        // Exponent = 128 + (number of digits - 1)
        // First, count digits and convert to BCD
        self.push(Self::DE);  // save value
        // Convert binary to BCD using double-dabble or division
        // For now, use simple division by powers of 10
        self.call_label("bin_to_bcd");  // DE=value -> stores BCD at stack area
        self.pop(Self::HL);  // HL = float address (exponent)
        self.pop(Self::BC);  // BC = float pointer
        self.ld_r_r(Self::HL_IND, Self::A);  // store exponent
        self.inc_rr(Self::HL);
        // Copy 4 BCD bytes from temp area
        self.ld_r_r(Self::HL_IND, Self::D);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::HL_IND, Self::E);
        self.inc_rr(Self::HL);
        self.ld_r_n(Self::HL_IND, 0);  // zero remaining
        self.inc_rr(Self::HL);
        self.ld_r_n(Self::HL_IND, 0);
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);  // return float pointer
        self.ret();

        // bin_to_bcd: Convert binary in stack to BCD in DE, exponent in A
        // Simple conversion: just store the raw integer value
        // For simplicity, we store the value directly (not true BCD)
        // This is a simplified float where mantissa = raw binary value
        self.label("bin_to_bcd");
        self.pop(Self::HL);  // return address
        self.pop(Self::DE);  // value
        self.push(Self::HL);  // put return address back
        // Just return the value in DE with exponent 128 (10^0 = 1)
        // This means the float stores the integer directly
        self.ld_r_n(Self::A, 128);  // exponent = 128 means value * 10^0 = value
        self.ret();

        // builtin_ftoi: (FTOI f) - convert float to integer (truncate)
        self.label("builtin_ftoi");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = float
        self.call_label("is_float");
        self.jr_cc_label(Self::Z, "ftoi_valid");
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);  // not a float
        self.ret();

        self.label("ftoi_valid");
        self.push(Self::HL);  // save float pointer
        self.call_label("float_ptr");  // HL = raw address
        // Read sign and exponent
        self.ld_r_r(Self::C, Self::HL_IND);  // C = sign
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::B, Self::HL_IND);  // B = exponent
        self.inc_rr(Self::HL);
        // Read BCD mantissa
        self.ld_r_r(Self::D, Self::HL_IND);  // high BCD
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::E, Self::HL_IND);  // low BCD
        // Convert BCD to binary
        // Exponent tells us decimal point position
        self.call_label("bcd_to_bin");  // DE=BCD, B=exp -> HL=binary
        // Apply sign
        self.bit_n_r(7, Self::C);
        self.jr_cc_label(Self::Z, "ftoi_positive");
        // Negate
        self.ex_de_hl();
        self.ld_rr_nn(Self::HL, 0);
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);

        self.label("ftoi_positive");
        // Convert to fixnum
        self.add_hl_hl();
        self.add_hl_hl();
        self.ld_r_r(Self::A, Self::L);
        self.or_n(Self::TAG_FIXNUM);
        self.ld_r_r(Self::L, Self::A);
        self.pop(Self::DE);  // discard saved float
        self.ret();

        // bcd_to_bin: Convert value in DE to binary in HL
        // Since we store raw binary (not BCD), just copy DE to HL
        self.label("bcd_to_bin");
        self.ex_de_hl();  // HL = DE (the stored value)
        self.ret();

        // builtin_fadd: (FADD a b) - add two floats
        self.label("builtin_fadd");
        // For now, simple implementation: convert to int, add, convert back
        // TODO: proper BCD addition with alignment
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");  // first arg
        self.push(Self::HL);
        // Convert first to int
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fadd_first_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fadd_have_first");
        self.label("fadd_first_int");
        // It's a fixnum, extract value
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fadd_have_first");
        self.pop(Self::DE);  // discard original first
        self.push(Self::HL);  // save first int
        // Get second arg
        self.pop(Self::HL);  // args
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // second arg
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fadd_second_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fadd_have_second");
        self.label("fadd_second_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fadd_have_second");
        self.pop(Self::DE);  // first int
        self.add_hl_rr(Self::DE);  // sum
        // Convert back to float
        self.push(Self::HL);
        self.call_label("alloc_float");
        self.push(Self::HL);
        self.call_label("float_ptr");
        self.ex_de_hl();
        self.pop(Self::BC);  // float pointer
        self.pop(Self::HL);  // sum
        // Store as simple integer float
        self.call_label("store_int_as_float");
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);
        self.ret();

        // float_to_int_internal: HL = float -> HL = integer value
        self.label("float_to_int_internal");
        self.call_label("float_ptr");
        self.ld_r_r(Self::C, Self::HL_IND);  // sign
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::B, Self::HL_IND);  // exp
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::D, Self::HL_IND);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::E, Self::HL_IND);
        self.call_label("bcd_to_bin");
        self.bit_n_r(7, Self::C);
        self.ret_cc(Self::Z);
        // Negate
        self.ex_de_hl();
        self.ld_rr_nn(Self::HL, 0);
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);
        self.ret();

        // store_int_as_float: DE = float addr, HL = integer
        self.label("store_int_as_float");
        self.bit_n_r(7, Self::H);
        self.jr_cc_label(Self::Z, "sif_positive");
        self.push(Self::DE);
        self.ex_de_hl();
        self.ld_rr_nn(Self::HL, 0);
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);
        self.pop(Self::DE);
        self.ex_de_hl();  // DE = positive value, HL = float addr
        self.ld_r_n(Self::HL_IND, 0x80);
        self.jr_label("sif_store");
        self.label("sif_positive");
        self.ex_de_hl();  // DE = value, HL = float addr
        self.ld_r_n(Self::HL_IND, 0x00);
        self.label("sif_store");
        self.inc_rr(Self::HL);
        self.push(Self::HL);
        self.push(Self::BC);
        self.call_label("bin_to_bcd_de");
        self.pop(Self::BC);
        self.pop(Self::HL);
        self.ld_r_r(Self::HL_IND, Self::A);  // exponent
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::HL_IND, Self::D);
        self.inc_rr(Self::HL);
        self.ld_r_r(Self::HL_IND, Self::E);
        self.inc_rr(Self::HL);
        self.ld_r_n(Self::HL_IND, 0);
        self.inc_rr(Self::HL);
        self.ld_r_n(Self::HL_IND, 0);
        self.ret();

        // bin_to_bcd_de: DE = value -> DE = BCD, A = exponent
        self.label("bin_to_bcd_de");
        self.jp_label("bin_to_bcd");

        // builtin_fsub: (FSUB a b) - subtract floats
        self.label("builtin_fsub");
        // Same as fadd but negate second
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.push(Self::HL);
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fsub_first_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fsub_have_first");
        self.label("fsub_first_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fsub_have_first");
        self.pop(Self::DE);
        self.push(Self::HL);
        self.pop(Self::HL);  // args
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fsub_second_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fsub_have_second");
        self.label("fsub_second_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fsub_have_second");
        self.pop(Self::DE);
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);  // HL = second - first? No: DE - HL
        // Wait, we have first in DE, second in HL. Want first - second
        self.ex_de_hl();
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);  // HL = first - second
        self.push(Self::HL);
        self.call_label("alloc_float");
        self.push(Self::HL);
        self.call_label("float_ptr");
        self.ex_de_hl();
        self.pop(Self::BC);
        self.pop(Self::HL);
        self.call_label("store_int_as_float");
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);
        self.ret();

        // builtin_fmul: (FMUL a b) - multiply floats
        self.label("builtin_fmul");
        // Simple: convert to int, multiply, convert back
        // Note: This loses precision for large numbers
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.push(Self::HL);
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fmul_first_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fmul_have_first");
        self.label("fmul_first_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fmul_have_first");
        self.pop(Self::DE);
        self.push(Self::HL);
        self.pop(Self::HL);
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fmul_second_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fmul_have_second");
        self.label("fmul_second_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fmul_have_second");
        self.pop(Self::DE);
        // Multiply DE * HL (16-bit)
        self.call_label("mul16");
        self.push(Self::HL);
        self.call_label("alloc_float");
        self.push(Self::HL);
        self.call_label("float_ptr");
        self.ex_de_hl();
        self.pop(Self::BC);
        self.pop(Self::HL);
        self.call_label("store_int_as_float");
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);
        self.ret();

        // mul16: DE * HL -> HL (16-bit result, overflow ignored)
        self.label("mul16");
        self.push(Self::BC);
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);
        self.ld_rr_nn(Self::HL, 0);
        self.ld_r_n(Self::A, 16);
        self.label("mul16_loop");
        self.add_hl_hl();
        self.ex_de_hl();
        self.add_hl_hl();
        self.ex_de_hl();
        self.jr_cc_label(Self::NC, "mul16_noadd");
        self.add_hl_rr(Self::BC);
        self.label("mul16_noadd");
        self.dec_r(Self::A);
        self.jr_cc_label(Self::NZ, "mul16_loop");
        self.pop(Self::BC);
        self.ret();

        // builtin_fdiv: (FDIV a b) - divide floats
        self.label("builtin_fdiv");
        // Simple integer division
        self.call_label("cdr");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.push(Self::HL);
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fdiv_first_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fdiv_have_first");
        self.label("fdiv_first_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fdiv_have_first");
        self.pop(Self::DE);
        self.push(Self::HL);
        self.pop(Self::HL);
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_float");
        self.jr_cc_label(Self::NZ, "fdiv_second_int");
        self.call_label("float_to_int_internal");
        self.jr_label("fdiv_have_second");
        self.label("fdiv_second_int");
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.srl_r(Self::H);
        self.rr_r(Self::L);
        self.label("fdiv_have_second");
        self.pop(Self::DE);  // first
        // HL = second (divisor), DE = first (dividend)
        // We want first / second = DE / HL
        self.push(Self::HL);  // save divisor
        self.ex_de_hl();      // HL = dividend
        self.pop(Self::DE);   // DE = divisor
        self.call_label("div16");  // HL = HL / DE
        self.push(Self::HL);
        self.call_label("alloc_float");
        self.push(Self::HL);
        self.call_label("float_ptr");
        self.ex_de_hl();
        self.pop(Self::BC);
        self.pop(Self::HL);
        self.call_label("store_int_as_float");
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);
        self.ret();

        // div16: HL / DE -> HL (quotient), uses BC
        self.label("div16");
        self.push(Self::BC);
        self.ld_r_r(Self::A, Self::D);
        self.or_r(Self::E);
        self.jr_cc_label(Self::NZ, "div16_ok");
        self.ld_rr_nn(Self::HL, 0);  // div by zero = 0
        self.pop(Self::BC);
        self.ret();
        self.label("div16_ok");
        self.ld_rr_nn(Self::BC, 0);  // quotient
        self.label("div16_loop");
        self.or_r(Self::A);
        self.sbc_hl_rr(Self::DE);
        self.jr_cc_label(Self::C, "div16_done");
        self.inc_rr(Self::BC);
        self.jr_label("div16_loop");
        self.label("div16_done");
        self.add_hl_rr(Self::DE);  // restore
        self.ld_r_r(Self::H, Self::B);
        self.ld_r_r(Self::L, Self::C);
        self.pop(Self::BC);
        self.ret();

        // special_cond: (COND (test1 expr1) (test2 expr2) ...)
        self.label("special_cond");
        self.call_label("cdr");  // skip COND, HL = clauses

        self.label("cond_loop");
        self.call_label("is_nil");
        self.ret_cc(Self::Z);  // no more clauses, return NIL
        self.push(Self::HL);
        self.call_label("car");  // HL = (test expr)
        self.push(Self::HL);
        self.call_label("car");  // HL = test
        self.call_label("interp");  // evaluate test
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "cond_next");
        // Test was true, evaluate expr
        self.pop(Self::HL);  // HL = (test expr)
        self.pop(Self::DE);  // discard rest
        self.call_label("cdr");
        self.call_label("car");  // HL = expr
        self.jp_label("interp");

        self.label("cond_next");
        self.pop(Self::HL);  // discard clause
        self.pop(Self::HL);  // HL = rest clauses
        self.call_label("cdr");
        self.jr_label("cond_loop");

        // special_progn: (PROGN expr1 expr2 ...) - evaluate all, return last
        self.label("special_progn");
        self.call_label("cdr");  // skip PROGN
        self.ld_rr_nn(Self::DE, Self::LISP_NIL);  // default result

        self.label("progn_loop");
        self.push(Self::DE);
        self.call_label("is_nil");
        self.pop(Self::DE);
        self.jr_cc_label(Self::Z, "progn_done");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");  // HL = result
        self.ex_de_hl();  // DE = result
        self.pop(Self::HL);
        self.push(Self::DE);  // save result before cdr (cdr uses DE)
        self.call_label("cdr");
        self.pop(Self::DE);  // restore result
        self.jr_label("progn_loop");

        self.label("progn_done");
        self.ex_de_hl();  // HL = last result
        self.ret();

        // special_and: (AND expr1 expr2 ...) - short-circuit AND
        self.label("special_and");
        self.call_label("cdr");  // skip AND
        self.ld_rr_nn(Self::DE, Self::LISP_T);  // default true

        self.label("and_loop");
        self.push(Self::DE);
        self.call_label("is_nil");
        self.pop(Self::DE);
        self.jr_cc_label(Self::Z, "and_done");
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "and_false");
        self.ex_de_hl();  // DE = this result (for returning last true value)
        self.pop(Self::HL);
        self.push(Self::DE);  // save result before cdr (cdr uses DE)
        self.call_label("cdr");
        self.pop(Self::DE);  // restore result
        self.jr_label("and_loop");

        self.label("and_false");
        self.pop(Self::HL);  // clean stack
        self.ld_rr_nn(Self::HL, Self::LISP_NIL);
        self.ret();

        self.label("and_done");
        self.ex_de_hl();
        self.ret();

        // special_or: (OR expr1 expr2 ...) - short-circuit OR
        self.label("special_or");
        self.call_label("cdr");  // skip OR

        self.label("or_loop");
        self.call_label("is_nil");
        self.ret_cc(Self::Z);  // no more, return NIL
        self.push(Self::HL);
        self.call_label("car");
        self.call_label("interp");
        self.call_label("is_nil");
        self.jr_cc_label(Self::Z, "or_next");
        // Found true value, return it
        self.pop(Self::DE);  // clean stack
        self.ret();

        self.label("or_next");
        self.pop(Self::HL);
        self.call_label("cdr");
        self.jr_label("or_loop");

        // special_let: (LET ((var1 val1) (var2 val2) ...) body)
        // Binds local variables then evaluates body
        // Entry: HL = (LET bindings body)
        self.label("special_let");
        // Save expr first
        self.push(Self::HL);        // stack: [expr]
        // Save old environment
        self.ld_hl_mem(Self::ENV_PTR);
        self.ex_de_hl();            // DE = old_env
        self.pop(Self::HL);         // HL = expr
        self.push(Self::DE);        // stack: [old_env]
        // Get bindings list and body
        self.call_label("cdr");     // skip LET -> HL = (bindings body)
        self.push(Self::HL);        // save (bindings body)
        self.call_label("cdr");
        self.call_label("car");     // HL = body
        self.ld_mem_hl(0x4028);     // save body at scratch
        self.pop(Self::HL);         // HL = (bindings body)
        self.call_label("car");     // HL = bindings list

        // Process each binding
        self.label("let_bind_loop");
        self.push(Self::HL);        // save bindings list
        self.call_label("is_nil");
        self.pop(Self::HL);
        self.jr_cc_label(Self::Z, "let_eval_body");
        // Get (var val) pair
        self.push(Self::HL);        // save rest
        self.call_label("car");     // HL = (var val)
        self.push(Self::HL);        // save (var val)
        self.call_label("car");     // HL = var
        self.ld_mem_hl(0x402A);     // save var at scratch
        self.pop(Self::HL);         // HL = (var val)
        self.call_label("cdr");
        self.call_label("car");     // HL = val expr
        self.call_label("interp"); // HL = evaluated value
        // Create binding (var . value)
        // After interp: HL = value
        // cons takes DE=car, BC=cdr, so DE = var, BC = value
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = value
        self.ld_hl_mem(0x402A);         // HL = var
        self.ex_de_hl();                // DE = var
        self.call_label("cons");        // HL = (var . value)
        // Add to environment: cons(binding, env)
        // After cons: HL = binding
        // cons takes DE=car, BC=cdr, so DE = binding, BC = env
        self.ex_de_hl();                // DE = binding
        self.ld_hl_mem(Self::ENV_PTR);
        self.ld_r_r(Self::B, Self::H);
        self.ld_r_r(Self::C, Self::L);  // BC = current env
        self.call_label("cons");        // HL = new env
        self.ld_mem_hl(Self::ENV_PTR);
        // Next binding
        self.pop(Self::HL);         // HL = rest of bindings
        self.call_label("cdr");     // HL = cdr(bindings)
        self.jr_label("let_bind_loop");

        self.label("let_eval_body");
        // Evaluate body
        self.ld_hl_mem(0x4028);     // HL = body
        self.call_label("interp"); // HL = result
        // Restore old environment
        self.ex_de_hl();            // DE = result
        self.pop(Self::HL);         // HL = old_env
        self.ld_mem_hl(Self::ENV_PTR);
        self.ex_de_hl();            // HL = result
        self.ret();

        // builtin_print: (PRINT x) - print value and return it
        self.label("builtin_print");
        self.call_label("cdr");
        self.call_label("car");
        self.call_label("interp");  // HL = value
        self.push(Self::HL);
        self.call_label("print_expr");
        // Print newline
        self.ld_r_n(Self::A, b'\r');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b'\n');
        self.call_label("acia_putc");
        self.pop(Self::HL);  // return the value
        self.ret();
    }

    /// SLA r instruction (shift left arithmetic)
    fn sla_r(&mut self, r: u8) {
        self.emit(0xCB);
        self.emit(0x20 | r);
    }

    /// RL r instruction (rotate left through carry)
    fn rl_r(&mut self, r: u8) {
        self.emit(0xCB);
        self.emit(0x10 | r);
    }

    /// SET b, r instruction (set bit b in register r)
    fn set_r(&mut self, b: u8, r: u8) {
        self.emit(0xCB);
        self.emit(0xC0 | (b << 3) | r);
    }

    /// Generate REPL
    fn generate_repl(&mut self) {
        self.label("repl");

        // Print prompt
        self.label("repl_loop");
        self.ld_r_n(Self::A, b'>');
        self.call_label("acia_putc");
        self.ld_r_n(Self::A, b' ');
        self.call_label("acia_putc");

        // Read input line
        self.call_label("acia_getline");

        // Read expression from input buffer
        self.call_label("read_expr");

        // Check for EOF
        self.ld_r_r(Self::A, Self::L);
        self.cp_n(Self::LISP_EOF as u8);
        self.jr_cc_label(Self::NZ, "repl_not_eof");
        self.ld_r_r(Self::A, Self::H);
        self.cp_n((Self::LISP_EOF >> 8) as u8);
        self.jr_cc_label(Self::Z, "repl_loop");  // Empty input, try again

        self.label("repl_not_eof");
        // Evaluate expression
        self.call_label("interp");

        // Print result
        self.call_label("print_expr");
        self.call_label("acia_newline");

        // Loop
        self.jp_label("repl_loop");
    }

    /// Generate banner string and print function
    fn generate_banner(&mut self) {
        self.label("print_banner");
        // Push string address
        self.ld_rr_nn(Self::HL, self.pc + 6); // Address after this instruction
        self.call_label("acia_puts");
        self.jp_label("acia_newline");

        // Banner string
        let banner = b"kz80_lisp v0.1\0";
        for &b in banner {
            self.emit(b);
        }
    }
}

/// Generate Z80 binary
pub fn generate_z80_binary() -> Vec<u8> {
    let mut codegen = CodeGen::new();
    codegen.generate()
}
