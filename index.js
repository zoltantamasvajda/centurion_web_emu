'use strict';
const REGISTERS = {
    A: 0x0000,
    B: 0x0000,
    R: 0x0000,
    D: 0x0000,
    E: 0x0000
}


var SP = 0x0000;    // Stack Pointer
var PC = 0xfc00;    // Progran Counter

const SENSE_SWITCH = 0b0000

function readReg(name) {
    switch (name[1]) {
        case 'L':
            return REGISTERS[name[0]] & 0xFF
        case 'H':
            return REGISTERS[name[0]] >> 8
        default:
            return REGISTERS[name[0]]
    }
}
function writeReg(name, value) {
    switch (name[1]) {
        case 'L':
            REGISTERS[name[0]] = ((REGISTERS[name[0]] >> 8) << 8) | value
        case 'H':
            REGISTERS[name[0]] = value << 8 | (REGISTERS[name[0]] & 0xff)
        default:
            REGISTERS[name[0]] = value
    }
}

const FLAGS =
{
    C: false,	// Carry Bit
    Z: false,	// Zero
    I: false,	// Disable Interrupts
    D: false,	// Decimal Mode (unused in this implementation)
    B: false,	// Break
    U: false,	// Unused
    V: false,	// Overflow
    N: false,	// Negative
};

function getFlag(flag) {
    return FLAGS[flag];
}

function setFlag(flag, value) {
    FLAGS[flag] = Boolean(value)
}


const MEM = new Array(0xFFFF)

function readmem(addr) {
    if (addr >= 0xF200 && addr <= 0xF21F)
        return mux_read(addr);



    return MEM[addr] ? MEM[addr] : 0x00
}

function writemem(addr, val) {
    if (addr >= 0xF200 && addr <= 0xF21F) {
        mux_write(addr, val);
        return;
    }
    MEM[addr] = val
    return
}

const BOOTLOADER = [
    0x1A, 0x02,
    0x73, 0x03,
    0x71, 0x80, 0x01,
    0x80, 0xC5,
    0xA1, 0xF2, 0x00,
    0x80, 0x8C,
    0xA1, 0xF2, 0x01,
    0x0E,
    0x0E,
    0x90, 0x10, 0x00,
    0x5F,
    0x7B, 0x79,
    0xC4, 0xBD, 0x00, // D=\0

    0x00, 0x00, //Break
    //0x7B, 0x72,

    0xC0, 0xC6,
    0x49,
    0xE5, 0xA2,
    0x14, 0x0A, 0xC0, 0xC3, 0x49, 0x14, 0x05, 0xC0, 0xC8, 0x49, 0x15, 0x50, 0x7B, 0x72, 0xC0, 0x50, 0x40, 0x31, 0x16, 0x48, 0xC5, 0xA1, 0x16, 0x7F, 0x18, 0x7F, 0xC0, 0x03, 0x49, 0x18, 0x3D, 0xD0, 0x0F, 0x00, 0xF5, 0xA2, 0x32, 0x20, 0xF5, 0xA2, 0xC0, 0x88, 0xE5, 0xA2, 0xD0, 0x83, 0x00, 0xF5, 0xA2, 0xD0, 0x81, 0x00, 0x06, 0x27, 0x30, 0x29, 0x17, 0xFB, 0xF5, 0xA2, 0x2F, 0x14, 0x2F, 0x06, 0x2F, 0xA0, 0x90, 0xFF, 0xF6, 0x2F, 0x02, 0x7B, 0x22, 0x43, 0x90, 0x01, 0x00, 0x2F, 0x00, 0x90, 0xF0, 0xFF, 0x2F, 0x02, 0x7B, 0x15, 0x45, 0x15, 0x03, 0x71, 0x01, 0x03, 0x7B, 0x11, 0x8D, 0x8A, 0xC5, 0xD2, 0xD2, 0xCF, 0xD2, 0x8D, 0x8A, 0x00, 0x07, 0x73, 0x85, 0x73, 0x73, 0x73, 0x11,

    0x81, 0xF2, 0x00,
    0x2C,
    0x2C,
    0x11, 0xF9,
    0x85, 0x41,
    0x15, 0x01,
    0x09,
    0xA1, 0xF2, 0x01,
    0x73, 0xEF,

    0x00,//Break

    0x84, 0xEE, 0x2C, 0x11, 0xFB, 0x84, 0xF5, 0xC0, 0x80, 0x43, 0x31, 0xC0, 0xE0, 0x49, 0x16, 0x04, 0xC0, 0xDF, 0x42, 0x31, 0xA4, 0xE6, 0x09, 0x73, 0x62, 0x73, 0x02, 0x73, 0xBF, 0xC0, 0x07, 0x49, 0x18, 0xBA, 0xA1, 0xF1, 0x40, 0x94, 0x2D, 0xD0, 0x00, 0x10, 0x5A, 0x14, 0xAF, 0x3A, 0xB1, 0xF1, 0x41, 0x7B, 0x3B, 0x03, 0x94, 0x1E, 0xD0, 0x04, 0x00, 0x5A, 0x15, 0xA0, 0xD0, 0x00, 0x20, 0x5A, 0x14, 0xF2, 0x2F, 0x04, 0x2F, 0x06, 0x90, 0x01, 0x00, 0x2F, 0x00, 0x90, 0xEA, 0x1F, 0x2F, 0x02, 0x7B, 0x1C, 0x00, 0x81, 0xF1, 0x44, 0x15, 0x84, 0x71, 0x01, 0x03, 0x7B, 0x2F, 0x71, 0xFC, 0x00, 0x85, 0x41, 0xA1, 0xF8, 0x00, 0x81, 0xF8, 0x01, 0x29, 0x15, 0xFA, 0x84, 0xF6, 0x09, 0x85, 0x41, 0xA1, 0xF1, 0x48, 0x84, 0xDF, 0x2C, 0x10, 0xFB, 0x09, 0x73, 0xA0, 0xA5, 0xA2, 0x90, 0x1F, 0x40, 0x5E, 0x90, 0x81, 0x00, 0xB5, 0x81, 0x80, 0x84, 0xA5, 0x81, 0x85, 0xA1, 0x14, 0x04, 0xC0, 0x0F, 0x40, 0x31, 0xA5, 0x81, 0x80, 0x83, 0xA5, 0x81, 0x3A, 0xB5, 0x81, 0x80, 0x85, 0xA5, 0x81, 0x2A, 0xA5, 0x81, 0xD0, 0x01, 0x90, 0xF5, 0x81, 0x28, 0xC0, 0x0E, 0x49, 0x15, 0xF3, 0x80, 0xFF, 0xA5, 0x81, 0x80, 0x08, 0x7B, 0x4E, 0x80, 0x41, 0xA1, 0xF8, 0x08, 0x0E, 0x2A, 0xA1, 0xF8, 0x08, 0x0E, 0xA1, 0xF8, 0x08, 0x0E, 0x90, 0x1F, 0x40, 0x2F, 0x00, 0x51, 0x80, 0x3B, 0x2F, 0x02, 0x2F, 0x34, 0x2F, 0x06, 0x80, 0x43, 0xA1, 0xF8, 0x08, 0x0E, 0x0E, 0x7B, 0x20, 0x73, 0x02, 0x73, 0x9A, 0x90, 0x01, 0x00, 0x2F, 0x00, 0x90, 0xEA, 0x1F, 0x2F, 0x02, 0x2F, 0x34, 0x2F, 0x06, 0x80, 0x45, 0xA1, 0xF8, 0x08, 0x80, 0x08, 0x7B, 0x0D, 0x7B, 0x03, 0x71, 0x01, 0x03, 0x81, 0xF8, 0x08, 0x15, 0x01, 0x09, 0x73, 0xDA, 0xC1, 0xF8, 0x09, 0x4A, 0x15, 0xFA, 0x09, 0x8C, 0x00, 0x8B, 0x00, 0x95, 0x41, 0xB3, 0x03, 0x79, 0x4C, 0x93, 0x47, 0xBE, 0x6D, 0xA2, 0x32, 0x40, 0x79, 0x4C, 0xE7, 0x4D, 0x14, 0x2A, 0xC0, 0x8D, 0x49, 0x14, 0x25, 0xC0, 0xB0, 0x49, 0x16, 0x25, 0x80, 0x09, 0x41, 0x31, 0x19, 0x0E, 0x80, 0x11, 0x41, 0x31, 0x16, 0x19, 0xC0, 0x05, 0x49, 0x18, 0x14, 0xC0, 0x0A, 0x48, 0x80, 0x04, 0x07, 0x37, 0x40, 0x29, 0x18, 0xFA, 0x40, 0x35, 0x73, 0xD0, 0x55, 0x40, 0x65, 0xA1, 0x09, 0x65, 0xA1, 0x73, 0xBE, 0xD5, 0x41, 0x7D, 0x80, 0x0C, 0xD0, 0x4B, 0x65, 0xF5
];

for (let i = 0; i < BOOTLOADER.length; i++) {
    MEM[0xfc00 + i] = BOOTLOADER[i]
}

main();

async function main() {
    setup();
    let halt = false
    while (!halt) {
        const opcode = readmem(PC)
        log("0x" + PC.toString(16).padStart(2, '0') + " 0x" + opcode.toString(16).padStart(2, '0'))
        PC++
        switch (opcode) {
            case 0x00:// hlt Halts the CPU AX/AH/AL
                halt = true;
                break;
            case 0x01: // nop No operation BX/BH/BL
                break;
            case 0x02: break; // fsn Set the sign flag RT/RH/RL
            case 0x03: break; // fcn Clear the sign flag DX/DH/DL
            case 0x04: break; // fsi Set the interrupt flag EX/EH/EL
            case 0x05: break; // fci Clear the interrupt flag SP 
            case 0x06: break; // fsc Set the carry flag PC (DON’T USE)
            case 0x07: break; // fcc Clear the carry flag EXTRA (DON’T USE)
            case 0x08: break; // fca Clear all flags
            case 0x09:       // ret Return from function
                {
                    PC = readReg('RT')
                    const hi = readmem(SP)
                    SP++
                    const lo = readmem(SP)
                    SP++
                    writeReg('RT', (hi << 8) + lo);

                    break;
                }
            case 0x0a: break; // reti Return from interrupt
            case 0x0b: break; // ??
            case 0x0c: break; // ??
            case 0x0d: break; // ??
            case 0x0e: // dly Delay 4.5ms
                await sleep(4.5)
                break;
            case 0x0f: break; // ??
            case 0x10: break; // bcs PC+N Branch if carry set
            case 0x11:       // bcc PC+N Branch if carry clear
                {
                    var N = readmem(PC)
                    PC++
                    if (!getFlag('C')) {
                        PC = PC + ((N & 0x80) ? N - 0x100 : N)
                    }
                    break;
                }
            case 0x12: break; // bns PC+N Branch if negative set
            case 0x13: break; // bnc PC+N Branch if negative clear
            case 0x14: break; // bzs PC+N Branch if zero set (Branch if equal)
            case 0x15:      // bzc PC+N Branch if zero clear (Branch if not equal)
                {
                    var N = readmem(PC)
                    PC++
                    if (!getFlag('Z')) {
                        PC = PC + ((N & 0x80) ? N - 0x100 : N)
                    }
                    break;
                };
            case 0x16: break; // blt PC+N Branch if less than
            case 0x17: break; // bge PC+N Branch if greater than or equal
            case 0x18: break; // bgt PC+N Branch if greater than
            case 0x19: break; // ble PC+N Branch if less than or equal
            case 0x1a: // bs1 PC+N Branch if sense switch 1 is set
                {
                    var N = readmem(PC)
                    PC++
                    if (SENSE_SWITCH === 1) {
                        PC = PC + N
                    }
                    break;
                }
            case 0x1b: break; // bs2 PC+N Branch if sense switch 2 is set
            case 0x1c: break; // bs3 PC+N Branch if sense switch 3 is set
            case 0x1d: break; // bs4 PC+N Branch if sense switch 4 is set
            case 0x1e: break; //
            case 0x1f: break; //
            case 0x20: break; // inc _L/H Increment byte of explicit register
            case 0x21: break; // dec _L/H Decrement byte of explicit register
            case 0x22: break; // clr _L/H Clear byte of explicit register (22 32 = CPU ID)
            case 0x23: break; // not _L/H Invert byte of explicit register
            case 0x24: break; // lsl _L/H Shift byte of explicit register left
            case 0x25: break; // lsr _L/H Shift byte of explicit register right
            case 0x26: break; // rrc _L/H Rotate byte of explicit register right (wraps through carry)
            case 0x27: break; // rlc _L/H Rotate byte of explicit register left (wraps through carry)
            case 0x28: break; // inc AL Increment byte of implicit AL register
            case 0x29: break; // dec AL Decrement byte of implicit AL register
            case 0x2a: break; // clr AL Clear byte of implicit AL register
            case 0x2b: break; // not AL Invert byte of implicit AL register
            case 0x2c: // srl AL Shift byte of implicit AL register left
                {
                    let n = readReg('AL')
                    setFlag('C', readReg('AL') & 0x01);
                    setFlag('Z', (readReg('AL') & 0x00FF) == 0x00);
                    setFlag('N', readReg('AL') & 0x0080);

                    let z = (n >> 1) | (n << (8 - 1));



                    writeReg('AL', z)

                    break;
                }
            case 0x2d: break; // sll AL Shift byte of implicit AL register right
            case 0x2e: break; // Memory mapping?
            case 0x2f: break; // DMA
            case 0x30: break; // inc _X Increment full word of explicit register
            case 0x31: break; // dec _X Decrement full word of explicit register
            case 0x32: break; // clr _X Clear full word of explicit register
            case 0x33: break; // not _X Invert full word of explicit register
            case 0x34: break; // lsl _X Shift full word of explicit register left
            case 0x35: break; // lsr _X Shift full word of explicit register right
            case 0x36: break; // rrc _X Rotate full word of explicit register right
            case 0x37: break; // rlc _X Rotate full word of explicit register left
            case 0x38: break; // inc AX Increment full word of implicit AX register
            case 0x39: break; // dec AX Decrement full word of implicit AX register
            case 0x3a: break; // clr AX Clear full word of implicit AX register
            case 0x3b: break; // not AX Invert full word of implicit AX register
            case 0x3c: break; // lsl AX Shift full word of implicit AX register left
            case 0x3d: break; // lsr AX Shift full word of implicit AX register right
            case 0x3e: break; // inc RT Increment full word of implicit RT register
            case 0x3f: break; // dec RT Decrement full word of implicit RT register
            case 0x40: break; // add _L/H, _L/H Add bytes of two explicit registers (left plus right stored in left)
            case 0x41: break; // sub _L/H, _L/H Subtract bytes of two explicit registers (left minus right stored in left)
            case 0x42: break; // and _L/H, _L/H AND bytes of two explicit registers (left AND right stored in left)
            case 0x43: break; // or _L/H, _L/H OR bytes of two explicit registers (left OR right stored in left)
            case 0x44: break; // xor _L/H, _L/H XOR bytes of two explicit registers (left XOR right stored in left)
            case 0x45: break; // mov _L/H, _L/H Copy byte of one explicit register into other explicit register (right into left)
            case 0x46: break; //
            case 0x47: break; // Execute micro code?
            case 0x48: break; // add BL, AL Add bytes of implicit AL and BL (AL plus BL stored in BL)
            case 0x49: break; // sub BL, AL Subtract bytes of implicit AL and BL (AL minus BL stored in BL)
            case 0x4a: break; // and BL, AL AND bytes of implicit AL and BL (AL AND BL stored in BL)
            case 0x4b: break; // or BL, AL OR bytes of implicit AL and BL (AL OR BL stored in BL)
            case 0x4c: break; // xor BL, AL XOR bytes of two implicit registers (AL XOR BL stored in BL)
            case 0x4d: break; // mov BL, AL Copy byte of one implicit register into other explicit register (AL into BL)
            case 0x4e: break; //
            case 0x4f: break; //
            case 0x50: break; // add _X, _X Add two explicit registers (left plus right stored in left)
            case 0x51: break; // sub _X, _X Subtract two explicit registers (left minus right stored in left)
            case 0x52: break; // and _X, _X AND two explicit registers (left AND right stored in left)
            case 0x53: break; // or _X, _X OR two explicit registers (left OR right stored in left)
            case 0x54: break; // xor _X, _X XOR two explicit registers (left XOR right stored in left)
            case 0x55: break; // mov _X, _X Copy one explicit register into other explicit register (right into left)
            case 0x56: break; //
            case 0x57: break; //
            case 0x58: break; // add BX, AX Add implicit AX and BX (AX plus BX stored in BX)
            case 0x59: break; // sub BX, AX Subtract implicit AX and BX (AX minus BX stored in BX)
            case 0x5a: break; // and BX, AX AND implicit AX and BX (AX AND BX stored in BX)
            case 0x5b: break; // or BX, AX OR implicit AX and BX (AX OR BX stored in BX)
            case 0x5c: break; // mov DX, AX Copy byte of one implicit register into other explicit register (AX into DX)
            case 0x5d: break; // mov BX, AX Copy byte of one implicit register into other explicit register (AX into BX)
            case 0x5e: break; // mov EX, AX Copy byte of one implicit register into other explicit register (AX into EX)
            case 0x5f:      // mov SP, AX Copy byte of one implicit register into other explicit register (AX into SP)
                {
                    SP = readReg('AX');
                    break;
                }
            case 0x60: break; // ld CX, #Imm. Load immediate address into full word CX
            case 0x61: break; // ld CX, Addr. Load direct address into full word CX
            case 0x62: break; // ld CX, [Addr.] Load indirect address into full word CX
            case 0x63: break; // ld CX, PC+N Load direct Program Counter offset by N address into full word CX
            case 0x64: break; // ld CX, [PC+N] Load indirect Program Counter offset by N address into full word CX
            case 0x65: break; // ld CX, _[R]_ Load indexed mode register into full word CX
            case 0x66: break; //
            case 0x67: break; //
            case 0x68: break; // st CX, #Imm. Store full word of CX into immediate address
            case 0x69: break; // st CX, Addr. Store full word of CX into direct address
            case 0x6a: break; // st CX, [Addr.] Store full word of CX into indirect address
            case 0x6b: break; // st CX, PC+N Store full word of CX into direct Program Counter offset by N address
            case 0x6c: break; // st CX, [PC+N] Store full word of CX into indirect Program Counter offset by N address
            case 0x6d: break; // st CX, _[R]_ Store full word of CX into indexed mode register
            case 0x6e: break; //
            case 0x6f: break; //
            case 0x70: break; // jump #D Jump to immediate address
            case 0x71: // jump A Jump to direct address
                {
                    const hi = readmem(PC)
                    PC++
                    const lo = readmem(PC)
                    PC++
                    PC = hi << 8 | lo;
                    break;
                }
            case 0x72: break; // jump [A] Jump to indirect address
            case 0x73:  // jump PC+N Jump to direct Program Counter offset by N address
                {
                    const N = readmem(PC)
                    PC++
                    PC = PC + ((N & 0x80) ? N - 0x100 : N)
                    break;
                }
            case 0x74:  // jump [PC+N] Jump to indirect Program Counter offset by N address
                break;
            case 0x75: break; // jump _[R]_ Jump to indexed mode register
            case 0x76: break; // syscall Call interrupt level 15
            case 0x77: break; //
            case 0x78: break; // call #D Call immediate address
            case 0x79: break; // call A Call direct address
            case 0x7a: break; // call [A] Call indirect address
            case 0x7b: // call PC+N Call direct Program Counter offset by N address
                {
                    const N = readmem(PC)
                    PC++
                    //lo
                    SP--
                    writemem(SP, readReg('RT') & 0xFF)
                    //hi
                    SP--
                    writemem(SP, readReg('RT') >> 0x08)

                    writeReg('RT', PC);

                    PC = PC + N
                    break;
                }
            case 0x7c: break; // call [PC+N] Call indirect Program Counter offset by N address
            case 0x7d: break; // call _[R]_ Call indexed mode register
            case 0x7e: break; // Memory banking?
            case 0x7f: break; // Memory banking?
            case 0x80:  // ld AL, #Imm. Load immediate address into byte of AL register
                {
                    const N = readmem(PC)
                    PC++
                    writeReg('AL', N)
                    break;
                }
            case 0x81: // ld AL, Addr. Load direct address into byte of AL register
                {
                    const hi = readmem(PC)
                    PC++
                    const lo = readmem(PC)
                    PC++
                    writeReg('AL', readmem(hi << 8 | lo))
                    break;
                }
            case 0x82: break; // ld AL, [Addr.] Load indirect address into byte of AL register
            case 0x83: break; // ld AL, PC+N Load direct Program Counter offset by N address into byte of AL register
            case 0x84: break; // ld AL, [PC+N] Load indirect Program Counter offset by N address into byte of AL register
            case 0x85:     // ld AL, _[R]_ Load indexed register into byte of AL register
                {
                    const r = readmem(readReg('RT'))
                    setFlag('Z', r === 0)
                    writeReg('AL', r)
                    writeReg('RT', readReg('RT') + 1)

                    break;
                }
            case 0x86: break; //
            case 0x87: break; //
            case 0x88: break; // ld AL, [AX] Load byte from memory address stored in AX into AL register
            case 0x89: break; // ld AL, [BX] Load byte from memory address stored in BX into AL register
            case 0x8a: break; // ld AL, [RT] Load byte from memory address stored in RT into AL register
            case 0x8b: break; // ld AL, [DX] Load byte from memory address stored in DX into AL register
            case 0x8c: break; // ld AL, [EX] Load byte from memory address stored in EX into AL register
            case 0x8d: break; // ld AL, [SP] Load byte from memory address stored in SP into AL register
            case 0x8e: break; //
            case 0x8f: break; //
            case 0x90:      // ld AX, #Imm. Load immediate address into full word of AX register
                {
                    const hi = readmem(PC)
                    PC++
                    const lo = readmem(PC)
                    PC++
                    writeReg('AX', [hi << 8 | lo])
                    break;
                }
            case 0x91: break; // ld AX, Addr. Load direct address into full word of AX register
            case 0x92: break; // ld AX, [Addr.] Load indirect address into full word of AX register
            case 0x93: break; // ld AX, PC+N Load direct Program Counter offset by N address into full word of AX register
            case 0x94: break; // ld AX, [PC+N] Load indirect Program Counter offset by N address into full word of AX register
            case 0x95: break; // ld AX, _[R]_ Load indexed register into full word of AX register
            case 0x96: break; //
            case 0x97: break; //
            case 0x98: break; // ld AX, [AX] Load word from memory address stored in AX into AX register
            case 0x99: break; // ld AX, [BX] Load word from memory address stored in BX into AX register
            case 0x9a: break; // ld AX, [RT] Load word from memory address stored in RT into AX register
            case 0x9b: break; // ld AX, [DX] Load word from memory address stored in DX into AX register
            case 0x9c: break; // ld AX, [EX] Load word from memory address stored in EX into AX register
            case 0x9d: break; // ld AX, [SP] Load word from memory address stored in SP into AX register
            case 0x9e: break; //
            case 0x9f: break; //
            case 0xa0: break; //
            case 0xa1:        // st AL, Addr. Store byte of AL register into direct address
                {
                    const hi = readmem(PC)
                    PC++
                    const lo = readmem(PC)
                    PC++
                    writemem(hi << 8 | lo, readReg('AL'))
                    break;
                }
            case 0xa2: break; // st AL, [Addr.] Store byte of AL register into indirect address
            case 0xa3: break; // st AL, PC+N Store byte of AL register into direct Program Counter offset by N address
            case 0xa4: break; // st AL, [PC+N] Store byte of AL register into indirect Program Counter offset by N address
            case 0xa5: break; // st AL, _[R]_ Store byte of AL register into indexed register
            case 0xa6: break; //
            case 0xa7: break; //
            case 0xa8: break; // st AL, [AX] Store byte from AL register to memory address stored in AX
            case 0xa9: break; // st AL, [BX] Store byte from AL register to memory address stored in BX
            case 0xaa: break; // st AL, [RT] Store byte from AL register to memory address stored in RT
            case 0xab: break; // st AL, [DX] Store byte from AL register to memory address stored in DX
            case 0xac: break; // st AL, [EX] Store byte from AL register to memory address stored in EX
            case 0xad: break; // st AL, [SP] Store byte from AL register to memory address stored in SP
            case 0xae: break; //
            case 0xaf: break; //
            case 0xb0: break; //
            case 0xb1: break; // st AX, Addr. Store AX register into direct address
            case 0xb2: break; // st AX, [Addr.] Store AX register into indirect address
            case 0xb3: break; // st AX, PC+N Store AX register into direct Program Counter offset by N address
            case 0xb4: break; // st AX, [PC+N] Store AX register into indirect Program Counter offset by N address
            case 0xb5: break; // st AX, _[R]_ Store AX register into indexed register
            case 0xb6: break; //
            case 0xb7: break; //
            case 0xb8: break; // st AX, [AX] Store word from AX register to memory address stored in AX
            case 0xb9: break; // st AX, [BX] Store word from AX register to memory address stored in BX
            case 0xba: break; // st AX, [RT] Store word from AX register to memory address stored in RT
            case 0xbb: break; // st AX, [DX] Store word from AX register to memory address stored in DX
            case 0xbc: break; // st AX, [EX] Store word from AX register to memory address stored in EX
            case 0xbd: break; // st AX, [SP] Store word from AX register to memory address stored in SP
            case 0xbe: break; //
            case 0xbf: break; //
            case 0xc0: break; // ld BL, #Imm. Load immediate address into byte ofBL register
            case 0xc1: break; // ld BL, Addr. Load direct address into byte of BL register
            case 0xc2: break; // ld BL, [Addr.] Load indirect address into byte of BL register
            case 0xc3: break; // ld BL, PC+N Load direct Program Counter offset by N address into byte of BL register
            case 0xc4: break; // ld BL, [PC+N] Load indirect Program Counter offset by N address into byte of BL register
            case 0xc5: break; // ld BL, _[R]_ Load indexed register into byte of BL register
            case 0xc6: break; //
            case 0xc7: break; //
            case 0xc8: break; // ld BL, [AX] Load byte from memory address stored in AX into BL register
            case 0xc9: break; // ld BL, [BX] Load byte from memory address stored in BX into BL register
            case 0xca: break; // ld BL, [RT] Load byte from memory address stored in RT into BL register
            case 0xcb: break; // ld BL, [DX] Load byte from memory address stored in DX into BL register
            case 0xcc: break; // ld BL, [EX] Load byte from memory address stored in EX into BL register
            case 0xcd: break; // ld BL, [SP] Load byte from memory address stored in SP into BL register
            case 0xce: break; //
            case 0xcf: break; //
            case 0xd0: break; // ld BX, #Imm. Load immediate address into full word of BX register
            case 0xd1: break; // ld BX, Addr. Load direct address into full word of BX register
            case 0xd2: break; // ld BX, [Addr.] Load indirect address into full word of BX register
            case 0xd3: break; // ld BX, PC+N Load direct Program Counter offset by N address into full word of BX register
            case 0xd4: break; // ld BX, [PC+N] Load indirect Program Counter offset by N address into full word of BX register
            case 0xd5: break; // ld BX, _[R]_ Load indexed register into full word of AX register
            case 0xd6: break; //
            case 0xd7: break; //
            case 0xd8: break; // ld BX, [AX] Load word from memory address stored in AX into BX register
            case 0xd9: break; // ld BX, [BX] Load word from memory address stored in BX into BX register
            case 0xda: break; // ld BX, [RT] Load word from memory address stored in RT into BX register
            case 0xdb: break; // ld BX, [DX] Load word from memory address stored in DX into BX register
            case 0xdc: break; // ld BX, [EX] Load word from memory address stored in EX into BX register
            case 0xdd: break; // ld BX, [SP] Load word from memory address stored in SP into BX register
            case 0xde: break; //
            case 0xdf: break; //
            case 0xe0: break; //
            case 0xe1: break; // st BL, Addr. Store byte of BL register into direct address
            case 0xe2: break; // st BL, [Addr.] Store byte of BL register into indirect address
            case 0xe3: break; // st BL, PC+N Store byte of BL register into direct Program Counter offset by N address
            case 0xe4: break; // st BL, [PC+N] Store byte of BL register into indirect Program Counter offset by N address
            case 0xe5: break; // st BL, _[R]_ Store byte of BL register into indexed register
            case 0xe6: break; //
            case 0xe7: break; //
            case 0xe8: break; // st BL, [AX] Store byte from BL register to memory address stored in AX
            case 0xe9: break; // st BL, [BX] Store byte from BL register to memory address stored in BX
            case 0xea: break; // st BL, [RT] Store byte from BL register to memory address stored in RT
            case 0xeb: break; // st BL, [DX] Store byte from BL register to memory address stored in DX
            case 0xec: break; // st BL, [EX] Store byte from BL register to memory address stored in EX
            case 0xed: break; // st BL, [SP] Store byte from BL register to memory address stored in SP
            case 0xee: break; //
            case 0xef: break; //
            case 0xf0: break; //
            case 0xf1: break; // st BX, Addr. Store BX register into direct address
            case 0xf2: break; // st BX, [Addr.] Store BX register into indirect address
            case 0xf3: break; // st BX, PC+N Store BX register into direct Program Counter offset by N address
            case 0xf4: break; // st BX, [PC+N] Store BX register into indirect Program Counter offset by N address
            case 0xf5: break; // st BX, _[R]_ Store BX register into indexed register
            case 0xf6: break; //
            case 0xf7: break; //
            case 0xf8: break; // st BX, [AX] Store word from BX register to memory address stored in AX
            case 0xf9: break; // st BX, [BX] Store word from BX register to memory address stored in BX
            case 0xfa: break; // st BX, [RT] Store word from BX register to memory address stored in RT
            case 0xfb: break; // st BX, [DX] Store word from BX register to memory address stored in DX
            case 0xfc: break; // st BX, [EX] Store word from BX register to memory address stored in EX
            case 0xfd: break; // st BX, [SP] Store word from BX register to memory address stored in SP
            case 0xfe: break; //
            case 0xff: break; //     
            default:
                break;
        }
        draw();
        await sleep(500)
    }
    log("Halted")

}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

function log(message) {
    const logDiv = document.getElementById('log')
    const newDiv = document.createElement('div');
    const textnode = document.createTextNode(message);
    newDiv.appendChild(textnode);
    logDiv.appendChild(newDiv);
    logDiv.scrollTo(0, logDiv.scrollHeight)
}

function setup() {
    const flagsDiv = document.getElementById('flags')
    const registersDiv = document.getElementById('registers')
    //Flags
    for (let flag in FLAGS) {
        const label = document.createElement('div');
        const textnode = document.createTextNode(flag);
        label.appendChild(textnode);
        const value = document.createElement('div');
        value.id = flag
        const spacer = document.createElement('div');
        flagsDiv.appendChild(label);
        flagsDiv.appendChild(value);
        flagsDiv.appendChild(spacer);

    }

    for (let register in REGISTERS) {
        const label = document.createElement('div');
        const textnode = document.createTextNode(register);
        label.appendChild(textnode);
        const valueLo = document.createElement('div');
        valueLo.id = register + 'Lo'
        const valueHi = document.createElement('div');
        valueHi.id = register + 'Hi'
        registersDiv.appendChild(label);
        registersDiv.appendChild(valueHi);
        registersDiv.appendChild(valueLo);
    }
}

function draw() {
    for (let flag in FLAGS) {
        const flagDiv = document.getElementById(flag);
        flagDiv.innerHTML = getFlag(flag)
    }
    for (let register in REGISTERS) {
        const registerDivLo = document.getElementById(register + 'Lo');
        const registerDivHi = document.getElementById(register + 'Hi');
        registerDivLo.innerHTML = '0x' + (readReg(register) & 0xff).toString(16).padStart(2, '0')
        registerDivHi.innerHTML = '0x' + (readReg(register) >> 8).toString(16).padStart(2, '0')
    }
}

/* Bit 0 of control is char pending. The real system uses mark parity so
   we erm ignore that */
function mux_write(addr, val) {
    let mux, data;
    addr &= 0xFF;
    mux = addr >> 1;
    data = addr & 1;

    if (mux == 0 && data) {
        if (val !== 0x8c) {
            val &= 0x7F;
            const mux = document.getElementById('mux');
            mux.innerHTML = mux.innerHTML + String.fromCharCode(val & 0x7F)
        }
    }
}

function mux_read(addr) {
    let mux, data;
    let ttystate;
    let ctrl = 0;

    addr &= 0xFF;
    mux = addr >> 1;
    data = addr & 1;

    if (mux != 0)
        return 0;

    if (data == 1)
        return 'A';
    return 2;
}