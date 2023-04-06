class RiscVEmulator {
  constructor() {
    this.pc = 0x0; // Program counter
    this.registers = new Int32Array(32); // 32 integer registers
    this.memory = new DataView(new ArrayBuffer(0x100000)); // 1MB memory
    this.pcStart = 0;
    this.externalInterrupts = 0;
    // Supported CSRs
    this.CSR = {
        mstatus: 0,
        mie: 0,
        mtvec: 0,
        mepc: 0,
        mcause: 0,
        mip: 0
    };
  }

  reset() {
    this.registers.fill(0);
    this.pc = 0x0;
  }

  requestExternalInterrupt(interruptNumber) {
    this.externalInterrupts |= (1 << interruptNumber);
  }

  getCSR(address) {
    switch (address) {
      // ...
      case 0x300: // mstatus
        return this.CSR.mstatus;
      case 0x304: // mie
        return this.CSR.mie;
      case 0x305: // mtvec
        return this.CSR.mtvec;
      case 0x341: // mepc
        return this.CSR.mepc;
      case 0x342: // mcause
        return this.CSR.mcause;
      case 0x344: // mip
        return this.CSR.mip;
      // ...
      default:
        throw new Error(`Unsupported CSR address: ${address}`);
    }
  }

  setCSR(address, value) {
    switch (address) {
      // ...
      case 0x300: // mstatus
        this.CSR.mstatus = value;
        break;
      case 0x304: // mie
        this.CSR.mie = value;
        break;
      case 0x305: // mtvec
        this.CSR.mtvec = value;
        break;
      case 0x341: // mepc
        this.CSR.mepc = value;
        break;
      case 0x342: // mcause
        this.CSR.mcause = value;
        break;
      case 0x344: // mip
        this.CSR.mip = value;
        break;
      // ...
      default:
        throw new Error(`Unsupported CSR address: ${address}`);
    }
  }

  loadProgram(program, isCompressed = false) {
    if (isCompressed) {
      for (let i = 0; i < program.length; i++) {
        const instruction = program[i];
        this.memory.setUint16(this.pc + i * 2, instruction);
      }
    } else {
      for (let i = 0; i < program.length; i++) {
        const instruction = program[i];
        this.memory.setUint32(this.pc + i * 4, instruction);
      }
    }
    // Set the program counter to the start of the loaded program
    this.pc = this.pcStart;
  }

  expandCompressedInstruction(instruction) {
    const opcode = instruction & 0x3;
    const rd = (instruction >> 7) & 0x1f;
    const rs1 = (instruction >> 7) & 0x1f;
    const rs2 = (instruction >> 2) & 0x1f;
    const funct3 = (instruction >> 13) & 0x7;
    let expandedInstruction = 0;

    switch (opcode) {
      case 0x0: // C0: Quadrant 0
        switch (funct3) {
          case 0x0: // C.ADDI4SPN
            const imm = ((instruction >> 5) & 0x3) | ((instruction >> 7) & 0x1c) | ((instruction << 1) & 0x40);
            expandedInstruction = 0x13 | ((rd + 8) << 7) | (2 << 15) | (imm << 20);
            break;
          // ??? Other Quadrant 0 instructions...
        }
        break;
      case 0x1: // C1: Quadrant 1
        switch (funct3) {
          case 0x0: // C.LW
            const immLw = ((instruction >> 7) & 0x38) | ((instruction >> 4) & 0x7);
            expandedInstruction = 0x03 | (2 << 12) | ((rs1 + 8) << 15) | (immLw << 20) | ((rd + 8) << 7);
            break;
          case 0x4: // C.SW
            const immSw = ((instruction >> 7) & 0x38) | ((instruction >> 4) & 0x7);
            expandedInstruction = 0x23 | (2 << 12) | ((rs1 + 8) << 15) | ((rd + 8) << 20) | (immSw << 7);
            break;
          // ??? Other Quadrant 1 instructions...
        }
        break;
      case 0x2: // C2: Quadrant 2
        switch (funct3) {
          case 0x0: // C.JR or C.MV
            if (rd === 0) {
              expandedInstruction = 0x67 | (rs1 << 15); // C.JR
            } else {
              expandedInstruction = 0x33 | (rs1 << 15) | (rs2 << 20) | (rd << 7); // C.MV
            }
            break;
          case 0x1: // C.ADD
            expandedInstruction = 0x33 | (0 << 12) | (rs1 << 15) | (rs2 << 20) | (rd << 7);
            break;
          case 0x2: // C.LI
            const immLi = ((instruction >> 2) & 0x1f) | ((instruction >> 7) & 0x20);
            expandedInstruction = 0x13 | (rd << 7) | (immLi << 20);
            break;
          // ??? Other Quadrant 2 instructions...
        }
        break;
      case 0x3: // C3: Quadrant 3
        switch (funct3) {
          case 0x0: // C.LD
            const immLd = ((instruction >> 5) & 0x3) | ((instruction >> 7) & 0x1c) | ((instruction << 1) & 0x40);
            expandedInstruction = 0x03 | (3 << 12) | ((rs1 + 8) << 15) | (immLd << 20) | ((rd + 8) << 7);
            break;
          case 0x4: // C.SD
            const immSd = ((instruction >> 5) & 0x3) | ((instruction >> 7) & 0x1c) | ((instruction << 1) & 0x40);
            expandedInstruction = 0x23 | (3 << 12) | ((rs1 + 8) << 15) | ((rd + 8) << 20) | (immSd << 7);
            break;
          // ??? Other Quadrant 3 instructions...
        }
        break;
      default:
        throw new Error(`Unknown compressed opcode: 0x${opcode.toString(16)}`);
    }

    return expandedInstruction;
  }

  fetchInstruction() {
    const instruction = this.memory.getUint16(this.pc);
    this.pc += 2;

    // Check if the instruction is a compressed instruction (not 11 in the two least significant bits)
    if ((instruction & 0x3) !== 0x3) {
      // Expand the compressed instruction to a 32-bit instruction
      const expandedInstruction = this.expandCompressedInstruction(instruction);
      return expandedInstruction;
    }

    // Fetch the remaining 16 bits for a 32-bit instruction and combine them
    const instructionUpper = this.memory.getUint16(this.pc) << 16;
    this.pc += 2;
    return instruction | instructionUpper;
  }

  step(instruction) {
    const opcode = instruction & 0x7f;

    switch (opcode) {
      // Handle RV32I instructions
      case 0x37: {
        // LUI
        const rd = (instruction >> 7) & 0x1f;
        const imm = instruction & 0xfffff000;
        this.registers[rd] = imm;
        break;
      }
      case 0x17: {
        // AUIPC
        const rd = (instruction >> 7) & 0x1f;
        const imm = instruction & 0xfffff000;
        this.registers[rd] = this.pc - 4 + imm;
        break;
      }
      case 0x6f: {
        // JAL
        const rd = (instruction >> 7) & 0x1f;
        const imm =
          ((instruction & 0x80000000) >> 20) | // imm[20]
          ((instruction & 0x7fe00000) >> 20) | // imm[10:1]
          ((instruction & 0x00100000) >> 9) | // imm[11]
          ((instruction & 0x000ff000) >> 20); // imm[19:12]
        this.registers[rd] = this.pc;
        this.pc = this.pc - 4 + imm;
        break;
      }
      case 0x67: {
        // JALR
        const rd = (instruction >> 7) & 0x1f;
        const rs1 = (instruction >> 15) & 0x1f;
        const imm = (instruction & 0xfff00000) >> 20;
        const signedImm = (imm << 20) >> 20; // Sign extend imm
        const nextPc = this.pc;
        this.pc = (this.registers[rs1] + signedImm) & ~1;
        this.registers[rd] = nextPc;
        break;
      }
      case 0x63: {
        // BEQ, BNE, BLT, BGE, BLTU, BGEU
        const funct3 = (instruction >> 12) & 0x7;
        const rs1 = (instruction >> 15) & 0x1f;
        const rs2 = (instruction >> 20) & 0x1f;
        const imm =
          ((instruction & 0x80000000) >> 19) | // imm[12]
          ((instruction & 0x7e000000) >> 20) | // imm[10:5]
          ((instruction & 0x00001000) >> 7) | // imm[4:1]
          ((instruction & 0x00000800) >> 20); // imm[11]

        const signedImm = (imm << 20) >> 20; // Sign extend imm
        const valRs1 = this.registers[rs1];
        const valRs2 = this.registers[rs2];
        let shouldBranch = false;

        switch (funct3) {
          case 0x0: // BEQ
            shouldBranch = valRs1 === valRs2;
            break;
          case 0x1: // BNE
            shouldBranch = valRs1 !== valRs2;
            break;
          case 0x4: // BLT
            shouldBranch = valRs1 < valRs2;
            break;
          case 0x5: // BGE
            shouldBranch = valRs1 >= valRs2;
            break;
          case 0x6: // BLTU
            shouldBranch = valRs1 >>> 0 < valRs2 >>> 0;
            break;
          case 0x7: // BGEU
            shouldBranch = valRs1 >>> 0 >= valRs2 >>> 0;
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x63: 0x${funct3.toString(16)}`);
        }

        if (shouldBranch) {
          this.pc = this.pc - 4 + signedImm;
        }

        break;
      }
      case 0x03: {
        // LB, LH, LW, LBU, LHU
        const funct3 = (instruction >> 12) & 0x7;
        const rd = (instruction >> 7) & 0x1f;
        const rs1 = (instruction >> 15) & 0x1f;
        const imm = (instruction & 0xfff00000) >> 20;
        const signedImm = (imm << 20) >> 20; // Sign extend imm
        const address = this.registers[rs1] + signedImm;

        switch (funct3) {
          case 0x0: // LB
            this.registers[rd] = this.memory.getInt8(address);
            break;
          case 0x1: // LH
            this.registers[rd] = this.memory.getInt16(address, true);
            break;
          case 0x2: // LW
            this.registers[rd] = this.memory.getInt32(address, true);
            break;
          case 0x4: // LBU
            this.registers[rd] = this.memory.getUint8(address);
            break;
          case 0x5: // LHU
            this.registers[rd] = this.memory.getUint16(address, true);
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x03: 0x${funct3.toString(16)}`);
        }

        break;
      }
      case 0x23: {
        // SB, SH, SW
        const funct3 = (instruction >> 12) & 0x7;
        const rs1 = (instruction >> 15) & 0x1f;
        const rs2 = (instruction >> 20) & 0x1f;
        const imm =
          ((instruction & 0xfe000000) >> 20) | // imm[11:5]
          ((instruction & 0x00000f80) >> 7); // imm[4:0]
        const signedImm = (imm << 20) >> 20; // Sign extend imm
        const address = this.registers[rs1] + signedImm;
        const value = this.registers[rs2];

        switch (funct3) {
          case 0x0: // SB
            this.memory.setInt8(address, value);
            break;
          case 0x1: // SH
            this.memory.setInt16(address, value, true);
            break;
          case 0x2: // SW
            this.memory.setInt32(address, value, true);
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x23: 0x${funct3.toString(16)}`);
        }

        break;
      }
      case 0x13: {
        // ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI
        const funct3 = (instruction >> 12) & 0x7;
        const rd = (instruction >> 7) & 0x1f;
        const rs1 = (instruction >> 15) & 0x1f;
        const imm = (instruction & 0xfff00000) >> 20;
        const signedImm = (imm << 20) >> 20; // Sign extend imm
        const shamt = imm & 0x1f;
        const funct7 = (instruction >> 25) & 0x7f;
        const valRs1 = this.registers[rs1];

        switch (funct3) {
          case 0x0: // ADDI
            this.registers[rd] = valRs1 + signedImm;
            break;
          case 0x1: // SLLI
            this.registers[rd] = valRs1 << shamt;
            break;
          case 0x2: // SLTI
            this.registers[rd] = valRs1 < signedImm ? 1 : 0;
            break;
          case 0x3: // SLTIU
            this.registers[rd] = valRs1 >>> 0 < signedImm >>> 0 ? 1 : 0;
            break;
          case 0x4: // XORI
            this.registers[rd] = valRs1 ^ signedImm;
            break;
          case 0x5: // SRLI, SRAI
            if (funct7 === 0x00) {
              this.registers[rd] = valRs1 >>> shamt;
            } else if (funct7 === 0x20) {
              this.registers[rd] = valRs1 >> shamt;
            } else {
              throw new Error(`Unknown funct7 for opcode 0x13 and funct3 0x5: 0x${funct7.toString(16)}`);
            }
            break;
          case 0x6: // ORI
            this.registers[rd] = valRs1 | signedImm;
            break;
          case 0x7: // ANDI
            this.registers[rd] = valRs1 & signedImm;
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x13: 0x${funct3.toString(16)}`);
        }
        break;
      }

      case 0x33: {
        // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND, M-extension instructions
        const funct3 = (instruction >> 12) & 0x7;
        const rd = (instruction >> 7) & 0x1f;
        const rs1 = (instruction >> 15) & 0x1f;
        const rs2 = (instruction >> 20) & 0x1f;
        const funct7 = (instruction >> 25) & 0x7f;
        const valRs1 = this.registers[rs1];
        const valRs2 = this.registers[rs2];

        switch (funct3) {
          case 0x0: // MUL, ADD, SUB
            if (funct7 === 0x00) {
              // ADD
              this.registers[rd] = valRs1 + valRs2;
            } else if (funct7 === 0x01) {
              // MUL
              this.registers[rd] = (valRs1 * valRs2) | 0;
            } else if (funct7 === 0x20) {
              // SUB
              this.registers[rd] = valRs1 - valRs2;
            } else {
              throw new Error(`Unknown funct7 for opcode 0x33 and funct3 0x0: 0x${funct7.toString(16)}`);
            }
            break;
          case 0x1: // MULH, SLL
            if (funct7 === 0x01) {
              const result = BigInt(valRs1) * BigInt(valRs2);
              this.registers[rd] = Number(result >> BigInt(32)); // MULH
            } else {
              this.registers[rd] = valRs1 << (valRs2 & 0x1f); // SLL
            }
            break;
          case 0x2: // MULHSU, SLT
            if (funct7 === 0x01) {
              const result = BigInt(valRs1) * BigInt(valRs2 >>> 0);
              this.registers[rd] = Number(result >> BigInt(32)); // MULHSU
            } else {
              this.registers[rd] = valRs1 < valRs2 ? 1 : 0; // SLT
            }
            break;
          case 0x3: // MULHU, SLTU
            if (funct7 === 0x01) {
              const result = BigInt(valRs1 >>> 0) * BigInt(valRs2 >>> 0);
              this.registers[rd] = Number(result >> BigInt(32)); // MULHU
            } else {
              this.registers[rd] = valRs1 >>> 0 < valRs2 >>> 0 ? 1 : 0; // SLTU
            }
            break;
          case 0x4: // DIV, XOR
            if (funct7 === 0x01) {
              this.registers[rd] = (valRs1 / valRs2) | 0; // DIV
            } else {
              this.registers[rd] = valRs1 ^ valRs2; // XOR case...
            }
            break;
          case 0x5: // DIVU, SRL, SRA
            if (funct7 === 0x00) {
              // SRL
              this.registers[rd] = valRs1 >>> (valRs2 & 0x1f);
            } else if (funct7 === 0x01) {
              // DIVU
              this.registers[rd] = ((valRs1 >>> 0) / (valRs2 >>> 0)) | 0;
            } else if (funct7 === 0x20) {
              // SRA
              this.registers[rd] = valRs1 >> (valRs2 & 0x1f);
            } else {
              throw new Error(`Unknown funct7 for opcode 0x33 and funct3 0x5: 0x${funct7.toString(16)}`);
            }
            break;
          case 0x6: // REM, OR
            if (funct7 === 0x01) {
              this.registers[rd] = valRs1 % valRs2; // REM
            } else {
              this.registers[rd] = valRs1 | valRs2; // OR case...
            }
            break;
          case 0x7: // REMU, AND
            if (funct7 === 0x01) {
              this.registers[rd] = (valRs1 >>> 0) % (valRs2 >>> 0) | 0; // REMU
            } else {
              this.registers[rd] = valRs1 & valRs2; // AND case...
            }
        }
        break;
      }
      case 0x0f: {
        // FENCE, FENCE.I
        const funct3 = (instruction >> 12) & 0x7;

        switch (funct3) {
          case 0x0: // FENCE
            // FENCE is a memory ordering instruction, but since JavaScript is single-threaded,
            // we don't need to implement any specific behavior here.
            break;
          case 0x1: // FENCE.I
            // FENCE.I is used for instruction cache synchronization. In our simplified emulator,
            // we don't have an instruction cache, so we don't need to implement any specific behavior here.
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x0F: 0x${funct3.toString(16)}`);
        }

        break;
      }
      case 0x73: {
        // SYSTEM instructions
        const funct3 = (instruction >> 12) & 0x7;
        const rd = (instruction >> 7) & 0x1f;
        const rs1 = (instruction >> 15) & 0x1f;
        const csr = (instruction >> 20) & 0xfff;
        const funct12 = instruction & 0xfff00000;
        const imm = (instruction & 0xfff00000) >> 20;
        const valRs1 = this.registers[rs1];

        switch (funct3) {
          case 0x0:
            if (funct12 === 0x000) {
              // ECALL
              // Implement handling for environment call, depending on the execution environment
            } else if (funct12 === 0x00100000) {
              // EBREAK
              // Implement handling for breakpoint, depending on the execution environment
            } else {
              throw new Error(`Unknown funct12 for opcode 0x73 and funct3 0x0: 0x${funct12.toString(16)}`);
            }
            break;
          case 0x1: // CSRRW
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, valRs1);
            break;
          case 0x2: // CSRRS
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, this.getCSR(csr) | valRs1);
            break;
          case 0x3: // CSRRC
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, this.getCSR(csr) & ~valRs1);
            break;
          case 0x5: // CSRRWI
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, imm);
            break;
          case 0x6: // CSRRSI
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, this.getCSR(csr) | imm);
            break;
          case 0x7: // CSRRCI
            this.registers[rd] = this.getCSR(csr);
            this.setCSR(csr, this.getCSR(csr) & ~imm);
            break;
          default:
            throw new Error(`Unknown funct3 for opcode 0x73: 0x${funct3.toString(16)}`);
        }

        break;
      }

      // Handle C extension instructions
      case 0x00: // C.ADDI4SPN, C.FLD, C.LW, C.FLW, C.FSD, C.SW, C.FSW
      case 0x01: // C.ADDI, C.JAL, C.LI, C.ADDI16SP, C.LUI, C.SRLI, C.SRAI, C.ANDI, C.SUB, C.XOR, C.OR, C.AND, C.J, C.BEQZ, C.BNEZ
      case 0x02: // C.SLLI, C.FLDSP, C.LWSP, C.FLWSP, C.JR, C.MV, C.EBREAK, C.JALR, C.ADD, C.FSDSP, C.SWSP, C.FSWSP
        // Implement C extension functionality
        break;

      default:
        throw new Error(`Unknown opcode: 0x${opcode.toString(16)}`);
    }
  }

  checkAndHandleInterrupts() {
    const MIE_BIT = 0x8; // Bit 3 of mstatus
    const INTERRUPT_ENABLED = (this.CSR.mstatus & MIE_BIT) !== 0;
  
    if (INTERRUPT_ENABLED) {
      const PENDING_INTERRUPTS = (this.CSR.mip | this.externalInterrupts) & this.CSR.mie;

      if (PENDING_INTERRUPTS !== 0) {
        // Save the current PC to mepc
        this.CSR.mepc = this.pc;
  
        // Set mcause to the interrupt cause (highest priority interrupt)
        this.CSR.mcause = 31 - Math.clz32(PENDING_INTERRUPTS);
  
        // Clear MIE bit in mstatus to disable interrupts during handling
        this.CSR.mstatus &= ~MIE_BIT;
  
        // Set the PC to the interrupt handler address in mtvec
        this.pc = this.CSR.mtvec;
        return true;
      }
    }
    return false;
  }


  run(maxInstructions = Infinity) {
    let instructionsExecuted = 0;

    while (instructionsExecuted < maxInstructions) {
        if(!checkAndHandleInterrupts()) {
            // Fetch the next instruction
            const instruction = this.fetchInstruction();

            // Check for termination conditions
            if (instruction === 0x00000013) {
                // NOP instruction (ADDI x0, x0, 0)
                console.log("NOP instruction encountered. Stopping execution.");
                break;
            }

            this.step(instruction);

            // Increment the instructions executed counter
            instructionsExecuted++;
        }
    }

    console.log(`Execution finished. ${instructionsExecuted} instructions executed.`);
  }
}
