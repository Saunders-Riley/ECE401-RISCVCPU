`define RISCV_RV32I_INSTR_NOP               32'h00000033    // add x0, x0, x0

// Opcodes - RV32I base instruction set
`define RISCV_RV32I_OPCODE_ARITH            7'h33       // arithmetic instructions
`define RISCV_RV32I_OPCODE_ARITH_IMM        7'h13       // arithmetic instructions with immediates
`define RISCV_RV32I_OPCODE_LOAD             7'h03       // load instructions
`define RISCV_RV32I_OPCODE_STORE            7'h23       // store instructions
`define RISCV_RV32I_OPCODE_BRANCH           7'h63       // branch instructions
`define RISCV_RV32I_OPCODE_JAL              7'h6F       // PC-relative jump instructions
`define RISCV_RV32I_OPCODE_JAL_REG          7'h67       // indirect jump instructions
`define RISCV_RV32I_OPCODE_LUI              7'h37       // load-upper instruction
`define RISCV_RV32I_OPCODE_AUIPC            7'h17       // add upper to PC instruction
`define RISCV_RV32I_OPCODE_SYSTEM           7'h73       // system instructions

// Funct3/7 Mappings - arithmetic
`define RISCV_RV32I_FUNCT3_ARITH_ADD        3'h0        // rd = rs1 + rs2
`define RISCV_RV32I_FUNCT7_ARITH_SUB        7'h20       // rd = rs1 - rs2               [funct3 = FUNCT3_ARITH_ADD]
`define RISCV_RV32I_FUNCT3_ARITH_XOR        3'h4        // rd = rs1 ^ rs2
`define RISCV_RV32I_FUNCT3_ARITH_OR         3'h6        // rd = rs1 | rs2
`define RISCV_RV32I_FUNCT3_ARITH_AND        3'h7        // rd = rs1 & rs2
`define RISCV_RV32I_FUNCT3_ARITH_LSH        3'h1        // rd = rs1 << rs2
`define RISCV_RV32I_FUNCT3_ARITH_RSH        3'h5        // rd = rs1 >> rs2
`define RISCV_RV32I_FUNCT7_ARITH_RSHA       7'h20       // rd = rs1 >>> rs2             [funct3 = FUNCT3_ARITH_RSH]
`define RISCV_RV32I_FUNCT3_ARITH_SLT        3'h2        // rd = (rs1 < rs2) ? 1 : 0
`define RISCV_RV32I_FUNCT3_ARITH_SLTU       3'h3        // rd = (rs1 < rs2) ? 1 : 0     [unsigned]

// Funct3/7 Mappings - load
`define RISCV_RV32I_FUNCT3_LOAD_LB          3'h0        // rd = mem[rs1][7:0]           [sign-extends]
`define RISCV_RV32I_FUNCT3_LOAD_LBU         3'h4        // rd = mem[rs1][7:0]
`define RISCV_RV32I_FUNCT3_LOAD_LH          3'h1        // rd = mem[rs1][15:0]          [sign-extends]
`define RISCV_RV32I_FUNCT3_LOAD_LHU         3'h5        // rd = mem[rs1][15:0]
`define RISCV_RV32I_FUNCT3_LOAD_LW          3'h2        // rd = mem[rs1][31:0]

// Funct3/7 Mappings - store
`define RISCV_RV32I_FUNCT3_STORE_SB         3'h0        // mem[rs1][7:0] = rs2[7:0]
`define RISCV_RV32I_FUNCT3_STORE_SH         3'h1        // mem[rs1][15:0] = rs2[15:0]
`define RISCV_RV32I_FUNCT3_STORE_SW         3'h2        // mem[rs1][31:0] = rs2[31:0]

// Funct3/7 Mappings - branch
`define RISCV_RV32I_FUNCT3_BEQ              3'h0        // PC += (rs1 == rs2) ? imm : 4
`define RISCV_RV32I_FUNCT3_BNE              3'h1        // PC += (rs1 != rs2) ? imm : 4
`define RISCV_RV32I_FUNCT3_BLT              3'h4        // PC += (rs1 < rs2) ? imm : 4
`define RISCV_RV32I_FUNCT3_BGE              3'h5        // PC += (rs1 >= rs2) ? imm : 4
`define RISCV_RV32I_FUNCT3_BLTU             3'h6        // PC += (rs1 < rs2) ? imm : 4  [unsigned]
`define RISCV_RV32I_FUNCT3_BGEU             3'h7        // PC += (rs1 >= rs2) ? imm : 4 [unsigned]

// Funct3/7 Mappings - system
`define RISCV_RV32I_FUNCT3_SYSTEM_ECALL     3'h0        // Environment Call
`define RISCV_RV32I_IMM12_SYSTEM_ECALL      12'h000     // Environment Call - transfer control to OS
`define RISCV_RV32I_FUNCT3_SYSTEM_EBREAK    3'h0        // Breakpoint
`define RISCV_RV32I_IMM12_SYSTEM_EBREAK     12'h001     // Breakpoint - transfer control to debugger
`define RISCV_RV32I_FUNCT3_SYSTEM_MRET      3'h0        // Exception Return - PC = MEPC + 4
`define RISCV_RV32I_IMM12_SYSTEM_MRET       12'h302     // Exception Return - PC = MEPC + 4
`define RISCV_RV32I_FUNCT3_SYSTEM_WFI       3'h0        // Wait For Interrupt
`define RISCV_RV32I_IMM12_SYSTEM_WFI        12'h105     // Wait For Interrupt
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRW     3'h1        // rd = csr; csr = rs           [swap csr and rd]
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRS     3'h2        // rd = csr; csr |= rs          [bitwise set csr]
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRC     3'h3        // rd = csr; csr &= ~rs         [bitwise unset csr]
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRWI    3'h5        // rd = csr; csr = uimm4        [uimm4 is [19:15] in I-type]
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRSI    3'h6        // rd = csr; csr |= uimm4
`define RISCV_RV32I_FUNCT3_SYSTEM_CSRRCI    3'h7        // rd = csr; csr &= ~uimm4

