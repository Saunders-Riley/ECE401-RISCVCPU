`include "riscv_instruction_set.vh"

module riscv_cpu_exec_unit(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // Pipeline Interface from Fetch
    input   logic[31:0] pipe_in_instr,
    input   logic[31:0] pipe_in_pcaddr,
    input   logic       pipe_stall_in,
    output  logic       fetch_stall_out,
    output  logic       fetch_branch,
    output  logic[31:0] fetch_branch_pcaddr,
    // AHB Data Interface
    // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
    // respectively
    output  logic[31:0] m_ahb_haddr,
    output  logic[2:0]  m_ahb_hsize,
    output  logic[1:0]  m_ahb_htrans,
    output  logic[31:0] m_ahb_hwdata,
    output  logic[3:0]  m_ahb_hwstrb,
    output  logic       m_ahb_hwrite,
    input   logic[31:0] m_ahb_hrdata,
    input   logic       m_ahb_hreadyin,
    input   logic       m_ahb_hresp,
);
    // Pipeline Stage 2 - Instruction Decode
    logic[31:0]     pl_fetch_instr;
    logic[31:0]     pl_fetch_pcaddr;
    logic[6:0]      pl_fetch_funct7     = pl_fetch_instr[31:25];
    logic[4:0]      pl_fetch_rs2        = pl_fetch_instr[24:20];
    logic[4:0]      pl_fetch_rs1        = pl_fetch_instr[19:15];
    logic[2:0]      pl_fetch_funct3     = pl_fetch_instr[14:12];
    logic[4:0]      pl_fetch_rd         = pl_fetch_instr[11:7];
    logic[6:0]      pl_fetch_opcode     = pl_fetch_instr[6:0];
    logic[11:0]     pl_fetch_imm_I      = pl_fetch_instr[11:0];
    logic[11:0]     pl_fetch_imm_S      = {pl_fetch_funct7, pl_fetch_rd};
    logic[12:0]     pl_fetch_imm_B      = {pl_fetch_funct7[6], pl_fetch_rd[0], pl_fetch_funct7[5:0], pl_fetch_rd[4:1], 1'b0};
    logic[19:0]     pl_fetch_imm_U      = pl_fetch_instr[31:12];
    logic[20:0]     pl_fetch_imm_J      = {pl_fetch_imm_U[19], pl_fetch_imm_U[7:0], pl_fetch_imm_U[8], pl_fetch_imm_U[18:9], 1'b0};

    // Pipeline Stage 3 - Instruction Execute
    logic[31:0]     pl_exec_instr;
    logic[31:0]     pl_exec_pcaddr;
    logic[31:0]     pl_exec_op1;
    logic[31:0]     pl_exec_op2;
    logic[31:0]     pl_exec_op3;
    logic[6:0]      pl_exec_funct7      = pl_exec_instr[31:25];
    logic[4:0]      pl_exec_rs2         = pl_exec_instr[24:20];
    logic[4:0]      pl_exec_rs1         = pl_exec_instr[19:15];
    logic[2:0]      pl_exec_funct3      = pl_exec_instr[14:12];
    logic[4:0]      pl_exec_rd          = pl_exec_instr[11:7];
    logic[6:0]      pl_exec_opcode      = pl_exec_instr[6:0];
    logic[11:0]     pl_exec_imm_I       = pl_exec_instr[11:0];
    logic[11:0]     pl_exec_imm_S       = {pl_exec_funct7, pl_exec_rd};
    logic[12:0]     pl_exec_imm_B       = {pl_exec_funct7[6], pl_exec_rd[0], pl_exec_funct7[5:0], pl_exec_rd[4:1], 1'b0};
    logic[19:0]     pl_exec_imm_U       = pl_exec_instr[31:12];
    logic[20:0]     pl_exec_imm_J       = {pl_exec_imm_U[19], pl_exec_imm_U[7:0], pl_exec_imm_U[8], pl_exec_imm_U[18:9], 1'b0};

    logic[31:0]     pl_exec_op1_fwd;
    logic[31:0]     pl_exec_op2_fwd;
    logic[31:0]     pl_exec_op3_fwd;
    logic[31:0]     alu_res;
    logic[31:0]     alu_byp;

    // Pipeline Stage 4 - Instruction Writeback
    logic[31:0]     pl_wb_instr;
    logic[31:0]     pl_wb_pcaddr;
    logic[31:0]     pl_wb_res;
    logic[31:0]     pl_wb_byp;
    logic[6:0]      pl_wb_funct7        = pl_wb_instr[31:25];
    logic[4:0]      pl_wb_rs2           = pl_wb_instr[24:20];
    logic[4:0]      pl_wb_rs1           = pl_wb_instr[19:15];
    logic[2:0]      pl_wb_funct3        = pl_wb_instr[14:12];
    logic[4:0]      pl_wb_rd            = pl_wb_instr[11:7];
    logic[6:0]      pl_wb_opcode        = pl_wb_instr[6:0];
    logic[11:0]     pl_wb_imm_I         = pl_wb_instr[11:0];
    logic[11:0]     pl_wb_imm_S         = {pl_wb_funct7, pl_wb_rd};
    logic[12:0]     pl_wb_imm_B         = {pl_wb_funct7[6], pl_wb_rd[0], pl_wb_funct7[5:0], pl_wb_rd[4:1], 1'b0};
    logic[19:0]     pl_wb_imm_U         = pl_wb_instr[31:12];
    logic[20:0]     pl_wb_imm_J         = {pl_wb_imm_U[19], pl_wb_imm_U[7:0], pl_wb_imm_U[8], pl_wb_imm_U[18:9], 1'b0};

    // Arithmetic Unit
    riscv_cpu_alu alu_inst(
        // Core Signals
        .cpu_clk(cpu_clk),
        .cpu_resetn(cpu_resetn),
        // Control Inputs
        .opcode(pl_exec_opcode),
        .funct3(pl_exec_funct3),
        .funct7(pl_exec_funct7),
        // Operand Inputs
        .op1(pl_exec_op1_fwd),
        .op2(pl_exec_op2_fwd),
        .op3(pl_exec_op3_fwd),
        // Result Outputs
        .res(alu_res),
        .byp(alu_byp)
    );

    // Register File
    riscv_cpu_regs regs_inst(
        // Core Signals
        .cpu_clk(),
        .cpu_resetn(),
        // Source Register 1
        .rs1_sel(),
        .rs1_data(),
        // Source Register 2
        .rs2_sel(),
        .rs2_data(),
        // Destination Register 1
        .rd1_sel(),
        .rd1_data(),
        .rd1_wren(),
        // Destination Register 2
        // NOTE: this is only used for some SYSTEM instructions
        //  where there's a single-cycle swap
        .rd2_sel(),
        .rd2_data
        .rd2_wren(),
    );

endmodule

module riscv_cpu_alu(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // Control Inputs
    input   logic[6:0]  opcode,
    input   logic[2:0]  funct3,
    input   logic[6:0]  funct7,
    // Operand Inputs
    input   logic[31:0] op1,
    input   logic[31:0] op2,
    input   logic[31:0] op3,
    // Result Outputs
    output  logic[31:0] res,
    output  logic[31:0] byp
);
    // This is entirely combinational logic, pipeline is implemented a layer
    // up in riscv_cpu_exec_unit
    always @(*) begin
        case(opcode)
            // Arithmetic Instructions, Arithmetic Instructions with Immediate
            // op1 = arithmetic operand 1
            // op2 = arithmetic operand 2
            // op3 = (unused)
            // res = arithmetic result
            // byp = (unused)
            RISCV_RV32I_OPCODE_ARITH,
            RISCV_RV32I_OPCODE_ARITH_IMM: begin
                byp = 32'h0000_0000;
                case(funct3)
                    RISCV_RV32I_FUNCT3_ARITH_ADD : begin
                        res = (funct7 == RISCV_RV32I_FUNCT7_ARITH_SUB) ? op1 - op2 : op1 + op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_XOR : begin
                        res = op1 ^ op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_OR  : begin
                        res = op1 | op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_AND : begin
                        res = op1 & op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_LSH : begin
                        res = op1 << op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_RSH : begin
                        res = (funct7 == RISCV_RV32I_FUNCT7_ARITH_RSHA) ? op1 >>> op2 : op1 >> op2;
                    end
                    RISCV_RV32I_FUNCT3_ARITH_SLT : begin
                        res =  (op1[31] == 1 && op2[31] == 0) ? 1 :
                                (op1[31] == 0 && op2[31] == 1) ? 0 :
                                (op1[31] == 1 && op2[31] == 1) ? ((op1[30:0] < op2[30:0]) ? 0 : 1) :
                                ((op1[30:0] < op2[30:0]) ? 1 : 0);
                    end
                    RISCV_RV32I_FUNCT3_ARITH_SLTU: begin
                        res = (op1 < op2) ? 1 : 0;
                    end
                endcase
            end
            // Branch Instructions
            // op1 = comparison operand 1
            // op2 = comparison operand 2
            // op3 = (unused)
            // res = comparison result
            // byp = (unused)
            RISCV_RV32I_OPCODE_BRANCH: begin
                byp = 32'h0000_0000;
                case(funct3)
                    RISCV_RV32I_FUNCT3_BEQ : begin
                        res = (op1 == op2) ? 1 : 0;
                    end
                    RISCV_RV32I_FUNCT3_BNE : begin
                        res = (op1 != op2) ? 1 : 0;
                    end
                    RISCV_RV32I_FUNCT3_BLT : begin
                        res =  (op1[31] == 1 && op2[31] == 0) ? 1 :
                                (op1[31] == 0 && op2[31] == 1) ? 0 :
                                (op1[31] == 1 && op2[31] == 1) ? ((op1[30:0] < op2[30:0]) ? 0 : 1) :
                                ((op1[30:0] < op2[30:0]) ? 1 : 0);
                    end
                    RISCV_RV32I_FUNCT3_BGE : begin
                        res =  ~((op1[31] == 1 && op2[31] == 0) ? 1 :
                                (op1[31] == 0 && op2[31] == 1) ? 0 :
                                (op1[31] == 1 && op2[31] == 1) ? ((op1[30:0] < op2[30:0]) ? 0 : 1) :
                                ((op1[30:0] < op2[30:0]) ? 1 : 0));
                    end
                    RISCV_RV32I_FUNCT3_BLTU: begin
                        res = (op1 < op2) ? 1 : 0;
                    end
                    RISCV_RV32I_FUNCT3_BGEU: begin
                        res = (op1 >= op2) ? 1 : 0;
                    end
                endcase
            end
            // Load/Store Instructions
            // op1 = address base
            // op2 = address offset
            // op3 = store data (if applicable)
            // res = memory address
            // byp = store data (if applicable, passthrough op3)
            RISCV_RV32I_OPCODE_LOAD,
            RISCV_RV32I_OPCODE_STORE: begin
                byp = op3;
                res = op1 + op2;
            end
            // PC-relative Jump, Indirect Jump Instructions
            // op1 = address base (0 for JAL, rd1 for JALR)
            // op2 = address offset
            // op3 = return address
            // res = jump address
            // byp = return address (passthrough op3)
            RISCV_RV32I_OPCODE_JAL,
            RISCV_RV32I_OPCODE_JAL_REG: begin
                byp = op3;
                res = op1 + op2;
            end
            // Set Upper Instructions
            // op1 = base (0 for LUI, PC for AUIPC)
            // op2 = immediate
            // op3 = (unused)
            // res = result
            // byp = (unused)
            RISCV_RV32I_OPCODE_AUIPC,
            RISCV_RV32I_OPCODE_LUI: begin
                byp = 32'h0000_0000;
                res = op1 + (op2 << 12);
            end
            // System Instructions
            // op1 = operand 1
            // op2 = operand 2
            // op3 = (unused)
            // res = result (to csr)
            // byp = result (to rd)
            RISCV_RV32I_OPCODE_SYSTEM: begin
//TODO - implement system instructions
            end
            default: begin
//TODO - implement bad instruction exception
                byp = 32'h0000_0000;
                res = 32'h0000_0000;
            end
        endcase
    end
endmodule

module riscv_cpu_regs(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // Source Register 1
    input   logic[11:0] rs1_sel,
    output  logic[31:0] rs1_data,
    // Source Register 2
    input   logic[11:0] rs2_sel,
    output  logic[31:0] rs2_data,
    // Destination Register 1
    input   logic[11:0] rd1_sel,
    input   logic[31:0] rd1_data,
    input   logic       rd1_wren,
    // Destination Register 2
    // NOTE: this is only used for some SYSTEM instructions
    //  where there's a single-cycle swap
    input   logic[4:0]  rd2_sel,
    input   logic[31:0] rd2_data
    input   logic       rd2_wren,
);

    logic[31:0] core_regs[31:0];    // page 0: core registers

    // Source Register 1 select
    always @(*) begin
//TODO - system register pages
        //else rs1_data   <= core_regs[rs1_sel[4:0]];
        rs1_data        <= core_regs[rs1_sel[4:0]];
    end

    // Source Register 2 select
    always @(*) begin
//TODO - system register pages
        //else rs2_data   <= core_regs[rs2_sel[4:0]];
        rs2_data        <= core_regs[rs2_sel[4:0]];
    end

    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            // Destination Register 1 writeback
            if(rd1_wren) begin
//TODO - system register pages
                //else core_regs[rd1_sel[4:0]]    <=  rd1_data;
                core_regs[rd1_sel[4:0]]         <= rd1_data;
            end
            // Destination Register 2 writeback
            // NOTE - rd2 is only used for some SYSTEM instructions
            //  where there's a single-cycle swap, is only valid when
            //  rd1 selects a system register, and only accesses
            //  core registers x0-x31
            if(rd2_wren && rd1_sel[11:5] != 7'h00) begin
                core_regs[rd2_sel[4:0]]         <= rd2_data;
            end
        end else begin
            core_regs[31:0]     <= 32'h0000_0000;
        end
    end

endmodule
