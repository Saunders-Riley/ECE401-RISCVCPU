`include "riscv_instruction_set.vh"

`define AHB_HTRANS_IDLE     2'b00
`define AHB_HTRANS_NONSEQ   2'b10


// riscv_cpu_exec_unit inst_riscv_cpu_exec_unit(
//     RESET_VECTOR    = 32'h0000_0000;
// )(
//     // Core Signals
//     .cpu_clk(),
//     .cpu_resetn(),
//     // AHB Instruction Interface
//     // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
//     // respectively
//     .imem_m_ahb_haddr(),
//     .imem_m_ahb_hsize(),
//     .imem_m_ahb_htrans(),
//     .imem_m_ahb_hwdata(),
//     .imem_m_ahb_hwstrb(),
//     .imem_m_ahb_hwrite(),
//     .imem_m_ahb_hrdata(),
//     .imem_m_ahb_hreadyin(),
//     .imem_m_ahb_hresp(),
//     // AHB Data Interface
//     // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
//     // respectively
//     .dmem_m_ahb_haddr(),
//     .dmem_m_ahb_hsize(),
//     .dmem_m_ahb_htrans(),
//     .dmem_m_ahb_hwdata(),
//     .dmem_m_ahb_hwstrb(),
//     .dmem_m_ahb_hwrite(),
//     .dmem_m_ahb_hrdata(),
//     .dmem_m_ahb_hreadyin(),
//     .dmem_m_ahb_hresp(),
// );


module riscv_cpu_exec_unit#(
    parameter   RESET_VECTOR    = 32'h0000_0000;
)(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // AHB Instruction Interface
    // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
    // respectively
    output  logic[31:0] imem_m_ahb_haddr,
    output  logic[2:0]  imem_m_ahb_hsize,
    output  logic[1:0]  imem_m_ahb_htrans,
    output  logic[31:0] imem_m_ahb_hwdata,
    output  logic[3:0]  imem_m_ahb_hwstrb,
    output  logic       imem_m_ahb_hwrite,
    input   logic[31:0] imem_m_ahb_hrdata,
    input   logic       imem_m_ahb_hreadyin,
    input   logic       imem_m_ahb_hresp,
    // AHB Data Interface
    // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
    // respectively
    output  logic[31:0] dmem_m_ahb_haddr,
    output  logic[2:0]  dmem_m_ahb_hsize,
    output  logic[1:0]  dmem_m_ahb_htrans,
    output  logic[31:0] dmem_m_ahb_hwdata,
    output  logic[3:0]  dmem_m_ahb_hwstrb,
    output  logic       dmem_m_ahb_hwrite,
    input   logic[31:0] dmem_m_ahb_hrdata,
    input   logic       dmem_m_ahb_hreadyin,
    input   logic       dmem_m_ahb_hresp,
);

    // Instruction memory interface is read-only - tie the write signals low
    assign          imem_m_ahb_hwdata = 32'h0000_0000;
    assign          imem_m_ahb_hwstrb = 4'h0;
    assign          imem_m_ahb_hwrite = 0;

    // Pipeline Stage 1 - Instruction Fetch
    logic[31:0]     prog_counter;
    logic[31:0]     prog_counter_pl;
    logic[31:0]     branch_jump_addr;
    logic           branch_jump;
    logic           pipe_stall;
    logic           pipe_flush;
    logic[31:0]     jump_offset = {imem_m_ahb_hrdata[31], imem_m_ahb_hrdata[19:12], imem_m_ahb_hrdata[20], imem_m_ahb_hrdata[30:21], 1'b0};

    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            // AHB address logic
            imem_m_ahb_haddr    <= (pipe_stall) ? imem_m_ahb_haddr : prog_counter;
            imem_m_ahb_htrans   <= (pipe_stall) ? imem_m_ahb_htrans : AHB_HTRANS_NONSEQ;
            // Program counter logic
            if(~pipe_stall) begin
                prog_counter_pl <= prog_counter;
                if(branch_jump) begin
                    prog_counter    <= branch_jump_addr
                end
                else if(imem_m_ahb_hrdata[6:0] == RISCV_RV32I_OPCODE_JAL) begin
                    prog_counter    <= prog_counter_pl + jump_offset;
                end
//                else if(imem_m_ahd_hrdata[6:0] == RISCV_RV32I_OPCODE_BRANCH) begin
//TODO - branch prediction
//               end
                else begin
                    prog_counter    <= prog_counter + 4;
                end
            end else begin
                prog_counter    <= prog_counter;
                prog_counter_pl <= prog_counter_pl;
            end
        end else begin
            prog_counter    <= RESET_VECTOR;
        end
    end

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

    logic[11:0]     regs_rs1_sel        = {7'h00, pl_fetch_rs1};
    logic[31:0]     regs_rs1_data;
    logic[11:0]     regs_rs2_sel;       = (pl_fetch_opcode == RISCV_RV32I_OPCODE_SYSTEM) ? pl_fetch_imm_I : pl_fetch_rs2;
    logic[31:0]     regs_rs2_data;

    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            pl_fetch_instr  <= (pipe_flush) ? RISCV_RV32I_INSTR_NOP :
                                    (pipe_stall) ? pl_fetch_instr : imem_m_ahb_hrdata;
            pl_fetch_pcaddr <= (pipe_flush) ? 32'h0000_0000 :
                                    (pipe_stall) ? pl_fetch_pcaddr : prog_counter_pl;
        end else begin
            pl_fetch_instr  <= RISCV_RV32I_INSTR_NOP;
            pl_fetch_pcaddr <= 32'h0000_0000;
        end
    end

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

    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            pl_exec_instr   <= (pipe_flush) ? RISCV_RV32I_INSTR_NOP :
                                    (pipe_stall) ? pl_exec_instr : pl_fetch_instr;
            pl_exec_pcaddr  <= (pipe_flush) ? 32'h0000_0000 :
                                    (pipe_stall) ? pl_exec_pcaddr : pl_fetch_pcaddr;
            case(pl_fetch_opcode)
                // Arithmetic Instructions
                // op1 = arithmetic operand 1
                // op2 = arithmetic operand 2
                // op3 = (unused)
                RISCV_RV32I_OPCODE_ARITH        : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : regs_rs2_data;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
                // Arithmetic Instructions with Immediate
                // op1 = arithmetic operand 1
                // op2 = arithmetic operand 2
                // op3 = (unused)
                RISCV_RV32I_OPCODE_ARITH_IMM    : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_I;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
                // Load Instructions
                // op1 = address base
                // op2 = address offset
                // op3 = store data (if applicable)
                RISCV_RV32I_OPCODE_LOAD         : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_I;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
                // Store Instructions
                // op1 = address base
                // op2 = address offset
                // op3 = store data
                RISCV_RV32I_OPCODE_STORE        : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_S;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : regs_rs2_data;
                end
                // Branch Instructions
                // op1 = comparison operand 1
                // op2 = comparison operand 2
                // op3 = branch offset
                RISCV_RV32I_OPCODE_BRANCH       : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : regs_rs2_data;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : pl_fetch_imm_B;
                end
                // PC-relative Jump Instruction
                // op1 = address base (0 for JAL, rd1 for JALR)
                // op2 = address offset
                // op3 = return address
                RISCV_RV32I_OPCODE_JAL          : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : 32'h0000_0000;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_J;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : pl_fetch_pcaddr + 4;
                end
                // Indirect Jump Instruction
                // op1 = address base (0 for JAL, rd1 for JALR)
                // op2 = address offset
                // op3 = return address
                RISCV_RV32I_OPCODE_JAL_REG      : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_I;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : pl_fetch_pcaddr + 4;
                end
                // Set Upper Instruction
                // op1 = base (0 for LUI, PC for AUIPC)
                // op2 = immediate
                // op3 = (unused)
                RISCV_RV32I_OPCODE_LUI          : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : 32'h0000_0000;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_U;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
                // Add Upper to PC Instruction
                // op1 = base (0 for LUI, PC for AUIPC)
                // op2 = immediate
                // op3 = (unused)
                RISCV_RV32I_OPCODE_AUIPC        : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : pl_fetch_imm_U;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
                // System Instructions
                // op1 = operand 1
                // op2 = operand 2
                // op3 = (unused)
                RISCV_RV32I_OPCODE_SYSTEM       : begin
                    pl_exec_op1 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op1 : regs_rs1_data;
                    pl_exec_op2 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op2 : regs_rs2_data;
                    pl_exec_op3 <= (pipe_flush) ? 32'h0000_0000 :
                                        (pipe_stall) ? pl_exec_op3 : 32'h0000_0000;
                end
            endcase
        end else begin
            pl_exec_instr   <= RISCV_RV32I_INSTR_NOP;
            pl_exec_pcaddr  <= 32'h0000_0000;
            pl_exec_op1     <= 32'h0000_0000;
            pl_exec_op2     <= 32'h0000_0000;
            pl_exec_op3     <= 32'h0000_0000;
        end
    end

    // Pipeline Stage 4 - Memory Access
    logic[31:0]     pl_mem_instr;
    logic[31:0]     pl_mem_pcaddr;
    logic[31:0]     pl_mem_res;
    logic[31:0]     pl_mem_byp;
    logic[6:0]      pl_mem_funct7        = pl_mem_instr[31:25];
    logic[4:0]      pl_mem_rs2           = pl_mem_instr[24:20];
    logic[4:0]      pl_mem_rs1           = pl_mem_instr[19:15];
    logic[2:0]      pl_mem_funct3        = pl_mem_instr[14:12];
    logic[4:0]      pl_mem_rd            = pl_mem_instr[11:7];
    logic[6:0]      pl_mem_opcode        = pl_mem_instr[6:0];
    logic[11:0]     pl_mem_imm_I         = pl_mem_instr[11:0];
    logic[11:0]     pl_mem_imm_S         = {pl_mem_funct7, pl_mem_rd};
    logic[12:0]     pl_mem_imm_B         = {pl_mem_funct7[6], pl_mem_rd[0], pl_mem_funct7[5:0], pl_mem_rd[4:1], 1'b0};
    logic[19:0]     pl_mem_imm_U         = pl_mem_instr[31:12];
    logic[20:0]     pl_mem_imm_J         = {pl_mem_imm_U[19], pl_mem_imm_U[7:0], pl_mem_imm_U[8], pl_mem_imm_U[18:9], 1'b0};

    always @(posege cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            pl_mem_instr    <= (pipe_flush) ? RISCV_RV32I_INSTR_NOP :
                                    (pipe_stall) ? pl_mem_instr : pl_exec_instr;
            pl_mem_pcaddr   <= (pipe_flush) ? 32'h0000_0000 :
                                    (pipe_stall) ? pl_mem_pcaddr : pl_exec_pcaddr;
            pl_mem_res      <= (pipe_flush) ? 32'h0000_0000 :
                                    (pipe_stall) ? pl_mem_res : alu_res;
            pl_mem_byp      <= (pipe_flush) ? 32'h0000_0000 :
                                    (pipe_stall) ? pl_mem_byp : alu_byp;
            case (pl_exec_opcode)
            // Non-memory instructions, generally ignored
            RISCV_RV32I_OPCODE_ARITH,
            RISCV_RV32I_OPCODE_ARITH_IMM,
            RISCV_RV32I_OPCODE_BRANCH,
            RISCV_RV32I_OPCODE_JAL,
            RISCV_RV32I_OPCODE_JAL_REG,
            RISCV_RV32I_OPCODE_LUI,
            RISCV_RV32I_OPCODE_AUIPC,
            RISCV_RV32I_OPCODE_SYSTEM : begin
                dmem_m_ahb_haddr    <= (pipe_stall) ? dmem_m_ahb_haddr : 32'h0000_0000;
                dmem_m_ahb_hsize    <= (pipe_stall) ? dmem_m_ahb_hsize : ;
                dmem_m_ahb_htrans   <= (pipe_stall) ? dmem_m_ahb_htrans : AHB_HTRANS_IDLE;
                dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                dmem_m_ahb_hwrite   <= (pipe_stall) ? dmem_m_ahb_hwrite : 0;
            end
            // Load Instructions
            RISCV_RV32I_OPCODE_LOAD : begin
                // This is the address-phase of an AHB read transaction
                // note that the pipe will stall until dmem_m_ahb_hready is asserted by the L1 D-cache
                dmem_m_ahb_haddr    <= (pipe_stall) ? dmem_m_ahb_haddr : {alu_res[31:2], 2'h0};
                dmem_m_ahb_hsize    <= (pipe_stall) ? dmem_m_ahb_hsize : pl_exec_funct3[1:0];
                dmem_m_ahb_htrans   <= (pipe_stall) ? dmem_m_ahb_htrans : AHB_HTRANS_NONSEQ;
                dmem_m_ahb_hwrite   <= (pipe_stall) ? dmem_m_ahb_hwrite : 0;
            end
            // Store Instructions
            RISCV_RV32I_OPCODE_STORE : begin
                // This is the address-phase of an AHB write transaction
                dmem_m_ahb_haddr    <= (pipe_stall) ? dmem_m_ahb_haddr : {alu_res[31:2], 2'h0};
                dmem_m_ahb_hsize    <= (pipe_stall) ? dmem_m_ahb_hsize : pl_exec_funct3[1:0];
                dmem_m_ahb_htrans   <= (pipe_stall) ? dmem_m_ahb_htrans : AHB_HTRANS_NONSEQ;
                dmem_m_ahb_hwrite   <= (pipe_stall) ? dmem_m_ahb_hwrite : 1;
            end
            default: 
        endcase
        end else begin
            pl_mem_instr    <= RISCV_RV32I_NOP;
            pl_mem_pcaddr   <= 32'h0000_0000;
            pl_mem_res      <= 32'h0000_0000;
            pl_mem_byp      <= 32'h0000_0000;
        end
    end

    // Pipeline Stage 5 - Instruction Writeback
    logic[31:0]     pl_wb_instr;
    logic[31:0]     pl_wb_pcaddr;
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

    logic[11:0]     regs_rd1_sel        = (pl_wb_opcode == RISCV_RV32I_OPCODE_SYSTEM) ? pl_wb_imm_I : {7'h0, pl_wb_rd};
    logic[31:0]     regs_rd1_data;
    logic           regs_rd1_wren;
    logic[4:0]      regs_rd2_sel        = (pl_wb_opcode == RISCV_RV32I_OPCODE_SYSTEM) ? pl_wb_rd : 5'h00;
    logic[31:0]     regs_rd2_data;
    logic           regs_rd2_wren;

    always @(posege cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            pl_wb_instr     <= (pipe_stall) ? pl_wb_instr : pl_mem_instr;
            pl_wb_pcaddr    <= (pipe_stall) ? pl_wb_pcaddr : pl_mem_pcaddr;
            case(pl_mem_opcode)
                RISCV_RV32I_OPCODE_ARITH,
                RISCV_RV32I_OPCODE_ARITH_IMM,
                RISCV_RV32I_OPCODE_LUI,
                RISCV_RV32I_OPCODE_AUIPC,
                RISCV_RV32I_OPCODE_SYSTEM : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data : pl_mem_res;
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 1;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data :
                                                (pl_mem_opcode == RISCV_RV32I_OPCODE_SYSTEM) ? pl_mem_byp : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren :
                                                (pl_mem_opcode == RISCV_RV32I_OPCODE_SYSTEM) ? 1 : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush : 0;
                    branch_jump         <= (pipe_stall) ? branch_jump : 0;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr : 32'h0000_0000;
                end
                RISCV_RV32I_OPCODE_LOAD : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data :
                                                (pl_mem_funct3[1:0] == 2'b10) ? dmem_m_ahb_hrdata :
                                                (pl_mem_funct3[1:0] == 2'b01) ?
                                                    (pl_mem_funct3[2]) ? (dmem_m_ahb_hrdata >>> (16 * dmem_m_ahb_haddr[0])) :
                                                        (dmem_m_ahb_hrdata >> (16 * dmem_m_ahb_hrdata[0])) :
                                                (pl_mem_funct3[2]) ? (dmem_m_ahb_hrdata >>> (8 * dmem_m_ahb_haddr[1:0])) :
                                                        (dmem_m_ahb_hrdata >> (8 * dmem_m_ahb_hrdata[1:0]));
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 1;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush : 0;
                    branch_jump         <= (pipe_stall) ? branch_jump : 0;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr : 32'h0000_0000;
                end
                RISCV_RV32I_OPCODE_STORE : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata :
                                                (pl_exec_funct3[1:0] == 2'b10) ? pl_mem_byp :
                                                (pl_exec_funct3[1:0] == 2'b01) ? (pl_mem_byp[15:0] << (16 * pl_mem_res[0])) :
                                                (pl_mem_byp[7:0] << (8 * pl_mem_res[1:0]));
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb :
                                                (pl_exec_funct3[1:0] == 2'b10) ? 4'F :
                                                (pl_exec_funct3[1:0] == 2'b01) ? (4'h3 << (2 * pl_mem_res[0])) :
                                                (4'h1 << pl_mem_res[1:0]);
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data : 32'h0000_0000;
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 0;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush : 0;
                    branch_jump         <= (pipe_stall) ? branch_jump : 0;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr : 32'h0000_0000;
                end
                RISCV_RV32I_OPCODE_JAL : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data : 32'h0000_0000;
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 0;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush : 0;
                    branch_jump         <= (pipe_stall) ? branch_jump : 0;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr : 32'h0000_0000;
                end
                RISCV_RV32I_OPCODE_BRANCH : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data : 32'h0000_0000;
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 0;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush :
                                                (pl_mem_res == 1) ? 1 : 0;
                    branch_jump         <= (pipe_stall) ? branch_jump :
                                                (pl_mem_res == 1) ? 1 : 0;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr :
                                                (pl_mem_res == 1) ? pl_mem_pcaddr + pl_mem_imm_B : 32'h0000_0000;
                end
                RISCV_RV32I_OPCODE_JAL_REG : begin
                    dmem_m_ahb_hwdata   <= (pipe_stall) ? dmem_m_ahb_hwdata : 32'h0000_0000;
                    dmem_m_ahb_hwstrb   <= (pipe_stall) ? dmem_m_ahb_hwstrb : 4'h0;
                    regs_rd1_data       <= (pipe_stall) ? regs_rd1_data : 32'h0000_0000;
                    regs_rd1_wren       <= (pipe_stall) ? regs_rd1_wren : 0;
                    regs_rd2_data       <= (pipe_stall) ? regs_rd2_data : 32'h0000_0000;
                    regs_rd2_wren       <= (pipe_stall) ? regs_rd2_wren : 0;
                    pipe_flush          <= (pipe_stall) ? pipe_flush : 1;
                    branch_jump         <= (pipe_stall) ? branch_jump : 1;
                    branch_jump_addr    <= (pipe_stall) ? branch_jump_addr : pl_mem_res;
                end
            endcase
        end else begin
            pl_wb_instr     <= RISCV_RV32I_INSTR_NOP;
            pl_fetch_pcaddr <= 32'h0000_0000;
        end
    end

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
        .cpu_clk(cpu_clk),
        .cpu_resetn(cpu_resetn),
        // Source Register 1
        .rs1_sel(regs_rs1_sel),
        .rs1_data(regs_rs1_data),
        // Source Register 2
        .rs2_sel(regs_rs2_sel),
        .rs2_data(regs_rs2_data),
        // Destination Register 1
        .rd1_sel(regs_rd1_sel),
        .rd1_data(regs_rd1_data),
        .rd1_wren(regs_rd1_wren),
        // Destination Register 2
        // NOTE: this is only used for some SYSTEM instructions
        //  where there's a single-cycle swap
        .rd2_sel(regs_rd2_sel),
        .rd2_data(regs_rd2_data),
        .rd2_wren(regs_rd2_wren),
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
