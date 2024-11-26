module riscv_cpu_exec_unit(
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
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
    input   logic[31:0] op3
);

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
        rs1_data = core_regs[rs1_sel[4:0]];
    end

    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin

        end else begin

        end
    end

endmodule
