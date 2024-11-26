`define AHB_HTRANS_IDLE     2'b00
`define AHB_HTRANS_NONSEQ   2'b10

`define RISCV_INSTR_NOP     32'b0000_0000_0000_0000__0000_0000_0011_0011

//riscv_cpu_fetch_unit#(
//    RESET_VECTOR    = 32'h00000000
//)(
//    // Core Signals
//    .cpu_clk(cpu_clk),
//    .cpu_resetn(cpu_resetn),
//    // AHB Instruction Interface
//    // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
//    // respectively
//    .m_ahb_haddr(imem_ahb_haddr),
//    .m_ahb_hsize(imem_ahb_hsize),
//    .m_ahb_htrans(imem_ahb_htrans),
//    .m_ahb_hwdata(imem_ahb_hwdata),
//    .m_ahb_hwstrb(imem_ahb_hwstrb),
//    .m_ahb_hwrite(imem_ahb_hwrite),
//    .m_ahb_hrdata(imem_ahb_hrdata),
//    .m_ahb_hreadyin(imem_ahb_hreadyin),
//    .m_ahb_hresp(imem_ahb_hresp),
//    // Pipeline interface
//    .pipe_instr(fetch_pipe_instr),
//    .pipe_pcaddr(fetch_pipe_pcaddr),
//    .pipe_stall_out(fetch_pipe_stall_out),
//    .fetch_stall_in(fetch_stall_in),
//    .fetch_branch(fetch_branch),
//    .fetch_branch_pcaddr(fetch_branch_pcaddr)
//);

module riscv_cpu_fetch_unit#(
    parameter   RESET_VECTOR    = 32'h00000000
)(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // AHB Instruction Interface
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
    // Pipeline interface
    output  logic[31:0] pipe_instr,
    output  logic[31:0] pipe_pcaddr,
    output  logic       pipe_stall_out,
    input   logic       fetch_stall_in,
    input   logic       fetch_branch,
    input   logic[31:0] fetch_branch_pcaddr
);

    // pcaddr[0] is the actual program address, pcaddr[1] is used to sync to
    // the instruction for pipe_pcaddr
    logic[31:0]     pcaddr[1:0];
    // JAL instruction offset
    logic[31:0]     jump_offset = {m_ahb_hrdata[31], m_ahb_hrdata[19:12], m_ahb_hrdata[20], m_ahb_hrdata[30:21], 1'b0};

    // AHB address logic
    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            m_ahb_haddr     <= pcaddr[0];
            m_ahb_htrans    <= AHB_HTRANS_NONSEQ;
        end else begin
            m_ahb_haddr     <= RESET_VECTOR;
            m_ahb_htrans    <= AHB_HTRANS_IDLE;
        end
    end

    // Pipeline Output logic
    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            pipe_instr      <= (fetch_stall_in)? pipe_instr : ((m_ahb_hreadyin & ~m_ahb_hresp)? m_ahb_hrdata : RISCV_INSTR_NOP);
            pipe_pcaddr     <= pcaddr[1];
        end else begin
            pipe_instr      <= RISCV_INSTR_NOP;
            pipe_pcaddr     <= RESET_VECTOR;
        end
    end

    // Program Counter logic
    always @(posedge cpu_clk, negedge cpu_resetn) begin
        if(cpu_resetn) begin
            if(~fetch_stall_in) begin
                if(fetch_branch) begin
                    pcaddr[1]       <= pcaddr[0];
                    pcaddr[0]       <= fetch_branch_pcaddr;
                end else if(m_ahb_hrdata[6:0] == 7'b0110111) begin
                    pcaddr[1]       <= pcaddr[0];
                    pcaddr[0]       <= pcaddr + jump_offset;
                end else begin
                    pcaddr[1]       <= pcaddr[0];
                    pcaddr[0]       <= pcaddr + 4;
                end
            end else begin
                pcaddr[1:0]     <= {pcaddr[1], pcaddr[0]};
            end
        end else begin
            pcaddr[1:0]     <= {RESET_VECTOR, RESET_VECTOR};
        end
    end

endmodule

