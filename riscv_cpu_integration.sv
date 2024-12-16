module riscv_cpu_integration(
    // Core Signals
    input   logic       cpu_clk,
    input   logic       cpu_resetn,
    // AHB External Interface
    // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
    // respectively
    output  logic[31:0] ext_m_ahb_haddr,
    output  logic[2:0]  ext_m_ahb_hsize,
    output  logic[1:0]  ext_m_ahb_htrans,
    output  logic[31:0] ext_m_ahb_hwdata,
    output  logic[3:0]  ext_m_ahb_hwstrb,
    output  logic       ext_m_ahb_hwrite,
    input   logic[31:0] ext_m_ahb_hrdata,
    input   logic       ext_m_ahb_hreadyin,
    input   logic       ext_m_ahb_hresp,
);

    logic[31:0]     icache_m_ahb_haddr;
    logic[2:0]      icache_m_ahb_hsize;
    logic[1:0]      icache_m_ahb_htrans;
    logic[31:0]     icache_m_ahb_hwdata;
    logic[3:0]      icache_m_ahb_hwstrb;
    logic           icache_m_ahb_hwrite;
    logic[31:0]     icache_m_ahb_hrdata;
    logic           icache_m_ahb_hreadyin;
    logic           icache_m_ahb_hresp;

    logic[31:0]     dcache_m_ahb_haddr;
    logic[2:0]      dcache_m_ahb_hsize;
    logic[1:0]      dcache_m_ahb_htrans;
    logic[31:0]     dcache_m_ahb_hwdata;
    logic[3:0]      dcache_m_ahb_hwstrb;
    logic           dcache_m_ahb_hwrite;
    logic[31:0]     dcache_m_ahb_hrdata;
    logic           dcache_m_ahb_hreadyin;
    logic           dcache_m_ahb_hresp;

    logic[31:0]     imem_m_ahb_haddr;
    logic[2:0]      imem_m_ahb_hsize;
    logic[1:0]      imem_m_ahb_htrans;
    logic[31:0]     imem_m_ahb_hwdata;
    logic[3:0]      imem_m_ahb_hwstrb;
    logic           imem_m_ahb_hwrite;
    logic[31:0]     imem_m_ahb_hrdata;
    logic           imem_m_ahb_hreadyin;
    logic           imem_m_ahb_hresp;

    logic[31:0]     dmem_m_ahb_haddr;
    logic[2:0]      dmem_m_ahb_hsize;
    logic[31:0]     dmem_m_ahb_htrans;
    logic[31:0]     dmem_m_ahb_hwdata;
    logic[3:0]      dmem_m_ahb_hwstrb;
    logic           dmem_m_ahb_hwrite;
    logic[31:0]     dmem_m_ahb_hrdata;
    logic           dmem_m_ahb_hreadyin;
    logic           dmem_m_ahb_hresp;

    // 1KiB L1 I-cache
    ahb_cache_readonly #(
        .N_WAYS(1),
        .W_ADDR(32),
        .W_DATA(32),
        // Cache line width must be be power of two times W_DATA. The cache will fill
        // one entire cache line on each miss, using a naturally-aligned burst.
        .W_LINE(W_DATA),
        .TMEM_PRELOAD(""),
        .DMEM_PRELOAD(""),
        .DEPTH(256) // Capacity in bits = DEPTH * W_LINE * N_WAYS.
    ) inst_ahb_cache_readonly (
        // Globals
        clk(cpu_clk),
        rst_n(cpu_resetn),
        // Upstream AHB-Lite slave
        .src_hready_resp(imem_m_ahb_hreadyin),
        .src_hready(1'b1),
        .src_hresp(imem_m_ahb_hresp),
        .src_haddr(imem_m_ahb_haddr),
        .src_hwrite(imem_m_ahb_hwrite),
        .src_htrans(imem_m_ahb_htrans),
        .src_hsize(imem_m_ahb_hsize),
        .src_hburst(3'b000),
        .src_hprot(4'b0001),
        .src_hmastlock(1'b0),
        .src_hwdata(imem_m_ahb_hwdata),
        .src_hrdata(imem_m_ahb_hrdata),
        // Downstream AHB-Lite master
        dst_hready_resp(),
        dst_hready(icache_m_ahb_hreadyin),
        dst_hresp(icache_m_ahb_hresp),
        dst_haddr(icache_m_ahb_haddr),
        dst_hwrite(icache_m_ahb_hwrite),
        dst_htrans(icache_m_ahb_htrans),
        dst_hsize(icache_m_ahb_hsize),
        dst_hburst(),
        dst_hprot(),
        dst_hmastlock(),
        dst_hwdata(icache_m_ahb_hwdata),
        dst_hrdata(icache_m_ahb_hrdata)
    );

    // 1KiB L1 D-cache
    ahb_cache_writeback #(
        .N_WAYS(1),
        .W_ADDR(32),
        .W_DATA(32),
        // Cache line width must be be power of two times W_DATA. The cache will
        // fill one entire cache line each miss, using a naturally-aligned burst.
        .W_LINE(W_DATA),
        // Capacity in bits = W_LINE * N_WAYS * DEPTH
        .DEPTH(256),
        .TMEM_PRELOAD(""),
        .DMEM_PRELOAD(""),
        // If number of masters is 0, exclusives are not supported. Excusives are
        // enforced on upstream accesses using a global monitor inside the cache;
        // exclusivity is not propagated downstream. This is suitable for use as
        // a "system cache", but exclusives over multiple top-level caches would
        // require coherent interconnect.
        .EXCL_N_MASTERS(0),
        // Granule address LSB = 3 -> 8-byte granule size
        .EXCL_GRANULE_LSB(3)
    ) inst_ahb_cache_writeback (
        // Globals
        .clk(cpu_clk),
        .rst_n(cpu_resetn),
        // Upstream AHB-Lite slave
        .src_hready_resp(dmem_m_ahb_hreadyin),
        .src_hready(1'b1),
        .src_hresp(dmem_m_ahb_hresp),
        .src_hexokay(),
        .src_haddr(dmem_m_ahb_haddr),
        .src_hwrite(dmem_m_ahb_hwrite),
        .src_htrans(dmem_m_ahb_htrans),
        .src_hsize(dmem_m_ahb_hsize),
        .src_hburst(3'b000),
        .src_hprot(4'b0000),
        .src_hmaster(1'b0),
        .src_hmastlock(1'b0),
        .src_hexcl(1'b0),
        .src_hwdata(dmem_m_ahb_hwdata),
        .src_hrdata(dmem_m_ahb_hrdata),
        // Downstream AHB-Lite master
        .dst_hready_resp(),
        .dst_hready(dcache_m_ahb_hreadyin),
        .dst_hresp(dcache_m_ahb_hresp),
        .dst_haddr(dcache_m_ahb_haddr),
        .dst_hwrite(dcache_m_ahb_hwrite),
        .dst_htrans(dcache_m_ahb_htrans),
        .dst_hsize(dmem_m_ahb_hsize),
        .dst_hburst(),
        .dst_hprot(),
        .dst_hmastlock(),
        .dst_hwdata(dcache_m_ahb_hwdata),
        .dst_hrdata(dcache_m_ahb_hrdata)
    );

    // AHB arbiter for external interface
    ahbl_arbiter #(
        .N_PORTS(2),
        .W_ADDR(32),
        .W_DATA(32),
        .CONN_MASK({N_PORTS{1'b1}}),
        .FAIR_ARBITRATION(0)
    ) inst_ahbl_arbiter (
        // Global signals
        .clk(cpu_clk),
        .rst_n(cpu_resetn),
        // From masters; function as slave ports
        .src_hready(2'b11),
        .src_hready_resp({dcache_m_ahb_hreadyin, icache_m_ahb_hreadyin}),
        .src_hresp({dcache_m_ahb_hresp, icache_m_ahb_hresp}),
        .src_hexokay(),
        .src_haddr({dcache_m_ahb_haddr, icache_m_ahb_haddr}),
        .src_hwrite({dcache_m_ahb_hwrite, icache_m_ahb_hwrite}),
        .src_htrans({dcache_m_ahb_hwrite, icache_m_ahb_hwrite}),
        .src_hsize({dcache_m_ahb_hsize, icache_m_ahb_hsize}),
        .src_hburst(),
        .src_hprot(),
        .src_hmaster(),
        .src_hmastlock(),
        .src_hexcl(),
        .src_hwdata({dcache_m_ahb_hwdata, icache_m_ahb_hwdata}),
        .src_hrdata({dcache_m_ahb_hrdata, icache_m_ahb_hrdata}),

        // To slave; functions as master port
        .dst_hready(),
        .dst_hready_resp(ext_m_ahb_hreadyin),
        .dst_hresp(ext_m_ahb_hresp),
        .dst_hexokay(),
        .dst_haddr(ext_m_ahb_haddr),
        .dst_hwrite(ext_m_ahb_hwrite),
        .dst_htrans(ext_m_ahb_htrans),
        .dst_hsize(ext_m_ahb_hsize),
        .dst_hburst(),
        .dst_hprot(),
        .dst_hmaster(),
        .dst_hmastlock(),
        .dst_hexcl(),
        .dst_hwdata(ext_m_ahb_hwdata),
        .dst_hrdata(ext_m_ahb_hrdata)
    );

    // RISCV-RV32I CPU Core
    // see riscv_cpu_core.sv
    riscv_cpu_exec_unit #(
        .RESET_VECTOR(32'h0000_0000);
    ) inst_riscv_cpu_exec_unit (
        // Core Signals
        .cpu_clk(cpu_clk),
        .cpu_resetn(cpu_resetn),
        // AHB Instruction Interface
        // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
        // respectively
        .imem_m_ahb_haddr(imem_m_ahb_haddr),
        .imem_m_ahb_hsize(imem_m_ahb_hsize),
        .imem_m_ahb_htrans(imem_m_ahb_htrans),
        .imem_m_ahb_hwdata(imem_m_ahb_hwdata),
        .imem_m_ahb_hwstrb(imem_m_ahb_hwstrb),
        .imem_m_ahb_hwrite(imem_m_ahb_hwrite),
        .imem_m_ahb_hrdata(imem_m_ahb_hrdata),
        .imem_m_ahb_hreadyin(imem_m_ahb_hreadyin),
        .imem_m_ahb_hresp(imem_m_ahb_hresp),
        // AHB Data Interface
        // AHB HCLK and HRESETN should be connected to CPU_CLK and CPU_RESETN,
        // respectively
        .dmem_m_ahb_haddr(dmem_m_ahb_haddr),
        .dmem_m_ahb_hsize(dmem_m_ahb_hsize),
        .dmem_m_ahb_htrans(dmem_m_ahb_htrans),
        .dmem_m_ahb_hwdata(dmem_m_ahb_hwdata),
        .dmem_m_ahb_hwstrb(dmem_m_ahb_hwstrb),
        .dmem_m_ahb_hwrite(dmem_m_ahb_hwrite),
        .dmem_m_ahb_hrdata(dmem_m_ahb_hrdata),
        .dmem_m_ahb_hreadyin(dmem_m_ahb_hreadyin),
        .dmem_m_ahb_hresp(dmem_m_ahb_hresp),
    );


endmodule
