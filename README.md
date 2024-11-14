ECE 401 Group C
Saunders Riley, Abinash Giri, Yue Wu

Class Project - RISC-V CPU Core

Description:
    This project implements a 3-stage simple RISC-V CPU core suitable for use as a microcontroller or embedded CPU core. This uses a Harvard architecture with AHB Lite buses for instruction and data memory, and implements the RV32I instruction set (only).

Files:
    riscv_cpu_integration.sv - top-level integration with CPU core, L1 cache and external buses.
    riscv_cpu_core.sv - CPU pipeline connecting fetch, exec, commit units
    riscv_cpu_fetch_unit.sv - instruction fetch unit with external AHB I-cache interface, branch prediction, etc.
    riscv_cpu_exec_unit.sv - execution unit with external AHB D-cache interface, ALU, register file.
    riscv_cpu_commit_unit.sv - writeback unit to collect data and correct branch mispredictions.

