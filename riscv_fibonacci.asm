__main:
    addi x0, x0, 0; # 0x00000013 : x0 = x0 + 0 ; NOP
    addi x1, x0, 0; # 0x00000093 : x1 = x0 + 0 ; x1 = 0 (fibonacci(0))
    addi x2, x0, 1; # 0x00100113 : x2 = x1 + 1 ; x2 = 1 (fibonacci(1))
    addi x3, x0, 0; # 0x00000193 : x3 = x0 + 0 ; x3 = 0 (memory address)
    addi x4, x0, 64; # 0x10000213 : x4 = x0 + 64
__loop:
    # Unrolling the loop here to process two at a time made the most sense
    # since it lets us re-use x1 and x2 for operands and results, x4 has to
    # be a clean multiple of 8 for this to work.
    add x1, x1, x2; # 0x002080B3 : x1 = x1 + x2 ; x1 = x1 + x2 (fibonacci(n))
    add x2, x1, x2; # 0x00208133 : x2 = x1 + x2 ; x2 = x1 + x2 (fibonnaci(n+1))
    sw x1, 0(x3) # 0x0011A023 : mem[x3 + 0] = x1
    sw x2, 4(x3) # 0x0021A223 : mem[x3 + 4] = x2
    addi x3, x3, 8; # 0x00818193 : x3 = x3 + 8
    beq x3, x4, __exit; # 0x00418463: if x3 == x4 then __exit
    addi x0, x0, 0;  #0x00000013 : x0 = x0 + 0 ; NOP (control hazard - NEEDS FIXED!)
    jal __loop  # 0xFE9FF0EF : jump to __loop and save position to ra
__exit:
    addi x0, x0, 0; #0x00000013 : x0 = x0 + 0 ; NOP (control hazard - NEEDS FIXED!)
    jal __exit  # 0x000000EF : jump to __exit and save position to ra
    