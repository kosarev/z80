    org 0x0
    ; @@ 0x0000 Returns immediately.
    ; @@ 0x0000 0xff on the bus picks the vector, read
    ; @@ 0x0000 0xfd from the page the I register names.
    ret                                 ; @@ 0x0000 c9           .instr
