    ; @@ 0x0100 .instr
    ; @@ 0x0100 ef
    ; @@ 0x0101 05 00
    ; @@ 0x0103 c9
    ; @@ 0x0028 c9 .callconv args_size=2
---
    org 0x28
    ret                                 ; @@ 0x0028 c9           .callconv args_size=2
    .space 215
    rst 0x28                            ; @@ 0x0100 ef           .instr
    db 0x05, 0x00                       ; @@ 0x0101 05 00
    ret                                 ; @@ 0x0103 c9
