    ; @@ 0x0100 .instr
    ; @@ 0x0100 dc 00 02
    ; @@ 0x0103 c9
    ; @@ 0x0200 c9 .callconv args_size=2
---
    org 0x100
    call c, 0x200                       ; @@ 0x0100 dc 00 02     .instr
    ret                                 ; @@ 0x0103 c9
    .space 252
    ret                                 ; @@ 0x0200 c9           .callconv args_size=2
