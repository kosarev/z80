    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 3f 07
    ; @@ 0x0105 c9
    ; @@ 0x0200 c9 .callconv args_size=2
---
    org 0x100
    call l_0200                         ; @@ 0x0100 cd 00 02     .instr
    db 0x3f                             ; @@ 0x0103 3f
    db 0x07                             ; @@ 0x0104 07
    ret                                 ; @@ 0x0105 c9
    .space 250
l_0200:
    ret                                 ; @@ 0x0200 c9           .callconv args_size=2
