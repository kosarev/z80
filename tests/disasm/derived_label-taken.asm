    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 c9
    ; @@ 0x0200 c9
    ; @@ 0x0300 .label l_0200
    ; @@ 0x0300 c9
---
    org 0x100
    call 0x200                          ; @@ 0x0100 cd 00 02     .instr
    ret                                 ; @@ 0x0103 c9
    .space 252
    ret                                 ; @@ 0x0200 c9
    .space 255
l_0200:                                 ; @@ 0x0300 .label l_0200
    db 0xc9                             ; @@ 0x0300 c9
