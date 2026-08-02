    ; @@ 0x0100 34 12 .word
    ; @@ 0x0200 .label routine
    ; @@ 0x0200 c9 .instr
---
    org 0x100
    dw 0x1234                           ; @@ 0x0100 34 12        .word n=1
    .space 254
routine:                                ; @@ 0x0200 .label routine
    ret                                 ; @@ 0x0200 c9           .instr
