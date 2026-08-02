    ; @@ 0x0100 00 02 .word -- Points at the routine.
    ; @@ 0x0200 .label routine
    ; @@ 0x0200 c9 .instr
---
    org 0x100
    dw routine                          ; @@ 0x0100 00 02        .word n=1 -- Points at the routine.
    .space 254
routine:                                ; @@ 0x0200 .label routine
    ret                                 ; @@ 0x0200 c9           .instr
