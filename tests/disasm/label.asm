    org 0x0
    call beep                           ; @@ 0x0000 cd 06 00     .instr
    ret                                 ; @@ 0x0003 c9
    db 0x01, 0x02                       ; @@ 0x0004 01 02
beep:                                   ; @@ 0x0006 .label beep -- A tiny routine.
    ret                                 ; @@ 0x0006 c9
