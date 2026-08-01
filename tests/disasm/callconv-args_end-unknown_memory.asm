    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 41
    ; @@ 0x0200 c9 .callconv args_end=0x00
---
    org 0x100
    call 0x200                          ; @@ 0x0100 cd 00 02     .instr
    db 0x41                             ; @@ 0x0103 41
    .space 252
    ret                                 ; @@ 0x0200 c9           .callconv args_end=0x00
