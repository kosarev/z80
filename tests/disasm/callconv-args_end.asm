    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 41 42 00
    ; @@ 0x0106 c9
    ; @@ 0x0200 c9 .callconv args_end=0x00
---
    org 0x100
    call 0x200                          ; @@ 0x0100 cd 00 02     .instr
    db 0x41                             ; @@ 0x0103 41
    db 0x42, 0x00                       ; @@ 0x0104 42 00
    ret                                 ; @@ 0x0106 c9
    .space 249
    ret                                 ; @@ 0x0200 c9           .callconv args_end=0x00
