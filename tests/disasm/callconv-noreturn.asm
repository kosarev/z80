    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 3f
    ; @@ 0x0200 c9 .callconv noreturn
---
    org 0x100
    call l_0200                         ; @@ 0x0100 cd 00 02     .instr
    db 0x3f                             ; @@ 0x0103 3f
    .space 252
l_0200:
    ret                                 ; @@ 0x0200 c9           .callconv noreturn
