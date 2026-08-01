    ; @@ 0x0100 .instr
    ; @@ 0x0100 cd 00 02
    ; @@ 0x0103 3f
    ; @@ 0x0200 c9 .callconv noreturn
---
    org 0x100
    call 0x200                          ; @@ 0x0100 cd 00 02     .instr
    db 0x3f                             ; @@ 0x0103 3f
    .space 252
    ret                                 ; @@ 0x0200 c9           .callconv noreturn
