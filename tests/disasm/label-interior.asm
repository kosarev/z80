    org 0x0
    call 0x6                            ; @@ 0x0000 cd           .instr
inside equ 0x0001                       ; @@ 0x0001 .label inside
                                        ; @@ 0x0001 06 00
    ret                                 ; @@ 0x0003 c9
    .space 2
    ret                                 ; @@ 0x0006 c9
---
    org 0x0
    call l_0006                         ; @@ 0x0000 cd           .instr
inside equ 0x0001                       ; @@ 0x0001 .label inside
                                        ; @@ 0x0001 06 00
    ret                                 ; @@ 0x0003 c9
    .space 2
l_0006:
    ret                                 ; @@ 0x0006 c9
