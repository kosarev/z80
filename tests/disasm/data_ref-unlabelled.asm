    ; @@ 0x0100 .data_ref
    ; @@ 0x0100 21 06 01
    ; @@ 0x0103 c9
    ; @@ 0x0106 2a
---
    org 0x100
    ld hl, 0x106                        ; @@ 0x0100 21 06 01     .data_ref
    ret                                 ; @@ 0x0103 c9
    .space 2
    db 0x2a                             ; @@ 0x0106 2a
