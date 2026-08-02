    ; @@ 0x0100 .data_ref
    ; @@ 0x0100 21 06 01
    ; @@ 0x0103 7e
    ; @@ 0x0104 c9
    ; @@ 0x0106 .label counter
    ; @@ 0x0106 2a
---
    org 0x100
    ld hl, counter                      ; @@ 0x0100 21 06 01     .data_ref
    ld a, (hl)                          ; @@ 0x0103 7e
    ret                                 ; @@ 0x0104 c9
    .space 1
counter:                                ; @@ 0x0106 .label counter
    db 0x2a                             ; @@ 0x0106 2a
