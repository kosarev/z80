    ; @@ 0x0100 .code_ref
    ; @@ 0x0100 21 06 01
    ; @@ 0x0103 e9
    ; @@ 0x0106 .label handler
    ; @@ 0x0106 3e 07
    ; @@ 0x0108 c9
---
    org 0x100
    ld hl, handler                      ; @@ 0x0100 21 06 01     .code_ref
    jp (hl)                             ; @@ 0x0103 e9
    .space 2
handler:                                ; @@ 0x0106 .label handler
    ld a, 0x7                           ; @@ 0x0106 3e 07
    ret                                 ; @@ 0x0108 c9
