    ; @@ 0x0100 .code_ref -- The continuation pushed as a return address.
    ; @@ 0x0100 21 08 01
    ; @@ 0x0103 e5
    ; @@ 0x0104 c9
    ; @@ 0x0105 00 00 00
    ; @@ 0x0108 3e 07
    ; @@ 0x010a c9
---
    org 0x100
    ld hl, l_0108                       ; @@ 0x0100 21 08 01     .code_ref -- The continuation pushed as a return address.
    push hl                             ; @@ 0x0103 e5
    ret                                 ; @@ 0x0104 c9
    db 0x00, 0x00, 0x00                 ; @@ 0x0105 00 00 00
l_0108:
    ld a, 0x7                           ; @@ 0x0108 3e 07
    ret                                 ; @@ 0x010a c9
