    ; @@ 0x0000 c9 .entry sp=0x0010
    ; @@ 0x0008 c9
    ; @@ 0x0009 af
    ; @@ 0x0010 08 00
    ; @@ 0x0012 09 00
---
    org 0x0
    ret                                 ; @@ 0x0000 c9           .entry sp=0x0010
    .space 7
    ret                                 ; @@ 0x0008 c9
    xor a                               ; @@ 0x0009 af
    .space 6
    db 0x08, 0x00, 0x09, 0x00           ; @@ 0x0010 08 00 09 00
