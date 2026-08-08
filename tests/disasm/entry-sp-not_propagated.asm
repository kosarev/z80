    ; @@ 0x0000 af .entry sp=0x0010
    ; @@ 0x0001 c9
    ; @@ 0x0008 3c
    ; @@ 0x0010 08 00
---
    org 0x0
    xor a                               ; @@ 0x0000 af           .entry sp=0x0010
    ret                                 ; @@ 0x0001 c9
    .space 6
    db 0x3c                             ; @@ 0x0008 3c
    .space 7
    db 0x08, 0x00                       ; @@ 0x0010 08 00
