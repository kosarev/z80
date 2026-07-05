    ; @@ 0x0000 0xf3 .entry sp=0xfff0 -- Hey.
    ; @@ 0x0001 0xaf
---
    org 0x0
    di                                  ; @@ 0x0000 f3           .entry sp=0xfff0 -- Hey.
    xor a                               ; @@ 0x0001 af
