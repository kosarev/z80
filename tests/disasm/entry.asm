    ; @@ 0x0000 0xf3 .entry -- Hey.
    ; @@ 0x0001 0xaf
---
    org 0x0
    di                                  ; @@ 0x0000 f3           .entry -- Hey.
    xor a                               ; @@ 0x0001 af
