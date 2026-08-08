    ; @@ 0x0000 f3 .instr -- Hey.
    ; @@ 0x0001 af
---
    org 0x0
    di                                  ; @@ 0x0000 f3           .instr -- Hey.
    xor a                               ; @@ 0x0001 af
