    ; @@ 0x0100 .instr
    ; @@ 0x0100 21 00 02
    ; @@ 0x0103 18 fc
    ; @@ 0x0105 c9
---
    org 0x100
    ld hl, 0x200                        ; @@ 0x0100 21           .instr
                                        ; @@ 0x0101 00
                                        ;    0x0101              warning: overlapping instruction: 'nop'
                                        ; @@ 0x0102 02
                                        ;    0x0102              warning: overlapping instruction: 'ld (bc), a'
    jr 0x101                            ; @@ 0x0103 18 fc
    db 0xc9                             ; @@ 0x0105 c9
