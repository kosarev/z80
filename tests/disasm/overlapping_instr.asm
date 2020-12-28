    rst 0x28                            ; @@ 0x2756 0xef instr
    dec sp                              ; @@ 0x2757 0x3b
    jr c, 0x2772                        ; @@ 0x2758 0x38 0x18

    jr 0x2734                           ; @@ 0x2771 0x18 0xc1 instr
---
    rst 0x28                            ; @@ 0x2756 0xef instr
    dec sp                              ; @@ 0x2757 0x3b
    jr c, 0x2772                        ; @@ 0x2758 0x38 0x18
    .space 23
    jr 0x2734                           ; @@ 0x2771 0x18 0xc1 instr
                                        ;    0x2772 0xc1 warning: overlapping instruction: 'pop bc'
