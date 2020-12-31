    rst 0x28                            ; @@ 0x2756 ef .instr
    dec sp                              ; @@ 0x2757 3b
    jr c, 0x2772                        ; @@ 0x2758 38 18

    jr 0x2734                           ; @@ 0x2771 18 c1 .instr
---
    rst 0x28                            ; @@ 0x2756 ef           .instr
    dec sp                              ; @@ 0x2757 3b
    jr c, 0x2772                        ; @@ 0x2758 38 18
    .space 23
    jr 0x2734                           ; @@ 0x2771 18           .instr
                                        ; @@ 0x2772 c1
                                        ;    0x2772              warning: overlapping instruction: 'pop bc'
