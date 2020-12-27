    ld (hl), 0x2                        ; @@ 0x11dc 0x36 0x02 instr
    dec hl                              ; @@ 0x11de 0x2b
    cp h                                ; @@ 0x11df 0xbc
    jr nz, 0x11dc                       ; @@ 0x11e0 0x20 0xfa
