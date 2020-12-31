    ld (hl), 0x2                        ; @@ 0x11dc 36 02        .instr
    dec hl                              ; @@ 0x11de 2b
    cp h                                ; @@ 0x11df bc
    jr nz, 0x11dc                       ; @@ 0x11e0 20 fa
