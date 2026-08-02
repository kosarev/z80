    ; @@ 0x0100 00 02 03 02 .jump_table n=2 -- Handlers of the two codes.
    ; @@ 0x0200 .label handle_nought
    ; @@ 0x0200 af c9
    ; @@ 0x0203 3e 01 c9
---
    org 0x100
    dw handle_nought                    ; @@ 0x0100 00 02        .jump_table n=2 -- Handlers of the two codes.
    dw l_0203                           ; @@ 0x0102 03 02
    .space 252
handle_nought:                          ; @@ 0x0200 .label handle_nought
    xor a                               ; @@ 0x0200 af
    ret                                 ; @@ 0x0201 c9
    .space 1
l_0203:
    ld a, 0x1                           ; @@ 0x0203 3e 01
    ret                                 ; @@ 0x0205 c9
