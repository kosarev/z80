    org 0x0
    ld hl, (word)                       ; @@ 0x0000 2a 08 00     .instr
    ld (word), hl                       ; @@ 0x0003 22 08 00
    ret                                 ; @@ 0x0006 c9
    .space 1
word:                                   ; @@ 0x0008 .label word
    db 0x34, 0x12                       ; @@ 0x0008 34 12
