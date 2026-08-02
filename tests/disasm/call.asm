    call 0xdaf                          ; @@ 0x0d6b cd af 0d .instr
    ld hl, 0x5c3c                       ; @@ 0x0d6e 21 3c 5c

    ld hl, 0x0                          ; @@ 0x0daf 21 00 00
---
    org 0xd6b
    call l_0daf                         ; @@ 0x0d6b cd af 0d     .instr
    ld hl, 0x5c3c                       ; @@ 0x0d6e 21 3c 5c
    .space 62
l_0daf:
    ld hl, 0x0                          ; @@ 0x0daf 21 00 00
