    call 0xdaf                          ; @@ 0x0d6b cd af 0d .instr
    ld hl, 0x5c3c                       ; @@ 0x0d6e 21 3c 5c

    ld hl, 0x0                          ; @@ 0x0daf 21 00 00
---
    call 0xdaf                          ; @@ 0x0d6b cd af 0d     .instr
    ld hl, 0x5c3c                       ; @@ 0x0d6e 21 3c 5c
    .space 62
    ld hl, 0x0                          ; @@ 0x0daf 21 00 00
