    org 0x100
    ld hl, (data)
    ld (0x5c5f), hl
data:
    ld a, 0x55
---
0x0100: 2a 06 01 22 5f 5c 3e 55
