    org 0x100
    ld (iy + 0x0), l
    ld a, (ix - 0x2)
---
0x0100: fd 75 00 dd 7e fe
