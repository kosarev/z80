    org 0x100
loop:
    jr next
    jr nz, loop
next:
    jr loop
---
0x0100: 18 02 20 fc 18 fa
