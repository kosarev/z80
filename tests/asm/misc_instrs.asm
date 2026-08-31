    org 0x100
loop:
    bit 7, (hl)
    im 1
    rst 0x28
    djnz loop
    ex de, hl
---
0x0100: cb 7e ed 56 ef 10 f9 eb
