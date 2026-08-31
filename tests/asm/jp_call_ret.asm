    org 0x100
start:
    call start
    call z, next
    jp m, start
    ret nc
next:
    jp (hl)
    ret
---
0x0100: cd 00 01 cc 0a 01 fa 00 01 d0 e9 c9
