    db 0xf3                             ; @@ 0x0000 0xf3 0x00
    db 0xaf                             ; @@ 0x0001 0xaf
===
    db 0xaf                             ; @@ 0x0001 0xaf
                                                    ^
disasm/bytes-redefined.asm:2:52: byte tag: Byte redefined.
    db 0xf3                             ; @@ 0x0000 0xf3 0x00
                                                         ^
disasm/bytes-redefined.asm:1:57: byte tag: Previously defined here.
