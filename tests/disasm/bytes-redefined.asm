    db 0xf3                             ; @@ 0x0000 f3 00
    db 0xaf                             ; @@ 0x0001 af
===
    db 0xaf                             ; @@ 0x0001 af
                                                    ^
disasm/bytes-redefined.asm:2:52: byte tag: Byte redefined.
    db 0xf3                             ; @@ 0x0000 f3 00
                                                       ^
disasm/bytes-redefined.asm:1:55: byte tag: Previously defined here.
