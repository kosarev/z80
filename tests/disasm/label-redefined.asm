    ; @@ 0x0000 0xc9 .label first
    ; @@ 0x0000 .label second
===
    ; @@ 0x0000 .label second
                 ^
disasm/label-redefined.asm:2:17: label tag: Label redefined.
    ; @@ 0x0000 0xc9 .label first
                      ^
disasm/label-redefined.asm:1:22: label tag: Previously defined here.
