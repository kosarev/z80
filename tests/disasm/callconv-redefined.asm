    ; @@ 0x0100 c9 .callconv args_size=1
    ; @@ 0x0100 .callconv noreturn
===
    ; @@ 0x0100 .callconv noreturn
                 ^
disasm/callconv-redefined.asm:2:17: callconv tag: Call convention redefined.
    ; @@ 0x0100 c9 .callconv args_size=1
                    ^
disasm/callconv-redefined.asm:1:20: callconv tag: Previously defined here.
