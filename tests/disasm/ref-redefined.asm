    ; @@ 0x0100 .code_ref
    ; @@ 0x0100 .data_ref
    ; @@ 0x0100 21 06 01
    ; @@ 0x0103 c9
    ; @@ 0x0106 c9
===
    ; @@ 0x0100 .data_ref
                 ^
disasm/ref-redefined.asm:2:17: data_ref tag: Reference redefined.
    ; @@ 0x0100 .code_ref
                 ^
disasm/ref-redefined.asm:1:17: code_ref tag: Previously defined here.
