
@@ 0x0000 .include_binary 'input.bin' -- The source binary.

---
    org 0x0
    ; @@ 0x0000 Included from binary file 'input.bin'.
    ; @@ 0x0000 The source binary.
    db 0xf3, 0xaf, 0x11, 0xff           ; @@ 0x0000 f3 af 11 ff
    db 0xff, 0xc3, 0xcb, 0x11           ; @@ 0x0004 ff c3 cb 11
