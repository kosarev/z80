
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

from ._error import Error
from ._instr import Const, Expr, Instr, Symbol, Value


# A run of instructions and label definitions at an address, which
# may be an expression over labels of other blocks.
class _Block:
    def __init__(self, addr: Expr) -> None:
        self.addr = addr
        self.items: list[Instr | Symbol] = []


# The machine-code-level program: the symbol context, the container
# of instructions and directives, and the semantic actions drivers
# such as the source-level assembler call.
class Code:
    def __init__(self) -> None:
        self.__symbols: dict[str, Symbol] = {}
        self.__blocks: list[_Block] = []
        self.org(0)

    # Symbols are interned per name, so every reference to a name
    # shares the object its definition binds.
    def get_symbol(self, name: str) -> Symbol:
        symbol = self.__symbols.get(name)
        if symbol is None:
            symbol = Symbol(name)
            self.__symbols[name] = symbol

        return symbol

    # Starts a new block at the given address, which may be an
    # expression over labels of other blocks. Blocks may overlap:
    # pages legally share addresses, and packing the produced code
    # into images is a separate concern.
    def org(self, addr: Value) -> None:
        if isinstance(addr, int):
            addr = Const(addr)

        self.__blocks.append(_Block(addr))

    # Appends instructions to the current block. A bare symbol
    # defines a label at the point it appears.
    def add(self, *items: Instr | Symbol) -> None:
        self.__blocks[-1].items.extend(items)

    # Binds every label to its address. Sizes are shape-determined,
    # so a block lays out as soon as its own address evaluates; the
    # worklist retries blocks whose addresses depend on labels of
    # blocks not laid out yet.
    def resolve(self) -> None:
        pending = self.__blocks
        while pending:
            error: Error | None = None
            remaining = []
            for block in pending:
                try:
                    addr = block.addr.evaluate()
                except Error as e:
                    error = e
                    remaining.append(block)
                    continue

                for item in block.items:
                    if isinstance(item, Symbol):
                        item.value = addr
                    else:
                        addr += item.size

            if len(remaining) == len(pending):
                assert error is not None
                raise error

            pending = remaining

    # The blocks of the code as (address, bytes) pairs, in the
    # order of their intake. Empty blocks vanish.
    def encode(self) -> list[tuple[int, bytes]]:
        blocks = []
        for block in self.__blocks:
            addr = block.addr.evaluate()
            image = b''
            for item in block.items:
                if isinstance(item, Instr):
                    image += item.encode(addr + len(image))

            if image:
                blocks.append((addr, image))

        return blocks
