
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing

from ._error import Error
from ._instr import Const, Expr, Instr, Symbol, Value


# The directive binding a symbol to the point of the block it
# appears at. The symbol itself is a value; defining it is an
# action, hence a directive.
class LabelDef:
    def __init__(self, symbol: Symbol) -> None:
        self.symbol = symbol

    def __str__(self) -> str:
        return f'{self.symbol}:'


# The directive emitting the values of its operands as bytes.
class DB:
    def __init__(self, *values: Value) -> None:
        self.values = tuple(Const(v) if isinstance(v, int) else v
                            for v in values)

    def __str__(self) -> str:
        return 'db ' + ', '.join(str(v) for v in self.values)

    @property
    def size(self) -> int:
        return len(self.values)

    def encode(self, addr: int | None = None) -> bytes:
        image = b''
        for value in self.values:
            v = value.evaluate()
            if not -0x80 <= v <= 0xff:
                raise Error(f'{v:#x} does not fit a byte.')

            image += (v & 0xff).to_bytes(1, 'little')

        return image


BlockItem: typing.TypeAlias = Instr | LabelDef | DB


# A run of instructions and directives at an address, which may be
# an expression over labels of other blocks.
class _Block:
    def __init__(self, addr: Expr) -> None:
        self.addr = addr
        self.items: list[BlockItem] = []


# The machine-code-level program: the symbol context, the container
# of instructions and directives, and the semantic actions drivers
# such as the source-level assembler call.
class Code:
    def __init__(self) -> None:
        self.__symbols: dict[str, Symbol] = {}
        self.__blocks: list[_Block] = []
        self.start_block(0)

    # Symbols are interned per name, so every reference to a name
    # shares the object its definition binds.
    def get_symbol(self, name: str) -> Symbol:
        symbol = self.__symbols.get(name)
        if symbol is None:
            symbol = Symbol._create(name)
            self.__symbols[name] = symbol

        return symbol

    def define_symbol(self, name: str, value: int) -> Symbol:
        symbol = self.get_symbol(name)
        symbol.define(value)
        return symbol

    # Starts a new block at the given address, which may be an
    # expression over labels of other blocks. Blocks may overlap:
    # pages legally share addresses, and packing the produced code
    # into images is a separate concern.
    def start_block(self, addr: Value) -> None:
        if isinstance(addr, int):
            addr = Const(addr)

        self.__blocks.append(_Block(addr))

    # Appends instructions and directives to the current block.
    def add(self, *items: BlockItem) -> None:
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
                    if isinstance(item, LabelDef):
                        item.symbol.define(addr)
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
                if not isinstance(item, LabelDef):
                    image += item.encode(addr + len(image))

            if image:
                blocks.append((addr, image))

        return blocks
