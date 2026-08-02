#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import collections
import os
import tempfile
import typing

from ._error import Error
from ._instr import (
    ADC,
    ADD,
    AF,
    AF2,
    AND,
    BC,
    BIT,
    CALL,
    CCF,
    CF,
    CP,
    CPD,
    CPDR,
    CPI,
    CPIR,
    CPL,
    DAA,
    DE,
    DEC,
    DI,
    DJNZ,
    EI,
    EX,
    EXX,
    HALT,
    HL,
    IM,
    IN,
    INC,
    IND,
    INDR,
    INI,
    INIR,
    IX,
    IXH,
    IXL,
    IY,
    IYH,
    IYL,
    JP,
    JR,
    LD,
    LDD,
    LDDR,
    LDI,
    LDIR,
    NC,
    NEG,
    NOP,
    NZ,
    OR,
    OTDR,
    OTIR,
    OUT,
    OUTD,
    OUTI,
    PE,
    PO,
    POP,
    PUSH,
    RES,
    RET,
    RETI,
    RETN,
    RL,
    RLA,
    RLC,
    RLCA,
    RLD,
    RR,
    RRA,
    RRC,
    RRCA,
    RRD,
    RST,
    SBC,
    SCF,
    SET,
    SLA,
    SLL,
    SP,
    SRA,
    SRL,
    SUB,
    XIM,
    XLD,
    XNEG,
    XNOP,
    XOR,
    XRETN,
    A,
    Add,
    At,
    B,
    C,
    CallInstr,
    D,
    E,
    H,
    IndexReg,
    Instr,
    IReg,
    JumpInstr,
    L,
    M,
    Op,
    P,
    R,
    Reg16,
    RetInstr,
    UnknownInstr,
    Z,
)
from ._machine import Z80Machine
from ._source import _SourcePos
from ._token import _Token


class _DisasmError(Error):
    def __init__(self, subject: '_Tag | _Token', message: str,
                 *notes: '_DisasmError') -> None:
        assert isinstance(subject.origin, _SourcePos)
        if isinstance(subject, _Token) and subject.literal is None:
            # There is nothing to name at positions past the last
            # token of the line.
            super().__init__(f'{subject.origin.inline_text}: {message}')
        else:
            super().__init__(
                f'{subject.origin.inline_text}: {subject}: {message}')
        self.subject = subject
        self.message = message
        self.notes = notes

    def verbalize(self, program_name: str | None = None) -> str:
        def g() -> typing.Generator[str, None, None]:
            assert isinstance(self.subject.origin, _SourcePos)
            yield self.subject.origin.context_text

            if program_name is not None:
                yield f'{program_name}: '

            yield f'{self.reason}'

            for n in self.notes:
                yield '\n'
                yield n.verbalize()

        return ''.join(g())


class _Tag:
    ID: str
    comment: str | _Token | None  # TODO

    def __init__(self, origin: _SourcePos | None, addr: int,
                 size: int) -> None:
        self.origin = origin
        self.addr = addr
        self.size = size
        self.comment = None

    def __str__(self) -> str:
        return f'{self.ID} tag'

    def __repr__(self) -> str:
        return f'({self.addr:#06x}, {self.ID}, {self.comment!r})'


class _CommentTag(_Tag):
    ID = 'comment'

    def __init__(self, origin: _SourcePos | None, addr: int,
                 comment: str) -> None:
        super().__init__(origin, addr, size=0)
        self.comment = comment


class _InlineCommentTag(_Tag):
    ID = 'inline_comment'

    def __init__(self, origin: _SourcePos, addr: int, comment: str) -> None:
        super().__init__(origin, addr, size=0)
        self.comment = comment


# A label naming an address. Parsed tags carry their source
# position; labels the disassembler derives for unnamed code
# targets have no origin and render as bare definition lines, so
# they are regenerated on every run rather than parsed back.
class _LabelTag(_Tag):
    ID = 'label'

    def __init__(self, origin: _SourcePos | None, addr: int,
                 name: str) -> None:
        super().__init__(origin, addr, size=0)
        self.name = name


class _HintTag(_Tag):
    ID = 'hint'

    def __init__(self, origin: _SourcePos | None, addr: int, hint: str):
        super().__init__(origin, addr, size=0)
        self.comment = hint


class _ByteTag(_Tag):
    ID = 'byte'

    def __init__(self, origin: _SourcePos | None, addr: int, value: int):
        super().__init__(origin, addr, size=1)
        self.value = value

    def __repr__(self) -> str:
        return (f'({self.addr:#06x}, {self.ID}, {self.value:#04x}, '
                f'{self.comment!r})')


class _IncludeBinaryTag(_Tag):
    ID = 'include_binary'

    def __init__(self, origin: _SourcePos, addr: int, filename: _Token,
                 image: bytes):
        super().__init__(origin, addr, size=len(image))
        self.filename = filename
        self.image = image

    def __repr__(self) -> str:
        return (f'({self.addr:#06x}, {self.ID}, {self.filename}, '
                f'{self.comment})')


class _InstrTag(_Tag):
    ID = 'instr'

    def __init__(self, origin: _SourcePos, addr: int) -> None:
        super().__init__(origin, addr, size=0)


# Same as _InstrTag, except that there can only be one entry tag,
# and that it can carry facts about the state of the machine as
# execution enters the tagged address, such as the value of SP.
class _EntryTag(_Tag):
    ID = 'entry'

    def __init__(self, origin: _SourcePos, addr: int,
                 sp: int | None = None) -> None:
        super().__init__(origin, addr, size=0)
        self.sp = sp


# Declares the calling convention of the routine at the tagged
# address: each call to it is followed by args_size inline argument
# bytes, or by argument bytes running up to and including the
# args_end terminator, or, with noreturn, execution never comes
# back at all.
class _CallConvTag(_Tag):
    ID = 'callconv'

    def __init__(self, origin: _SourcePos, addr: int,
                 args_size: int | None = None,
                 args_end: int | None = None,
                 noreturn: bool = False) -> None:
        super().__init__(origin, addr, size=0)
        self.args_size = args_size
        self.args_end = args_end
        self.noreturn = noreturn


# Presents the n words at the tagged address as dw directives,
# their values rendered by label where one is defined.
class _WordTag(_Tag):
    ID = 'word'

    def __init__(self, origin: _SourcePos, addr: int,
                 n: int = 1) -> None:
        super().__init__(origin, addr, size=0)
        self.n = n


# Declares the n words at the tagged address to be a table of code
# addresses, as evidenced by some dispatching code reading it:
# implies the word presentation and makes every entry's value an
# instruction address.
class _JumpTableTag(_WordTag):
    ID = 'jump_table'


# Declares the immediate operand of the instruction at the tagged
# address to be an address rather than a plain number, so that it
# renders by the label of whatever it points at. Jump and call
# targets need no such tag, being addresses by their nature;
# immediates are ambiguous, so the source has to say which of them
# are references and the tool never guesses.
class _DataRefTag(_Tag):
    ID = 'data_ref'

    def __init__(self, origin: _SourcePos, addr: int) -> None:
        super().__init__(origin, addr, size=0)


# Same as _DataRefTag, except that the referenced address is also
# declared to hold an instruction, as evidenced by the code that
# eventually passes control to it: routines called through a
# register, and continuations pushed as return addresses.
class _CodeRefTag(_DataRefTag):
    ID = 'code_ref'


# The machine state as known at a specific point of execution:
# for every field, the set of values it is known to possibly
# have, with None standing for values that cannot be known. While
# memory is known to be never written on any path since the
# recorded instant that the entry tag's facts and the image bytes
# jointly describe (memory_clobbered is exactly {False}), stack
# words still hold their image values, so returns can be
# followed.
class _State:
    def __init__(self, sp: int | None = None,
                 memory_clobbered: bool | None = None) -> None:
        self.sps: set[int | None] = {sp}
        self.memory_clobbered: set[bool | None] = {memory_clobbered}

    # Accumulates the facts of another state, returning whether
    # anything new has been learnt.
    def update(self, other: '_State') -> bool:
        updated = False

        if not other.sps <= self.sps:
            self.sps |= other.sps
            updated = True

        if not other.memory_clobbered <= self.memory_clobbered:
            self.memory_clobbered |= other.memory_clobbered
            updated = True

        return updated


# Marks an address as reachable by execution and therefore
# considered an instruction, in a given machine state.
class _DisasmTag(_Tag):
    instr: Instr

    ID = 'disasm'

    def __init__(self, origin: _SourcePos | None, addr: int,
                 state: _State | None = None) -> None:
        super().__init__(origin, addr, size=0)
        self.state = _State() if state is None else state


class _UnknownInstrError(Exception):
    pass


class Z80InstrBuilder:
    __INSTRS: typing.ClassVar[dict[str, type[Instr]]] = {
        'Aadd': ADD,
        'Aadc': ADC,
        'Aand': AND,
        'Acp': CP,
        'add': ADD,
        'adc': ADC,
        'Aor': OR,
        'Asbc': SBC,
        'Asub': SUB,
        'Axor': XOR,
        'bit': BIT,
        'call': CALL,
        'ccf': CCF,
        'cpl': CPL,
        'daa': DAA,
        'dec': DEC,
        'di': DI,
        'djnz': DJNZ,
        'ei': EI,
        'ex': EX,
        'exx': EXX,
        'halt': HALT,
        'im': IM,
        'xim': XIM,
        'inc': INC,
        'in': IN,
        'Iind': IND,
        'Iindr': INDR,
        'Iini': INI,
        'Iinir': INIR,
        'jp': JP,
        'jr': JR,
        'ld': LD,
        'xld': XLD,
        'Lldd': LDD,
        'Llddr': LDDR,
        'Lldi': LDI,
        'Lldir': LDIR,
        'Mcpd': CPD,
        'Mcpdr': CPDR,
        'Mcpi': CPI,
        'Mcpir': CPIR,
        'neg': NEG,
        'xneg': XNEG,
        'nop': NOP,
        'xnop': XNOP,
        'Orlc': RLC,
        'Orl': RL,
        'Orr': RR,
        'Orrc': RRC,
        'Osla': SLA,
        'Osll': SLL,
        'Osra': SRA,
        'Osrl': SRL,
        'out': OUT,
        'pop': POP,
        'push': PUSH,
        'res': RES,
        'ret': RET,
        'retn': RETN,
        'xretn': XRETN,
        'reti': RETI,
        'rla': RLA,
        'rlca': RLCA,
        'rld': RLD,
        'rra': RRA,
        'rrd': RRD,
        'rrca': RRCA,
        'rst': RST,
        'sbc': SBC,
        'scf': SCF,
        'set': SET,
        'Toutd': OUTD,
        'Totdr': OTDR,
        'Touti': OUTI,
        'Totir': OTIR,
    }

    __OPS: typing.ClassVar[dict[str, Op]] = {
        'a': A,
        'af': AF,
        'af\'': AF2,
        'c': C,
        'Cc': CF,
        'Cm': M,
        'Cnc': NC,
        'Cnz': NZ,
        'Cpe': PE,
        'Cpo': PO,
        'Cp': P,
        'Cz': Z,
        'de': DE,
        'Gaf': AF,
        'Gbc': BC,
        'Gde': DE,
        'Ghl': HL,
        'Gix': IX,
        'Giy': IY,
        'hl': HL,
        'i': IReg,
        'r': R,
        'ix': IX,
        'iy': IY,
        'Pbc': BC,
        'Pde': DE,
        'Phl': HL,
        'Pix': IX,
        'Piy': IY,
        'Psp': SP,
        'Ra': A,
        'Rb': B,
        'Rc': C,
        'Rd': D,
        'Re': E,
        'Rh': H,
        'Rl': L,
        'Rixh': IXH,
        'Rixl': IXL,
        'Riyh': IYH,
        'Riyl': IYL,
        'sp': SP,
    }

    def __build_op(self, addr: int, text: str) -> Op:
        if text.startswith('R('):
            text = text[1:]

        if text.startswith('('):
            assert text[-1] == ')'
            return At(self.__build_op(addr, text[1:-1]))

        # Base operand.
        # TODO: Have a proper base class for operand objects other than
        # primitive integers and strings.
        op: Op
        if text in self.__OPS:
            op = self.__OPS[text]
            text = ''
        elif text.startswith(('W', 'N', 'U')):
            ops = text.split()
            op = int(ops[0][1:], base=0)
            text = ' '.join(ops[1:])
        elif text.startswith('D$'):
            op = addr
            text = text[2:].strip()
        elif text.startswith(('ix', 'iy')):
            op = self.__OPS[text[:2]]
            text = text[2:].strip()
        else:
            raise _UnknownInstrError()

        # Offset.
        if text != '' and text[0] in ('+', '-'):
            sign = text[0]
            text = text[1:].strip()

            offset = int(text, base=0)
            text = ''

            if sign == '-':
                offset = -offset

            if isinstance(op, int):
                op += offset
            elif isinstance(op, IndexReg):
                op = Add(op, offset)
            else:
                raise _UnknownInstrError()

        assert text == '', text

        return op

    def build_instr(self, addr: int, image: bytes) -> Instr:
        original_text, size = Z80Machine._disasm(image)
        if size > len(image):
            # TODO: Too few bytes to disassemble this instruction.
            assert 0, image

        try:
            text = original_text.split(maxsplit=1)

            assert len(text) > 0, (original_text, image)
            name = text.pop(0)

            if name not in self.__INSTRS:
                raise _UnknownInstrError()

            # Parse operands.
            ops = []
            if text:
                text = text[0].split(',')
                while text:
                    op_text = text.pop(0).strip()
                    op = self.__build_op(addr, op_text)

                    if op is not None:
                        ops.append(op)

            instr = self.__INSTRS[name](*ops)
            instr.addr = addr
            instr.size = size
        except _UnknownInstrError:
            instr = UnknownInstr(addr, image[0])
            instr.text = original_text
            return instr

        return instr


class _TagSet:
    def __init__(self) -> None:
        self.infront_tags: list[_Tag] = []
        self.inline_tags: list[_Tag] = []
        self.byte_tag: _Tag | None = None
        self.disasm_tag: _Tag | None = None
        self.label_tag: _Tag | None = None
        self.word_tag: _Tag | None = None
        self.ref_tag: _Tag | None = None

        # The first tag found to reference the address as code,
        # set as the referencing tags are processed. Marked
        # addresses with no .label tag get derived l_xxxx labels,
        # so their references render symbolically and the output
        # reassembles into relocatable form. A mark alone
        # produces no output, so it does not make the set
        # non-empty.
        self.code_target_tag: _Tag | None = None

    @property
    def empty(self) -> bool:
        return (len(self.infront_tags) == 0 and
                len(self.inline_tags) == 0 and
                self.byte_tag is None and
                self.disasm_tag is None and
                self.label_tag is None and
                self.word_tag is None)


class _AsmLine:
    _MAX_NUM_OF_BYTES_PER_LINE = 4
    _BYTES_INDENT = 40
    __COMMENT_INDENT = (_BYTES_INDENT + len('; @@ 0x0000') +
                        len(' ff') * _MAX_NUM_OF_BYTES_PER_LINE +
                        len('  '))

    def __init__(self, command: str | _Tag | None = None,
                 addr: int | None = None, xbytes: list[int] | None = None,
                 comment: str | _Tag | None = None, size: int = 0,
                 as_equ: bool = False):
        self.command = command
        self.addr = addr
        self.xbytes = [] if xbytes is None else xbytes
        self.comment = comment
        self.size = size
        self.as_equ = as_equ

    @staticmethod
    def _verbalize_comment(comment: str, force_leader: bool = True) -> str:
        if comment.startswith('.'):
            force_leader = True
        if force_leader:
            comment = f'-- {comment}'
        return comment

    def __str__(self) -> str:
        if isinstance(self.command, _LabelTag) and self.command.origin is None:
            # Derived labels render as bare definitions carrying
            # no annotation, so they are regenerated rather than
            # parsed back.
            return f'{self.command.name}:'

        if isinstance(self.command, _LabelTag):
            # Label definitions render as real assembly so the
            # roundtrip check sees them; the annotation remains
            # the source of truth. Labels at addresses that do
            # not start a line become equ definitions.
            assert isinstance(self.addr, int)
            name = self.command.name
            if self.as_equ:
                line = f'{name} equ {self.addr:#06x}'
            else:
                line = f'{name}:'
            line = line.ljust(self._BYTES_INDENT)
            line += f'; @@ {self.addr:#06x} .label {name}'
            comment = self.command.comment
            if comment is not None:
                assert isinstance(comment, _Token)
                assert isinstance(comment.literal, str)
                line += f' {self._verbalize_comment(comment.literal)}'
            return line.rstrip()

        line = ' ' * 4
        out_of_line = isinstance(self.command, _Tag)
        if self.command is not None and not out_of_line:
            line += str(self.command)
        if (self.addr is not None or
                len(self.xbytes) > 0 or
                self.comment is not None):
            if not out_of_line:
                line = line.ljust(self._BYTES_INDENT)
            line += ';'
        if self.addr is not None:
            if not isinstance(self.comment, _HintTag):
                line += ' @@'
            else:
                line += '   '
            line += f' {self.addr:#06x}'
        if len(self.xbytes) > 0:
            assert self.addr is not None
            line += ' {}'.format(' '.join(f'{b:02x}' for b in self.xbytes))
        if out_of_line:
            assert isinstance(self.command, _Tag)
            assert isinstance(self.command.comment, str)
            line += ' ' + self._verbalize_comment(self.command.comment,
                                                  force_leader=False)
        if self.comment is not None:
            line = line.ljust(self.__COMMENT_INDENT)
            if isinstance(self.comment, _HintTag):
                line += str(self.comment.comment)
            else:
                line += str(self.comment)
        return line.rstrip()


class _Disasm:
    __TAG_PRIORITIES: typing.ClassVar[dict[type['_Tag'], int]] = {
        # These form the binary image so they have to be
        # processed first.
        _ByteTag: 0,
        _IncludeBinaryTag: 0,
        _InstrTag: 0,
        _EntryTag: 0,
        _CallConvTag: 0,
        _LabelTag: 0,

        # These need the image bytes to be in place: they read
        # word values out of them, or the operands of the
        # instructions decoded from them.
        _WordTag: 1,
        _JumpTableTag: 1,
        _DataRefTag: 1,
        _CodeRefTag: 1,

        _DisasmTag: 1,

        _CommentTag: 2,
        _InlineCommentTag: 2,
    }

    def __init__(self) -> None:
        # TODO: Let user choose the CPU type.
        self.__instr_builder = Z80InstrBuilder()

        # Translates addresses to tags associated with those
        # addresses.
        self.__tags: collections.defaultdict[int, _TagSet] = (
            collections.defaultdict(_TagSet))

        # Tags to process stored in order.
        self.__worklists: dict[int, collections.deque[_Tag]] = {}

        # The entry tag, if there is one.
        self.__entry_tag: _EntryTag | None = None

        # Translates routine addresses to their declared calling
        # conventions.
        self.__callconvs: dict[int, _CallConvTag] = {}

        # Translates label names to their tags.
        self.__label_names: dict[str, _LabelTag] = {}

    def __get_worklist(self, tag: _Tag) -> collections.deque[_Tag]:
        # Use deque because of its popleft() being much faster
        # than list's pop(0).
        Worklist = collections.deque

        priority = self.__TAG_PRIORITIES[type(tag)]
        if priority not in self.__worklists:
            self.__worklists[priority] = Worklist()
        return self.__worklists[priority]

    def add_tags(self, *tags: _Tag) -> None:
        for tag in reversed(tags):
            self.__get_worklist(tag).appendleft(tag)

    def __process_byte_tag(self, tag: _Tag) -> None:
        prev_tag = self.__tags[tag.addr].byte_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Byte redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__tags[tag.addr].byte_tag = tag

    def __process_include_binary_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _IncludeBinaryTag)
        new_tags: list[_Tag] = []

        comment = f'Included from binary file {tag.filename.literal!r}.'
        new_tags.append(_CommentTag(tag.origin, tag.addr, comment))

        if tag.comment is not None:
            assert isinstance(tag.comment, _Token)  # TODO
            assert isinstance(tag.comment.literal, str)  # TODO
            new_tags.append(_CommentTag(tag.origin, tag.addr,
                                        tag.comment.literal))

        for i, b in enumerate(tag.image):
            new_tags.append(_ByteTag(tag.origin, tag.addr + i, b))

        # TODO: Not really adding tags.
        self.add_tags(*new_tags)

    def __process_comment_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _CommentTag)
        self.__tags[tag.addr].infront_tags.append(tag)

    def __process_inline_comment_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _InlineCommentTag)
        self.__tags[tag.addr].inline_tags.append(tag)

    def __process_instr_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _InstrTag)
        self.__tags[tag.addr].inline_tags.append(tag)
        self.add_tags(_DisasmTag(tag.origin, tag.addr))

    def __process_label_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _LabelTag)
        tags = self.__tags[tag.addr]
        prev_tag = tags.label_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Label redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        prev_name_tag = self.__label_names.get(tag.name)
        if prev_name_tag is not None:
            raise _DisasmError(
                tag, 'Label name redefined.',
                _DisasmError(prev_name_tag, 'Previously defined here.'))

        tags.label_tag = tag
        self.__label_names[tag.name] = tag

    def __process_entry_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _EntryTag)
        prev_tag = self.__entry_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Entry redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__entry_tag = tag
        self.__tags[tag.addr].inline_tags.append(tag)

        # At the recorded instant the entry tag describes, memory
        # is still exactly as the image bytes have it.
        self.add_tags(_DisasmTag(tag.origin, tag.addr,
                                 _State(tag.sp, memory_clobbered=False)))

    # The value of the word at the given address, or None if
    # either of its bytes is not known.
    def __get_word_value(self, addr: int) -> int | None:
        lo = self.__tags[addr].byte_tag
        hi = self.__tags[(addr + 1) % 0x10000].byte_tag
        if lo is None or hi is None:
            return None

        assert isinstance(lo, _ByteTag)
        assert isinstance(hi, _ByteTag)
        return hi.value * 0x100 + lo.value

    def __process_word_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _WordTag)
        self.__tags[tag.addr].inline_tags.append(tag)

        for i in range(tag.n):
            addr = (tag.addr + i * 2) % 0x10000
            tags = self.__tags[addr]
            prev_tag = tags.word_tag
            if prev_tag is not None:
                raise _DisasmError(
                    tag, 'Word redefined.',
                    _DisasmError(prev_tag, 'Previously defined here.'))

            value = self.__get_word_value(addr)
            if value is None:
                raise _DisasmError(
                    tag, f'No byte value for the word at {addr:#06x}.')

            tags.word_tag = tag

            if isinstance(tag, _JumpTableTag):
                self.__mark_code_target(value, tag)
                self.add_tags(_DisasmTag(tag.origin, value))

    def __process_callconv_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _CallConvTag)
        prev_tag = self.__callconvs.get(tag.addr)
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Call convention redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__callconvs[tag.addr] = tag
        self.__tags[tag.addr].inline_tags.append(tag)

    # Registers a reference tag and applies it as soon as the
    # instruction carrying the operand is disassembled.
    def __process_ref_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _DataRefTag)
        prev_tag = self.__tags[tag.addr].ref_tag
        if prev_tag is not None:
            raise _DisasmError(
                tag, 'Reference redefined.',
                _DisasmError(prev_tag, 'Previously defined here.'))

        self.__tags[tag.addr].ref_tag = tag
        self.__tags[tag.addr].inline_tags.append(tag)

        # The tagged address holds the instruction whose operand
        # is the reference, so it is an instruction address too.
        self.add_tags(_DisasmTag(tag.origin, tag.addr))
        self.__apply_ref_tag(tag.addr)

    # The address the immediate operand of the given instruction
    # refers to. Only loads of a 16-bit register can carry one;
    # jump and call targets are references already.
    def __get_ref_target(self, tag: _Tag, instr: Instr) -> int | None:
        if isinstance(instr, UnknownInstr):
            return None

        if isinstance(instr, JumpInstr):
            raise _DisasmError(
                tag, 'Jump and call targets are references already.')

        ops = instr.ops
        if (len(ops) != 2 or not isinstance(ops[1], int) or
                not isinstance(ops[0], Reg16)):
            raise _DisasmError(
                tag, 'No immediate operand to refer with.')

        return ops[1]

    # Makes the referenced address an instruction, where the
    # reference says it is code. Runs once the tagged instruction
    # is disassembled, from whichever of the two tags comes last.
    def __apply_ref_tag(self, addr: int) -> None:
        tags = self.__tags[addr]
        tag = tags.ref_tag
        if not isinstance(tag, _CodeRefTag):
            return

        disasm_tag = tags.disasm_tag
        if disasm_tag is None:
            return

        assert isinstance(disasm_tag, _DisasmTag)
        target = self.__get_ref_target(tag, disasm_tag.instr)
        if target is None:
            return

        self.__mark_code_target(target, tag)
        self.add_tags(_DisasmTag(tag.origin, target))

    # Marks an address as referenced as code by the given tag,
    # the first referencing tag serving as the mark's provenance.
    def __mark_code_target(self, addr: int, tag: _Tag) -> None:
        tags = self.__tags[addr]
        if tags.code_target_tag is None:
            tags.code_target_tag = tag

    # The fall-through address of a call to a routine of the given
    # convention: past the inline argument bytes, or nowhere for a
    # non-returning routine or where the argument bytes run into
    # unknown memory.
    def __get_fallthrough_addr(self, conv: _CallConvTag,
                               next_addr: int) -> int | None:
        if conv.noreturn:
            return None

        if conv.args_size is not None:
            return (next_addr + conv.args_size) % 0x10000

        assert conv.args_end is not None
        addr = next_addr
        while True:
            t = self.__tags[addr].byte_tag
            if t is None:
                return None

            assert isinstance(t, _ByteTag)
            addr = (addr + 1) % 0x10000
            if t.value == conv.args_end:
                return addr

    def __process_disasm_tag(self, tag: _Tag) -> None:
        assert isinstance(tag, _DisasmTag)
        tags = self.__tags[tag.addr]
        prev_tag = tags.disasm_tag
        if prev_tag is not None:
            # The instruction is already disassembled; reconsider
            # it if and only if the tag brings new information.
            assert isinstance(prev_tag, _DisasmTag)
            if not prev_tag.state.update(tag.state):
                return

            tag = prev_tag
        else:
            MAX_INSTR_SIZE = 4

            instr_image = []
            assert isinstance(tag.addr, int), tag.addr
            for i in range(tag.addr, tag.addr + MAX_INSTR_SIZE):
                if self.__tags[i].byte_tag is None:
                    break

                t = self.__tags[i].byte_tag
                assert isinstance(t, _ByteTag)
                instr_image.append(t.value)

            if len(instr_image) == 0:
                return

            tag.instr = self.__instr_builder.build_instr(
                tag.addr, bytes(instr_image))
            tags.disasm_tag = tag
            self.__apply_ref_tag(tag.addr)

        instr = tag.instr
        if isinstance(instr, UnknownInstr):
            return

        assert isinstance(instr.addr, int)
        assert isinstance(instr.size, int)
        next_addr = instr.addr + instr.size

        if isinstance(instr, RetInstr):
            # Conditional returns may fall through.
            if instr.conditional:
                self.add_tags(_DisasmTag(instr.origin, next_addr))

            # Disassemble return targets. Only where memory is
            # known to be never written on any path, the words at
            # the possible SP values still hold their image
            # values.
            if tag.state.memory_clobbered == {False}:
                for sp in tag.state.sps:
                    if sp is None:
                        continue

                    lo = self.__tags[sp].byte_tag
                    hi = self.__tags[(sp + 1) % 0x10000].byte_tag
                    if lo is None or hi is None:
                        continue

                    assert isinstance(lo, _ByteTag)
                    assert isinstance(hi, _ByteTag)
                    target = hi.value * 0x100 + lo.value
                    state = _State(sp=(sp + 2) % 0x10000,
                                   memory_clobbered=False)
                    self.add_tags(_DisasmTag(instr.origin, target,
                                             state))

            return

        if isinstance(instr, JumpInstr):
            # Disassemble the jump target, unless indirect. The
            # target of an explicit operand is referenced as
            # code; rst targets are encoded in the opcode and
            # reference theirs implicitly.
            jump_target = instr.target
            if not isinstance(jump_target, At):
                assert isinstance(jump_target, int)
                if not isinstance(instr, RST):
                    self.__mark_code_target(jump_target, tag)
                self.add_tags(_DisasmTag(instr.origin, jump_target))

            # Calls are assumed to return, and conditional jumps
            # may fall through.  An unconditional call obeys its
            # target's declared calling convention: inline argument
            # bytes shift the fall-through address and stay data,
            # and a non-returning target has no fall-through at
            # all.  A conditional call ignores the convention, as
            # its not-taken path genuinely runs the following
            # bytes.
            if isinstance(instr, CallInstr) or instr.conditional:
                fallthrough_addr: int | None = next_addr
                if (isinstance(instr, CallInstr) and
                        not instr.conditional and
                        not isinstance(jump_target, At)):
                    conv = self.__callconvs.get(jump_target)
                    if conv is not None:
                        fallthrough_addr = self.__get_fallthrough_addr(
                            conv, next_addr)

                if fallthrough_addr is not None:
                    self.add_tags(_DisasmTag(instr.origin,
                                             fallthrough_addr))

            return

        # Disassemble the following instruction.
        self.add_tags(_DisasmTag(instr.origin, next_addr))

    __TAG_PROCESSORS: typing.ClassVar[dict[
        type['_Tag'],
        typing.Callable[['_Disasm', typing.Any], None]]] = {
        _ByteTag: __process_byte_tag,
        _CommentTag: __process_comment_tag,
        _IncludeBinaryTag: __process_include_binary_tag,
        _InlineCommentTag: __process_inline_comment_tag,
        _InstrTag: __process_instr_tag,
        _EntryTag: __process_entry_tag,
        _CallConvTag: __process_callconv_tag,
        _LabelTag: __process_label_tag,
        _WordTag: __process_word_tag,
        _JumpTableTag: __process_word_tag,
        _DataRefTag: __process_ref_tag,
        _CodeRefTag: __process_ref_tag,
        _DisasmTag: __process_disasm_tag,
    }

    def __process_tag(self, tag: _Tag) -> None:
        assert tag.addr is not None
        process = self.__TAG_PROCESSORS[type(tag)]
        process(self, tag)

    def disassemble(self) -> None:
        while self.__worklists:
            priority = min(self.__worklists)
            worklist = self.__worklists[priority]
            tag = worklist.popleft()

            if len(worklist) == 0:
                del self.__worklists[priority]

            self.__process_tag(tag)

    def __get_inline_comments(
            self, addr: int,
            first_instr_byte: bool = False) -> (
                typing.Generator[str | _Tag, None, None]):
        for tag in self.__tags[addr].inline_tags:
            if isinstance(tag, (_InstrTag, _EntryTag, _CallConvTag,
                                _WordTag, _DataRefTag)):
                comment = f'.{tag.ID}'
                if isinstance(tag, _EntryTag) and tag.sp is not None:
                    comment += f' sp={tag.sp:#06x}'
                if isinstance(tag, _WordTag):
                    comment += f' n={tag.n}'
                if isinstance(tag, _CallConvTag):
                    if tag.noreturn:
                        comment += ' noreturn'
                    elif tag.args_size is not None:
                        comment += f' args_size={tag.args_size}'
                    else:
                        assert tag.args_end is not None
                        comment += f' args_end={tag.args_end:#04x}'
                if tag.comment is not None:
                    assert isinstance(tag.comment, _Token)
                    assert isinstance(tag.comment.literal, str)
                    comment += ' ' + _AsmLine._verbalize_comment(
                        tag.comment.literal)
                yield comment
            elif isinstance(tag, _InlineCommentTag):
                assert isinstance(tag.comment, str)
                yield _AsmLine._verbalize_comment(tag.comment,
                                                  force_leader=False)
            else:
                assert 0, tag

        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            assert isinstance(disasm_tag, _DisasmTag)
            instr = disasm_tag.instr
            if not first_instr_byte:
                yield _HintTag(instr.origin, addr,
                               'warning: overlapping instruction: '
                               f'{str(instr)!r}')

            if isinstance(instr, UnknownInstr):
                yield _HintTag(instr.origin, addr,
                               'warning: unknown instruction: '
                               f'{instr.text!r}')

    # Whether the address gets a derived label: it must be
    # referenced as code, carry no .label tag of its own, start a
    # decoded instruction that is not hidden inside another
    # rendered span, and its derived name must be free. Spans are
    # at most four bytes, so any covering one starts within three
    # bytes back.
    def __has_derived_label(self, addr: int) -> bool:
        tags = self.__tags[addr]
        if tags.code_target_tag is None or tags.label_tag is not None:
            return False

        if tags.disasm_tag is None:
            return False

        for back in range(1, 4):
            t = self.__tags[(addr - back) % 0x10000].disasm_tag
            if t is not None:
                assert isinstance(t, _DisasmTag)
                size = t.instr.size
                if isinstance(size, int) and size > back:
                    return False

            if (back < 2 and
                    self.__tags[(addr - back) % 0x10000].word_tag
                    is not None):
                return False

        # A free-standing label elsewhere may own the derived
        # name; better to leave the references numeric than to
        # bind them wrongly.
        return f'l_{addr:04x}' not in self.__label_names

    # The name the given address renders by, if any: its label's,
    # or its derived one's.
    def __get_label_name(self, addr: int) -> str | None:
        label_tag = self.__tags[addr].label_tag
        if label_tag is not None:
            assert isinstance(label_tag, _LabelTag)
            return label_tag.name

        if self.__has_derived_label(addr):
            return f'l_{addr:04x}'

        return None

    def __is_commentless_addr(self, addr: int) -> bool:
        tags = self.__tags[addr]
        if tags.disasm_tag is not None or len(tags.infront_tags) != 0:
            return False

        if tags.label_tag is not None:
            return False

        return not any(True for _ in self.__get_inline_comments(addr))

    def __get_instr_text(self, instr: Instr) -> str:
        text = str(instr)

        # Render jump and call targets at labelled addresses by
        # their names. The target is the only numeric operand of
        # these instructions, so plain text substitution is exact.
        if (isinstance(instr, JumpInstr) and
                not isinstance(instr, (RetInstr, RST))):
            target = instr.target
            if isinstance(target, int):
                name = self.__get_label_name(target)
                if name is not None:
                    text = text.replace(f'{target:#x}', name)

        # Immediate operands are plain numbers unless the source
        # says otherwise; where it does, render them by the label
        # of what they point at.
        instr_addr = instr.addr
        if isinstance(instr_addr, int):
            ref_tag = self.__tags[instr_addr].ref_tag
            if isinstance(ref_tag, _DataRefTag):
                ref_target = self.__get_ref_target(ref_tag, instr)
                if ref_target is not None:
                    ref_name = self.__get_label_name(ref_target)
                    if ref_name is not None:
                        text = text.replace(f'{ref_target:#x}', ref_name)

        # Likewise for memory operands, whose brackets make the
        # substitution unambiguous. I/O ports look the same but
        # are not addresses.
        if not isinstance(instr, (IN, OUT)):
            for op in instr.ops:
                if not isinstance(op, At):
                    continue

                target = op.ops[0]
                if not isinstance(target, int):
                    continue

                label_tag = self.__tags[target].label_tag
                if label_tag is not None:
                    assert isinstance(label_tag, _LabelTag)
                    text = text.replace(f'({target:#x})',
                                        f'({label_tag.name})')

        return text

    def __get_instr_lines(self, instr: Instr) -> (
            typing.Generator[_AsmLine, None, None]):
        command: None | str = self.__get_instr_text(instr)
        addr = instr.addr
        assert isinstance(addr, int)
        t = self.__tags[addr].byte_tag
        assert isinstance(t, _ByteTag)
        xbytes = [t.value]

        assert isinstance(instr.size, int)
        end_addr = addr + instr.size
        byte_addr = addr + 1

        while addr < end_addr:
            while (byte_addr < end_addr and
                   (len(xbytes) == 0 or
                    self.__is_commentless_addr(byte_addr)) and
                   len(xbytes) < _AsmLine._MAX_NUM_OF_BYTES_PER_LINE):
                t = self.__tags[byte_addr].byte_tag
                assert isinstance(t, _ByteTag)
                xbytes.append(t.value)
                byte_addr += 1

            for tag in self.__tags[addr].infront_tags:
                yield _AsmLine(addr=addr, command=tag)

            label_tag = self.__tags[addr].label_tag
            if label_tag is not None:
                yield _AsmLine(command=label_tag, addr=addr,
                               as_equ=addr != instr.addr)
            elif addr == instr.addr and self.__has_derived_label(addr):
                yield _AsmLine(command=_LabelTag(None, addr,
                                                 f'l_{addr:04x}'),
                               addr=addr)

            first_instr_byte = addr == instr.addr
            inline_comments = list(
                self.__get_inline_comments(addr, first_instr_byte))
            while len(xbytes) > 0 or len(inline_comments) > 0:
                comment = None
                if (len(inline_comments) > 0 and
                    (len(xbytes) == 0 or
                        not isinstance(inline_comments[0], _HintTag))):
                    comment = inline_comments.pop(0)

                yield _AsmLine(command=command, addr=addr, xbytes=xbytes,
                               comment=comment, size=len(xbytes))

                command = None
                xbytes = []

            addr = byte_addr

    def __get_data_lines(self, addr: int) -> (
            typing.Generator[_AsmLine, None, None]):
        tags = self.__tags[addr]
        for tag in tags.infront_tags:
            yield _AsmLine(addr=addr, command=tag)

        if tags.label_tag is not None:
            yield _AsmLine(command=tags.label_tag, addr=addr)

        if tags.byte_tag is None:
            return

        # Words render as dw directives, their values by label
        # where one is defined, so the reference survives
        # reassembly.
        if tags.word_tag is not None:
            value = self.__get_word_value(addr)
            assert value is not None
            name = self.__get_label_name(value)
            operand = f'{value:#06x}' if name is None else name

            assert isinstance(tags.byte_tag, _ByteTag)
            hi_tag = self.__tags[(addr + 1) % 0x10000].byte_tag
            assert isinstance(hi_tag, _ByteTag)
            xbytes = [tags.byte_tag.value, hi_tag.value]
            inline_comments = list(self.__get_inline_comments(addr))

            command: str | None = f'dw {operand}'
            while len(xbytes) > 0 or len(inline_comments) > 0:
                comment = None
                if len(inline_comments) > 0:
                    comment = inline_comments.pop(0)

                yield _AsmLine(command=command, addr=addr, xbytes=xbytes,
                               comment=comment, size=len(xbytes))
                command = None
                xbytes = []

            return

        assert isinstance(tags.byte_tag, _ByteTag)
        xbytes = [tags.byte_tag.value]
        inline_comments = list(self.__get_inline_comments(addr))

        if len(inline_comments) == 0:
            byte_addr = addr + 1

            while len(xbytes) < _AsmLine._MAX_NUM_OF_BYTES_PER_LINE:
                byte_tag = self.__tags[byte_addr].byte_tag
                if byte_tag is None:
                    break

                if not self.__is_commentless_addr(byte_addr):
                    break

                # Break at aligned addresses to reduce the amount
                # of changes when adding more addresses to
                # disassemble. This makes the output more
                # friendly to source version control systems,
                # such as git.
                if byte_addr % _AsmLine._MAX_NUM_OF_BYTES_PER_LINE == 0:
                    break

                assert isinstance(byte_tag, _ByteTag)
                xbytes.append(byte_tag.value)
                byte_addr += 1

        while len(xbytes) > 0 or len(inline_comments) > 0:
            comment = None
            if len(inline_comments) > 0:
                comment = inline_comments.pop(0)

            command = 'db {}'.format(', '.join(f'{b:#04x}' for b in xbytes))

            yield _AsmLine(command=command, addr=addr, xbytes=xbytes,
                           comment=comment, size=len(xbytes))
            xbytes = []

    def __get_lines_for_addr(self, addr: int) -> (
            typing.Generator[_AsmLine, None, None]):
        disasm_tag = self.__tags[addr].disasm_tag
        if disasm_tag is not None:
            assert isinstance(disasm_tag, _DisasmTag)
            yield from self.__get_instr_lines(disasm_tag.instr)
        else:
            yield from self.__get_data_lines(addr)

    def __get_asm_lines(self) -> typing.Generator[_AsmLine, None, None]:
        yield _AsmLine()

        addr = None
        for a in sorted(a for a, t in self.__tags.items() if not t.empty):
            if addr is None:
                addr = a
                yield _AsmLine(command=f'org 0x{addr:x}')
            elif a < addr:
                continue
            elif a > addr:
                yield _AsmLine(command=f'.space {a - addr}')
                addr = a

            assert a == addr

            for line in self.__get_lines_for_addr(addr):
                yield line
                addr += line.size

    def _get_output(self) -> typing.Generator[str, None, None]:
        for line in self.__get_asm_lines():
            yield f'{line}\n'

    def save_output(self, filename: str) -> None:
        tmp_name: str | None = None
        try:
            with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
                tmp_name = f.name
                for chunk in self._get_output():
                    f.write(chunk)

            os.rename(tmp_name, filename)
            tmp_name = None
        finally:
            if tmp_name is not None:
                os.remove(tmp_name)
