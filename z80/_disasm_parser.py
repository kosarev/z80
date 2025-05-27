#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import re
import typing

from ._disasm import (_Disasm, _IncludeBinaryTag, _InstrTag, _CommentTag,
                      _ByteTag, _InlineCommentTag, _AsmLine)
from ._disasm import _DisasmError
from ._disasm import _Tag
from ._source import _SourceFile
from ._token import _Token
from ._token import _Tokeniser


class _DisasmTagParser(object):
    __TAG_LEADER = re.compile('@@')

    __tok: _Token | None

    __TOKENS = re.compile(
        r'([_0-9a-zA-Z]+)|'           # A number or identifier.
        r"'(\\'|\\\\|[^'\\\n])*'|"    # A string.
        r'(--)|'                      # The tag comment leader.
        r'.')                         # Or any other single character.

    def __init__(self, source_file: _SourceFile) -> None:
        self.__toks = _Tokeniser(source_file)
        self.__tok = None

    def __fetch_token(self, error: str | None = None) -> _Token | None:
        self.__toks.skip_whitespace()

        self.__toks.start_token()
        self.__toks.skip(self.__TOKENS)
        tok: _Token | None = self.__toks.end_token()

        assert tok is not None
        if tok.literal == '\n':
            tok.literal = None

        if tok.literal is None:
            if error is not None:
                assert 0  # TODO
                # raise _DisasmError(tok.pos, error)

            tok = None
        else:
            # Translate escape sequences.
            if tok.literal.startswith("'"):
                if tok.literal == "'":
                    assert 0  # TODO
                    # raise _DisasmError(tok.pos, 'Unterminated string.')

                tok.literal = (tok.literal[1:-1].
                               replace('\\\\', '\\').
                               replace("\\'", "'"))

        self.__tok = tok

        return self.__tok

    def __evaluate_numeric_literal(self, literal: str,
                                   base: int = 0) -> int | None:
        try:
            return int(literal, base)
        except ValueError:
            return None

    def __parse_include_binary_tag(self, addr: int,
                                   name: _Token) -> _IncludeBinaryTag:
        filename = self.__fetch_token('A filename expected.')
        self.__fetch_token()

        assert filename is not None
        assert isinstance(filename.literal, str)
        with open(filename.literal, 'rb') as f:
            image = f.read()

        return _IncludeBinaryTag(name.pos, addr, filename, image)

    def __parse_instr_tag(self, addr: int, name: _Token) -> _InstrTag:
        self.__fetch_token()
        return _InstrTag(name.pos, addr)

    __TAG_PARSERS = {
        _IncludeBinaryTag.ID: __parse_include_binary_tag,
        _InstrTag.ID: __parse_instr_tag,
    }

    # Parses and returns a subsequent tag.
    def __iter__(self) -> typing.Generator[_Tag, None, None]:
        while self.__toks.skip_next(self.__TAG_LEADER):
            tok = self.__fetch_token()

            # Parse optional tag address.
            assert tok is not None and tok.literal is not None
            addr = self.__evaluate_numeric_literal(tok.literal)
            if addr is not None:
                tok = self.__fetch_token()

            tags: list[_Tag] = []

            # Collect bytes, if any specified.
            byte_offset = 0
            while tok is not None:
                assert tok is not None and tok.literal is not None
                value = self.__evaluate_numeric_literal(tok.literal, base=16)
                if value is None:
                    break

                assert addr is not None
                tags.append(_ByteTag(tok.pos, addr + byte_offset, value))
                byte_offset += 1

                tok = self.__fetch_token()

            # Parse regular tag.
            subject_tag = None
            if tok == '.':
                tok = self.__fetch_token()
                assert tok is not None and tok.literal is not None
                parser = self.__TAG_PARSERS.get(tok.literal, None)
                if not parser:
                    assert 0  # TODO
                    # raise _DisasmError(tok, 'Unknown tag.')

                assert addr is not None
                subject_tag = parser(self, addr, tok)
                tags.append(subject_tag)
                tok = self.__tok

                if tok is not None and tok != '--':
                    assert 0  # TODO
                    # raise _DisasmError(tok,
                    #                    'End of line or a comment expected.')

            # Parse comment, if specified.
            if tok == '--':
                tok = self.__fetch_token()

            if tok is not None:
                self.__toks.skip_rest_of_line()
                comment = self.__toks.end_token()

                if subject_tag is None:
                    assert addr is not None
                    assert comment is not None and comment.literal is not None
                    if comment.pos.column_no < _AsmLine._BYTES_INDENT:
                        tags.append(_CommentTag(tok.pos, addr,
                                                comment.literal))
                    else:
                        tags.append(_InlineCommentTag(tok.pos, addr,
                                                      comment.literal))
                else:
                    assert subject_tag.comment is None
                    subject_tag.comment = comment

            yield from tags

    def parse(self) -> list[_Tag]:
        tags = []
        addr = 0
        for tag in self:
            if tag.addr is None:
                tag.addr = addr
            else:
                addr = tag.addr

            tags.append(tag)
            addr += tag.size

        return tags
