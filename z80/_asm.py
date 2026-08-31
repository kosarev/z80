
#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2026 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import re
import typing

from ._code import Code, LabelDef
from ._error import Error
from ._source import _SourceFile
from ._token import _Token, _Tokeniser


# An error about a source construct.
class AsmError(Error):
    def __init__(self, subject: _Token, message: str) -> None:
        if subject.literal in (None, '\n'):
            # There is nothing to name at positions past the last
            # token of the line.
            super().__init__(f'{subject.pos.inline_text}: {message}')
        else:
            super().__init__(
                f'{subject.pos.inline_text}: {subject}: {message}')
        self.subject = subject
        self.message = message

    def verbalize(self, program_name: str | None = None) -> str:
        def g() -> typing.Generator[str, None, None]:
            yield self.subject.pos.context_text

            if program_name is not None:
                yield f'{program_name}: '

            yield self.reason

        return ''.join(g())


# The source-level assembler: a driver of Code's semantic actions,
# translating source constructs into calls on the Code it builds.
class Asm:
    __NAME = re.compile(r'[_a-zA-Z][_0-9a-zA-Z]*$')

    __TOKENS = re.compile(
        r'([_0-9a-zA-Z]+)|'   # A number or identifier.
        r'\n|'                # An end of line.
        r'.')                 # Or any other single character.

    __toks: _Tokeniser
    __code: Code

    def __fetch_token(self) -> _Token:
        self.__toks.skip_whitespace()
        self.__toks.start_token()
        self.__toks.skip(self.__TOKENS)
        tok = self.__toks.end_token()

        if tok.literal == ';':
            # A comment runs to the end of the line.
            self.__toks.skip_rest_of_line()
            self.__toks.start_token()
            self.__toks.skip(self.__TOKENS)
            tok = self.__toks.end_token()

        return tok

    # Fetches a subsequent token and checks it is the expected one.
    def __skip_token(self, literal: str,
                     error: str | None = None) -> None:
        tok = self.__fetch_token()
        if tok != literal:
            if error is None:
                error = f'{literal!r} expected.'

            raise AsmError(tok, error)

    def __parse_end_of_line(self) -> None:
        tok = self.__fetch_token()
        if tok.literal not in (None, '\n'):
            raise AsmError(tok, 'End of line expected.')

    # Fetches a subsequent token and evaluates it as a number.
    def __parse_number(self) -> int:
        tok = self.__fetch_token()
        if tok.literal is not None:
            try:
                return int(tok.literal, base=0)
            except ValueError:
                pass

        raise AsmError(tok, 'A number expected.')

    def __parse_org(self) -> None:
        self.__code.start_block(self.__parse_number())
        self.__parse_end_of_line()

    def __parse_line(self, tok: _Token) -> None:
        lit = tok.literal
        assert lit is not None

        if lit == 'org':
            self.__parse_org()
            return

        # Not a directive or instruction, so a symbol definition.
        if self.__NAME.match(lit) is None:
            raise AsmError(tok, 'Invalid construct.')

        self.__skip_token(':', "':' expected after a symbol name.")
        self.__code.add(LabelDef(self.__code.get_symbol(lit)))

    def assemble(self, source_file: _SourceFile) -> Code:
        self.__toks = _Tokeniser(source_file)
        self.__code = Code()
        while True:
            tok = self.__fetch_token()
            if tok.literal is None:
                break

            if tok.literal != '\n':
                self.__parse_line(tok)

        return self.__code
