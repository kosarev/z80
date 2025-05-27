#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2025 Ivan Kosarev.
#   mail@ivankosarev.com
#
#   Published under the MIT license.

import typing


class Error(Exception):
    def __init__(self, reason: str) -> None:
        super().__init__(reason)
        self.reason = reason

    def verbalize(self, program_name: str | None = None) -> str:
        def g() -> typing.Generator[str]:
            if program_name is not None:
                yield '%s: ' % program_name

            yield self.reason

        return ''.join(g())
