#   Z80 CPU Emulator.
#   https://github.com/kosarev/z80
#
#   Copyright (C) 2017-2019 Ivan Kosarev.
#   ivan@kosarev.info
#
#   Published under the MIT license.


class Error(Exception):
    def __init__(self, reason):
        super().__init__(reason)
        self.reason = reason

    def verbalize(self, program_name=None):
        def g():
            if program_name is not None:
                yield '%s: ' % program_name

            yield self.reason

        return ''.join(g())
