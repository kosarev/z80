# -*- coding: utf-8 -*-

import os
import z80
import unittest


class TestInstrBuilder(unittest.TestCase):
    def __str__(self):
        return 'Z80IntrsBuilder'

    def runTest(self):
        TESTS = (
            (b'\xcb\x30', 'sll b'),
            (b'\xdd\x24', 'inc ixh'),
            (b'\xdd\x2c', 'inc ixl'),
            (b'\xfd\x24', 'inc iyh'),
            (b'\xfd\x2c', 'inc iyl'),
            (b'\xdd\xe5', 'push ix'),
            (b'\xfd\xe1', 'pop iy'),
            (b'\xdd\xe8', 'ret pe'),
            (b'\xed\x00', 'xnop 0xed00'),
            (b'\xed\x45', 'retn'),
            (b'\xed\x55', 'xretn 0xed55'),
            (b'\xed\x4d', 'reti'),
            (b'\xed\x4c', 'xneg 0xed4c'),
            (b'\xed\x4e', 'xim 0xed4e, 0x0'),
            (b'\xed\x4f', 'ld r, a'),
            (b'\xed\x50', 'in d, (c)'),
            (b'\xed\x63\x00\x00', 'xld 0xed63, (0x0), hl'),
            (b'\xed\xa3', 'outi'),
            (b'\xed\xb3', 'otir'),
            (b'\xed\xab', 'outd'),
            (b'\xed\xbb', 'otdr'),
            (b'\xed\x67', 'rrd'),
            (b'\xed\xa0', 'ldi'),
            (b'\xed\xa1', 'cpi'),
            (b'\xed\xb1', 'cpir'),
            (b'\xed\xb9', 'cpdr'),
            (b'\xed\xa9', 'cpd'),
            (b'\xed\xa2', 'ini'),
            (b'\xed\xb2', 'inir'),
            (b'\xed\xaa', 'ind'),
            (b'\xed\xba', 'indr'),
            (b'\xed\xa8', 'ldd'),
            )

        builder = z80.Z80InstrBuilder()
        for code, text in TESTS:
            instr = builder.build_instr(0, code)
            if isinstance(instr, z80.UnknownInstr):
                self.fail(f'unknown instruction: {instr.text}')

            self.assertEqual(str(instr), text)


class DisasmTestCase(unittest.TestCase):
    maxDiff = None

    def __init__(self, id, path):
        super().__init__()
        self.__id = id
        self.__path = path

    def __str__(self):
        return self.__id

    def runTest(self):
        d = z80._Disasm()

        old_wd = os.getcwd()
        try:
            with open(self.__path) as f:
                text = f.read()

            expected_output = ''
            expected_error = ''

            split = tuple(text.split('===', maxsplit=1))
            if len(split) == 2:
                input, expected_error = split
            else:
                split = tuple(text.split('---', maxsplit=1))
                if len(split) == 2:
                    input, expected_output = split
                else:
                    input, = split
                    expected_output = '\n' + input

            os.chdir(os.path.dirname(self.__path))

            error = ''
            try:
                source = z80._SourceFile(self.__id, input)
                d.add_tags(*z80._DisasmTagParser(source).parse())
                d.disassemble()

                output = ''.join(d._get_output())
                self.assertEqual(expected_output, output)
            except z80.Error as e:
                error = '\n%s\n' % e.verbalize()

            self.assertEqual(expected_error, error)
        finally:
            os.chdir(old_wd)


def suite():
    suite = unittest.TestSuite()

    suite.addTest(TestInstrBuilder())

    suite_dir = os.path.dirname(__file__)
    disasm_tests_dir = 'disasm'

    for filename in os.listdir(os.path.join(suite_dir, disasm_tests_dir)):
        if filename.endswith('.asm'):
            short_path = os.path.join(disasm_tests_dir, filename)
            full_path = os.path.join(suite_dir, short_path)
            suite.addTest(DisasmTestCase(short_path, full_path))

    return suite


if __name__ == '__main__':
    unittest.TextTestRunner().run(suite())
