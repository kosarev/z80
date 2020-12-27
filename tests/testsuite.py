# -*- coding: utf-8 -*-

import os
import z80
import unittest


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

            split = tuple(text.split('---', maxsplit=1))
            if len(split) == 2:
                input, expected_output = split
            else:
                split = tuple(text.split('===', maxsplit=1))
                input, expected_error = split

            os.chdir(os.path.dirname(self.__path))

            error = ''
            try:
                d.parse_tags(self.__id, input)
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
