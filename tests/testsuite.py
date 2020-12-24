# -*- coding: utf-8 -*-

import os
import z80
import unittest


class DisasmTestCase(unittest.TestCase):
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

            input, expected_output = tuple(text.split('---', maxsplit=1))

            os.chdir(os.path.dirname(self.__path))
            d.parse_tags(self.__path, input)

            d.disassemble()

            output = ''.join(d._get_output())
            self.assertEqual(output, expected_output)
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
