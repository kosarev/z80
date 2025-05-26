#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import inspect
import os
import platform
from setuptools import Extension, setup


here = os.path.abspath(os.path.dirname(__file__))

with open(os.path.join(here, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()


cxx_flags = []
if platform.system() == 'Windows':
    pass
else:
    cxx_flags.extend([
        '-std=c++11', '-Wall', '-fno-exceptions', '-fno-rtti',
        '-O3',
        # '-S', '-fverbose-asm',  # TODO
    ])

z80_emulator_module = Extension(
    name='z80._z80',
    extra_compile_args=cxx_flags,
    sources=['z80/_z80module.cpp'],
    language='c++')


# TODO: Update the URL once we have a published documentation.
# TODO: Do we have a name for the emulator?
setup(name='z80',
      version='1.0b3',
      description='Fast and flexible Z80/i8080 emulator',
      long_description=long_description,
      long_description_content_type='text/markdown',
      author='Ivan Kosarev',
      author_email='mail@ivankosarev.com',
      url='https://github.com/kosarev/z80',
      license='MIT',
      ext_modules=[z80_emulator_module],
      packages=['z80'],
      install_requires=[],
      entry_points={
          'console_scripts': [
              'z80 = z80:main',
          ],
      },
      test_suite='tests.testsuite.suite',
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'Intended Audience :: Information Technology',
          'Intended Audience :: Education',
          'Operating System :: OS Independent',
          'Programming Language :: C++',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: Implementation :: CPython',
          'Topic :: Software Development',
          'Topic :: Software Development :: Libraries',
          'Topic :: System :: Emulators',
      ],
      )
