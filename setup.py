#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import distutils.sysconfig, os
# from distutils.core import Extension
from setuptools import Extension, setup

# Work around the problem with the warning about '-Wstrict-prototypes'.
# https://bugs.python.org/issue1222585
config_vars = distutils.sysconfig.get_config_vars()
opt_to_remove = '-Wstrict-prototypes'
for var in ['OPT']:
    if var in config_vars:
        opts = config_vars[var].split()
        if opt_to_remove in opts:
            opts.remove(opt_to_remove)
    config_vars[var] = ' '.join(opts)


z80_emulator_module = Extension(
    name='z80._z80',
    extra_compile_args=['-std=c++11', '-Wall', '-fno-exceptions', '-fno-rtti',
                        '-O3',
                        '-UNDEBUG',  # TODO
                       ],
    sources=['z80.cpp', 'z80/_z80module.cpp'],
    language='c++')


# TODO: Update the URL once we have a published documentation.
# TODO: Do we have a name for the emulator?
setup(name='z80',
      version='0.1a',
      description='Z80 Simulator',
      # TODO: long_description=...
      author='Ivan Kosarev',
      author_email='ivan@kosarev.info',
      url='https://github.com/kosarev/z80/',
      ext_modules=[z80_emulator_module],
      packages=['z80'],
      install_requires=[],
      classifiers=[
          'Development Status :: 1 - Planning',
          'Intended Audience :: Developers',
          'Intended Audience :: Education',
          'License :: OSI Approved :: MIT License',
          'Operating System :: OS Independent',
          'Programming Language :: C++',
          'Programming Language :: Python :: 3',
          'Programming Language :: Python :: Implementation :: CPython',
          'Topic :: Software Development',
          'Topic :: Software Development :: Libraries',
          'Topic :: System :: Emulators',
      ],
      # TODO: license=...
      # TODO: Respect other parameters.
      )
