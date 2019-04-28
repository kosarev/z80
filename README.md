# z80
Fast and flexible i8080/Z80 emulator.

[![Build Status](https://travis-ci.org/kosarev/z80.svg?branch=master)](https://travis-ci.org/kosarev/z80)


## Quick facts

* Implements accurate machine cycle-level emulation.

* Supports undocumented flags and instructions.

* Passes the well-known `cputest`, `8080pre`, `8080exer`,
  `8080exm`, `prelim` and `zexall` tests.

* Follows a modular event-driven design for flexible interfacing.

* Employs compile-time polymorphism for zero performance
  overhead.

* Cache-friendly implementation without large code switches and
  data tables.

* Offers default modules for the breakpoints support and generic
  memory.

* Supports multiple independently customized emulator instances.

* Written in strict C++11.

* Does not rely on implementation-defined or unspecified
  behavior.

* Single-header implementation.

* Provides a generic Python API and instruments to create custom
  bindings.


## Feedback

Any notes on overall design, improving performance and testing
approaches are highly appreciated. Please use the email given at
<https://github.com/kosarev>. Thanks!
