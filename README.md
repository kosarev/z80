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

* Provides a generic Python 3 API and instruments to create
  custom bindings.

* MIT license.


## Hello world

```c++
#include "z80.h"

class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    typedef z80::z80_cpu<my_emulator> base;

    my_emulator() {}

    void on_set_pc(z80::fast_u16 pc) {
        std::printf("pc = 0x%04x\n", static_cast<unsigned>(pc));
        base::on_set_pc(pc);
    }
};

int main() {
    my_emulator e;
    e.on_step();
    e.on_step();
    e.on_step();
}
```
[hello.cpp](https://github.com/kosarev/z80/blob/master/examples/hello.cpp)

Output:
```
pc = 0x0000
pc = 0x0001
pc = 0x0002
```

In this example we derive our custom emulator class,
`my_emulator`, from a
[mix-in](https://en.wikipedia.org/wiki/Mixin) that implements the
logic and default interfaces necessary to emulate the Z80
processor.

The `on_set_pc()` method overrides its default counterpart to
print the current value of the `PC` register before changing it.
For this compile-time polymorphism to be able to do its job, we
pass the type of the custom emulator to the processor mix-in as a
parameter.

The `main()` function creates an instance of the emulator and
asks it to execute a few instructions, thus triggering the custom
version of `on_set_pc()`.


## Feedback

Any notes on overall design, improving performance and testing
approaches are highly appreciated. Please use the email given at
<https://github.com/kosarev>. Thanks!
