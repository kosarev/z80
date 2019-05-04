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
The following section reveals what are those instructions and
where the emulator get them from.


## Adding memory

Every time the CPU emulator needs to access memory, it calls
`on_read()` and `on_write()` methods.
Their default implementations do not really access any memory;
`on_read()` simply returns `0x00`, meaning the emulator in the
example above actually executes a series of `NOP`s, and
`on_write()` does literally nothing.

Since both the reading and writing functions are considered by
the `z80::z80_cpu` class to be handlers, which we know because
they have the `on` preposition in their names, we can use the
same technique as with `on_set_pc()` above to override the
default handlers to actually read and write something.

```c++
class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    ...

    fast_u8 on_read(fast_u16 addr) {
        assert(addr < z80::address_space_size);
        fast_u8 n = memory[addr];
        std::printf("read 0x%02x at 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(addr));
        return n;
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        assert(addr < z80::address_space_size);
        std::printf("write 0x%02x at 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(addr));
        memory[addr] = static_cast<least_u8>(n);
    }

private:
    least_u8 memory[z80::address_space_size] = {
        0x21, 0x34, 0x12,  // ld hl, 0x1234
        0x3e, 0x07,        // ld a, 7
        0x77,              // ld (hl), a
    };
};
```
[adding_memory.cpp](https://github.com/kosarev/z80/blob/master/examples/adding_memory.cpp)

Output:
```
read 0x21 at 0x0000
pc = 0x0001
read 0x34 at 0x0001
read 0x12 at 0x0002
pc = 0x0003
read 0x3e at 0x0003
pc = 0x0004
read 0x07 at 0x0004
pc = 0x0005
read 0x77 at 0x0005
pc = 0x0006
write 0x07 at 0x1234
```


## Feedback

Any notes on overall design, improving performance and testing
approaches are highly appreciated. Please use the email given at
<https://github.com/kosarev>. Thanks!
