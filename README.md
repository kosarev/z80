# z80
Fast and flexible Z80/i8080 emulator.

[![Build Status](https://travis-ci.org/kosarev/z80.svg?branch=master)](https://travis-ci.org/kosarev/z80)


## Quick facts

* Implements accurate machine cycle-level emulation.

* Supports undocumented instructions, flags and registers.

* Passes the well-known `cputest`, `8080pre`, `8080exer`,
  `8080exm`, `prelim` and `zexall` tests.

* Follows a modular event-driven design for flexible interfacing.

* Employs compile-time polymorphism for zero performance
  overhead.

* Cache-friendly implementation without large code switches and
  data tables.

* Offers default modules for the breakpoint support and generic
  memory.

* Supports multiple independently customized emulator instances.

* Written in strict C++11.

* Does not rely on implementation-defined or unspecified
  behavior.

* Single-header implementation.

* Provides a generic Python 3 API and instruments to create
  custom bindings.

* MIT license.


## Contents

* [Hello world](#hello-world)
* [Adding memory](#adding-memory)
* [Input and output](#input-and-output)
* [Accessing processor's state](#accessing-processors-state)
* [Modules](#modules)
* [The root module](#the-root-module)
* [State modules](#state-modules)
* [Feedback](#feedback)


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

Building:
```shell
$ git clone git@github.com:kosarev/z80.git
$ cmake z80
$ make
$ make test
$ make hello  # Or 'make examples' to build all examples at once.
```

Running:
```
$ ./examples/hello
pc = 0x0000
pc = 0x0001
pc = 0x0002
```

In this example we derive our custom emulator class,
`my_emulator`, from a
[mix-in](https://en.wikipedia.org/wiki/Mixin) that implements the
logic and default interfaces necessary to emulate the Zilog Z80
processor.
As you may guess, replacing `z80_cpu` with `i8080_cpu` would give
us a similar Intel 8080 emulator.

The `on_set_pc()` method overrides its default counterpart to
print the current value of the `PC` register before changing it.
For this compile-time polymorphism to be able to do its job, we
pass the type of the custom emulator to the processor mix-in as a
parameter.

The `main()` function creates an instance of the emulator and
asks it to execute a few instructions, thus triggering the custom
version of `on_set_pc()`.
The following section reveals what are those instructions and
where the emulator gets them from.


## Adding memory

Every time the CPU emulator needs to access memory, it calls
`on_read()` and `on_write()` methods.
Their default implementations do not really access any memory;
`on_read()` simply returns `0x00`, meaning the emulator in the
example above actually executes a series of `nop`s, and
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


## Input and output

Aside of memory, another major way the processors use to
communicate with the outside world is via input and output ports.
If you read the previous sections, it's now easy to guess that
there is a couple of handlers that do that.
These are `on_input()` and `on_output()`.

Note that the handlers have different types of parameters that
store the port address, because i8080 only supports 256 ports
while Z80 extends that number to 64K.

```c++
    // i8080_cpu
    fast_u8 on_input(fast_u8 port)
    void on_output(fast_u8 port, fast_u8 n)

    // z80_cpu
    fast_u8 on_input(fast_u16 port)
    void on_output(fast_u16 port, fast_u8 n)
```

The example:
```c++
class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    ...

    fast_u8 on_input(fast_u16 port) {
        fast_u8 n = 0xfe;
        std::printf("input 0x%02x from 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(port));
        return n;
    }

    void on_output(fast_u16 port, fast_u8 n) {
        std::printf("output 0x%02x to 0x%04x\n", static_cast<unsigned>(n),
                    static_cast<unsigned>(port));
    }

private:
    least_u8 memory[z80::address_space_size] = {
        0xdb,        // in a, (0xfe)
        0xee, 0x07,  // xor 7
        0xd3,        // out (0xfe), a
    };
};
```
[input_and_output.cpp](https://github.com/kosarev/z80/blob/master/examples/input_and_output.cpp)


## Accessing processor's state

Sometimes it's necessary to examine and/or alter the current
state of the CPU emulator and do that in a way that is
transparent to the custom code in overridden handlers.
For this purpose the default state interface implemented in the
`i8080_state<>` and `z80_state<>` classes provdes a number of
getters and setters for registers, register pairs, interrupt
flip-flops and other fields constituting the internal state of
the emulator.
By convention, calling such functions does not fire up any
handlers. The example below demonstrates a typical usage.

Note that there are no such accessors for memory as it is
external to the processor emulators and they themselves have to
use handlers, namely, the `on_read()` and `on_write()` ones, to
deal with memory.

```c++
class my_emulator : public z80::z80_cpu<my_emulator> {
public:
    ...

    void on_step() {
        std::printf("hl = %04x\n", static_cast<unsigned>(get_hl()));
        base::on_step();

        // Start over on every new instruction.
        set_pc(0x0000);
    }
```
[accessing_state.cpp](https://github.com/kosarev/z80/blob/master/examples/accessing_state.cpp)


## Modules

By overriding handlers we can extend and otherwise alter the
default behavior of CPU emulators.
That's good, but what do we do if it's not enough?
For example, what if the default representation of the
processor's internal state doesn't fit the needs of your
application?
Say, you might be forced to follow a particular order of
registers or you just want to control the way they are packed in
a structure because there's some external binary API to be
compatible with.
Or, what if you don't need to emulate the whole processor's
logic, and just want to check if a given sequence of bytes forms
a specific instruction?

That's where modules come into play.
To understand what they are and how to use them, let's take a
look at the definitions of the emulator classes and see what's
under the hood.

```c++
template<typename D>
class i8080_cpu : public i8080_executor<i8080_decoder<i8080_state<root<D>>>>
{};

template<typename D>
class z80_cpu : public z80_executor<z80_decoder<z80_state<root<D>>>>
{};
```

Each of these classes is no more than a stack of a few other
mix-ins.
The `root<>` template provides helpers that make it possible to
call handlers of the most derived class in the heirarchy, `D`,
which is why it takes that class as its type parameter.
It also contains dummy implementations of the standard handlers,
such as `on_output()`, so you don't have to define them when you
don't need them.

`i8080_state<>` and `z80_state<>` have been mentioned in the
previous section as classes that define transparent accessors to
the processor state, e.g., `set_hl()`.
They also define corresponding handlers, like `on_set_hl()`, that
other modules use to inspect and modify the state.

`i8080_decoder<>` and `z80_decoder<>` modules analyze op-codes
and fire up handlers for specific instructions, e.g, `on_halt()`.

Finally, the job of `i8080_executor<>` and `z80_executor<>` is to
implement handlers like `on_halt()` to actually execute
corresponding instructions.

The convention is that modules shall communicate with each other
only via handlers.
Indeed, if they would call the transparent accessors or refer to
data fields directly, then those accessors wouldn't be
transparent anymore and handlers would never be called.
This also means that modules are free to define transparent
accessors in a way that seems best for their purpose or even not
define them at all.

All and any of the standard modules can be used and customized
independently of each other.
Moreover, all and any of the modules can be replaced with custom
implementations.
New modules can be developed and used separately or together with
the standard ones.
In all cases the only requirement is to implement handlers other
modules rely on.


## The root module

```c++
template<typename D>
class root {
public:
    typedef D derived;

    ...

    fast_u8 on_read(fast_u16 addr) {
        unused(addr);
        return 0x00;
    }

    void on_write(fast_u16 addr, fast_u8 n) {
        unused(addr, n);
    }

    ...

protected:
    const derived &self() const{ return static_cast<const derived&>(*this); }
    derived &self() { return static_cast<derived&>(*this); }
};
```

The main function of the root module is to define the `self()`
method that other modules can use to call handlers. For example,
a decoder could do `self().on_ret()` whenever it runs into a
`ret` instruction.

Aside of that, the module contains dummy implementations of the
standard handlers that do nothing or, if they have to return
something, return some default values.


## State modules

```c++
template<typename B>
class i8080_state : public internals::cpu_state_base<B> {
public:
    ...

    bool get_iff() const { ... }
    void set_iff(bool f) { ... }

    ...
};

template<typename B>
class z80_state : public internals::cpu_state_base<z80_decoder_state<B>> {
public:
    ...

    void exx_regs() { ... }
    void on_exx_regs() { exx_regs(); }

    ...
};
```

The purpose of state modules is to provide handlers to access the
internal state of the emulated CPU.
They also usually store the fields of the state, thus defining
its layout in memory.

Regardless of the way the fields are represented and stored, the
default getting and setting handlers for register pairs use
access handlers for the corresponding 8-bit registers to obtain
or set the 16-bit values.
Furthermore, the low half of the register pair is always
retrieved and set before the high half.
This means that by default handlers for 8-bit registers are
getting called even if originally a value of a register pair they
are part of has been queried.
Custom implementations of processor states, however, are not
required to do so.

```c++
    fast_u16 on_get_bc() {
        // Always get the low byte first.
        fast_u8 l = self().on_get_c();
        fast_u8 h = self().on_get_b();
        return make16(h, l);

    void on_set_bc(fast_u16 n) {
        // Always set the low byte first.
        self().on_set_c(get_low8(n));
        self().on_set_b(get_high8(n));
    }
```

Aside of the usual getters and setters for the registers and
flip-flops, both the i8080 and Z80 states have to provide an
`on_ex_de_hl_regs()` handler that exchanges `hl` and `de`
registers the same way the `xchg` and `ex de, hl` do.
And the Z80 state additionally has to have an `on_exx_regs()`
that swaps register pairs just as the `exx` instruction does.
The default swapping handlers do their work by accessing
registers directly, without relying on the getting and setting
handlers, similarly to how silicon implementations of the
processors toggle internal flip-flops demux'ing access to
register cells without actually transferring their values.

Because the CPUs have a lot of similarities, processor-specific
variants of modules usually share some common code in helper base
classes that in turn are defined in the `internal` class.
That class defines entities that are internal to the
implementation of the library.
The client code is therefore supposed to be written as if the
module classes are derived directly from their type parameters,
`B`.

Note that `z80_state` has an additional mix-in in its inheritance
chain, `z80_decoder_state<>`, whereas `i8080_state` is derived
directly from the generic base.
This is because Z80 decoders are generally not stateless objects;
they have to track which of the `IX`, `IY` or `HL` registers has
to be used as the index register for the current instruction.
The decoder state class stores and provides access to that
information.

```c++
template<typename B>
class z80_decoder_state : public B {
public:
    ...

    iregp get_iregp_kind() const { ... }
    void set_iregp_kind(iregp r) { ... }

    iregp on_get_iregp_kind() const { return get_iregp_kind(); }
    void on_set_iregp_kind(iregp r) { set_iregp_kind(r); }

    ...
};
```

In its simplest form, a custom state module can be a structure defining the
necessary state fields together with corresponding access handlers.

```c++
template<typename B>
struct my_state : public B {
    fast_u16 pc;

    ...

    fast_u16 on_get_pc() const { return pc; }
    void on_set_pc(fast_u16 n) { pc = n; }

    ...

    // These always have to be explicitly defined.
    void on_ex_de_hl_regs() {}
    void on_ex_af_alt_af_regs() {}
    void on_exx_regs() {}
};

```
[custom_state.cpp](https://github.com/kosarev/z80/blob/master/examples/custom_state.cpp)


## Feedback

Any notes on overall design, improving performance and testing
approaches are highly appreciated.
Please file an issue or use the email given at
<https://github.com/kosarev>.
Thanks!
