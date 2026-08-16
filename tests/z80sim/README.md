# z80sim

This simulates the actual Z80 chip, transistor by transistor,
using the netlist recovered from die photographs (Visual6502 /
Z80Explorer data). Its purpose is to establish what the
hardware really does and to check the emulator against it.

It is a symbolic simulator: instead of concrete 0s and 1s,
node and transistor states are represented as symbolic
expressions. A register
bit is set to a named term rather than to a value, and
everything computed from it is an expression over such terms.
So a single run proves how an instruction behaves for all
register contents and operands at once, and whole families of
instructions are tested together. The results are checked
against expected expressions written independently of the
simulation.

There is one more source of freedom the simulator covers: the
order in which the gates of the circuit react to a change. On
the real chip this comes down to signal timing, and for a few
things the Z80's circuit leaves the result undetermined: it
depends on which gate happens to react first. By default the
simulator tests every instruction under *all* possible orders
at once. A pass then means every node ends up as its expected
value under every order — or, for the racing nodes, that the
possible outcomes are exactly the expected set.

## Findings

- The XF and YF flags after `scf`/`ccf` are not merely
  undocumented but not fully implemented: the circuit does not
  produce a definite value for them. Each of the two bits,
  independently, ends up as one of A, F, A | F or A & F — the
  last not predicted by any known model of the flags — and
  which one wins is a race. Which chip you have, how warm it is
  and what instruction ran before merely shift the odds. See
  <https://github.com/kosarev/z80/issues/51>.

## Dependencies

- `eqbool` 0.5 or later — the library the symbolic expressions
  are built on; it also answers whether two expressions mean
  the same thing and whether a combination of order constraints
  is possible.
- `z3-solver` — used only to pretty-print reported
  expressions.

## Running

Run from inside this directory. Computed states are cached in
`__z80sim_cache/`, so a rerun only computes what no earlier run
has.

```shell
./z80sim.py                  # test everything, in all orders
./z80sim.py --new-only       # only what has no recorded pass
./z80sim.py --single-order   # one fixed update order only
./z80sim.py --threads=8      # run tests in parallel
./z80sim.py --seed=568       # shuffle the update order
./z80sim.py --all-orders=<instr>   # report an instruction's
                                   # racing nodes and their
                                   # outcomes
```

When a check fails, the report lists the values the node can
end up with and the order conditions under which each one
arises.
