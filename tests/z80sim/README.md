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
./z80sim.py --probe-acceptance-latch   # print the instruction
                                       # register through halted
                                       # fetches and the ~nmi
                                       # acceptance, half-tick
                                       # by half-tick
```

When a check fails, the report lists the values the node can
end up with and the order conditions under which each one
arises.

## Findings

Each finding names the run it is derived from and covered by,
so it can be reproduced and traced to the checks in the code.

Where a finding says that a state is preserved or that nothing
else changes, the state in question is the sampled one: the
whole register file including the shadow registers, PC, SP, I
and R, the flags of both banks, the interrupt and exchange
flip-flops, the internal instruction register and the `~halt`
pin. Claims do not extend to the rest of the circuit's
internal nodes.

- The XF and YF flags after `scf`/`ccf` are not merely
  undocumented but not fully implemented: the circuit does not
  produce a definite value for them. Each of the two bits,
  independently, ends up as one of A, F, A | F or A & F — the
  last not predicted by any known model of the flags — and
  which one wins is a race. Which chip you have, how warm it is
  and what instruction ran before merely shift the odds. See
  <https://github.com/kosarev/z80/issues/51>.

  Covered by the `scf/ccf` sequence of a plain `./z80sim.py`
  run, which asserts the exact outcome set via `check_set()`;
  `./z80sim.py --all-orders=scf/ccf` reports the racing nodes,
  their outcomes and the order conditions each arises under.

- The undocumented flag behaviour of the block instructions
  (`ldi`/`ldd`, `cpi`/`cpd`, `ini`/`ind`, `outi`/`outd` and
  their repeating forms) is deterministic: the circuit
  produces the same values no matter which of its gates react
  first — there is no race like the `scf`/`ccf` one above.
  Testing real chips could not have settled this: a test
  program can record the values its chip produces, but it
  cannot show that no race exists behind them.

  Covered by the block instruction sequences (`ldi/ldd`,
  `ldir/lddr`, `cpi/cpd`, `cpir/cpdr`, `ini/ind`, `inir/indr`,
  `outi/outd`, `otir/otdr`) of a plain `./z80sim.py` run:
  their flag expectations in `test_node()` are single values,
  and a pass means no node has any other possible outcome
  under any order of gate reactions.

- A halted Z80 is not stopped. That it keeps running fetch
  cycles, so that memory refresh continues, is well known; the
  simulation shows the fetches are complete ones: each byte
  read from the bus is loaded into the internal instruction
  register, early in the fetch cycle, just as in normal
  execution — only the execution of what was fetched is
  suppressed. Being invisible to programs, this was out of
  reach of test programs on real chips. Also proven: whatever
  byte those fetches return and however long the CPU stays
  halted, nothing changes but the refresh register.

  Covered by the `halt` sequence of a plain `./z80sim.py` run:
  `test_halted_dynamics()` feeds a halted fetch a symbolic
  byte and compares the complete sampled state of consecutive
  halted cycles. The load timing is shown by
  `./z80sim.py --probe-acceptance-latch`.

- The fetch cycle that starts the acceptance of an interrupt —
  non-maskable, or maskable in mode 1 — reads its byte from
  the bus like any other fetch, and the byte lands in the
  internal instruction register on the same schedule, where it
  then sits untouched through the whole acceptance sequence.
  It is only never executed. This, again, is invisible to
  programs and so beyond what testing real chips could show.

  Derived with `./z80sim.py --probe-acceptance-latch`, which
  follows the register half-tick by half-tick from a halted
  fetch through the ~nmi acceptance and its pushes; the
  acceptance tests of a plain `./z80sim.py` run
  (`test_nmi_from_halt()`, `test_int_im1_from_halt()`) assert
  the states the acceptance leaves behind.
