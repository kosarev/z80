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
simulation, and every check covers the complete architectural
state — both register banks, PC, SP, I, R, WZ, the flags and
the interrupt and exchange flip-flops — so an instruction
with a side effect anywhere in that state cannot pass.

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
./z80sim.py --probe-bus-templates --single-order
                                   # print the address bus and
                                   # the control pins through
                                   # representative machine
                                   # cycles, half-tick by
                                   # half-tick
```

When a check fails, the report lists the values the node can
end up with and the order conditions under which each one
arises.

## Findings

Each finding names the run it is derived from and covered by,
so it can be reproduced and traced to the checks in the code.

Where a finding says that a state is preserved or that nothing
else changes, the state in question is the sampled one: both
banks of the register file and of the flags (the alternate
bank is what programmers know as BC', DE', HL' and AF'; on
the die it is simply a second bank, with flip-flops selecting
which one is in use), PC, SP, I and R, the interrupt and
exchange flip-flops, the internal instruction register and
the `~halt` pin. Claims do not extend to the rest of the circuit's
internal nodes.

- During an instruction's internal cycles — the five ticks of
  `add hl, <rp>`, for instance — the address bus carries the
  refresh address I:R with all control lines idle, and it
  keeps carrying it through the extra ticks of extended fetch
  cycles. Together with the refresh phases of every fetch,
  this means the value of the I register chooses the addresses
  hardware sees on the bus for much of the time between
  transfers — one more reason why moving I into a contended
  page upsets a ZX Spectrum. Also proven along the way: at
  every sampled half-tick, the address bus and the control
  pins are free of internal races — no order of gate reactions
  can change what they carry.

  Derived with `./z80sim.py --probe-bus-templates` and covered
  by `test_bus_timeline()` of a plain `./z80sim.py` run, which
  checks the bus at every half-tick of 54 instruction
  families' machine cycles.

- In interrupt mode 2, the vector byte the interrupting
  device supplies is used as-is: the two bytes of the handler
  address are read from I:vector and I:vector + 1, with the
  vector's bit 0 respected. Documentation disagrees on whether
  the Z80 forces the vector even; on this netlist no forcing
  exists, and an odd vector simply reads a pointer that
  straddles two table entries.

  Derived with `./z80sim.py --probe-bus-templates`, which
  shows the table read addresses on the bus with the vector
  fully symbolic.

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

  The halted fetches all read the address right after the
  halt instruction, with the refresh address advancing
  normally in between — so from outside, a halted Z80 looks
  exactly like a CPU reading that one byte over and over.

  Covered by the `halt` sequence of a plain `./z80sim.py` run:
  `test_halted_dynamics()` feeds a halted fetch a symbolic
  byte and compares the complete sampled state of consecutive
  halted cycles. The load timing is shown by
  `./z80sim.py --probe-acceptance-latch` and the fetch
  addresses by `./z80sim.py --probe-bus-templates`.

- WZ — the internal pointer register often called MEMPTR —
  survives a `~reset` pulse: it keeps its value while PC, I, R
  and the interrupt flip-flops restart. Documentation commonly
  assumes reset clears it, but we are not aware of any
  experimental data on this. An experiment is possible: `bit n, (hl)` copies two bits of W into the
  XF and YF flags without changing WZ, so code placed at
  address 0 that runs this right after a reset pulse — which
  needs hardware control over the memory at the reset vector —
  would show whether a preloaded value survived. This finding
  predicts it does. Also proven
  along the way: both banks of the register file and of the
  flags, and the exchange flip-flops, keep their values
  through a reset taken from the halted state.

  Covered by `test_reset_from_halt()` of a plain `./z80sim.py`
  run.

- A halt executed while an interrupt is already pending never
  enters the halted state: the `~halt` pin stays high
  throughout, and the acceptance follows the halt
  instruction's fetch cycle directly, its return address
  resuming past the halt. This holds both for `ei; halt` with
  the interrupt active from before the `ei` — the classic
  idiom is atomic — and for an interrupt arriving during the
  halt instruction's own fetch cycle. On a machine like the ZX
  Spectrum this is easy to hit: a halt reached while the frame
  interrupt is still active simply never shows the halted
  state.

  Covered by `test_ei_halt_atomicity()` and
  `test_int_at_halt_m1()` of a plain `./z80sim.py` run, which
  assert `~halt` high at every sampled half-tick through the
  halt fetch and into the acceptance.

- When an interrupt is accepted at the boundary of `ld a, i`
  or `ld a, r` — the instructions that copy the interrupt
  flip-flop IFF2 into the PF flag — the flag reads 0: the acceptance's
  clearing of the flip-flop wins over the copy. That much
  matches what testing real NMOS chips established. What
  testing could not establish: the outcome is deterministic —
  the circuit produces the 0 no matter which of its gates
  react first — so the famous unreliability of probing
  interrupt state this way is entirely about whether an
  interrupt lands on that boundary, never about which value
  wins when it does.

  Covered by `test_int_at_ld_a_ir()` of a plain `./z80sim.py`
  run.

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
