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
state — both register banks, IX, IY, PC, SP, I, R, WZ, the
flags and the interrupt and exchange flip-flops — so an
instruction with a side effect anywhere in that state cannot
pass.

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

The netlist itself is not part of this repository. It comes
from the Visual 6502 and Z80 Explorer projects — the exact
repositories and commits are pinned at the top of
`z80sim.py` — as three files, `nodenames.js`, `segdefs.js`
and `transdefs.js`, to be placed in this directory.

Then, from inside this directory:

```shell
./z80sim.py
```

This tests everything, in all orders. Computed states are
cached in `__z80sim_cache/`, so a rerun only computes what no
earlier run has.

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
which one is in use), IX and IY, PC, SP, I and R, the
interrupt and exchange flip-flops, the internal instruction
register and the `~halt` pin. Claims do not extend to the
rest of the circuit's internal nodes.

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
  checks the bus at every half-tick of the covered
  instructions' machine cycles.

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

- The Z80's obscure 'special reset' is real, and the netlist
  gives it an exact shape. The `~reset` pin is sampled in the
  second half of every tick, but the sampling instant in a
  fetch's tick 0 is unlike the others: a reset pulse caught
  at that instant and no other forces PC to zero and changes
  nothing else — the interrupt
  flip-flops, the interrupt mode, I, WZ and all the registers
  keep their values, and the refresh counter R keeps counting
  without a break, showing that no general reset took place.
  This is the behaviour debug hardware exploited to seize a
  running CPU without wiping its context, researched on real
  chips by Tony Brewer; the netlist confirms it and sharpens
  it: a pulse caught at any later sampling instant — even a
  pulse only one half-tick wide — performs the full reset, and
  a pulse that covers no sampling instant does nothing at
  all. All three outcomes are deterministic: no order of gate
  reactions changes them.

  Mapped by `./z80sim.py --probe-special-reset` and certified
  by `test_special_reset()` of a plain `./z80sim.py` run.

- A DD/FD prefix makes the prefixed instruction atomic
  against both kinds of interrupt. A pending maskable
  interrupt is not accepted between the prefix and its opcode
  — the fetch after the prefix is an ordinary opcode fetch,
  proven down to the acknowledge strobe staying quiet — and,
  contrary to a widely repeated story, a non-maskable
  interrupt is not accepted there either: an `~nmi` edge
  caught during the prefix fetch waits until the whole
  prefixed instruction has completed, and the return address
  pushed — verified byte by byte on the data bus during the
  push writes — points past both bytes. Nothing is lost: the
  handler returns to the instruction after the prefixed one,
  which executed exactly as prefixed. The folklore that an
  NMI striking between prefix and opcode abandons the prefix,
  so the return executes the opcode unprefixed, does not
  happen on this netlist. Both behaviours are deterministic:
  no order of gate reactions changes them.

  The deferral compounds: each prefix postpones the
  acceptance by one more instruction, so an `~nmi` caught
  during the first of three chained prefixes is accepted only
  after the first non-prefix instruction completes, and an
  unbroken stream of prefix bytes holds the non-maskable
  interrupt off indefinitely — its one loophole.

  Covered by `test_int_at_prefix_gap()`,
  `test_nmi_at_prefix_gap()` and `test_nmi_at_prefix_chain()`
  of a plain `./z80sim.py` run.

- The undocumented `out (c), 0` instruction (ED 71) drives
  zero onto the data bus on this netlist, as expected of an
  NMOS Z80 — the CMOS parts are known to drive 0xFF instead,
  making this one of the few points where the chip families
  genuinely differ. The byte driven during the port write is
  checked for every register field, including the exchange
  flip-flops' routing for the named registers; the sibling
  ED 70, `in (c)`, sets the flags and stores nowhere, which
  the complete-state checks establish without any special
  arrangement.

  Covered by the `out (c), r` and `in r, (c)` sequences of a
  plain `./z80sim.py` run, whose register fields are fully
  symbolic and include field 6.

- The maskable interrupt pin is sampled at exactly one
  instant per instruction — the second half of tick 2 of a
  four-tick fetch — and a pulse one half-tick wide
  landing on that instant is accepted, while an equal pulse
  anywhere else in the instruction is ignored entirely, the
  machine running on as if nothing had happened. The
  time-honoured rule that `~int` must be held active until
  acknowledged is board-level prudence about not knowing the
  chip's phase; the chip itself needs the level at one
  half-tick and nothing more.

  Mapped by the pulse sweep and certified by
  `test_int_sampling_instant()` of a plain `./z80sim.py` run.

- The undocumented interrupt mode written by `im` encodings
  ED 4E and ED 6E — often called 'IM 0/1' — is a real,
  distinct state of the mode latch: the latch stores the
  instruction's mode field verbatim, so this state is not the
  same latch contents as plain IM 0. In acceptance it behaves
  exactly as mode 0: the acceptance tests condition on the
  latch's high bit alone, so their passing certifies the two
  states accept identically.

  Covered by the `im/xim n` sequence and
  `test_int_im0_from_halt()` of a plain `./z80sim.py` run.

- The XF and YF flags after `scf`/`ccf` are not merely
  undocumented but not fully implemented: the circuit does not
  produce a definite value for them. Each of the two bits,
  independently, ends up as one of A, F, A | F or A & F — the
  last not predicted by any known model of the flags — and
  which one wins is a race. Which chip you have and how warm
  it is shift the odds. See
  <https://github.com/kosarev/z80/issues/51>.

  What the instruction before the `scf`/`ccf` does is not a
  matter of odds, however: it decides whether the race exists
  at all. For the pairings covered so far:

  - After an instruction that writes the accumulator together
    with the flags — `adc` and `add` with any operand, `and`
    with a register operand, `or` with an immediate one — the
    race disappears entirely: XF and YF copy A's bits, and no
    order of gate reactions can produce anything else. A DD
    or FD prefix on the `scf`/`ccf` makes no difference here:
    the bits copy A either way.

  - After `call cc, nn` or `ld r, n`, which write neither,
    the four-value race stands exactly as described above.

  - `cp n`, which writes the flags but leaves the
    accumulator, gives the same result as the first group —
    XF and YF come out as A's bits, nothing else possible —
    but only when the `scf`/`ccf` follows immediately. If the
    `scf`/`ccf` carries a DD or FD prefix, so that the prefix
    fetch sits between the two instructions, the four-value
    race is back in full.

  So the deciding factor is not flag writing as such but what
  the last pass through the ALU left behind: where it wrote
  the accumulator, that fresh value forces the outcome.
  Community research by Patrik Rak established the rule that
  the previous instruction's flag writing selects the
  behaviour, modelled since as a hidden 'Q register'; the die
  has no such register — the closed-world sweep finds no
  state cell holding it — and the mechanism is timing: the
  flags of the preceding instruction retire later than its
  other results, and whether their write is still in flight
  when `scf`/`ccf` samples is what the 'Q' rule was
  describing. Where the write is fresh, the outcome is
  forced; where there is none, the circuit's indecision shows
  and the chip families are free to differ.

  Covered by the paired sequences — `adc n; scf/ccf` and
  kin — of a plain `./z80sim.py` run, each asserting the
  exact outcome its preceding instruction produces: the
  single forced value where the accumulator was freshly
  written, the four-value set otherwise.

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
  flags, IX and IY, and the exchange flip-flops, keep their
  values through a reset taken from the halted state.

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

## Related tickets

- [#42](https://github.com/kosarev/z80/issues/42) —
  supporting the `scf`/`ccf` flag behaviour in the emulator;
  the pairing results above give the rules the emulator is to
  follow.
- [#43](https://github.com/kosarev/z80/issues/43) — passing
  Patrik Rak's test suite, which exercises that same
  behaviour on real chips.
- [#50](https://github.com/kosarev/z80/issues/50) — the
  symbolic simulation work this tool implements.
- [#51](https://github.com/kosarev/z80/issues/51) — the
  `scf`/`ccf` race itself, including the open hardware
  experiment for the A & F outcome.
- [#54](https://github.com/kosarev/z80/issues/54) — a
  planned simulator change: transistors holding their states
  rather than nodes.
