#!/usr/bin/env python3

import z80


def main():
    m = z80.Z80Machine()

    for i in range(10):
        print(f'Step {i}: PC={m.pc}')

        # Limit runs to a single tick so each time we execute
        # exactly one instruction.
        m.ticks_to_stop = 1
        m.run()


if __name__ == "__main__":
    main()
