# Bee FPC

This tree is the FPC/Lazarus continuation of Bee 0.7.9. Delphi projects,
Delphi-only duplicates and batch build scripts have been removed.

## Projects

- `bee/source/bee.lpi`: compatible Bee 0.7.9 archiver.
- `bee/source/beemix.lpi`: experimental four-expert 1/2/4/8-bit mixer.
- `beeopt/BeeOpt.lpi`: command-line table optimizer; it has no LCL dependency.

The historical four-bit model remains in `bee_modeller.pas`. The new model is
implemented in `bee_model.pas`; four concrete expert classes are generated from
one generic type. `bee_mixer.pas` combines their bit probabilities, three
internal-order taps per model and a match expert using an adaptive integer
mixer and integer APM. Integer arithmetic keeps i386 and x86-64 streams
identical.

## Optimize a table

```text
beeopt --bits=1 --input=sample.bin --output=table-1.tbl
beeopt --bits=2 --input=sample.bin --output=table-2.tbl
beeopt --bits=4 --input=sample.bin --output=table-4.tbl
beeopt --bits=8 --input=sample.bin --output=table-8.tbl
```

BeeOpt retains the original genetic search: one population per table level,
one survivor per population, 15% crossover probability, 1% mutation
probability and a complete population recalculation every 2000 ages. All four
models use the historical 15 levels because context-list depth is independent
of symbol alphabet size. The dictionary is not a command-line
choice; it follows the historical `d=(age div 2000)+1` rule and is capped at
`d9` by the model. Optimization runs continuously and saves its resumable
`.dat` state plus the current strongest table after every creature.
`--turns=n` provides a bounded run for testing. The input may be one file or a
directory of sample files. After each complete population cycle BeeOpt reports
age, current population, dictionary, improvements, compressed size and its
percentage of the original size.

An alternative simulated-annealing search starts from the default table:

```text
beeopt --method=annealing --bits=2 --input=sample.bin \
  --state=sample-2bit-annealing.dat --output=sample-2bit-annealing.tbl
```

It keeps separate current and best tables, cools after every evaluation,
occasionally accepts worse candidates, reheats after 500 evaluations without
a new best, and applies the same dictionary schedule (`d1` for evaluations
0..1999, then `d2`, up to `d9`). Its PRNG state, temperatures and both tables
are saved, so a resumed run continues the same annealing chain. The genetic
method remains the default. Annealing reports progress whenever it finds a new
global best, and also at the end of a run bounded with `--turns`. Each candidate
changes one table byte; step sizes 1, 2, 4, 8, 16 and 32 have probabilities
60%, 25%, 10%, 3%, 1.5% and 0.5%, respectively. Model-level changes move only
to an adjacent valid level.

The table size follows the model alphabet:

```text
bits  symbols  table bytes
1       2          57
2       4          37
4      16          43
8     256         262
```

The first byte is a context-list level in the range 1..15 and is independent
of the symbol alphabet. Consequently, every genetic optimization uses 15
level populations, including the 1-bit and 2-bit models. The remaining bytes
are alphabet-dependent because their variable portion is indexed by the
number of alternatives that can actually occur.

The formula is `1 + (8 / bits) * (2^bits + 5)`: one byte selects the model
level, then every symbol position in a byte has its own column. A column holds
one parameter per possible alternative count plus four control parameters.
Tables are optimized independently and every stored parameter is active.

## Exercise the mixer

```text
beemix c input.bin output.bmx --dictionary=3 \
  --table1=table-1.tbl --table2=table-2.tbl \
  --table4=table-4.tbl --table8=table-8.tbl
beemix d output.bmx restored.bin
```

The `.bmx` test format embeds the original size, dictionary level and all four
tables. It is intentionally separate from the historical `.bee` format until
the new format is finalized.

## Compatibility test

`tests/model_compat.lpr` encodes the same input with the historical four-bit
model and the new generated four-bit expert, then requires byte-identical range
coder output.
