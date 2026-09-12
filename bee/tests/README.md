# Modeller compatibility with Bee 0.7.9

The reference is `bee/source/bee_modeller.pas` from commit
`5c2c183d1801b67ee3c43485025299eea39ba748` (1 December 2007), whose header
identifies Bee 0.7.9 build 0485. It predates the 2026 changes.

The current modeller retains the original tree, update order, arithmetic,
parameter mapping (including `/8`), local names, comments and CRLF layout.
The remaining changes serve specific purposes:

- Delphi mode and pointer arithmetic allow Free Pascal to compile the original syntax.
- Packed nodes avoid Win64 alignment padding; pointers remain native-width.
- Pointer-sized list moves fix the original Win32-only copy on Win64.
- Splitting preparation, learning and byte completion exposes the same PPM
  operations to the standalone coder and mixer without duplicating the model.
- `Bee_Model` adapts the existing text-table API and range coder to `TBaseCoder`.

No alternative tree, 32-bit indexes, d10, new probability formula or parameter
optimization is included. The unused frequency-injection method was removed.

## Reproduce

Run from PowerShell with FPC on PATH:

```powershell
./bee/tests/modeller_compat.ps1 -InputFile ./build/dickens/dickens -Table ./build/m02-ppm-20260912/fixed.table -Dictionary 9
./bee/tests/modeller_compat.ps1 -InputFile ./build/tree32-20260912/random.bin -Table ./build/m02-ppm-20260912/fixed.table -Dictionary 0 -Architecture i386
```

These input paths refer to local experiment data, not committed fixtures.
Any existing file and compatible 43-byte text table may be supplied;
omitting `-Table` selects the default table. Each invocation uses a fresh
build directory and reads the historical reference directly from Git.

The reference receives only a unit rename, compiler directives and the
pointer-width copy correction. The test compares entire compressed streams
from the reference, current `TBaseCoder` and adapter, and decodes each stream.
It does not claim binary archive compatibility for the complete Bee application.

Validation on 12 September 2026 used a clean HEAD source snapshot with only
`bee_modeller.pas` and `bee_model.pas` replaced by the proposed files:

- Win64: Dickens, 10,192,446 bytes, d9, PPM table: 2,219,499 bytes, all equal.
- Win64 and Win32: deterministic random data, 600,000 bytes, d0: 614,368 bytes,
  all equal; this corpus exercises dictionary pruning and node reuse.
- Win64 default table: all 256 byte values and empty input, all equal.

The checks cover these inputs and fresh models; they are not an exhaustive
validation of solid archives or all table configurations.
