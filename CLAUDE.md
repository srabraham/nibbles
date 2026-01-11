# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Nibbles is a code golf language that is purely functional, statically typed, and fully lazy. It uses Polish (prefix) notation and compiles to Haskell. Each instruction is half a byte (a "nibble") in the binary form.

**Note:** This project is no longer actively maintained but accepts pull requests.

## Build Commands

### Prerequisites

Install required Haskell packages:
```bash
cabal install --lib bytestring containers dlist filepath mtl murmur-hash process split template-haskell haskell-src-meta
```

For GHC 9+, see `docs/haskell9.md` for additional memoize package setup.

### Build the compiler
```bash
ghc nibbles.hs
```

### Run a Nibbles program
```bash
./nibbles program.nbl           # Literate form (.nbl)
./nibbles program.nbb           # Binary form (.nbb)
echo "code" | ./nibbles         # From stdin
./nibbles -hs program.nbl       # Only generate out.hs (don't run)
./nibbles -c program.nbl        # Compile to binary (.nbb)
./nibbles -e program.nbb        # Expand binary to literate form
./nibbles -i program.nbl        # Interpreter mode (experimental, no GHC compilation)
```

### Run tests
```bash
ruby test/testall.rb                          # Full test suite
ghc -package ghc test/test.hs && test/test    # Unit tests only
ruby test/integration_test.rb                 # Single integration test
```

## Architecture

### Compilation Pipeline

1. **nibbles.hs** - Main entry point. Handles CLI args, reads input (.nbl or .nbb files), calls compiler, generates `out.hs`, and runs it via GHC.

2. **Parse.hs** - Lexer/parser for both literate (.nbl) and binary (.nbb) forms. Handles conversion between the two representations.

3. **Compile.hs** - Core compilation logic. Takes parsed code and produces Haskell implementation (`Impl`), binary representation, and literate output.

4. **Ops.hs** - Defines all ~86 language operations. Each op has a binary code, literate syntax, and Haskell implementation. Operations are defined using DSL patterns with type inference.

5. **Header.hs** - Runtime library embedded in generated Haskell code. Contains helper functions (list ops, string handling, etc.) used by compiled programs.

### Key Data Types (in Types.hs, Expr.hs)

- `Code` - Represents input as either `Lit` (literate) or `Nib` (binary nibbles)
- `Impl` - Compiled implementation with Haskell code, type info, and dependency tracking
- `DualImpl` - Extended Impl that supports both compilation and interpretation modes
- `VT` - Value types (VInt, VChr, VList, VNil, etc.)

### Interpreter Infrastructure (experimental)

- **Value.hs** - Runtime value type `RValue` for interpreter mode
- **RuntimeOps.hs** - RValue versions of all operations from Header.hs
- **Interpret.hs** - Interpreter core with context management
- **OpsTH.hs** - Template Haskell utilities for dual-mode operation definitions

The `~>>` operator in OpsHelper.hs creates dual definitions that work for both compilation and interpretation.

### Implicit Parameters

The codebase uses `?isSimple` as an implicit parameter throughout to toggle between simple mode (disabled extensions) and normal mode.

### Testing

Tests are extracted from comments in `Ops.hs` using the pattern:
```haskell
-- Example (size N): input -> output
-- Test: input -> output
```

Additional tests are in `test/AdditionalTests.hs` and `test/TutorialTests.hs`.
