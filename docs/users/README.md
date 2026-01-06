# Gram HS - User Documentation

**Audience**: Haskell developers using the gram-hs library

Welcome! This documentation helps you use the gram-hs library in your projects.

## Quick Start

### Installation

**Prerequisites**:
- GHC 9.10.3
- Cabal 3.6.2.0 or later

**Using ghcup (recommended)**:
```bash
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3
ghcup install cabal
ghcup set cabal
```

**Add to your project**:
```yaml
build-depends:
  - pattern
  # Optional:
  - subject  # For Subject types
  - gram      # For Gram serialization
```

### Basic Usage

```haskell
import Pattern.Core (pattern, patternWith)

-- Create an atomic pattern
atomA = pattern "A"

-- Create a pattern with elements
atomB = pattern "B"
relationship = patternWith "knows" [atomA, atomB]

-- Inspect pattern structure
main = do
  putStrLn $ "Pattern value: " ++ value atomA
  putStrLn $ "Elements count: " ++ show (length (elements relationship))
```

## Documentation

- **[Getting Started](guides/getting-started.md)** - Installation and basic usage
- **[Pattern Construction](guides/pattern-construction.md)** - Creating and working with patterns
- **[Graph Lens](guides/graph-lens.md)** - Using graph lens for graph operations
- **[Gram Serialization](guides/gram-serialization.md)** - Serializing and parsing gram notation

## Examples

- **[Basic Patterns](examples/basic-patterns.md)** - Creating and querying patterns
- **[Graph Operations](examples/graph-operations.md)** - Graph lens examples

## API Reference

Generate API documentation with:
```bash
cabal haddock
```

## For Language Porters

If you're porting this library to another language, see **[Reference Documentation](../reference/PORTING-GUIDE.md)** instead.
