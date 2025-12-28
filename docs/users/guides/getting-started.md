# Getting Started with Gram HS

This guide will help you get started using the gram-hs library in your Haskell projects.

## Installation

### Prerequisites

- **GHC**: 9.10.3 (specified in `cabal.project`)
- **Cabal**: 3.6.2.0 or later

### Installing Prerequisites

**Using ghcup (recommended)**:
```bash
# Install GHC 9.10.3
ghcup install ghc 9.10.3
ghcup set ghc 9.10.3

# Install Cabal (if needed)
ghcup install cabal
ghcup set cabal
```

**Verify installation**:
```bash
ghc --version    # Should show 9.10.3
cabal --version  # Should show 3.6.2.0 or later
```

### Adding to Your Project

Add the pattern library to your `cabal.project` or `package.yaml`:

```yaml
build-depends:
  - pattern
```

Or if using the mono-repo directly:

```yaml
build-depends:
  - pattern
  - subject  # If using Subject types
  - gram      # If using Gram serialization
```

## Quick Start

### Basic Pattern Creation

```haskell
import Pattern.Core (Pattern(..), pattern, patternWith)

-- Create an atomic pattern (no elements)
atomA :: Pattern String
atomA = pattern "A"

-- Create a pattern with elements
atomB = pattern "B"
relationship = patternWith "knows" [atomA, atomB]

-- Inspect pattern structure
main = do
  putStrLn $ "Pattern value: " ++ value atomA
  putStrLn $ "Elements count: " ++ show (length (elements relationship))
```

### Working with Patterns

```haskell
import Pattern.Core (Pattern(..), length, depth, size)

-- Create nested patterns
root = patternWith "root" 
  [ patternWith "level1" [pattern "level2"]
  , pattern "other"
  ]

-- Query pattern properties
main = do
  putStrLn $ "Length: " ++ show (length root)      -- Number of direct elements
  putStrLn $ "Size: " ++ show (size root)          -- Total number of patterns
  putStrLn $ "Depth: " ++ show (depth root)        -- Maximum nesting depth
```

## Next Steps

- **[Pattern Construction](pattern-construction.md)** - Learn how to create and manipulate patterns
- **[Graph Lens](graph-lens.md)** - Use graph lens for graph operations (if implemented)
- **[Gram Serialization](gram-serialization.md)** - Serialize and parse gram notation
- **[Examples](../examples/)** - Browse working code examples

## Common Patterns

### Creating a Simple Graph

```haskell
import Pattern.Core (pattern, patternWith)

-- Create nodes
alice = pattern "alice"
bob = pattern "bob"

-- Create relationship
knows = patternWith "knows" [alice, bob]

-- Create graph container
graph = patternWith "graph" [alice, bob, knows]
```

### Working with Nested Structures

```haskell
-- Create nested pattern
tree = patternWith "tree"
  [ patternWith "branch1" [pattern "leaf1", pattern "leaf2"]
  , patternWith "branch2" [pattern "leaf3"]
  ]
```

## Getting Help

- **API Documentation**: Generate with `cabal haddock`
- **Examples**: See `examples/` directory
- **Reference Documentation**: For porting or advanced topics, see [Reference Documentation](../../reference/README.md)

