# Pattern Construction

This guide shows you how to create and work with patterns in gram-hs.

## Creating Atomic Patterns

An atomic pattern is a pattern with no elements. It's the fundamental building block:

```haskell
import Pattern.Core (Pattern(..), pattern)

-- Using the constructor function (recommended)
atomA :: Pattern String
atomA = pattern "A"

-- Using record syntax
atomB :: Pattern String
atomB = Pattern { value = "B", elements = [] }

-- With different value types
numberAtom :: Pattern Int
numberAtom = pattern 42

boolAtom :: Pattern Bool
boolAtom = pattern True
```

## Creating Patterns with Elements

Patterns can contain other patterns as elements:

```haskell
import Pattern.Core (pattern, patternWith)

-- Create elements first
alice = pattern "alice"
bob = pattern "bob"

-- Create pattern with elements
relationship = patternWith "knows" [alice, bob]

-- Multiple elements
group = patternWith "group" [alice, bob, pattern "charlie"]
```

## Nested Patterns

Patterns can be nested to arbitrary depth:

```haskell
-- Two levels
nested = patternWith "root"
  [ patternWith "level1" [pattern "level2"]
  ]

-- Three levels
deep = patternWith "root"
  [ patternWith "level1"
    [ patternWith "level2" [pattern "level3"]
    ]
  ]
```

## Accessing Pattern Data

Use field accessors to inspect patterns:

```haskell
import Pattern.Core (Pattern(..))

-- Get the value
getValue :: Pattern String -> String
getValue p = value p

-- Get the elements
getElements :: Pattern String -> [Pattern String]
getElements p = elements p

-- Check if atomic
isAtomic :: Pattern v -> Bool
isAtomic p = null (elements p)
```

## Query Functions

The library provides query functions for pattern analysis:

```haskell
import Pattern.Core (length, size, depth, values)

myPattern = patternWith "root"
  [ patternWith "a" [pattern "x", pattern "y"]
  , pattern "b"
  ]

-- Number of direct elements
length myPattern  -- 2

-- Total number of patterns (including nested)
size myPattern    -- 5

-- Maximum nesting depth
depth myPattern   -- 2

-- All values in the pattern
values myPattern  -- ["root", "a", "x", "y", "b"]
```

## Common Patterns

### Creating Relationships

```haskell
-- Two-element pattern represents a relationship
source = pattern "source"
target = pattern "target"
edge = patternWith "connects" [source, target]
```

### Creating Sequences

```haskell
-- Pattern with ordered elements
sequence = patternWith "sequence"
  [ pattern "first"
  , pattern "second"
  , pattern "third"
  ]
```

### Building Hierarchies

```haskell
-- Tree-like structure
tree = patternWith "root"
  [ patternWith "branch1" [pattern "leaf1", pattern "leaf2"]
  , patternWith "branch2" [pattern "leaf3"]
  ]
```

## Type Safety

All patterns in a structure must share the same value type:

```haskell
-- ✅ Valid: All patterns use String
valid = patternWith "root" [pattern "a", pattern "b"]

-- ❌ Invalid: Mixed types won't compile
-- invalid = patternWith "root" [pattern "a", pattern 42]
```

## See Also

- **[Getting Started](getting-started.md)** - Installation and basic usage
- **[Examples](../examples/)** - More working examples
- **[API Reference](../api/)** - Complete API documentation

