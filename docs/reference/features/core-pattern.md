# Core Pattern Feature Specification

**Status**: ✅ Implemented  
**Location**: `libs/pattern/src/Pattern/Core.hs`  
**Feature**: Core Pattern data type and basic operations

## Overview

The core Pattern type is a recursive data structure representing decorated sequences. It forms the foundation for all pattern-based operations.

## Core Type

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

### Key Properties

- **Recursive**: Elements are themselves Patterns, enabling arbitrary nesting
- **Parametric**: Value type `v` can be any type
- **Type-safe**: All patterns in a structure share the same value type
- **Sequence-based**: Elements form the pattern sequence itself

## Field Accessors

```haskell
value :: Pattern v -> v
elements :: Pattern v -> [Pattern v]
```

## Construction Functions

### `pattern` - Create Atomic Pattern

```haskell
pattern :: v -> Pattern v
pattern v = Pattern v []
```

Creates an atomic pattern (no elements).

### `patternWith` - Create Pattern with Elements

```haskell
patternWith :: v -> [Pattern v] -> Pattern v
patternWith v es = Pattern v es
```

Creates a pattern with the given value and elements.

### `fromList` - Create Pattern from Value List

```haskell
fromList :: v -> [v] -> Pattern v
fromList v vs = patternWith v (map pattern vs)
```

Creates a pattern with value `v` and elements created from the value list.

## Query Functions

### `length` - Number of Direct Elements

```haskell
length :: Pattern v -> Int
```

Returns the number of direct elements (O(1)).

### `size` - Total Number of Patterns

```haskell
size :: Pattern v -> Int
```

Returns the total number of patterns in the structure, including nested patterns (O(n)).

### `depth` - Maximum Nesting Depth

```haskell
depth :: Pattern v -> Int
```

Returns the maximum nesting depth (O(n)).

### `values` - Extract All Values

```haskell
values :: Pattern v -> [v]
```

Extracts all values from the pattern structure as a flat list (O(n)).

## Typeclass Instances

- **Show**: Manual instance for readable display
- **Eq**: Structural equality (`deriving Eq`)
- **Ord**: Lexicographic ordering
- **Functor**: Structure-preserving value transformation
- **Foldable**: Value aggregation
- **Traversable**: Effectful traversal
- **Semigroup**: Pattern combination
- **Monoid**: Identity pattern
- **Hashable**: Structure-preserving hashing
- **Applicative**: Zip-like application
- **Comonad**: Context-aware computation

See `docs/reference/features/typeclass-instances.md` for detailed specifications.

## Predicate Functions

### Value Predicates

```haskell
anyValue :: (v -> Bool) -> Pattern v -> Bool
allValues :: (v -> Bool) -> Pattern v -> Bool
```

### Structure Predicates

```haskell
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
matches :: Eq v => Pattern v -> Pattern v -> Bool
contains :: Eq v => Pattern v -> Pattern v -> Bool
```

## Comonad Functions

```haskell
extract :: Pattern v -> v
duplicate :: Pattern v -> Pattern (Pattern v)
extend :: (Pattern v -> w) -> Pattern v -> Pattern w
depthAt :: Pattern v -> Pattern Int
sizeAt :: Pattern v -> Pattern Int
indicesAt :: Pattern v -> Pattern [Int]
```

## Examples

### Atomic Pattern

```haskell
atom = pattern "atom1"
-- Pattern { value = "atom1", elements = [] }
```

### Pattern with Elements

```haskell
elem1 = pattern "elem1"
elem2 = pattern "elem2"
p = patternWith "pattern" [elem1, elem2]
-- Pattern { value = "pattern", elements = [elem1, elem2] }
```

### Nested Pattern

```haskell
level3 = pattern "level3"
level2 = patternWith "level2" [level3]
level1 = patternWith "level1" [level2]
root = patternWith "root" [level1]
```

## Test Coverage

Comprehensive test coverage in `libs/pattern/tests/Spec/Pattern/CoreSpec.hs`:
- Construction function tests
- Query function tests
- Typeclass law tests
- Predicate function tests
- Edge case tests

## Design Evolution

The core Pattern type has remained stable since Feature 1. All subsequent features build on this foundation without changing the core structure.

## See Also

- **[Typeclass Instances](typeclass-instances.md)** - Typeclass specifications
- **[Graph Lens](graph-lens.md)** - Graph interpretation
- **[Architecture](../ARCHITECTURE.md)** - Design principles

