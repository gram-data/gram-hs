# Data Model: Ord Instance for Pattern

**Feature**: 009-ord-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

This document describes the `Ord` instance for the `Pattern` type, which enables ordering, sorting, and use in ordered data structures like `Data.Set` and `Data.Map`.

## Core Entity

### Pattern Type

The `Pattern` type is a recursive data structure representing decorated sequences:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Existing Properties**:
- Has `Eq` instance for equality comparison
- Has `Show` instance for display
- Has `Functor`, `Foldable`, and `Traversable` instances
- Recursive structure enables arbitrary nesting

### Ord Instance

**Type Signature**:
```haskell
instance Ord v => Ord (Pattern v) where
  compare :: Pattern v -> Pattern v -> Ordering
```

**Ordering Semantics**:
- **Lexicographic ordering**: Compare value first, then elements recursively
- **Structure-preserving**: Based on `toTuple()` semantics: `(value, elements)`
- **Consistent with Eq**: Patterns that are equal according to `Eq` compare as `EQ`
- **Total ordering**: Provides transitive, antisymmetric, reflexive ordering

**Implementation**:
```haskell
instance Ord v => Ord (Pattern v) where
  compare (Pattern v1 els1) (Pattern v2 els2) =
    compare v1 v2 <> compare els1 els2
```

This is equivalent to:
```haskell
compare p1 p2 = compare (toTuple p1) (toTuple p2)
```

## Ordering Rules

### Rule 1: Value-First Comparison

If two patterns have different values, ordering is determined by value comparison:

```haskell
Pattern { value = "a", elements = [...] } < Pattern { value = "b", elements = [...] }
```

### Rule 2: Elements Comparison (Recursive)

If two patterns have the same value, ordering is determined by comparing elements recursively:

```haskell
-- Compare elements list lexicographically
Pattern { value = "root", elements = [p1, p2] } < Pattern { value = "root", elements = [p1, p2, p3] }
```

### Rule 3: Nested Structure Comparison

When comparing elements, nested patterns are compared recursively using the same rules:

```haskell
-- Compare nested patterns recursively
Pattern { value = "a", elements = [Pattern { value = "b", elements = [] }] }
vs
Pattern { value = "a", elements = [Pattern { value = "c", elements = [] }] }
-- Compares "b" vs "c" at nested level
```

### Rule 4: Consistency with Equality

Patterns that are equal according to `Eq` must compare as `EQ`:

```haskell
p1 == p2  ==>  compare p1 p2 == EQ
```

## Edge Cases

### Atomic Patterns (No Elements)

Atomic patterns are compared by value only, then empty elements list:

```haskell
Pattern { value = "a", elements = [] } < Pattern { value = "b", elements = [] }
```

### Different Element Counts

Patterns with fewer elements come before patterns with more elements (when values are equal):

```haskell
Pattern { value = "root", elements = [p1] } < Pattern { value = "root", elements = [p1, p2] }
```

### Same Flattened Values, Different Structures

Patterns with the same flattened values but different structures are distinguished:

```haskell
-- These have same flattened values ["a", "b"] but different structures
Pattern { value = "a", elements = [Pattern { value = "b", elements = [] }] }
vs
Pattern { value = "a", elements = [] }, Pattern { value = "b", elements = [] }  -- Not a valid pattern, but illustrates the point
-- First has nested structure, second would be flat (if it were valid)
-- Ordering distinguishes them based on structure
```

### Type Constraint

The `Ord` instance requires `Ord v` constraint:

```haskell
-- This compiles (String has Ord instance)
compare (pattern "a") (pattern "b") :: Ordering

-- This doesn't compile if CustomType doesn't have Ord instance
compare (pattern customValue1) (pattern customValue2) :: Ordering
-- Error: No instance for (Ord CustomType)
```

## Validation Rules

### Total Ordering Properties

The `Ord` instance must satisfy:

1. **Transitivity**: If `p1 < p2` and `p2 < p3`, then `p1 < p3`
2. **Antisymmetry**: If `p1 < p2`, then `p2 > p1` (and vice versa)
3. **Reflexivity**: `p1 <= p1` always true

### Consistency with Eq

The `Ord` instance must be consistent with `Eq`:

- If `p1 == p2`, then `compare p1 p2 == EQ`
- If `p1 /= p2`, then `compare p1 p2 /= EQ`

### Lexicographic Ordering

The ordering must follow lexicographic rules:

- Compare value first using `Ord v`
- If values are equal, compare elements recursively using `Ord [Pattern v]`
- Elements are compared lexicographically (first element, then second, etc.)

## Relationships

### Relationship to Eq Instance

The `Ord` instance uses the same comparison order as `Eq`:
- Both compare value first, then elements
- This ensures consistency automatically

### Relationship to toTuple Function

The ordering semantics are based on `toTuple()`:

```haskell
toTuple (Pattern v els) = (v, els)
compare p1 p2 = compare (toTuple p1) (toTuple p2)
```

This preserves structure and distinguishes patterns with different structures.

### Relationship to Standard Library

The `Ord` instance enables:
- `Data.Set (Pattern v)`: Patterns as set elements
- `Data.Map (Pattern v) a`: Patterns as map keys
- `sort :: [Pattern v] -> [Pattern v]`: Sorting pattern lists
- `minimum`, `maximum`: Finding extremal patterns

## State Transitions

N/A - `Ord` instance is a pure function with no state.

## Notes

- The `Ord` instance is automatically derived from the lexicographic ordering of `(value, elements)`
- No additional state or caching is needed
- The implementation is straightforward and follows standard Haskell conventions
- Performance is O(n) where n is the number of nodes in the pattern structure (worst case when comparing equal patterns)

