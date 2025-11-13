# Quickstart: Ord Instance for Pattern

**Feature**: 009-ord-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

The `Ord` instance for `Pattern` enables ordering, sorting, and use in ordered data structures. Patterns are ordered using lexicographic ordering: value first, then elements recursively.

## Basic Usage

### Comparing Patterns

```haskell
import Pattern.Core

-- Compare atomic patterns
let p1 = pattern "a"
let p2 = pattern "b"
compare p1 p2  -- LT (because "a" < "b")

-- Compare patterns with elements
let p3 = patternWith "root" [pattern "a", pattern "b"]
let p4 = patternWith "root" [pattern "a", pattern "c"]
compare p3 p4  -- LT (because "b" < "c" in second element)

-- Compare patterns with different values
let p5 = patternWith "a" [pattern "x"]
let p6 = patternWith "b" [pattern "x"]
compare p5 p6  -- LT (because "a" < "b", value comparison first)
```

### Using Comparison Operators

```haskell
-- Less than
pattern "a" < pattern "b"  -- True

-- Less than or equal
pattern "a" <= pattern "a"  -- True

-- Greater than
pattern "b" > pattern "a"  -- True

-- Greater than or equal
pattern "b" >= pattern "b"  -- True
```

### Finding Extremal Patterns

```haskell
-- Minimum pattern
min (pattern "b") (pattern "a")  -- pattern "a"

-- Maximum pattern
max (pattern "a") (pattern "b")  -- pattern "b"

-- With multiple patterns
minimum [pattern "c", pattern "a", pattern "b"]  -- pattern "a"
maximum [pattern "c", pattern "a", pattern "b"]  -- pattern "c"
```

## Standard Library Integration

### Data.Set

```haskell
import qualified Data.Set as Set

-- Create a set of patterns
let patterns = [pattern "c", pattern "a", pattern "b", pattern "a"]
let patternSet = Set.fromList patterns
-- Set maintains sorted order: {pattern "a", pattern "b", pattern "c"}
-- Duplicate (pattern "a") is removed (by Eq)

-- Check membership
Set.member (pattern "b") patternSet  -- True
Set.member (pattern "x") patternSet  -- False

-- Insert pattern
let newSet = Set.insert (pattern "d") patternSet
-- New set: {pattern "a", pattern "b", pattern "c", pattern "d"}
```

### Data.Map

```haskell
import qualified Data.Map as Map

-- Create a map with patterns as keys
let patternMap = Map.fromList 
  [ (pattern "a", 1)
  , (pattern "b", 2)
  , (pattern "c", 3)
  ]

-- Lookup by pattern
Map.lookup (pattern "b") patternMap  -- Just 2
Map.lookup (pattern "x") patternMap  -- Nothing

-- Insert pattern key
let newMap = Map.insert (pattern "d") 4 patternMap
-- New map: {pattern "a" -> 1, pattern "b" -> 2, pattern "c" -> 3, pattern "d" -> 4}
```

### Sorting

```haskell
import Data.List

-- Sort list of patterns
let unsorted = [pattern "c", pattern "a", pattern "b"]
let sorted = sort unsorted
-- Result: [pattern "a", pattern "b", pattern "c"]

-- Sort patterns with elements
let patterns = 
  [ patternWith "root" [pattern "b"]
  , patternWith "root" [pattern "a"]
  , patternWith "root" [pattern "c"]
  ]
let sorted = sort patterns
-- Result: [patternWith "root" [pattern "a"], patternWith "root" [pattern "b"], patternWith "root" [pattern "c"]]
```

## Advanced Examples

### Nested Pattern Ordering

```haskell
-- Patterns with nested structures are compared recursively
let p1 = patternWith "root" 
  [ patternWith "inner" [pattern "a"]
  ]
let p2 = patternWith "root" 
  [ patternWith "inner" [pattern "b"]
  ]
compare p1 p2  -- LT (compares "a" vs "b" at nested level)
```

### Patterns with Different Structures

```haskell
-- Patterns with same flattened values but different structures are distinguished
let p1 = patternWith "a" [pattern "b"]
let p2 = patternWith "a" [pattern "b", pattern "c"]
compare p1 p2  -- LT (p1 has fewer elements, comes first when values are equal)
```

### Consistency with Equality

```haskell
-- Patterns that are equal (by Eq) compare as equal
let p1 = pattern "a"
let p2 = pattern "a"
p1 == p2  -- True
compare p1 p2  -- EQ

-- Patterns that are not equal don't compare as equal
let p3 = pattern "a"
let p4 = pattern "b"
p3 == p4  -- False
compare p3 p4  -- LT (not EQ)
```

## Type Constraints

The `Ord` instance requires that the value type has an `Ord` instance:

```haskell
-- This works (String has Ord instance)
compare (pattern "a") (pattern "b") :: Ordering

-- This works (Int has Ord instance)
compare (pattern 1) (pattern 2) :: Ordering

-- This doesn't compile if CustomType doesn't have Ord instance
data CustomType = CustomType String
compare (pattern (CustomType "a")) (pattern (CustomType "b"))
-- Error: No instance for (Ord CustomType)
```

## Common Patterns

### Organizing Patterns by Type

```haskell
-- Group patterns by their value (decoration type)
let patterns = 
  [ patternWith "node" [pattern "A"]
  , patternWith "edge" [pattern "B"]
  , patternWith "node" [pattern "C"]
  ]
let sorted = sort patterns
-- Patterns with "edge" come before "node" (alphabetically)
-- Result: [patternWith "edge" [pattern "B"], patternWith "node" [pattern "A"], patternWith "node" [pattern "C"]]
```

### Building Pattern Indexes

```haskell
-- Create an index mapping patterns to metadata
import qualified Data.Map as Map

let patternIndex = Map.fromList
  [ (patternWith "user" [pattern "alice"], "Alice User")
  , (patternWith "user" [pattern "bob"], "Bob User")
  , (patternWith "group" [pattern "admins"], "Admin Group")
  ]

-- Lookup by pattern
Map.lookup (patternWith "user" [pattern "alice"]) patternIndex
-- Just "Alice User"
```

### Finding Unique Patterns

```haskell
import qualified Data.Set as Set

-- Remove duplicate patterns (by Eq) from a list
let patterns = 
  [ pattern "a"
  , pattern "b"
  , pattern "a"  -- duplicate
  , pattern "c"
  ]
let unique = Set.toList (Set.fromList patterns)
-- Result: [pattern "a", pattern "b", pattern "c"] (sorted, duplicates removed)
```

## Notes

- Ordering is based on structure (value + elements), not flattened values
- Patterns with different structures but same flattened values are distinguished
- The `Ord` instance is consistent with the `Eq` instance
- Ordering follows lexicographic rules: value first, then elements recursively
- Performance is O(n) where n is the number of nodes in the pattern structure

