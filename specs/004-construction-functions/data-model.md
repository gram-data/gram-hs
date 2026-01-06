# Data Model: Construction Functions

**Feature**: 004-construction-functions  
**Date**: 2025-01-28

## Overview

This feature adds three constructor functions to simplify pattern creation:
- `point` - creates atomic patterns (patterns with no elements)
- `pattern` - creates patterns with elements (1, 2, or more elements)
- `fromList` - creates patterns from a list of values (convenience function)

These functions provide a more convenient API than the verbose record syntax while maintaining full functional equivalence.

## Core Functions

### point

**Purpose**: Create an atomic pattern (pattern with no elements) from a single value.

**Type Signature**: `point :: v -> Pattern v`

**Behavior**: 
- Takes a value of any type `v`
- Returns a `Pattern v` with that value and an empty elements list
- Functionally equivalent to `Pattern { value = x, elements = [] }`

**Example**:
```haskell
-- Create atomic pattern with string value
atom1 :: Pattern String
atom1 = point "atom1"

-- Equivalent to:
atom1' :: Pattern String
atom1' = Pattern { value = "atom1", elements = [] }
```

### pattern

**Purpose**: Create a pattern with elements from a value and a list of pattern elements.

**Type Signature**: `pattern :: v -> [Pattern v] -> Pattern v`

**Behavior**:
- Takes a value of any type `v` and a list of `Pattern v` elements
- Returns a `Pattern v` with that value and those elements
- Preserves the order of elements in the input list
- Handles empty lists (produces atomic pattern, equivalent to `point`)
- Functionally equivalent to `Pattern { value = x, elements = ps }`

**Example**:
```haskell
-- Create singular pattern (one element)
singular :: Pattern String
singular = pattern "soccer" [point "a team sport involving kicking a ball"]

-- Create pair pattern (two elements)
pair :: Pattern String
pair = pattern "knows" [point "Alice", point "Bob"]

-- Create extended pattern (many elements)
extended :: Pattern String
extended = pattern "graph" 
  [ point "elem1"
  , point "elem2"
  , point "elem3"
  ]

-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = pattern "empty" []  -- Equivalent to point "empty"
```

## Functional Equivalence

Both constructor functions produce patterns that are functionally identical to patterns created with record syntax:

```haskell
-- These are equivalent:
p1 = point "test"
p2 = Pattern { value = "test", elements = [] }
-- p1 == p2 is True

-- These are equivalent:
p3 = pattern "soccer" [point "a team sport involving kicking a ball"]
p4 = Pattern { value = "soccer", elements = [Pattern { value = "a team sport involving kicking a ball", elements = [] }] }
-- p3 == p4 is True
```

## Type Safety

Both functions preserve type safety:
- `point :: v -> Pattern v` - preserves the type parameter `v`
- `pattern :: v -> [Pattern v] -> Pattern v` - ensures all patterns share the same type `v`
- Type system enforces that all patterns in a structure share the same value type

## Element Order Preservation

The `pattern` function preserves the order of elements in the input list:

```haskell
-- Order matters
p1 = pattern "seq" [point "a", point "b", point "c"]
p2 = pattern "seq" [point "c", point "b", point "a"]
-- p1 /= p2 (order is different)
```

## Edge Cases

### Empty List in pattern

When `pattern` receives an empty list, it produces an atomic pattern:

```haskell
-- These are equivalent:
p1 = pattern "test" []
p2 = point "test"
-- p1 == p2 is True
```

### Nested Patterns

Both functions work correctly with nested patterns:

```haskell
-- Nested construction
nested :: Pattern String
nested = pattern "outer" 
  [ pattern "middle" 
      [ pattern "inner" 
          [ point "innermost" ]
      ]
  ]
```

### All Value Types

Both functions work with any value type:

```haskell
-- String values
strPattern :: Pattern String
strPattern = point "text"

-- Integer values
intPattern :: Pattern Int
intPattern = point 42

-- Custom types
data Person = Person { name :: String, age :: Maybe Int }
personPattern :: Pattern Person
personPattern = point (Person "Alice" (Just 30))
```

## Relationship to Pattern Data Type

These constructor functions are convenience wrappers around the Pattern data type:

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

- `point v` = `Pattern { value = v, elements = [] }`
- `pattern v ps` = `Pattern { value = v, elements = ps }`

The functions don't change the underlying data structure; they simply provide a more convenient API.

### fromList

**Purpose**: Create a pattern from a list of values by converting each value to an atomic pattern.

**Type Signature**: `fromList :: v -> [v] -> Pattern v`

**Behavior**:
- Takes a decoration value of type `v` and a list of values `[v]`
- Converts each value in the list to an atomic pattern using `point`
- Creates a pattern with those atomic patterns as elements using `pattern`
- Preserves the order of values in the input list
- Handles empty lists (produces atomic pattern, equivalent to `point`)
- Functionally equivalent to: `fromList decoration values = pattern decoration (map point values)`

**Example**:
```haskell
-- Convert list of strings to pattern
p :: Pattern String
p = fromList "graph" ["Alice", "Bob", "Charlie"]

-- Equivalent to:
p' :: Pattern String
p' = pattern "graph" [point "Alice", point "Bob", point "Charlie"]

-- Empty list produces atomic pattern
atomic :: Pattern String
atomic = fromList "empty" []  -- Equivalent to point "empty"
```

**Relationship to Other Functions**:
- `fromList decoration values` = `pattern decoration (map point values)`
- More convenient than manually mapping `point` over a list
- Useful when you have raw data (list of values) rather than existing patterns

## Implementation Status

- ✅ Design complete
- ⏳ Implementation planned
- ⏳ Tests planned
- ⏳ Documentation planned

