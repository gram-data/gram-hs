# Data Model: Paramorphism for Structure-Aware Folding

**Feature**: 028-paramorphism-folding  
**Date**: 2025-01-28

## Overview

The paramorphism function for `Pattern` enables structure-aware folding where the folding function receives both the current pattern subtree and recursively computed results from children. This extends `Foldable` (value-only folding) to provide structure-aware folding, just as `Comonad` extends `Functor` to provide structure-aware transformation.

## Core Entity: Paramorphism Function

### Definition

**Paramorphism** is a recursion scheme that enables folding over recursive structures while preserving access to the original structure at each position. For `Pattern`, paramorphism provides structure-aware folding where the folding function receives:
1. The current pattern subtree (`Pattern v`)
2. The list of recursively computed results from children (`[r]`)

This enables structure-aware aggregations that consider structural properties (depth, element count, nesting level) in addition to values.

### Function Signature

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
```

**Parameters**:
- `f :: Pattern v -> [r] -> r`: Folding function that receives:
  - Current pattern subtree: `Pattern v`
  - Recursively computed results from children: `[r]`
  - Returns aggregated result: `r`
- `pattern :: Pattern v`: Pattern to fold over
- Returns: Aggregated result of type `r`

### Implementation

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Breakdown**:
- `para f (Pattern v els)`: Applies paramorphism function `f` to pattern
- `f (Pattern v els) (map (para f) els)`: Provides folding function with:
  - Current pattern subtree: `Pattern v els` (full pattern structure)
  - Recursively computed results: `map (para f) els` (results from all children)
- Returns aggregated result of type `r`

### Categorical Interpretation

From a category theory perspective, paramorphism is a recursion scheme that extends catamorphism (fold) to preserve access to the original structure. The relationship to other operations:

- **Foldable**: Provides value-only folding (`foldr`, `foldl`, `foldMap`). Only accesses values, not structure.
- **Paramorphism**: Provides structure-aware folding (`para`). Accesses both values and structure.
- **Comonad**: Provides structure-aware transformation (`extend`). Accesses structure for transformation, not folding.

The relationship: Paramorphism extends `Foldable` to see structure, just as `Comonad` extends `Functor` to see structure.

### Example: Depth-Weighted Sum

```haskell
-- Pattern structure:
-- Pattern { value = 10, elements = [
--   Pattern { value = 5, elements = [] },
--   Pattern { value = 3, elements = [] }
-- ]}

-- Depth-weighted sum using paramorphism:
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Execution:
-- para f (Pattern 10 [Pattern 5 [], Pattern 3 []])
-- = f (Pattern 10 [Pattern 5 [], Pattern 3 []]) 
--     [para f (Pattern 5 []), para f (Pattern 3 [])]
-- = f (Pattern 10 [Pattern 5 [], Pattern 3 []]) 
--     [f (Pattern 5 []) [], f (Pattern 3 []) []]
-- = f (Pattern 10 [Pattern 5 [], Pattern 3 []]) 
--     [5 * 0 + 0, 3 * 0 + 0]
-- = 10 * 1 + (5 + 3)
-- = 10 + 8
-- = 18
```

## Relationship to Other Operations

### Foldable (Value-Only Folding)

```haskell
-- Foldable: Only values, no structure access
foldr (+) 0 pattern  -- Sums all values, ignores structure
toList pattern        -- Extracts all values as flat list
```

**Use when**: You only need values, not structural information.

### Paramorphism (Structure-Aware Folding)

```haskell
-- Paramorphism: Structure + values
para (\p rs -> value p * depth p + sum rs) pattern
-- Provides access to pattern structure (depth, element count, etc.)
```

**Use when**: You need structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations).

### Comonad (Structure-Aware Transformation)

```haskell
-- Comonad: Structure-aware transformation, not folding
extend (\p -> value p * depth p) pattern
-- Transforms values based on structure, returns Pattern
```

**Use when**: You need structure-aware transformation (not aggregation).

## Structure-Aware Aggregation Patterns

### Depth-Weighted Aggregation

```haskell
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)
```

### Element-Count-Aware Aggregation

```haskell
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)
```

### Nesting-Level Statistics

```haskell
nestingLevelStats :: Pattern Int -> (Int, Int, Int)  -- (sum, count, maxDepth)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))
```

### Structure-Preserving Transformation During Fold

```haskell
structureAwareTransform :: Pattern Int -> Pattern Int
structureAwareTransform = para (\p rs -> Pattern (value p * depth p) rs)
```

## Value Processing Order

For a `Pattern v`:
1. Recursively compute results for all children using `para f`
2. Apply folding function `f` to: (1) current pattern subtree, (2) list of child results
3. Return aggregated result

This follows standard paramorphism pattern: process children first, then combine with current pattern.

## Edge Cases

### Atomic Pattern (No Elements)

```haskell
-- Pattern { value = 5, elements = [] }
para f (Pattern 5 [])
-- = f (Pattern 5 []) []
-- Folding function receives pattern and empty list of child results
```

### Pattern with Single Element

```haskell
-- Pattern { value = 10, elements = [Pattern { value = 5, elements = [] }] }
para f (Pattern 10 [Pattern 5 []])
-- = f (Pattern 10 [Pattern 5 []]) [para f (Pattern 5 [])]
-- = f (Pattern 10 [Pattern 5 []]) [f (Pattern 5 []) []]
-- Folding function receives pattern and single child result
```

### Nested Pattern

```haskell
-- Pattern { value = 1, elements = [
--   Pattern { value = 2, elements = [Pattern { value = 3, elements = [] }] }
-- ]}
para f (Pattern 1 [Pattern 2 [Pattern 3 []]])
-- Recursively processes all nesting levels
-- Folding function receives full pattern structure at each level
```

## Type Constraints

- **Value Type**: `Pattern v` where `v` can be any type
- **Result Type**: `r` can be any type (determined by folding function)
- **No additional constraints**: Paramorphism works with any value type that supports pattern structure

## Performance Characteristics

- **Time Complexity**: O(n) where n is the total number of nodes in the pattern structure
- **Space Complexity**: O(d) where d is the maximum nesting depth (for recursion stack)
- **Order Preservation**: Element order is preserved when aggregating results

