# Quickstart: Paramorphism for Structure-Aware Folding

**Feature**: 028-paramorphism-folding  
**Date**: 2025-01-28

## Introduction

Paramorphism enables structure-aware folding over patterns. Unlike `Foldable` (which only provides values), paramorphism gives your folding function access to the full pattern structure at each position, enabling sophisticated aggregations that consider depth, element count, and nesting level.

## Basic Usage

### Import Required Modules

```haskell
import Pattern.Core (Pattern(..), pattern, point, para)
import Data.Foldable (foldr, toList)
```

### Create a Pattern

```haskell
-- Create a pattern with integer values
let p = pattern 10 [point 5, point 3]

-- Pattern structure:
-- Pattern { value = 10, elements = [
--   Pattern { value = 5, elements = [] },
--   Pattern { value = 3, elements = [] }
-- ]}
```

### Basic Paramorphism

```haskell
-- Simple structure-aware aggregation
let result = para (\p rs -> value p + sum rs) p
-- result = 18 (10 + 5 + 3)
```

### Compare with Foldable

```haskell
-- Foldable: Value-only folding
let foldableSum = foldr (+) 0 p
-- foldableSum = 18 (only values, no structure access)

-- Paramorphism: Structure-aware folding
let paraSum = para (\p rs -> value p + sum rs) p
-- paraSum = 18 (same result, but with structure access available)
```

## Common Patterns

### Depth-Weighted Sum

Compute sum of values weighted by their nesting depth:

```haskell
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Example
let p = pattern 10 [point 5, point 3]
depthWeightedSum p
-- = 10 * 1 + (5 * 0 + 3 * 0) + sum of child results
-- = 10 + 0 + 0 + (5 + 3)
-- = 18
```

### Element-Count-Aware Aggregation

Aggregate values considering element count at each position:

```haskell
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)

-- Example
let p = pattern 10 [point 5, point 3]
elementCountSum p
-- = 10 * 2 + (5 * 0 + 3 * 0) + sum of child results
-- = 20 + 0 + 0 + (5 + 3)
-- = 28
```

### Nesting-Level Statistics

Compute statistics considering nesting levels:

```haskell
nestingLevelStats :: Pattern Int -> (Int, Int, Int)  -- (sum, count, maxDepth)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))

-- Example
let p = pattern 10 [point 5, point 3]
nestingLevelStats p
-- = (18, 3, 1)  -- (sum, count, maxDepth)
```

### Structure-Preserving Transformation During Fold

Transform pattern values based on structure while preserving pattern structure:

```haskell
structureAwareTransform :: Pattern Int -> Pattern Int
structureAwareTransform = para (\p rs -> Pattern (value p * depth p) rs)

-- Example
let p = pattern 10 [point 5, point 3]
structureAwareTransform p
-- = Pattern { value = 10 * 1, elements = [
--     Pattern { value = 5 * 0, elements = [] },
--     Pattern { value = 3 * 0, elements = [] }
--   ]}
-- = Pattern { value = 10, elements = [
--     Pattern { value = 0, elements = [] },
--     Pattern { value = 0, elements = [] }
--   ]}
```

## When to Use Paramorphism vs. Foldable vs. Comonad

### Use Foldable When:

- You only need values, not structural information
- You want standard aggregation operations (sum, product, concatenation)
- You need to extract values as a flat list

```haskell
-- Example: Sum all values
sumValues = foldr (+) 0 pattern

-- Example: Extract all values as list
allValues = toList pattern
```

### Use Paramorphism When:

- You need structure-aware aggregations (depth-weighted sums, nesting-level statistics)
- You need to consider structural properties (depth, element count, nesting level) in aggregation
- You need structure-preserving transformations during fold

```haskell
-- Example: Depth-weighted sum
depthWeightedSum = para (\p rs -> value p * depth p + sum rs) pattern

-- Example: Element-count-aware aggregation
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs) pattern
```

### Use Comonad When:

- You need structure-aware transformation (not aggregation)
- You need to transform values based on structural context
- You need to return a Pattern structure (not an aggregated value)

```haskell
-- Example: Depth-weighted values
depthWeightedValues = extend (\p -> value p * depth p) pattern
-- Returns Pattern with transformed values
```

## Edge Cases

### Atomic Pattern (No Elements)

```haskell
let atom = point 5
para (\p rs -> value p + sum rs) atom
-- = 5 + 0
-- = 5
```

### Pattern with Single Element

```haskell
let p = pattern 10 [point 5]
para (\p rs -> value p + sum rs) p
-- = 10 + (5 + 0)
-- = 15
```

### Nested Pattern

```haskell
let nested = pattern 1 [pattern 2 [point 3]]
para (\p rs -> value p + sum rs) nested
-- = 1 + (2 + (3 + 0))
-- = 6
```

## Advanced Examples

### Computing Depth-Aware Statistics

```haskell
depthStats :: Pattern Int -> (Int, Int, Double)  -- (sum, count, avgDepth)
depthStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', d' + d'')) (0, 0, 0) rs
      depthValue = fromIntegral (depth p)
  in (value p + s, 1 + c, (depthValue + d) / fromIntegral (1 + c)))
```

### Structure-Aware Filtering During Fold

```haskell
filterDeepPatterns :: Pattern Int -> [Int]
filterDeepPatterns = para (\p rs -> 
  if depth p > 1 
    then value p : concat rs
    else concat rs)
```

## Performance Notes

- **Time Complexity**: O(n) where n is the total number of nodes
- **Space Complexity**: O(d) where d is the maximum nesting depth
- **Order Preservation**: Element order is preserved when aggregating

## Next Steps

- Read the [user guide](../docs/guide/06-advanced-morphisms.md) for detailed explanations
- Check the [reference documentation](../docs/reference/features/paramorphism.md) for complete API details
- See [porting guide](../docs/reference/PORTING-GUIDE.md) for implementing in other languages

