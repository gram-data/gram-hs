# Type Signatures: Paramorphism for Structure-Aware Folding

**Feature**: 028-paramorphism-folding  
**Date**: 2025-01-28

## Core Function

### `para`

```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
```

**Description**: Paramorphism function that enables structure-aware folding over patterns. The folding function receives both the current pattern subtree and recursively computed results from children.

**Parameters**:
- `f :: Pattern v -> [r] -> r`: Folding function that receives:
  - `Pattern v`: Current pattern subtree (full pattern structure)
  - `[r]`: List of recursively computed results from children
  - Returns: Aggregated result of type `r`
- `pattern :: Pattern v`: Pattern to fold over

**Returns**: Aggregated result of type `r`

**Example**:
```haskell
-- Depth-weighted sum
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)

-- Usage
let p = pattern 10 [point 5, point 3]
depthWeightedSum p  -- 18 (10*1 + 5*0 + 3*0 + sum of child results)
```

## Folding Function Signature

### Standard Pattern

```haskell
f :: Pattern v -> [r] -> r
```

**Parameters**:
- `Pattern v`: Current pattern subtree (full pattern structure including value and elements)
- `[r]`: List of recursively computed results from children (one result per child element)

**Returns**: Aggregated result of type `r`

**Access to Pattern Structure**:
- `value p`: Access pattern's decoration value
- `elements p`: Access pattern's element list
- `depth p`: Access pattern's nesting depth
- `length (elements p)`: Access pattern's element count
- Full pattern structure available for structure-aware operations

**Example**:
```haskell
-- Structure-aware aggregation considering depth
para (\p rs -> value p * depth p + sum rs) pattern

-- Structure-aware aggregation considering element count
para (\p rs -> value p * length (elements p) + sum rs) pattern

-- Structure-preserving transformation during fold
para (\p rs -> Pattern (value p * depth p) rs) pattern
```

## Common Patterns

### Depth-Weighted Sum

```haskell
depthWeightedSum :: Pattern Int -> Int
depthWeightedSum = para (\p rs -> value p * depth p + sum rs)
```

**Type**: `Pattern Int -> Int`

**Description**: Computes sum of all values weighted by their nesting depth.

### Element-Count-Aware Aggregation

```haskell
elementCountSum :: Pattern Int -> Int
elementCountSum = para (\p rs -> value p * length (elements p) + sum rs)
```

**Type**: `Pattern Int -> Int`

**Description**: Computes sum considering element count at each position.

### Nesting-Level Statistics

```haskell
nestingLevelStats :: Pattern Int -> (Int, Int, Int)
nestingLevelStats = para (\p rs -> 
  let (s, c, d) = foldr (\(s', c', d') (s'', c'', d'') -> 
    (s' + s'', c' + c'', max d' d'')) (0, 0, 0) rs
  in (value p + s, 1 + c, max (depth p) d))
```

**Type**: `Pattern Int -> (Int, Int, Int)`

**Description**: Returns tuple of (sum, count, maxDepth) considering nesting levels.

### Structure-Preserving Transformation

```haskell
structureAwareTransform :: Pattern Int -> Pattern Int
structureAwareTransform = para (\p rs -> Pattern (value p * depth p) rs)
```

**Type**: `Pattern Int -> Pattern Int`

**Description**: Transforms pattern values based on structure while preserving pattern structure.

## Relationship to Other Operations

### Comparison with Foldable

```haskell
-- Foldable: Value-only folding
foldr :: (a -> b -> b) -> b -> Pattern a -> b
foldr (+) 0 pattern  -- Only values, no structure access

-- Paramorphism: Structure-aware folding
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para (\p rs -> value p * depth p + sum rs) pattern  -- Structure + values
```

### Comparison with Comonad

```haskell
-- Comonad: Structure-aware transformation
extend :: (Pattern v -> w) -> Pattern v -> Pattern w
extend (\p -> value p * depth p) pattern  -- Transformation, not folding

-- Paramorphism: Structure-aware folding
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para (\p rs -> value p * depth p + sum rs) pattern  -- Folding, not transformation
```

## Laws and Properties

### Structure Access Property

```haskell
para (\p _ -> p) pattern == pattern
```

**Description**: Paramorphism can extract the full pattern structure.

### Value Access Property

```haskell
para (\p rs -> value p : concat rs) pattern == toList pattern
```

**Description**: Paramorphism can simulate `Foldable` when ignoring structure (but includes pattern's own value).

### Recursive Structure Property

```haskell
para f (Pattern v els) == f (Pattern v els) (map (para f) els)
```

**Description**: Standard paramorphism pattern: process children first, then combine with current pattern.

### Order Preservation Property

```haskell
para (\p rs -> value p : concat rs) pattern == toList pattern
```

**Description**: Paramorphism preserves element order when aggregating results.

## Type Constraints

- **Value Type**: `Pattern v` where `v` can be any type
- **Result Type**: `r` can be any type (determined by folding function)
- **No additional constraints**: Paramorphism works with any value type that supports pattern structure

## Performance Characteristics

- **Time Complexity**: O(n) where n is the total number of nodes in the pattern structure
- **Space Complexity**: O(d) where d is the maximum nesting depth (for recursion stack)
- **Order Preservation**: Element order is preserved when aggregating results

