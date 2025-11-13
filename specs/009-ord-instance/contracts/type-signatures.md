# Type Signatures: Ord Instance for Pattern

**Feature**: 009-ord-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Ord Instance

### Type Signature

```haskell
instance Ord v => Ord (Pattern v) where
  compare :: Pattern v -> Pattern v -> Ordering
  (<)      :: Pattern v -> Pattern v -> Bool
  (<=)     :: Pattern v -> Pattern v -> Bool
  (>)      :: Pattern v -> Pattern v -> Bool
  (>=)     :: Pattern v -> Pattern v -> Bool
  max      :: Pattern v -> Pattern v -> Pattern v
  min      :: Pattern v -> Pattern v -> Pattern v
```

### Implementation

```haskell
instance Ord v => Ord (Pattern v) where
  compare (Pattern v1 els1) (Pattern v2 els2) =
    compare v1 v2 <> compare els1 els2
```

**Note**: The other methods (`<`, `<=`, `>`, `>=`, `max`, `min`) are automatically provided by the default implementations in the `Ord` typeclass based on `compare`.

### Type Constraints

- **Required**: `Ord v` - The value type must have an `Ord` instance
- **Provided**: `Ord (Pattern v)` - Patterns are orderable when values are orderable

### Semantics

- **Lexicographic ordering**: Compare value first, then elements recursively
- **Structure-preserving**: Based on `toTuple()` semantics
- **Consistent with Eq**: `p1 == p2` implies `compare p1 p2 == EQ`
- **Total ordering**: Transitive, antisymmetric, reflexive

## Standard Library Integration

### Data.Set

```haskell
-- Type signature
Data.Set.fromList :: [Pattern v] -> Data.Set.Set (Pattern v)
Data.Set.member :: Pattern v -> Data.Set.Set (Pattern v) -> Bool
Data.Set.insert :: Pattern v -> Data.Set.Set (Pattern v) -> Data.Set.Set (Pattern v)
```

**Requirement**: `Ord (Pattern v)` instance (satisfied when `Ord v`)

### Data.Map

```haskell
-- Type signature
Data.Map.fromList :: [(Pattern v, a)] -> Data.Map.Map (Pattern v) a
Data.Map.lookup :: Pattern v -> Data.Map.Map (Pattern v) a -> Maybe a
Data.Map.insert :: Pattern v -> a -> Data.Map.Map (Pattern v) a -> Data.Map.Map (Pattern v) a
```

**Requirement**: `Ord (Pattern v)` instance (satisfied when `Ord v`)

### Sorting

```haskell
-- Type signature
Data.List.sort :: [Pattern v] -> [Pattern v]
Data.List.sortBy :: (Pattern v -> Pattern v -> Ordering) -> [Pattern v] -> [Pattern v]
```

**Requirement**: `Ord (Pattern v)` instance (satisfied when `Ord v`)

## Test Contracts

### Unit Test Requirements

1. **Atomic Pattern Comparison**
   - Test: Compare two atomic patterns with different values
   - Expected: Ordering determined by value comparison
   - Test: Compare two atomic patterns with same value
   - Expected: Compare as equal (`EQ`)

2. **Pattern with Elements Comparison**
   - Test: Compare patterns with same value but different elements
   - Expected: Ordering determined by elements comparison
   - Test: Compare patterns with different element counts
   - Expected: Shorter list comes before longer list (when values equal)

3. **Nested Pattern Comparison**
   - Test: Compare patterns with nested structures
   - Expected: Recursive comparison of nested patterns
   - Test: Compare patterns with same flattened values but different structures
   - Expected: Distinguished based on structure

4. **Consistency with Eq**
   - Test: Patterns that are equal according to `Eq` compare as `EQ`
   - Expected: `p1 == p2` implies `compare p1 p2 == EQ`
   - Test: Patterns that are not equal according to `Eq` don't compare as `EQ`
   - Expected: `p1 /= p2` implies `compare p1 p2 /= EQ`

### Property-Based Test Requirements

1. **Total Ordering Properties**
   - Property: Transitivity - If `p1 < p2` and `p2 < p3`, then `p1 < p3`
   - Property: Antisymmetry - If `p1 < p2`, then `p2 > p1`
   - Property: Reflexivity - `p1 <= p1` always true

2. **Consistency with Eq**
   - Property: `p1 == p2` implies `compare p1 p2 == EQ`
   - Property: `compare p1 p2 == EQ` implies `p1 == p2`

3. **Lexicographic Ordering**
   - Property: `compare p1 p2 == compare (toTuple p1) (toTuple p2)`
   - Property: Value comparison determines ordering when values differ
   - Property: Elements comparison determines ordering when values are equal

### Integration Test Requirements

1. **Data.Set Integration**
   - Test: Create `Data.Set` with patterns
   - Expected: Patterns are maintained in sorted order
   - Test: Insert patterns into set
   - Expected: Duplicate patterns (by `Eq`) are not duplicated in set
   - Test: Lookup patterns in set
   - Expected: Correct pattern matching using ordering

2. **Data.Map Integration**
   - Test: Create `Data.Map` with patterns as keys
   - Expected: Patterns can be used as keys
   - Test: Lookup patterns in map
   - Expected: Correct pattern matching using ordering
   - Test: Insert patterns into map
   - Expected: Patterns with same value (by `Eq`) overwrite each other

3. **Sorting Integration**
   - Test: Sort list of patterns
   - Expected: Patterns are sorted according to `Ord` instance
   - Test: Sort patterns with various structures
   - Expected: Correct ordering for all pattern structures

## Test Coverage Requirements

- **Unit tests**: Cover all edge cases (atomic patterns, different element counts, nested structures, consistency with Eq)
- **Property-based tests**: Verify total ordering properties, consistency with Eq, lexicographic ordering rules
- **Integration tests**: Verify `Data.Set`, `Data.Map`, and sorting functionality
- **Coverage target**: 100% of `Ord` instance methods covered by tests

## Performance Requirements

- **Comparison complexity**: O(n) where n is the number of nodes in the pattern structure
- **Worst case**: When comparing equal patterns, must traverse entire structure
- **Best case**: When values differ, comparison completes in O(1) (value comparison only)
- **No performance optimizations needed**: Standard lexicographic comparison is sufficient

