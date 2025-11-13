# Research: Ord Instance for Pattern

**Feature**: 009-ord-instance  
**Date**: 2025-01-27  
**Status**: Complete

## Research Objectives

1. Determine the correct lexicographic ordering semantics for Pattern type
2. Verify consistency requirements between `Ord` and `Eq` instances
3. Understand best practices for implementing `Ord` for recursive types in Haskell
4. Confirm that `toTuple()`-based ordering is the correct approach

---

## Findings

### 1. Lexicographic Ordering for Recursive Types

**Decision**: Use lexicographic ordering with value-first comparison, then elements recursively.

**Rationale**: 
- Standard Haskell convention for recursive types is to compare fields in order
- Pattern's structure is `(value, elements)`, so value should be compared first
- This matches the existing `Eq` instance which compares value first, then elements
- Lexicographic ordering provides total ordering when `Ord v` is satisfied

**Alternatives considered**:
- **Elements-first ordering**: Rejected - doesn't match `Eq` instance semantics and is less intuitive
- **Flattened value ordering**: Rejected - loses structural information and doesn't distinguish patterns with different structures but same flattened values
- **Size-based ordering**: Rejected - doesn't provide meaningful ordering for patterns with same size

**References**:
- Haskell Report: Standard `Ord` instance for recursive types uses lexicographic ordering
- GHC documentation: `Ord` instances should compare fields in declaration order

### 2. Consistency Between Ord and Eq

**Decision**: Ensure `Ord` instance is consistent with `Eq` instance: `p1 == p2` implies `compare p1 p2 == EQ`.

**Rationale**:
- Fundamental requirement in Haskell: ordering must respect equality
- Prevents subtle bugs where equal patterns are ordered differently
- Enables correct behavior in `Data.Set` and `Data.Map` (equal patterns should map to same key)

**Implementation approach**:
- Use same comparison order as `Eq`: value first, then elements
- This ensures consistency automatically since both use lexicographic comparison

**Alternatives considered**:
- **Different ordering**: Rejected - would violate consistency requirement and cause bugs in standard library usage

### 3. toTuple() Based Ordering

**Decision**: Use `toTuple()` semantics for ordering, which preserves structure and distinguishes patterns with different structures.

**Rationale**:
- `toTuple()` returns `(v, [Pattern v])`, directly representing Pattern's structure
- Ordering by tuple is equivalent to: `compare (toTuple p1) (toTuple p2)`
- This distinguishes patterns with same flattened values but different structures
- Matches the conceptual model: value is decoration, elements form the pattern

**Implementation**:
```haskell
instance Ord v => Ord (Pattern v) where
  compare (Pattern v1 els1) (Pattern v2 els2) =
    compare v1 v2 <> compare els1 els2
```

This is equivalent to `compare (toTuple p1) (toTuple p2)` because:
- `toTuple (Pattern v els) = (v, els)`
- `compare (v1, els1) (v2, els2) = compare v1 v2 <> compare els1 els2`

**Alternatives considered**:
- **toList() based ordering**: Rejected - loses structural information, patterns with different structures but same flattened values would compare equal
- **Custom ordering function**: Rejected - unnecessary complexity, standard lexicographic ordering is sufficient

### 4. Total Ordering Properties

**Decision**: The `Ord` instance must provide total ordering (transitive, antisymmetric, reflexive).

**Rationale**:
- Required by Haskell's `Ord` typeclass
- Enables correct behavior in ordered data structures
- Standard lexicographic ordering automatically provides these properties when `Ord v` is satisfied

**Verification**:
- **Transitivity**: If `p1 < p2` and `p2 < p3`, then `p1 < p3` (follows from lexicographic ordering)
- **Antisymmetry**: If `p1 < p2`, then `p2 > p1` (follows from lexicographic ordering)
- **Reflexivity**: `p1 <= p1` always true (follows from lexicographic ordering)

**Implementation guarantee**: Standard `compare` implementation for tuples provides these properties automatically.

### 5. Type Constraint Requirements

**Decision**: `Ord` instance requires `Ord v` constraint: `instance Ord v => Ord (Pattern v)`.

**Rationale**:
- Pattern's value must be orderable to compare patterns
- Type system enforces this constraint, preventing compilation if `v` doesn't have `Ord` instance
- Matches standard Haskell practice for parameterized types

**Alternatives considered**:
- **No constraint**: Rejected - cannot compare patterns if values aren't orderable
- **Partial ordering**: Rejected - Haskell's `Ord` requires total ordering

---

## Summary

The research confirms that:
1. Lexicographic ordering (value-first, then elements recursively) is the correct approach
2. `toTuple()`-based ordering preserves structure and distinguishes patterns correctly
3. Consistency with `Eq` instance is automatically maintained by using same comparison order
4. Standard Haskell `Ord` instance implementation provides total ordering properties
5. `Ord v` constraint is required and enforced by type system

No unresolved questions remain. The implementation approach is straightforward and follows standard Haskell conventions.

