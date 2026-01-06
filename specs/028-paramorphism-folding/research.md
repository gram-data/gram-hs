# Research: Paramorphism for Structure-Aware Folding

**Feature**: 028-paramorphism-folding  
**Date**: 2025-01-28  
**Status**: Complete

## Research Questions

### RQ-001: What is the standard paramorphism pattern for recursive tree structures?

**Decision**: Use standard paramorphism pattern where the folding function receives both the current pattern subtree and recursively computed results from children.

**Rationale**:
- Standard paramorphism pattern is well-established in functional programming literature (Meijer, Fokkinga, Paterson - "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire")
- Provides natural access to pattern structure at each position
- Enables structure-aware aggregations without manual traversal
- Aligns with standard recursion schemes (catamorphism, paramorphism, anamorphism)
- Proven pattern used in libraries like `recursion-schemes` and `data-fix`

**Implementation Pattern**:
```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Breakdown**:
- `para f (Pattern v els)`: Applies paramorphism function `f` to pattern
- `f (Pattern v els) (map (para f) els)`: Provides folding function with:
  - Current pattern subtree: `Pattern v els`
  - Recursively computed results from children: `map (para f) els`
- Returns aggregated result of type `r`

**Alternatives Considered**:
- **Post-order processing**: Process children first, then pattern. Rejected because standard paramorphism uses the pattern as-is (provides full subtree).
- **Pre-order processing**: Process pattern first, then children. Already included in standard pattern (pattern subtree is provided, processing order is determined by folding function).
- **Manual traversal**: Manually traverse pattern structure. Rejected because paramorphism provides the abstraction we need and follows standard patterns.

**References**:
- Meijer, E., Fokkinga, M., & Paterson, R. (1991). "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire"
- `recursion-schemes` library: Standard implementation of recursion schemes including paramorphism
- `data-fix` library: Fixed-point types and recursion schemes
- Gibbons, J. (2003). "Origami Programming" - Tutorial on recursion schemes

---

### RQ-002: Should we implement additional paramorphism variants (paraMap, paraWith)?

**Decision**: Start with core `para` function. Consider `paraMap` and `paraWith` if concrete use cases emerge, but defer to future enhancement.

**Rationale**:
- Core `para` function provides essential structure-aware folding capability
- Additional variants can be added later if needed without breaking existing code
- Keeps initial implementation focused and simple
- Standard `para` function is sufficient for documented use cases (depth-weighted sums, structure-aware aggregations)
- YAGNI principle: Don't implement features until they're needed

**Core Implementation**:
```haskell
para :: (Pattern v -> [r] -> r) -> Pattern v -> r
para f (Pattern v els) = 
  f (Pattern v els) (map (para f) els)
```

**Deferred Variants**:
- `paraMap :: (Pattern v -> [r] -> r) -> Pattern v -> Pattern r`: Structure-preserving paramorphism that returns a Pattern structure. Could be useful for structure-aware transformations during fold, but can be implemented later if needed.
- `paraWith :: (Pattern v -> [r] -> r) -> Pattern v -> r`: With explicit accumulator. May be equivalent to `para` (accumulator can be passed through folding function), so deferring avoids duplication.

**Alternatives Considered**:
- **Implement all variants immediately**: Rejected because it increases complexity without clear use cases. Better to start simple and add variants when needed.
- **Only implement `paraMap`**: Rejected because `para` is more fundamental and `paraMap` can be derived from it (or implemented later). Core `para` function is sufficient for MVP.

**Future Considerations**:
- If structure-preserving transformations during fold become common, add `paraMap`
- If explicit accumulator pattern becomes useful, add `paraWith`
- Monitor use cases and add variants based on actual needs

---

### RQ-003: How should we document the relationship between paramorphism, Foldable, and Comonad?

**Decision**: Provide comprehensive documentation in user guide, reference documentation, and porting notes explaining:
1. How paramorphism extends `Foldable` to provide structure access (like `Comonad` extends `Functor`)
2. When to use each operation (`Foldable` for value-only, paramorphism for structure-aware folding, `Comonad` for structure-aware transformation)
3. Examples comparing all three operations on the same pattern

**Rationale**:
- Understanding relationships helps developers choose the right operation for their use case
- Clear documentation reduces confusion and improves developer experience
- Examples demonstrate practical differences and use cases
- Porting notes help implementers in other languages understand the concepts
- Comprehensive documentation aligns with project standards (100% Haddock coverage, user guide, reference docs)

**Documentation Structure**:

1. **User Guide** (`docs/guide/06-advanced-morphisms.md`):
   - Add new section: "Paramorphism: Structure-Aware Folding"
   - Intuitive explanation: Paramorphism extends `Foldable` to provide structure access
   - Examples: Depth-weighted sums, nesting-level statistics, element-count-aware aggregations
   - Relationship to `Foldable` and `Comonad`: When to use each operation
   - Practical use cases: Structure-aware analyses that are impossible with `Foldable` alone

2. **Reference Documentation** (`docs/reference/features/paramorphism.md`):
   - Complete reference documentation for paramorphism
   - Type signatures and function documentation
   - Laws and properties
   - Implementation details
   - Examples and use cases
   - Performance characteristics

3. **Porting Guide** (`docs/reference/PORTING-GUIDE.md`):
   - Add new section: "Paramorphism Implementation"
   - How to implement paramorphism in other languages
   - Language-specific considerations (type systems, recursion patterns)
   - Examples in multiple languages (Rust, TypeScript, Python)
   - Relationship to `Foldable` and `Comonad` in other languages

**Content Strategy**:
- Start with intuitive explanations before formal definitions
- Provide concrete examples showing when to use each operation
- Compare operations side-by-side on the same pattern
- Explain the mathematical relationship (paramorphism extends `Foldable` like `Comonad` extends `Functor`)
- Include porting guidance for multiple languages

**Alternatives Considered**:
- **Only user guide**: Rejected because reference documentation and porting notes are essential for completeness. Reference docs provide detailed API information, and porting notes help implementers.
- **Only reference documentation**: Rejected because user guide provides intuitive introduction needed for learning. Developers need both intuitive explanations and detailed reference.
- **Separate documentation files**: Considered but rejected because existing structure (user guide + reference + porting guide) is well-established and works well.

---

### RQ-004: How should property-based tests verify structure access?

**Decision**: Use property-based tests that verify the folding function receives the correct pattern structure at each position, and that structure-aware aggregations produce correct results.

**Rationale**:
- Property-based tests can verify structure access by checking that folding functions can extract structural properties (depth, element count)
- Structure-aware aggregations (depth-weighted sums) provide concrete verification
- Tests serve as executable specifications for implementations in other languages
- Property-based testing aligns with project standards (used for all typeclass instances)
- Tests verify both structure access and correctness of aggregations

**Test Strategy**:

1. **Structure Access Verification**:
   ```haskell
   -- Verify paramorphism provides access to pattern structure
   para (\p _ -> depth p) pattern == depth pattern
   para (\p _ -> length (elements p)) pattern == length (elements pattern)
   ```

2. **Structure-Aware Aggregation Verification**:
   ```haskell
   -- Verify depth-weighted sum produces correct results
   para (\p rs -> value p * depth p + sum rs) pattern
   -- Should produce correct depth-weighted sum
   ```

3. **Relationship to Foldable Verification**:
   ```haskell
   -- Verify paramorphism can simulate Foldable (when ignoring structure)
   para (\p rs -> value p : concat rs) pattern == toList pattern
   ```

4. **Order Preservation Verification**:
   ```haskell
   -- Verify paramorphism preserves element order
   para (\p rs -> value p : concat rs) pattern == toList pattern
   -- (order should match Foldable toList)
   ```

**Performance Considerations**:
- Use `quickProperty` helper (20 test cases max) for all property-based tests
- Limit pattern generator size to keep tests fast (<10ms total)
- Reuse existing `Arbitrary` instances from `Properties.hs`
- Monitor test execution time and adjust if needed

**Alternatives Considered**:
- **Only unit tests**: Rejected because property-based tests provide better coverage and serve as executable specifications. Unit tests verify specific cases, but property-based tests verify general properties.
- **Only structure access tests**: Rejected because structure-aware aggregation tests provide concrete verification. Structure access tests verify capability, but aggregation tests verify correctness.
- **Manual verification**: Rejected because automated tests provide continuous verification and serve as executable specifications for other implementations.

---

## Summary

All research questions have been resolved:

1. **Standard paramorphism pattern**: Use standard recursive pattern where folding function receives pattern subtree and child results
2. **Additional variants**: Defer to future enhancement, start with core `para` function
3. **Documentation**: Comprehensive documentation in user guide, reference, and porting notes
4. **Property-based testing**: Verify structure access and structure-aware aggregations using property-based tests

The implementation will follow standard paramorphism patterns, provide comprehensive documentation, and include thorough testing. The relationship to `Foldable` and `Comonad` will be clearly documented to help developers choose the right operation.

