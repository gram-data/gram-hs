# Feature Specification: Ord Instance for Pattern

**Feature Branch**: `009-ord-instance`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "add Ord to Pattern using toTuple() based ordering for 8.1 of @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Order Patterns by Structure (Priority: P1)

Developers need to compare and order patterns to enable standard library operations like sorting, using patterns as keys in maps, and organizing patterns in sets. This requires an `Ord` instance that provides consistent, predictable ordering based on pattern structure.

**Why this priority**: The `Ord` instance is fundamental for enabling standard Haskell library features like `Data.Set` and `Data.Map` with patterns. Without ordering, patterns cannot be used as keys in ordered data structures, limiting their utility in many common scenarios.

**Independent Test**: Can be fully tested by comparing patterns with different values and structures, verifying that ordering follows lexicographic rules (value first, then elements recursively). This delivers immediate value for pattern organization and standard library integration.

**Acceptance Scenarios**:

1. **Given** two patterns with different values, **When** they are compared, **Then** ordering is determined by value comparison first
2. **Given** two patterns with the same value but different elements, **When** they are compared, **Then** ordering is determined by element comparison (recursively)
3. **Given** two patterns with the same value and same number of elements, **When** they are compared, **Then** ordering compares elements lexicographically (first element, then second, etc.)
4. **Given** patterns with nested structures, **When** they are compared, **Then** ordering recursively compares nested patterns using the same rules

---

### User Story 2 - Use Patterns in Ordered Collections (Priority: P1)

Developers need to use patterns as keys in `Data.Map` and elements in `Data.Set` to organize and index pattern-based data structures efficiently.

**Why this priority**: Enabling patterns in ordered collections is a primary use case for the `Ord` instance. This allows developers to build indexes, organize patterns, and perform efficient lookups on pattern-based data.

**Independent Test**: Can be fully tested by creating `Data.Set (Pattern v)` and `Data.Map (Pattern v) a` instances and verifying that patterns are correctly ordered and can be used for lookups. This delivers immediate value for pattern organization.

**Acceptance Scenarios**:

1. **Given** a set of patterns, **When** they are inserted into a `Data.Set`, **Then** they are maintained in sorted order
2. **Given** patterns used as keys in a `Data.Map`, **When** lookups are performed, **Then** patterns are correctly matched using their ordering
3. **Given** patterns with duplicate values but different structures, **When** they are inserted into a set, **Then** both are retained (they are distinct patterns)
4. **Given** patterns sorted using `sort`, **When** the sorted list is examined, **Then** patterns are ordered according to the `Ord` instance rules

---

### User Story 3 - Verify Ordering Consistency with Equality (Priority: P2)

Developers need assurance that the `Ord` instance is consistent with the existing `Eq` instance, ensuring that equal patterns compare as equal and ordering respects structural equality.

**Why this priority**: Consistency between `Eq` and `Ord` is a fundamental requirement in Haskell. Patterns that are equal must compare as equal, and the ordering must respect the same structural semantics as equality.

**Independent Test**: Can be fully tested by verifying that `p1 == p2` implies `compare p1 p2 == EQ`, and that ordering respects the same structural comparison rules as equality. This delivers value for ensuring correctness and preventing subtle bugs.

**Acceptance Scenarios**:

1. **Given** two patterns that are equal according to `Eq`, **When** they are compared, **Then** `compare` returns `EQ`
2. **Given** two patterns that are not equal according to `Eq`, **When** they are compared, **Then** `compare` returns `LT` or `GT` (not `EQ`)
3. **Given** patterns with the same structure but different values, **When** they are compared, **Then** ordering is determined by value comparison (consistent with how `Eq` compares value first)
4. **Given** patterns with the same value but different element structures, **When** they are compared, **Then** ordering is determined by element comparison (consistent with how `Eq` compares elements)

---

### Edge Cases

- What happens when comparing atomic patterns (no elements)? (Should compare by value only, then empty elements list)
- What happens when comparing patterns with different numbers of elements? (Shorter list should come before longer list if values are equal)
- What happens when comparing deeply nested patterns? (Should recursively compare nested structures)
- How does ordering handle patterns with the same flattened values but different structures? (Should distinguish them based on structure, not flattened values)
- What happens when the value type `v` doesn't have an `Ord` instance? (The `Ord (Pattern v)` instance should require `Ord v`, preventing compilation if `v` is not orderable)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide an `Ord` instance for `Pattern v` that requires `Ord v` constraint
- **FR-002**: System MUST order patterns using lexicographic ordering: value first, then elements recursively
- **FR-003**: System MUST ensure that `Ord` instance is consistent with existing `Eq` instance (equal patterns compare as equal)
- **FR-004**: System MUST use structure-preserving ordering (based on `toTuple()` semantics) that distinguishes patterns with different structures even if they have the same flattened values
- **FR-005**: System MUST enable patterns to be used as keys in `Data.Map` and elements in `Data.Set`
- **FR-006**: System MUST enable patterns to be sorted using standard `sort` function
- **FR-007**: System MUST provide total ordering (transitive, antisymmetric, reflexive) for all patterns when `Ord v` is satisfied

### Key Entities

- **Pattern**: The recursive data structure with `value :: v` and `elements :: [Pattern v]`. The `Ord` instance must order patterns based on their structure, comparing value first, then elements recursively.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can successfully use `Pattern v` as keys in `Data.Map` when `Ord v` is satisfied, with 100% of test cases passing
- **SC-002**: Developers can successfully use `Pattern v` as elements in `Data.Set` when `Ord v` is satisfied, with 100% of test cases passing
- **SC-003**: Patterns can be sorted using standard `sort` function, producing correctly ordered lists in all test scenarios
- **SC-004**: The `Ord` instance maintains consistency with `Eq` instance: all patterns that compare as equal are also equal according to `Eq`, verified by 100% of consistency tests passing
- **SC-005**: Ordering correctly distinguishes patterns with different structures even when they have the same flattened values, verified by all structural distinction tests passing
- **SC-006**: The implementation follows lexicographic ordering rules (value first, then elements recursively), verified by all ordering property tests passing

## Assumptions

- The value type `v` will have an `Ord` instance when patterns need to be ordered (enforced by type constraint)
- Developers understand that ordering is based on structure (value + elements), not flattened values
- The `Ord` instance will be used primarily for standard library integration (`Data.Set`, `Data.Map`, sorting)
- Ordering semantics align with the existing `Eq` instance (value-first, then elements)

## Dependencies

- **Pattern.Core module**: Requires existing `Pattern` data type definition
- **Eq instance**: The `Ord` instance must be consistent with the existing `Eq` instance
- **toTuple function**: The ordering semantics are based on `toTuple()` structure-preserving representation

## Out of Scope

- Custom ordering functions beyond the standard `Ord` instance
- Ordering based on flattened values (using `toList()`)
- Performance optimizations beyond standard lexicographic comparison
- Ordering that ignores structure differences
