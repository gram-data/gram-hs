# Feature Specification: Paramorphism for Structure-Aware Folding

**Feature Branch**: `028-paramorphism-folding`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Paramorphism is essential for patterns, because structure is information. Details are in '3.5 Paramorphism for Structure-Aware Folding' and '12.5 Paramorphism Implementation' of @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Fold Patterns with Structure Awareness (Priority: P1) 🎯 MVP

As a developer working with patterns, I need to fold over pattern structures while having access to the full pattern subtree at each position, so that I can perform structure-aware aggregations that consider both values and structural context.

**Why this priority**: Patterns aren't just containers for values—the structure of the pattern *is information*. Standard `Foldable` operations (`foldr`, `foldl`, `foldMap`) only provide access to values, not the pattern structure itself. Paramorphism enables folding operations that can consider structural context (depth, element count, nesting level) when aggregating results. This is essential for operations like computing depth-aware sums, structure-preserving transformations during fold, and context-dependent aggregations.

**Independent Test**: Can be fully tested by applying paramorphism operations to patterns and verifying that: (1) the folding function receives both the current pattern subtree and recursively computed results from children, (2) structure-aware aggregations produce correct results, and (3) paramorphism works for atomic patterns, patterns with elements, and nested patterns. This delivers the ability to fold patterns with full structural awareness.

**Acceptance Scenarios**:

1. **Given** an atomic pattern with value 5, **When** I apply paramorphism with a function that sums values weighted by depth, **Then** the function receives the full pattern structure and computes the correct depth-weighted sum
2. **Given** a pattern with multiple elements containing values [10, 20, 30], **When** I apply paramorphism with a function that aggregates values while considering element count, **Then** the function receives each pattern subtree and computes aggregations that reflect structural information
3. **Given** a deeply nested pattern structure, **When** I apply paramorphism, **Then** the folding function receives the full pattern subtree at each position, enabling depth-aware and structure-aware aggregations
4. **Given** a pattern where I need to compute structure-preserving transformations during fold, **When** I apply paramorphism, **Then** I can access the original pattern structure at each position while computing aggregated results
5. **Given** a pattern where aggregation depends on structural context (e.g., depth, element count, nesting level), **When** I apply paramorphism, **Then** I can perform context-dependent aggregations that consider structural information

---

### User Story 2 - Perform Structure-Aware Aggregations (Priority: P1)

As a developer working with patterns, I need to compute aggregations that depend on structural properties (depth, element count, nesting level) in addition to values, so that I can perform sophisticated analyses that consider both data and structure.

**Why this priority**: Many real-world operations require understanding both the values in a pattern and the structural context in which they appear. For example, computing depth-weighted sums, aggregating values based on nesting level, or performing structure-aware statistics. Standard `Foldable` operations cannot access structural information, making these operations impossible without manual traversal. Paramorphism enables these structure-aware aggregations naturally.

**Independent Test**: Can be fully tested by applying paramorphism to compute structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations) and verifying that results correctly reflect both values and structural properties. This delivers the ability to perform sophisticated structure-aware analyses.

**Acceptance Scenarios**:

1. **Given** a pattern with values at different nesting levels, **When** I compute a depth-weighted sum using paramorphism, **Then** values at deeper nesting levels contribute differently to the result based on their depth
2. **Given** a pattern with varying element counts at different positions, **When** I aggregate values considering element count, **Then** the aggregation reflects the structural property (element count) in addition to values
3. **Given** a pattern where I need to compute statistics that depend on nesting level, **When** I apply paramorphism, **Then** I can access nesting level information and compute level-aware statistics
4. **Given** a pattern where aggregation behavior should differ based on structural context, **When** I apply paramorphism, **Then** I can perform context-dependent aggregations that adapt to structural properties

---

### User Story 3 - Understand Relationship to Foldable and Comonad (Priority: P2)

As a developer learning about pattern operations, I need clear documentation explaining how paramorphism relates to `Foldable` (value-only folding) and `Comonad` (structure-aware transformations), so that I can choose the right operation for each use case.

**Why this priority**: Understanding the relationship between paramorphism, `Foldable`, and `Comonad` helps developers choose the right abstraction. Just as `Comonad` extends `Functor` to see structure (`extend` vs `fmap`), Paramorphism extends `Foldable` to see structure (`para` vs `foldr`). This relationship is important for developers to understand when to use each operation. However, this is documentation-focused and builds on the core functionality, so it's P2.

**Independent Test**: Can be fully tested by creating documentation that explains: (1) how paramorphism extends `Foldable` to provide structure access, (2) how paramorphism relates to `Comonad` (both provide structure access but for different purposes), and (3) when to use each operation. This delivers understanding that enables effective use of pattern operations.

**Acceptance Scenarios**:

1. **Given** a developer familiar with `Foldable`, **When** they read about paramorphism, **Then** they understand that paramorphism provides structure-aware folding where `Foldable` provides value-only folding
2. **Given** a developer familiar with `Comonad`, **When** they read about paramorphism, **Then** they understand that both provide structure access but paramorphism is for folding while `Comonad` is for transformations
3. **Given** a developer choosing between operations, **When** they read the documentation, **Then** they can determine when to use `Foldable` (value-only), paramorphism (structure-aware folding), or `Comonad` (structure-aware transformation)
4. **Given** a developer learning pattern operations, **When** they read examples comparing paramorphism with `Foldable` and `Comonad`, **Then** they understand the relationships and can apply the right operation for each use case

---

### Edge Cases

- What happens when applying paramorphism to an atomic pattern (pattern with no elements)?
- What happens when applying paramorphism to a pattern with an empty elements list?
- What happens when applying paramorphism to a pattern with a single element (singular pattern)?
- What happens when applying paramorphism to a pattern with many elements?
- How does paramorphism handle patterns with different value types (strings, integers, custom types)?
- What happens when the folding function accesses structural properties (depth, element count) that vary across the pattern?
- How does paramorphism preserve or respect element order in patterns with multiple elements?
- What happens when applying paramorphism to nested patterns with varying depths?
- How does paramorphism handle patterns where the folding function produces different types?
- What happens when the folding function needs to access both the current pattern subtree and recursively computed results from children?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a way to fold over pattern structures while providing access to the full pattern subtree at each position
- **FR-002**: System MUST support paramorphism operations where the folding function receives: (1) the current pattern subtree, and (2) the list of recursively computed results from children
- **FR-003**: System MUST enable structure-aware aggregations that consider structural properties (depth, element count, nesting level) in addition to values
- **FR-004**: System MUST enable depth-aware operations (e.g., depth-weighted sums, depth-based aggregations)
- **FR-005**: System MUST enable structure-preserving transformations during fold operations
- **FR-006**: System MUST enable context-dependent aggregations that adapt to structural properties
- **FR-007**: System MUST work with patterns containing any value type (strings, integers, custom types)
- **FR-008**: System MUST handle atomic patterns (patterns with no elements) correctly
- **FR-009**: System MUST handle patterns with elements correctly
- **FR-010**: System MUST handle deeply nested patterns correctly
- **FR-011**: System MUST preserve or respect element order during paramorphism operations
- **FR-012**: System MUST provide clear documentation explaining the relationship between paramorphism, `Foldable`, and `Comonad`
- **FR-013**: System MUST provide examples showing when to use paramorphism vs. `Foldable` vs. `Comonad`
- **FR-014**: System MUST provide examples demonstrating structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations)

### Key Entities

- **Pattern**: A recursive data structure representing a decorated sequence. Contains a value (decoration) and a list of pattern elements. The elements form the pattern itself; the value describes what kind of pattern it is.
- **Paramorphism**: A folding operation that provides access to the full pattern subtree at each position, enabling structure-aware aggregations. The folding function receives both the current pattern subtree and recursively computed results from children.
- **Structure-Aware Folding**: Folding operations that consider structural properties (depth, element count, nesting level) in addition to values when aggregating results.
- **Foldable Instance**: A typeclass instance that enables value-only folding operations over pattern structures (`foldr`, `foldl`, `foldMap`, `toList`). Provides access to values but not structural context.
- **Comonad Instance**: A typeclass instance that enables structure-aware transformations (`extract`, `duplicate`, `extend`). Provides access to structural context for transformations, not folding.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can fold over any pattern structure (atomic, with elements, nested) with full structural awareness, accessing the pattern subtree at each position in 100% of cases
- **SC-002**: Paramorphism correctly provides access to both the current pattern subtree and recursively computed results from children with 100% accuracy for all pattern structures
- **SC-003**: Structure-aware aggregations (depth-weighted sums, nesting-level statistics, element-count-based aggregations) produce correct results that reflect both values and structural properties with 100% accuracy
- **SC-004**: Paramorphism operations work correctly for patterns with values of any type (strings, integers, custom types) with 100% success rate
- **SC-005**: Paramorphism of nested patterns (3+ levels deep) completes successfully and correctly provides structural access at all levels
- **SC-006**: Element order is preserved or respected during paramorphism operations with 100% consistency
- **SC-007**: Documentation clearly explains the relationship between paramorphism, `Foldable`, and `Comonad`, with 90% of test readers successfully understanding when to use each operation
- **SC-008**: Examples demonstrate structure-aware aggregations effectively, with 90% of test readers successfully understanding how to perform depth-aware, nesting-level-aware, and element-count-aware aggregations
- **SC-009**: Developers can perform structure-aware folding operations that are impossible with `Foldable` alone, enabling new classes of pattern analyses

## Assumptions

- Paramorphism operations process all positions in the pattern structure, providing access to the full pattern subtree at each position
- The folding function receives both the current pattern subtree and recursively computed results from children, enabling structure-aware aggregations
- Paramorphism enables operations that consider structural properties (depth, element count, nesting level) in addition to values
- The implementation will follow standard paramorphism patterns for recursive tree structures
- Property-based testing will be used to verify paramorphism operations across many pattern structures
- The specification focuses on developer needs (structure-aware folding capabilities) rather than implementation details
- Paramorphism operations are pure (no side effects) - this is standard for functional folding operations
- The paramorphism implementation will work with any value type that supports the folding operations (no additional constraints beyond what standard folding interfaces require)
- Paramorphism complements `Foldable` (value-only folding) and `Comonad` (structure-aware transformation), providing structure-aware folding capabilities
- Documentation will explain the relationship between paramorphism, `Foldable`, and `Comonad` to help developers choose the right operation

## Dependencies

- **Prerequisites**: Pattern data type must exist (✅ Complete - Feature 001)
- **Prerequisites**: Pattern must have Eq instance for testing (✅ Complete - Feature 002)
- **Prerequisites**: Pattern must have Show instance for debugging (✅ Complete - Feature 002)
- **Prerequisites**: Pattern must have Foldable instance (✅ Complete - Feature 006)
- **Prerequisites**: Pattern must have Comonad instance (✅ Complete - Feature 014)
- **No blocking dependencies**: This feature can be implemented independently, though understanding the relationship to `Foldable` and `Comonad` is important

## Notes

- Paramorphism extends `Foldable` to provide structure-aware folding, just as `Comonad` extends `Functor` to provide structure-aware transformation
- Patterns aren't just containers for values—the structure of the pattern *is information*. Paramorphism enables folding operations that can consider this structural information
- The relationship between paramorphism, `Foldable`, and `Comonad` is important: `Foldable` provides value-only folding, paramorphism provides structure-aware folding, and `Comonad` provides structure-aware transformation
- Examples should demonstrate structure-aware aggregations that are impossible with `Foldable` alone, such as depth-weighted sums, nesting-level statistics, and element-count-based aggregations
- Documentation should clearly explain when to use each operation: `Foldable` for value-only aggregation, paramorphism for structure-aware aggregation, and `Comonad` for structure-aware transformation
- Paramorphism enables structure-preserving transformations during fold operations, where the folding function can access the original pattern structure while computing aggregated results
- Context-dependent aggregations that adapt to structural properties (depth, element count, nesting level) are key use cases for paramorphism
- The signature `para :: (Pattern v -> [r] -> r) -> Pattern v -> r` provides the folding function with both the current pattern subtree and recursively computed results from children, enabling structure-aware folding
