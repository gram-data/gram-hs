# Data Model: Pattern Structure (Authoritative Definition)

**Feature**: 003-pattern-structure-review  
**Date**: 2025-01-27  
**Status**: Authoritative Reference

This document establishes the authoritative definition of the Pattern data structure and related concepts. All other documentation should align with these definitions.

---

## Core Entity: Pattern

### Definition

A **Pattern** is a sequence of elements with associated metadata. While implemented using a recursive tree structure, the primary semantic is sequence-based. The structure stores a value and contains zero or more Pattern elements. The tree implementation supports the sequence semantics, but the conceptual model emphasizes sequences.

### Structure

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
  deriving (Eq)
```

**Note**: `Show` is implemented as a manual instance. `Functor`, `Foldable`, and `Traversable` are planned but not yet implemented (see Implementation Status section).

### Fields

- **value** (`v`): The value associated with this pattern instance. The value field stores data of any type and is associated with the pattern sequence itself, not with individual elements in the sequence. Type parameter `v` allows for different value types.

- **elements** (`[Pattern v]`): The elements contained within a pattern, forming the sequence structure. Each element in the sequence is itself a Pattern, enabling recursive nesting while maintaining the sequence semantic. An empty list `[]` represents a leaf pattern (a sequence with no elements). A non-empty list represents a pattern containing one or more pattern elements in sequence.

### Conceptual Model: Sequence-Based

**Primary Semantic**: Patterns are conceptually sequences of elements.

- Each pattern represents a sequence where the `value` is metadata about the sequence
- The `elements` field contains the sequence itself
- Elements maintain their sequence order
- Each element in the sequence is itself a Pattern, enabling nested sequences

**Example**: The pattern "3 1 4 1 9 5" is conceptually a sequence of 6 elements, where "3 1 4 1 9 5" might be the metadata (value) describing the sequence.

### Implementation Model: Recursive Tree

**Implementation Detail**: Patterns are implemented as recursive trees.

- The tree structure supports the sequence semantics
- Each pattern node stores a value and contains child patterns
- The recursive structure enables arbitrary nesting depth
- Tree traversal provides access to sequence elements

**Relationship**: The tree implementation is how sequences are represented in memory. The sequence semantic is what developers should think about when using patterns. The tree structure supports sequence operations (ordering, length, access by position).

### Type Constraints

- `Pattern` is parameterized over value type `v`
- All patterns in a structure must share the same value type `v` (enforced by type system)
- Pattern structures must be finite (no infinite recursion)
- Type consistency is enforced by Haskell's type system

### Relationships

- **Self-referential**: Each `Pattern` contains zero or more Pattern elements
- **Recursive**: The sequence structure can be arbitrarily deep (patterns containing patterns containing patterns, etc.)
- **Hierarchical**: Elements form a sequence hierarchy with the parent pattern as the sequence container

---

## Pattern Variants

Pattern variants are **structural classifications** based on their element structure that can be **interpreted through different graph views**. Variants are determined by the structure of elements, and views provide different semantic interpretations of those structures.

### Leaf Pattern

A pattern with no elements (`elements == []`).

**Structure**: Empty sequence
**Status**: ✅ Implemented (this is the basic Pattern structure)

**Example**:
```haskell
leafPattern :: Pattern String
leafPattern = Pattern { value = "node1", elements = [] }
```

### Node

A pattern interpreted as a **node** when it has no child elements that are graph elements themselves. Typically, this means `elements == []` (a leaf pattern).

**Structure**: Empty sequence (leaf pattern)
**Status**: ⏳ Planned (classification function `isNode` not yet implemented)

**Validation** (planned): `isNode :: Pattern v -> Bool`

### Relationship

A pattern interpreted as a **relationship** when it has exactly 2 child elements, and both child elements are nodes (leaf patterns).

**Structure**: Exactly 2 elements, both are leaf patterns
**Status**: ⏳ Planned (classification function `isRelationship` not yet implemented)

**Validation** (planned): `isRelationship :: Pattern v -> Bool`

**Constraints** (planned):
- `length (elements p) == 2`
- `all isNode (elements p)`

**Example** (structure exists, classification function planned):
```haskell
nodeA = Pattern { value = "A", elements = [] }
nodeB = Pattern { value = "B", elements = [] }
relationship = Pattern { value = "knows", elements = [nodeA, nodeB] }
```

### Subgraph

A pattern interpreted as a **subgraph** when all child elements are graph elements (nodes, relationships, or other subgraphs).

**Structure**: All elements are graph elements
**Status**: ⏳ Planned (classification function `isSubgraph` not yet implemented)

**Validation** (planned): `isSubgraph :: Pattern v -> Bool`

**Constraints** (planned):
- `all isGraphElement (elements p)`

### Path

A pattern interpreted as a **path** when it is a subgraph and all relationships in the path chain correctly (target of one equals source of next).

**Structure**: Subgraph with chained relationships
**Status**: ⏳ Planned (classification function `isPath` not yet implemented)

**Validation** (planned): `isPath :: Pattern v -> Bool`

**Constraints** (planned):
- `isSubgraph p`
- `chainsCorrectly (elements p)`

---

## Graph Views

Graph views provide different semantic interpretations of pattern structures. Views are planned but not yet implemented.

### GraphView Typeclass (Planned)

```haskell
class GraphView view where
  type Direction view :: *
  
  interpretNode :: view -> Pattern v -> Bool
  interpretRel  :: view -> Pattern v -> Bool
  
  direction :: view -> Pattern v -> Direction view
  canChain  :: view -> Pattern v -> Pattern v -> Bool
  
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

**Status**: ⏳ Planned (not yet implemented)

### Standard Views (Planned)

- **DirectedView**: Relationships have explicit direction (source → target)
- **UndirectedView**: Relationships are undirected (edge set)

**Status**: ⏳ Planned (not yet implemented)

---

## Typeclass Instances

### Implemented

- **Eq**: ✅ Implemented (`deriving Eq`)
- **Show**: ✅ Implemented (manual instance)

### Planned

- **Functor**: ⏳ Planned (TODO.md Feature 4) - Enables value transformation while preserving structure
- **Foldable**: ⏳ Planned (TODO.md Feature 5) - Enables aggregation over pattern values
- **Traversable**: ⏳ Planned (TODO.md Feature 6) - Enables effectful traversal

---

## Terminology Standards

### Primary Terms

- **Pattern**: The data structure representing a sequence of elements
- **value**: The value field storing metadata about the pattern sequence
- **elements**: The elements field containing the sequence of pattern elements
- **sequence**: The conceptual model (primary semantic)
- **tree**: The implementation model (supporting detail)

### Avoid These Terms

- ❌ "metadata" (use "value")
- ❌ "children" (use "elements")
- ❌ "child patterns" (use "elements")
- ❌ "tree" as primary model (use "sequence" as primary, "tree" as implementation)

---

## Validation Rules

### Pattern Structure

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v` (enforced by type system)
3. **Structure integrity**: Field accessors must always return valid values

### Pattern Variant Classification (Planned)

1. **Mutual exclusivity**: A pattern can be classified as node, relationship, subgraph, or path, but not multiple simultaneously (when classification functions are implemented)
2. **Hierarchical**: Classification depends on child element structure
3. **Recursive**: Classification applies recursively to child elements

---

## State Transitions

### Pattern Construction

- **Leaf pattern creation**: `Pattern v []` - creates a pattern with value `v` and empty element list
- **Pattern with elements creation**: `Pattern v [p1, p2, ...]` - creates a pattern with value `v` and element patterns `p1, p2, ...`

### Pattern Inspection

- **Value access**: `value pattern` - retrieves the value stored in the pattern
- **Elements access**: `elements pattern` - retrieves the list of element patterns

---

## Design Principles

1. **Sequence-Based Semantics**: Patterns are conceptually sequences; tree structure is implementation detail
2. **Schema-Lazy**: Patterns don't commit to specific graph semantics; interpretation happens in views (when implemented)
3. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns (when implemented)
4. **Open-ended**: New views can be defined for any graph-like interpretation (when implemented)
5. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition (when implemented)

---

## Implementation Status Summary

| Component | Status | Notes |
|-----------|--------|-------|
| Pattern data type | ✅ Implemented | Basic structure with value and elements |
| Eq instance | ✅ Implemented | `deriving Eq` |
| Show instance | ✅ Implemented | Manual instance |
| Functor instance | ⏳ Planned | TODO.md Feature 4 |
| Foldable instance | ⏳ Planned | TODO.md Feature 5 |
| Traversable instance | ⏳ Planned | TODO.md Feature 6 |
| Classification functions | ⏳ Planned | TODO.md Features 8-11 |
| Navigation functions | ⏳ Planned | TODO.md Feature 12 |
| GraphView typeclass | ⏳ Planned | TODO.md Feature 15 |
| Graph operations | ⏳ Planned | TODO.md Feature 15 |

---

## References

- **Implementation**: `src/Pattern/Core.hs`
- **Design Documentation**: `DESIGN.md`
- **Project Overview**: `README.md`
- **Implementation Roadmap**: `TODO.md`

