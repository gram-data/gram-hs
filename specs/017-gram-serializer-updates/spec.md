# Feature Specification: Gram Serializer Updates (Path Support)

**Feature Branch**: `017-gram-serializer-updates`
**Created**: 2025-11-28
**Status**: Draft
**Input**: User description: "update serialization to support output of path notations when appropriate (the Pattern is a valid path)"

## User Scenarios & Testing

### User Story 1 - Serialize Single Edge Patterns (Priority: P1)

As a developer, I want the serializer to produce standard path notation `(a)-[r]->(b)` when it encounters a Pattern structure representing a single edge, so that the output is more readable and idiomatic than the raw bracket notation `[r | a, b]`.

**Why this priority**: This is the fundamental unit of graph structure in Gram. Without this, graphs look like lists of lists.

**Independent Test**: Create an edge pattern `[r | a, b]`, serialize it, and assert the output matches `(a)-[r]->(b)` (ignoring whitespace).

**Acceptance Scenarios**:

1. **Given** a Pattern with structure `[r | a, b]` (relationship `r` and two node elements `a`, `b`), **When** `toGram` is called, **Then** the output string is `(a)-[r]->(b)`.
2. **Given** a Pattern `[r | a, b]` where `r` is empty/anonymous, **When** `toGram` is called, **Then** the output is `(a)-->(b)`.
3. **Given** a Pattern `[r | a, b]` where `a` and `b` are identified nodes, **When** `toGram` is called, **Then** the output uses their references or definitions correctly.

---

### User Story 2 - Serialize Walk Patterns (Priority: P2)

As a developer, I want the serializer to produce chained path notation `(a)-[r1]->(b)-[r2]->(c)` when it encounters a Pattern structure representing a walk (sequence of connected edges), so that complex paths are readable.

**Why this priority**: Walks are the natural way to express connectivity in Gram.

**Independent Test**: Create a walk pattern containing two connected edges, serialize it, and assert the output is a single chained string.

**Acceptance Scenarios**:

1. **Given** a Walk Pattern `[walk | [r1 | a, b], [r2 | b, c]]`, **When** `toGram` is called, **Then** the output is `(a)-[r1]->(b)-[r2]->(c)`.
2. **Given** a Walk Pattern with disconnected edges (e.g. `a->b` and `c->d`), **When** `toGram` is called, **Then** it falls back to standard list/bracket notation or separate statements (depending on implementation constraints).

---

### User Story 3 - Round-Trip Correctness (Priority: P1)

As a developer, I want to ensure that parsing a serialized path results in the exact same Pattern structure as the original, satisfying the property `parse . serialize . parse == parse`.

**Why this priority**: Semantic stability is critical for data integrity. Normalizing the text representation is fine, but the structure must be preserved.

**Independent Test**: Run the full test suite including `Spec.Gram.ParseSpec` and round-trip tests on the corpus.

**Acceptance Scenarios**:

1. **Given** any valid path input from the corpus (e.g. `(a)-[r]->(b)`), **When** it is parsed to `P1`, then serialized to `S`, then parsed again to `P2`, **Then** `P1` must equal `P2`.
2. **Given** an input in bracket notation that represents an edge `[r | (a), (b)]`, **When** it is parsed to `P1`, serialized to `S` (which should be path notation), and parsed back to `P2`, **Then** `P1` must equal `P2`.

---

### Edge Cases

- **Non-Edge Triplets**: `[r | a, b, c]` should NOT be serialized as an edge (too many elements).
- **Single Element**: `[r | a]` should NOT be serialized as an edge (too few elements).
- **Reverse Edges**: Since the Pattern structure is directed, `(b)<-[r]-(a)` typically parses to `[r | a, b]`. The serializer will likely normalize this to `(a)-[r]->(b)`. This is acceptable behavior.
- **Anonymous Nodes**: `()-->()` should be handled correctly.

## Requirements

### Functional Requirements

- **FR-001**: The serializer MUST identify Pattern structures that conform to the "Edge Pattern" definition: a pattern with exactly two elements (source and target nodes).
- **FR-002**: The serializer MUST identify Pattern structures that conform to the "Walk Pattern" definition: a specific container pattern holding a sequence of connected Edge Patterns.
- **FR-003**: When serializing an Edge Pattern, the output MUST use Gram path syntax `(source)-[relationship]->(target)`.
- **FR-004**: When serializing a Walk Pattern, the output MUST use chained Gram path syntax `(n1)-[r1]->(n2)-[r2]->(n3)`.
- **FR-005**: The serializer MUST default to the standard arrow `-->` (or `-[...]->`) unless specific metadata indicates otherwise (note: currently arrow type is lost, so default is expected).
- **FR-006**: The serializer MUST fall back to standard bracket notation `[...]` for any pattern that does not strictly meet Edge or Walk criteria.

### Key Entities

- **Pattern**: The core data structure (`Pattern v`).
- **Edge Pattern**: `Pattern r [a, b]`.
- **Walk Pattern**: `Pattern w [edge1, edge2, ...]`.

## Success Criteria

### Measurable Outcomes

- **SC-001**: 100% of valid path inputs in the corpus pass the round-trip test (`parse . serialize . parse == parse`).
- **SC-002**: Edge Patterns are serialized as strings starting with `(` and containing `->`.
- **SC-003**: Zero regressions in existing non-path serialization tests.

## Assumptions

- The parser's definition of Edge and Walk patterns (as implemented in `Gram.Transform`) is the source of truth for what constitutes these structures.
- Normalization of arrow types (e.g., `==>` becoming `-->`) is acceptable for now as the distinct types are not preserved in the Pattern model.
