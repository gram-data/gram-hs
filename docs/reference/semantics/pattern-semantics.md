# Pattern Semantics

**Status**: ✅ Implemented  
**Reference**: Core Pattern type semantics

## Core Concept

Patterns are **decorated sequences**: the elements form the pattern itself, and the value provides decoration about that pattern.

## Structural Classifications

### Atomic Pattern
A pattern with no elements (`elements == []`). Fundamental building blocks.

### Singular Pattern
A pattern with exactly one element (`length (elements p) == 1`).

### Pattern with Elements
A pattern containing one or more pattern elements in sequence.

### Nested Pattern
A pattern containing patterns that themselves contain patterns, enabling arbitrary nesting depth.

## Sequence Semantics

- Elements form the pattern sequence itself
- Sequence order is essential
- Each element is itself a Pattern
- Recursive nesting enables complex structures

## Value Semantics

- Value provides decoration about the pattern
- Value is not part of the pattern sequence
- All patterns in a structure share the same value type

## See Also

- **[Core Pattern](../features/core-pattern.md)** - Core type specification
- **[Architecture](../ARCHITECTURE.md)** - Design principles

