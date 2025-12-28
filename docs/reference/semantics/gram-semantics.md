# Gram Notation Semantics

**Status**: ✅ Implemented  
**Reference**: Gram serialization and parsing

## Overview

Gram notation is a text format for representing `Pattern Subject` structures. It supports both pattern notation and path notation.

## Pattern Notation

### Definition Rule
**Brackets create definitions, bare identifiers create references**

- `[...]` always defines a pattern (anonymous or identified)
- A bare identifier always references an existing pattern
- Each identified pattern can only be defined once

### Examples

```
[a]                 // Defines pattern 'a' (empty)
[a {k:"v"}]        // Defines pattern 'a' with properties
[a:Label]          // Defines pattern 'a' with a label

[b | a]            // Defines 'b', references 'a'
[b | [a]]          // Defines both 'b' and 'a'
```

## Path Notation

### Basic Syntax
Paths consist of nodes connected by relationships:

```
(a)                    // Node 'a'
(a:Person)             // Node 'a' with label 'Person'
(a)-[r]->(b)           // Relationship 'r' from 'a' to 'b'
(a)-[:knows]->(b)      // Anonymous relationship with label 'knows'
```

### First-Appearance Definition
In path notation, the first appearance defines an element.

### Direction Matters
Path relationships map to pattern notation's left-to-right ordering:
- `(a)-[r]->(b)` translates to `[r | a, b]`
- `(a)<-[r]-(b)` translates to `[r | b, a]`

## Semantic Constraints

### Single Definition
An identified pattern can only be defined once within a file.

### Immutability
Once defined, a pattern's structure, labels, and properties cannot be changed.

### No Direct Self-Reference
A pattern cannot contain itself as a direct element.

### Forward References
References to patterns defined later in the file are allowed.

## See Also

- **[Gram Serialization](../features/gram-serialization.md)** - Implementation specification
- **[Extended Semantics](../../design/EXTENDED-SEMANTICS.md)** - Extended semantics (historical)

