# Gram Serialization Feature Specification

**Status**: ✅ Implemented  
**Location**: `libs/gram/src/Gram/`  
**Features**: Serialization, Parsing, Validation (Features 14, 16, 20)

## Overview

Gram serialization converts `Pattern Subject` structures to/from gram notation text format. The library handles all value types, nested patterns, relationships, and anonymous subjects.

## Serialization

### `toGram` - Serialize to Gram Notation

```haskell
toGram :: Pattern Subject -> String
```

Converts a `Pattern Subject` to gram notation string.

**Handles**:
- All value types (standard and extended)
- Nested patterns
- Relationships
- Anonymous subjects
- Labels and properties

## Parsing

### `fromGram` - Parse from Gram Notation

```haskell
fromGram :: String -> Either ParseError (Pattern Subject)
```

Parses gram notation to `Pattern Subject`.

**Supports**:
- Standard and extended values
- Relationships
- Nesting
- Pattern and path notation

## Validation

### `Gram.Validate` Module

```haskell
validate :: Gram -> Either ValidationError Gram
```

**Checks**:
- Duplicate definition checking
- Undefined reference checking
- Arity consistency checking

## Round-Trip Verification

Validated structural equality after serialization/deserialization cycles against the full test corpus (Feature 20).

## Test Coverage

- **Parsing Conformance**: 100% pass rate against `tree-sitter-gram` corpus (Feature 16)
- **Round-Trip Tests**: Validated against full test corpus
- **Validation Tests**: Comprehensive validation rule tests

## See Also

- **[Gram Semantics](../semantics/gram-semantics.md)** - Semantic specification
- **[Subject Library](subject.md)** - Subject type specification
- **[Implementation](../IMPLEMENTATION.md)** - Implementation patterns

