# Canonical JSON Format Specification

**Version**: 0.1.0  
**Status**: ✅ Implemented  
**Feature**: 029-canonical-json-pattern

## Overview

The canonical JSON format provides a standardized, deterministic JSON representation of `Pattern<Subject>` structures. This format enables:

- **Interoperability**: Seamless data exchange between implementations
- **Validation**: Formal JSON Schema for verification
- **Type Safety**: Generated TypeScript and Rust definitions
- **Testing**: Byte-for-byte comparison for equivalence checking

## Core Principles

1. **Determinism**: Identical structures produce identical JSON (sorted keys)
2. **Completeness**: All pattern features representable in JSON
3. **Simplicity**: Direct mapping between Haskell and JSON types
4. **Standards Compliance**: Valid JSON Schema Draft 2020-12

## Format Specification

### Pattern Structure

```json
{
  "value": <Subject>,
  "elements": [<Pattern>, ...]
}
```

**Fields**:
- `value`: Subject (required)
- `elements`: Array of nested Pattern objects (required, may be empty)

### Subject Structure

```json
{
  "symbol": <string>,
  "labels": [<string>, ...],
  "properties": {<string>: <Value>, ...}
}
```

**Fields**:
- `symbol`: Identity string (required, may be empty for anonymous subjects)
- `labels`: Array of label strings (required, sorted alphabetically in canonical form)
- `properties`: Map of property names to values (required, keys sorted alphabetically in canonical form)

### Value Types

Values use a discriminated union approach with 10 supported types:

#### Simple Types (No Discriminator)

**Integer**:
```json
42
```

**Decimal**:
```json
3.14
```

**Boolean**:
```json
true
```

**String**:
```json
"Hello, World!"
```

#### Complex Types (With Type Discriminator)

**Symbol**:
```json
{
  "type": "symbol",
  "value": "identifier"
}
```

**Tagged String**:
```json
{
  "type": "tagged",
  "tag": "json",
  "content": "{\"key\": \"value\"}"
}
```

**Range**:
```json
{
  "type": "range",
  "lower": 1.0,
  "upper": 10.0
}
```
*Note: `lower` and `upper` can be `null` for unbounded ranges*

**Measurement**:
```json
{
  "type": "measurement",
  "unit": "kg",
  "value": 5.0
}
```

**Array**:
```json
[1, 2, 3, "mixed", true]
```
*Arrays can contain any Value types (including nested arrays)*

**Map**:
```json
{
  "key1": "value1",
  "key2": 42
}
```
*Maps do NOT have a `type` field (this distinguishes them from complex types)*

## Examples

### Simple Pattern

**Gram**:
```
(person {name: "Alice", age: 30})
```

**JSON**:
```json
{
  "value": {
    "symbol": "person",
    "labels": [],
    "properties": {
      "age": 30,
      "name": "Alice"
    }
  },
  "elements": []
}
```

### Pattern with Labels

**Gram**:
```
(alice:Person:User {active: true})
```

**JSON**:
```json
{
  "value": {
    "symbol": "alice",
    "labels": ["Person", "User"],
    "properties": {
      "active": true
    }
  },
  "elements": []
}
```

### Nested Pattern

**Gram**:
```
[parent | child | grandchild]
```

**JSON**:
```json
{
  "value": {
    "symbol": "parent",
    "labels": [],
    "properties": {}
  },
  "elements": [
    {
      "value": {
        "symbol": "child",
        "labels": [],
        "properties": {}
      },
      "elements": [
        {
          "value": {
            "symbol": "grandchild",
            "labels": [],
            "properties": {}
          },
          "elements": []
        }
      ]
    }
  ]
}
```

### Complex Value Types

**Symbol Value**:
```json
{
  "value": {
    "symbol": "node",
    "labels": [],
    "properties": {
      "type": {
        "type": "symbol",
        "value": "identifier"
      }
    }
  },
  "elements": []
}
```

**Range Value**:
```json
{
  "value": {
    "symbol": "sensor",
    "labels": [],
    "properties": {
      "range": {
        "type": "range",
        "lower": 0.0,
        "upper": 100.0
      }
    }
  },
  "elements": []
}
```

**Measurement Value**:
```json
{
  "value": {
    "symbol": "reading",
    "labels": [],
    "properties": {
      "temperature": {
        "type": "measurement",
        "unit": "°C",
        "value": 23.5
      }
    }
  },
  "elements": []
}
```

**Array Value**:
```json
{
  "value": {
    "symbol": "data",
    "labels": [],
    "properties": {
      "numbers": [1, 2, 3, 4, 5],
      "mixed": [42, "text", true, {"nested": "map"}]
    }
  },
  "elements": []
}
```

**Map Value**:
```json
{
  "value": {
    "symbol": "config",
    "labels": [],
    "properties": {
      "settings": {
        "enabled": true,
        "timeout": 30,
        "endpoint": "https://example.com"
      }
    }
  },
  "elements": []
}
```

## Canonical Form

The canonical form enforces additional constraints for deterministic output:

1. **Sorted Keys**: All object keys sorted alphabetically
2. **Consistent Formatting**: 2-space indentation, Unix line endings
3. **No Trailing Whitespace**: Clean output
4. **Stable Metadata**: Fixed timestamp/hash for deterministic mode

### CLI Usage

**Generate Canonical JSON**:
```bash
gramref parse input.gram --format json --value-only --canonical
```

**Roundtrip Test**:
```bash
# Gram → JSON → Gram
echo '(person {name: "Alice"})' | gramref parse --format json --value-only | \
  gramref convert --from json --to gram
```

## JSON Schema

A formal JSON Schema (Draft 2020-12) is available:

```bash
gramref schema --format json-schema > pattern-schema.json
```

This schema can be used to:
- Validate JSON output from implementations
- Generate types in other languages
- Document the canonical structure

## Type Definitions

Type-safe definitions are available for TypeScript and Rust:

**TypeScript**:
```bash
gramref schema --format typescript > pattern.ts
```

**Rust**:
```bash
gramref schema --format rust > pattern.rs
```

## Implementation Notes

### Number Representation

JSON does not distinguish between integers and decimals that are whole numbers. For example:
- `2.0` serializes as `2`
- `-1.0` serializes as `-1`

Implementations should handle this by:
- Using semantic equivalence for comparison
- Accepting both integer and decimal representations during parsing

### Empty Collections

- Empty arrays: `[]`
- Empty objects: `{}`
- Empty labels: `[]`
- Empty properties: `{}`
- Anonymous subject: `{"symbol": "", ...}`

### Special Characters

Strings are JSON-escaped following standard rules:
- Quotes: `\"`
- Newlines: `\n`
- Backslashes: `\\`
- Unicode: `\uXXXX`

## Testing

The canonical JSON format is validated through:

- **35+ Unit Tests**: Covering all value types and patterns
- **200 QuickCheck Properties**: Random pattern generation and roundtrip testing
- **Semantic Equivalence**: Handling integer/decimal ambiguity
- **Corpus Tests**: Validation against tree-sitter-gram test corpus

## See Also

- **[Gram Serialization](gram-serialization.md)** - Overall serialization features
- **[JSON Schema Specification](../../specs/029-canonical-json-pattern/contracts/json-schema.json)** - Formal schema
- **[TypeScript Types](../../specs/029-canonical-json-pattern/contracts/typescript-types.ts)** - Type definitions
- **[Rust Types](../../specs/029-canonical-json-pattern/contracts/rust-types.rs)** - Struct definitions
