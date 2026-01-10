# gramref CLI

The `gramref` CLI serves as the reference implementation and conformance testing tool for the gram/pattern ecosystem. It provides canonical outputs for pattern operations, generates test suites, and enables validation of ports to other languages.

## Building

From the project root:

```bash
cabal build gramref-cli
```

Or to install:

```bash
cabal install gramref-cli
```

## Commands

### Parse

Parse gram notation and output canonical representation:

```bash
gramref parse input.gram [--format json|gram|debug] [--value-only] [--deterministic] [--canonical]
```

Reads from stdin if no file is provided:

```bash
cat input.gram | gramref parse
```

**Output Options:**
- `--value-only`: Output only the pattern value without metadata wrapper. Useful for direct comparison.
- `--deterministic`: Use fixed timestamp and hash for deterministic output. Automatically enables `--canonical`.
- `--canonical`: Sort JSON keys alphabetically at all nesting levels for reliable comparison.

**Examples:**
```bash
# Value-only output for comparison
gramref parse input.gram --value-only

# Deterministic output for testing
gramref parse input.gram --deterministic

# All flags combined
gramref parse input.gram --value-only --deterministic --canonical
```

### Match

Execute pattern matching and output bindings:

```bash
gramref match <pattern-file> <data-file> [--format json|table|count]
```

### Transform

Apply pattern transformations:

```bash
gramref transform <operation> <input-file> [--format json|gram|debug]
```

Operations: `fold`, `map`, `filter`, `reverse`, `flatten`, `normalize`

### Generate

Generate test data and patterns:

```bash
gramref generate [--type TYPE] [--count N] [--seed S] [--complexity LEVEL] [--format FORMAT] [--value-only] [--deterministic] [--canonical]
```

Generator types: `pattern` (default), `graph`, `suite`, `property`

**Test Suite Generation:**
The `--type suite` option generates test cases in the test suite format specification:

```bash
# Generate 10 test cases with seed 42
gramref generate --type suite --count 10 --seed 42

# Generate test suite with specific complexity
gramref generate --type suite --count 5 --complexity standard --seed 100

# Generate with canonical JSON output
gramref generate --type suite --count 3 --seed 42 --canonical
```

**Complexity Levels:**
- `minimal`: Simple atomic patterns
- `basic`: Patterns with optional labels (default)
- `standard`: Patterns with labels and properties
- `complex`: Patterns with labels, properties, and nested elements (1-2 levels)
- `adversarial`: Patterns with labels, properties, and deeper nesting (1-3 levels)

**Examples:**
```bash
# Generate 10 test patterns
gramref generate --type pattern --count 10 --seed 42

# Generate test suite for port testing
gramref generate --type suite --count 50 --complexity complex --seed 42 > test_suite.json
```

### Validate

Run conformance test suites:

```bash
gramref validate <test-suite> [--runner external-command]
```

### Convert

Convert between different representations:

```bash
gramref convert <input-file> --from <format> --to <format>
```

Supported formats: `gram`, `json`, `cypher`, `dot`, `mermaid`

**Examples:**
```bash
# Gram → JSON
gramref convert pattern.gram --from gram --to json

# JSON → Gram (roundtrip)
gramref convert pattern.json --from json --to gram
```

### Schema

Generate JSON Schema or type definitions for Pattern<Subject>:

```bash
gramref schema [--format json-schema|typescript|rust]
```

**Formats:**
- `json-schema`: JSON Schema Draft 2020-12 (default)
- `typescript`: TypeScript type definitions with interfaces and type guards
- `rust`: Rust struct definitions with serde annotations

**Examples:**
```bash
# Generate JSON Schema
gramref schema --format json-schema > pattern-schema.json

# Generate TypeScript types
gramref schema --format typescript > pattern.ts

# Generate Rust types
gramref schema --format rust > pattern.rs
```

**Use Cases:**
- **Validation**: Validate JSON output from implementations against the schema
- **Type Safety**: Generate type-safe code for TypeScript/Rust ports
- **Documentation**: Formal specification of the canonical JSON format
- **Interoperability**: Share schema with downstream projects

## Output Formats

### JSON (Canonical)

All JSON output follows strict rules for determinism with metadata including version, command, timestamp, and hash.

**Output Options:**
- `--value-only`: Output only the result value without metadata wrapper (Meta, Result, Diagnostics). Useful for direct comparison of pattern values.
- `--deterministic`: Ensure deterministic output by using fixed timestamp (1970-01-01T00:00:00+0000) and fixed hash (all zeros). Automatically enables `--canonical`.
- `--canonical`: Produce JSON with sorted keys at all nesting levels. Ensures equivalent data structures produce byte-for-byte identical JSON strings.

**Example Output (default):**
```json
{
  "Meta": {
    "Version": "0.1.0",
    "Command": "parse",
    "Timestamp": "2025-12-27T21:12:24.2086+0000",
    "Hash": "..."
  },
  "Result": {
    "Type": "Pattern",
    "Value": { ... }
  }
}
```

**Example Output (--value-only):**
```json
{
  "value": { ... },
  "elements": [ ... ]
}
```

### Gram

Pretty-printed gram notation.

### Debug

Haskell Show instance output for debugging.

## Exit Codes

- `0` - Success
- `1` - Parse error
- `2` - Validation failure
- `3` - Invalid arguments
- `4` - IO error
- `5` - Timeout/resource limit

## Use Cases

### Language Port Testing

The CLI tool is designed to facilitate testing ports to other languages (e.g., gram-rs):

**1. Generate Test Suites:**
```bash
# Generate comprehensive test suite
gramref generate --type suite --count 100 --complexity complex --seed 42 > test_suite.json
```

**2. Compare Outputs:**
```bash
# Get deterministic, value-only output from reference implementation
gramref parse input.gram --value-only --deterministic > expected.json

# Compare with port implementation output
diff expected.json port_output.json
```

**3. Automated Testing:**
```bash
# Generate test cases and validate against port
gramref generate --type suite --count 10 --seed 42 | \
  gramref validate --runner "cargo run --bin gram-rs"
```

### Deterministic Output for Testing

Use `--deterministic` flag to ensure identical outputs across runs:

```bash
# Same seed produces identical output
gramref parse input.gram --deterministic --seed 42
gramref parse input.gram --deterministic --seed 42  # Identical output
```

### Canonical JSON for Comparison

Use `--canonical` flag to ensure sorted keys for reliable comparison:

```bash
# Canonical output with sorted keys
gramref parse input.gram --canonical

# Combined with value-only for clean comparison
gramref parse input.gram --value-only --canonical
```

