# Changelog

All notable changes to the gram library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-01-10

### Added
- **Gram.JSON module**: Canonical JSON serialization/deserialization for Pattern<Subject>
  - ToJSON/FromJSON instances for Pattern, Subject, and all Value types
  - Bidirectional conversion between Gram notation and JSON
  - Canonical form with deterministic output (sorted keys)
  - Support for all 10 value types (integer, decimal, boolean, string, symbol, tagged, array, map, range, measurement)
  - Metadata wrapper support (version, timestamp, hash)
- **Gram.Schema module**: Schema and type definition generation
  - Gram.Schema.JSONSchema: JSON Schema Draft 2020-12 generation
  - Gram.Schema.TypeScript: TypeScript type definitions with interfaces and type guards
  - Gram.Schema.Rust: Rust struct definitions with serde derives
- **Comprehensive test suite**:
  - 35+ JSON roundtrip unit tests
  - 200 QuickCheck property-based tests
  - 17 schema generation tests
  - Semantic equivalence checking for integer/decimal ambiguity

### Changed
- Gram.Serialize and Gram.Parse remain backward compatible

## [0.1.2.0] - Previous

### Added
- Initial library structure
- Gram.Serialize module for serializing Pattern Subject to gram notation
- Gram.Parse module for parsing gram notation to Pattern Subject

