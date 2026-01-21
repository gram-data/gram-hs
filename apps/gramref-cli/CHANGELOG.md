# Changelog

All notable changes to the gramref CLI will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.2.0] - 2026-01-21

### Breaking
- **JSON output**: Parse/convert results now use `Result.Type` `"PatternList"` and `Result.Value` as a JSON array of patterns instead of a single `"Pattern"` object. Affects `parse`, `convert`, `match`, and `transform` when output is JSON.
- **gram dependency**: Requires `gram >=0.3.0` for the container-aware parsing/serialization API.

### Changed
- All commands now operate on `[Pattern Subject]` to align with gram’s `fromGram`/`toGram` list-based API.

## [1.1.0] - Previous

### Added
- Initial multi-command CLI (parse, convert, match, transform, validate, generate, schema).
