# Reference Documentation

**Audience**: Developers porting gram-hs to other languages

This directory contains authoritative reference documentation for language porters. This documentation focuses on architecture, specifications, implementation patterns, and porting guidance.

## Quick Navigation

- **[Porting Guide](PORTING-GUIDE.md)** - ⭐ Start here! Implementation roadmap and dependencies
- **[Architecture](ARCHITECTURE.md)** - Core design principles and category-theoretic foundations
- **[Specification](SPECIFICATION.md)** - Authoritative feature specifications with current implementation status
- **[Implementation](IMPLEMENTATION.md)** - Implementation patterns and language-agnostic algorithms

## Documentation Structure

### Core Reference Documents

- **[PORTING-GUIDE.md](PORTING-GUIDE.md)** - Complete implementation roadmap with phases, dependencies, and CLI tool integration
- **[ARCHITECTURE.md](ARCHITECTURE.md)** - Design principles, category theory foundations, key design decisions
- **[SPECIFICATION.md](SPECIFICATION.md)** - Current feature specifications, implementation status, API contracts
- **[IMPLEMENTATION.md](IMPLEMENTATION.md)** - Implementation patterns, testing strategies, common pitfalls

### Feature Specifications

See `features/` directory for feature-by-feature reference documentation:
- **[core-pattern.md](features/core-pattern.md)** - Core Pattern type specification
- **[typeclass-instances.md](features/typeclass-instances.md)** - Typeclass instances specification
- **[graph-lens.md](features/graph-lens.md)** - Graph Lens specification (Feature 23)
- **[gram-serialization.md](features/gram-serialization.md)** - Gram serialization specification

### Semantic Specifications

See `semantics/` directory for semantic specifications:
- **[pattern-semantics.md](semantics/pattern-semantics.md)** - Pattern semantics
- **[graph-interpretation.md](semantics/graph-interpretation.md)** - Graph interpretation semantics
- **[gram-semantics.md](semantics/gram-semantics.md)** - Gram notation semantics

## For Library Users

If you're a Haskell developer using the library, see **[User Documentation](../users/README.md)** instead.

## Design Documents

For historical design context, see **[Design Documents](../design/README.md)**.

