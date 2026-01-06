# Design Documents

**Purpose**: Historical and aspirational design documents for gram-hs features

This directory contains design documents that describe features at various stages of implementation. Each document is marked with its implementation status.

## Status Legend

- **✅ Implemented** - Feature exists in code, see reference documentation for current spec
- **⏳ Planned** - Feature is prioritized for implementation
- **❌ Deferred** - Feature is not currently planned
- **📝 Design Only** - Design document, no implementation planned

## Design Documents Index

| Document | Status | Implementation | Reference |
|----------|--------|----------------|-----------|
| [DESIGN.md](../design/DESIGN.md) | ✅ Implemented | Core Pattern type, typeclass instances, Graph Lens | [Reference Documentation](../reference/SPECIFICATION.md) |
| [graph-lens.md](../design/graph-lens.md) | ✅ Implemented | Feature 23: `libs/pattern/src/Pattern/Graph.hs` | [Graph Lens Spec](../reference/features/graph-lens.md) |
| [SEMANTICS.md](../design/SEMANTICS.md) | ✅ Implemented | Gram serialization and parsing | [Gram Semantics](../reference/semantics/gram-semantics.md) |
| [EXTENDED-SEMANTICS.md](../design/EXTENDED-SEMANTICS.md) | ✅ Implemented | Gram pattern and path notation | [Gram Semantics](../reference/semantics/gram-semantics.md) |
| [gram-hs-cli-plan.md](../design/gram-hs-cli-plan.md) | ✅ Implemented | `apps/gram-hs-cli/` | [CLI README](../../apps/gram-hs-cli/README.md) |
| [pattern-matching-dsl-design.md](../design/pattern-matching-dsl-design.md) | ❌ Deferred | Predicate matching (Feature 9) provides similar functionality | See `TODO.md` |
| [pattern-category.md](../design/pattern-category.md) | 📝 Design Only | Not currently planned | Category-theoretic exploration |

## Location

Design documents are located in the `design/` directory at the repository root. This directory serves as the canonical location for all design documents (both implemented and aspirational). The `docs/design/` directory contains only this index README that provides navigation to the design documents.

## Reference Documentation

For current, authoritative specifications of implemented features, see **[Reference Documentation](../reference/README.md)**.

## For Library Users

If you're a Haskell developer using the library, see **[User Documentation](../users/README.md)** instead.
