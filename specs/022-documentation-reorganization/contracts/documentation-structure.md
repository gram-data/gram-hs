# Documentation Structure Contract

**Feature**: 022-documentation-reorganization  
**Date**: 2025-01-27  
**Type**: Documentation Structure Specification

## Overview

This contract defines the required structure and organization of documentation to serve two distinct audiences: library users and language porters.

## Documentation Hierarchy

### Required Structure

```
docs/
├── users/                    # User-facing documentation
│   ├── README.md            # REQUIRED: User entry point
│   ├── api/                 # OPTIONAL: API reference
│   ├── guides/              # REQUIRED: Usage guides
│   └── examples/            # OPTIONAL: Code examples
│
├── reference/               # Porter-facing documentation
│   ├── README.md            # REQUIRED: Reference index
│   ├── PORTING-GUIDE.md     # REQUIRED: Implementation roadmap
│   ├── ARCHITECTURE.md      # REQUIRED: Design principles
│   ├── SPECIFICATION.md     # REQUIRED: Feature specifications
│   ├── IMPLEMENTATION.md   # REQUIRED: Implementation patterns
│   ├── semantics/           # OPTIONAL: Semantic specs
│   └── features/            # REQUIRED: Feature-by-feature reference
│
├── design/                  # Design documents
│   ├── README.md            # REQUIRED: Design doc index
│   ├── implemented/         # OPTIONAL: Implemented features
│   └── aspirational/       # OPTIONAL: Future features
│
└── history/                 # Historical artifacts
    └── specs/               # OPTIONAL: Historical specs
```

## Entry Points

### User Documentation Entry Point

**File**: `docs/users/README.md`

**Required Content**:
- Installation instructions
- Quick start examples
- Links to usage guides
- Links to API reference
- Navigation to examples

**Must NOT Include**:
- Implementation details
- Porting information
- Design rationale
- Internal architecture

### Reference Documentation Entry Points

**Primary**: `docs/reference/PORTING-GUIDE.md`
- Implementation roadmap
- Phase dependencies
- Testing strategy
- Language considerations

**Secondary**: `docs/reference/README.md`
- Reference documentation index
- Links to architecture, specification, implementation
- Navigation to feature docs

## Status Markers

### Design Document Status Format

All design documents MUST include status header:

```markdown
**Status**: ✅ Implemented (Feature N) | ⏳ Planned | ❌ Deferred | 📝 Design Only
**Reference**: See `docs/reference/features/[feature].md` for current spec
```

### Status Values

- **✅ Implemented**: Feature exists in code, link to reference doc required
- **⏳ Planned**: Feature is prioritized for implementation
- **❌ Deferred**: Feature is not currently planned
- **📝 Design Only**: Design document, no implementation planned

## Link Requirements

### Internal Links

- Use relative paths: `docs/reference/ARCHITECTURE.md`
- Consistent format: `docs/[section]/[file].md`
- No absolute URLs for internal docs

### Cross-References

- User docs → Reference docs: Only for advanced topics
- Reference docs → Design docs: For historical context
- Design docs → Reference docs: If feature is implemented
- Historical specs → Reference docs: If feature is implemented

## Navigation Requirements

### User Documentation Navigation

- Clear hierarchy: README → Guides → Examples
- No dead ends: Every page has navigation
- Breadcrumbs: Show current location
- Search: Link to search functionality (if available)

### Reference Documentation Navigation

- Clear hierarchy: README → Porting Guide → Features
- Cross-links: Architecture ↔ Specification ↔ Implementation
- Feature index: List all features with status
- Quick links: Common tasks (testing, porting, architecture)

## Content Requirements

### User Documentation Content

- **Must Include**: Usage examples, API reference, installation
- **Must Exclude**: Implementation details, porting info
- **Format**: Markdown with code examples
- **Tone**: User-friendly, task-oriented

### Reference Documentation Content

- **Must Include**: Architecture, specifications, implementation patterns
- **Must Include**: Current implementation status
- **Format**: Markdown with formal definitions
- **Tone**: Technical, authoritative

### Design Document Content

- **Must Include**: Status marker, historical context
- **Must Include**: Link to reference doc (if implemented)
- **Format**: Preserve original design document
- **Tone**: Design rationale, conceptual

## Validation Rules

### Structure Validation

1. ✅ All required entry points exist
2. ✅ All required directories exist
3. ✅ Status markers present on design docs
4. ✅ Links are valid and relative
5. ✅ Navigation is complete

### Content Validation

1. ✅ User docs exclude implementation details
2. ✅ Reference docs include current status
3. ✅ Design docs have status markers
4. ✅ Historical artifacts are marked
5. ✅ Porting guide includes CLI tool docs

## Compliance

This contract MUST be satisfied for the documentation reorganization feature to be considered complete.

Violations:
- Missing required entry points → Feature incomplete
- Missing status markers → Feature incomplete
- Broken links → Feature incomplete
- User docs contain implementation details → Feature incomplete

