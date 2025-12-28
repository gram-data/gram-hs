# Data Model: Documentation Reorganization

**Feature**: 022-documentation-reorganization  
**Date**: 2025-01-27

## Core Entities

### User Documentation

**Definition**: Documentation structure focused on Haskell library users, containing installation, quick start, API reference, and usage guides without implementation details.

**Structure**:
- **Location**: `docs/users/`
- **Entry Point**: `docs/users/README.md`
- **Sections**:
  - `api/` - API reference (or links to generated docs)
  - `guides/` - Usage guides with examples
  - `examples/` - Working code examples

**Key Attributes**:
- **Audience**: Haskell developers using the library
- **Focus**: Usage, examples, API reference
- **Excludes**: Implementation details, porting information, design rationale
- **Format**: Markdown files with code examples

**Validation Rules**:
- Must not reference implementation details
- Must not include porting information
- Must include working code examples
- Must be accessible without understanding internals

**Relationships**:
- Links to reference documentation for advanced topics (when needed)
- References generated API documentation
- Contains usage guides organized by feature

### Reference Documentation

**Definition**: Documentation structure focused on language porters, containing architecture, specifications, implementation guides, and porting roadmap.

**Structure**:
- **Location**: `docs/reference/`
- **Entry Point**: `docs/reference/README.md` and `docs/reference/PORTING-GUIDE.md`
- **Sections**:
  - `ARCHITECTURE.md` - Core design principles
  - `SPECIFICATION.md` - Authoritative feature spec
  - `IMPLEMENTATION.md` - Implementation patterns
  - `PORTING-GUIDE.md` - Implementation roadmap
  - `semantics/` - Semantic specifications
  - `features/` - Feature-by-feature reference

**Key Attributes**:
- **Audience**: Developers porting to other languages
- **Focus**: Architecture, specifications, implementation patterns
- **Includes**: Design rationale, category-theoretic foundations, implementation guidance
- **Format**: Markdown files with formal definitions

**Validation Rules**:
- Must be language-agnostic where possible
- Must include current implementation status
- Must provide authoritative specifications
- Must link to design documents for context

**Relationships**:
- Links to design documents for historical context
- References feature specifications
- Contains porting guide with implementation order

### Design Documents

**Definition**: Historical and aspirational design documents that describe features, clearly marked with implementation status.

**Structure**:
- **Location**: `design/` (existing) or `docs/design/` (reorganized)
- **Organization**:
  - `implemented/` - Design docs for implemented features
  - `aspirational/` - Design docs for future features
- **Status Header**: Each document has status marker at top

**Key Attributes**:
- **Status**: ✅ Implemented | ⏳ Planned | ❌ Deferred | 📝 Design Only
- **Reference Link**: Link to current reference documentation (if implemented)
- **Historical Context**: Preserves original design thinking
- **Format**: Markdown files with status headers

**Validation Rules**:
- Must have status marker at top
- Must link to reference docs if implemented
- Must preserve original design context
- Must indicate if superseded by implementation

**State Transitions**:
- Design Only → Planned (when feature is prioritized)
- Planned → Implemented (when feature is complete)
- Implemented → (status remains, link to reference added)

**Relationships**:
- Links to reference documentation for implemented features
- Referenced by reference docs for historical context
- Organized by implementation status

### Historical Specs

**Definition**: Development artifacts from the feature development sequence, preserved for historical context but marked as non-authoritative.

**Structure**:
- **Location**: `specs/` (preserved in place)
- **README**: `specs/README.md` explaining historical nature
- **Format**: Existing spec directories (001-025)

**Key Attributes**:
- **Purpose**: Historical development context
- **Status**: Non-authoritative (development artifacts)
- **Format**: Existing spec structure preserved
- **Markers**: README explains they are historical

**Validation Rules**:
- Must be clearly marked as historical
- Must not be used as current specification
- Must be preserved for context
- Must link to current docs where applicable

**Relationships**:
- Referenced by reference documentation for development history
- Links to current reference docs (if feature is implemented)
- Preserved for historical context

### Porting Guide

**Definition**: Comprehensive guide providing implementation roadmap, dependencies, feature order, and testing strategies for porters.

**Structure**:
- **Location**: `docs/reference/PORTING-GUIDE.md`
- **Sections**:
  1. Overview
  2. Implementation Phases (Pattern → Subject → Gram)
  3. Feature Implementation Order
  4. Testing Strategy (CLI tool integration)
  5. Language-Specific Considerations
  6. Getting Started Checklist

**Key Attributes**:
- **Audience**: Language porters
- **Focus**: Implementation roadmap and dependencies
- **Includes**: CLI tool usage, testing workflows, language considerations
- **Format**: Markdown with code examples and workflows

**Validation Rules**:
- Must show clear dependencies
- Must provide implementation order
- Must include CLI tool documentation
- Must address language portability

**Relationships**:
- References feature specifications
- Links to architecture documentation
- Integrates CLI tool documentation

### Feature Specification

**Definition**: Authoritative, current specification of a feature with implementation status, API contracts, and behavioral specifications.

**Structure**:
- **Location**: `docs/reference/features/[feature-name].md`
- **Sections**:
  - Overview
  - Current Status (✅ Implemented | ⏳ Planned)
  - API Reference
  - Implementation Notes
  - Test Coverage
  - Design Evolution (if applicable)

**Key Attributes**:
- **Status**: Current implementation status
- **Authority**: Single source of truth for feature
- **Format**: Markdown with code examples
- **Links**: To design docs, tests, implementation

**Validation Rules**:
- Must reflect current implementation
- Must be authoritative (not historical)
- Must include implementation status
- Must link to design docs for context

**Relationships**:
- Links to design documents for historical context
- Referenced by porting guide
- Links to test coverage
- Referenced by architecture docs

## Entity Relationships

```
User Documentation
    └─→ (links to) Reference Documentation (for advanced topics)

Reference Documentation
    ├─→ (contains) Porting Guide
    ├─→ (contains) Feature Specifications
    ├─→ (links to) Design Documents (for context)
    └─→ (references) Historical Specs (for development history)

Design Documents
    ├─→ (links to) Feature Specifications (if implemented)
    └─→ (organized by) Implementation Status

Historical Specs
    └─→ (links to) Reference Documentation (if feature implemented)

Porting Guide
    ├─→ (references) Feature Specifications
    ├─→ (links to) Architecture Documentation
    └─→ (integrates) CLI Tool Documentation
```

## Documentation Structure Validation

### User Documentation Validation
- ✅ No implementation details present
- ✅ No porting information present
- ✅ Working examples included
- ✅ Clear navigation structure

### Reference Documentation Validation
- ✅ Language-agnostic where possible
- ✅ Current implementation status included
- ✅ Authoritative specifications provided
- ✅ Links to design docs present

### Design Documents Validation
- ✅ Status markers present
- ✅ Links to reference docs (if implemented)
- ✅ Historical context preserved
- ✅ Clear organization by status

### Historical Specs Validation
- ✅ Clearly marked as historical
- ✅ README explains purpose
- ✅ Links to current docs (where applicable)
- ✅ Preserved for context

