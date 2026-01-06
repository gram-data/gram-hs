# Quickstart: Documentation Reorganization

**Feature**: 022-documentation-reorganization  
**Date**: 2025-01-27

## Overview

This quickstart guide explains how to implement the documentation reorganization feature. The goal is to create separate documentation structures for library users and language porters, with clear status markers and comprehensive porting guidance.

## Implementation Steps

### Step 1: Create Documentation Structure

Create the `docs/` directory hierarchy:

```bash
mkdir -p docs/users/{api,guides,examples}
mkdir -p docs/reference/{semantics,features}
mkdir -p docs/design/{implemented,aspirational}
mkdir -p docs/history/specs
```

### Step 2: Create User Documentation

1. **Create `docs/users/README.md`**:
   - Installation instructions
   - Quick start examples
   - Links to guides and API reference
   - No implementation or porting details

2. **Create Usage Guides** (`docs/users/guides/`):
   - `getting-started.md` - Basic usage
   - `pattern-construction.md` - Creating patterns
   - `graph-lens.md` - Using graph lens
   - `gram-serialization.md` - Serialization usage

3. **Add Examples** (`docs/users/examples/`):
   - Working code examples
   - Common use cases
   - Copy-paste ready snippets

### Step 3: Create Reference Documentation

1. **Create `docs/reference/PORTING-GUIDE.md`**:
   - Implementation phases (Pattern → Subject → Gram)
   - Feature implementation order
   - CLI tool usage for testing
   - Language-specific considerations

2. **Create `docs/reference/ARCHITECTURE.md`**:
   - Core design principles
   - Category-theoretic foundations
   - Key design decisions
   - Language-agnostic concepts

3. **Create `docs/reference/SPECIFICATION.md`**:
   - Current feature specifications
   - Implementation status matrix
   - API contracts
   - Behavioral specifications

4. **Create `docs/reference/IMPLEMENTATION.md`**:
   - Implementation patterns
   - Language-agnostic algorithms
   - Testing strategies
   - Common pitfalls

5. **Create Feature Specifications** (`docs/reference/features/`):
   - One file per major feature
   - Current implementation status
   - API reference
   - Implementation notes

### Step 4: Organize Design Documents

1. **Add Status Markers** to all design documents:
   ```markdown
   **Status**: ✅ Implemented (Feature 23) | ⏳ Deferred | 📝 Design Only
   **Reference**: See `docs/reference/features/graph-lens.md` for current spec
   ```

2. **Organize by Status**:
   - Move implemented features to `docs/design/implemented/` (optional)
   - Move deferred features to `docs/design/aspirational/`
   - Or keep in `design/` with status markers

3. **Create `docs/design/README.md`**:
   - Index of all design documents
   - Status table
   - Links to reference docs

### Step 5: Mark Historical Artifacts

1. **Create `specs/README.md`**:
   - Explain these are historical development artifacts
   - Link to current reference documentation
   - Explain development sequence

2. **Optionally Move to `docs/history/specs/`**:
   - Preserve for historical context
   - Add redirect notes in old location

### Step 6: Update Main README

Update root `README.md` to be user-focused:
- Installation and quick start
- Links to user documentation
- Link to porting guide (for porters)
- Remove implementation details

### Step 7: Validate Structure

1. **Check Required Files**:
   - ✅ `docs/users/README.md`
   - ✅ `docs/reference/PORTING-GUIDE.md`
   - ✅ `docs/reference/ARCHITECTURE.md`
   - ✅ `docs/reference/SPECIFICATION.md`
   - ✅ `docs/design/README.md`
   - ✅ `specs/README.md`

2. **Check Status Markers**:
   - ✅ All design documents have status
   - ✅ Implemented features link to reference docs

3. **Check Links**:
   - ✅ All internal links are valid
   - ✅ Relative paths work correctly
   - ✅ No broken references

## Example: Status Marker

Add to top of design document:

```markdown
# Graph Lens Design Document

**Status**: ✅ Implemented (Feature 23)  
**Reference**: See `docs/reference/features/graph-lens.md` for current specification  
**Design Doc**: This document describes the original design. The current implementation may differ.

## Overview
[Original design content preserved]
```

## Example: Porting Guide Section

```markdown
# Porting Guide

## Implementation Phases

### Phase 1: Pattern Library (Foundation)
**Priority**: Must be first  
**Dependencies**: None

1. Core Pattern Type
2. Basic Typeclass Instances
3. Construction Functions
[...]

### Phase 2: Subject Library
**Priority**: Before Gram serialization  
**Dependencies**: None (can be parallel with Pattern)

[...]

### Phase 3: Gram Serialization
**Priority**: After Pattern and Subject  
**Dependencies**: Pattern library, Subject library

[...]
```

## Testing the Reorganization

1. **User Perspective**:
   - Can a new user find installation in < 5 minutes?
   - Can they complete a basic example?
   - Do they encounter porting/implementation details?

2. **Porter Perspective**:
   - Can a porter find the roadmap in < 10 minutes?
   - Can they understand dependencies?
   - Can they use the CLI tool for testing?

3. **Structure Validation**:
   - All required files exist
   - All status markers present
   - All links valid
   - Navigation complete

## Success Criteria

- ✅ User can find installation and complete example in 5 minutes
- ✅ Porter can identify roadmap and dependencies in 10 minutes
- ✅ 100% of design documents have status markers
- ✅ 100% of implemented features have reference docs
- ✅ Porter can use CLI tool for testing in 15 minutes

## Next Steps

After completing the reorganization:

1. Update any external links pointing to old locations
2. Update CI/CD documentation links (if applicable)
3. Announce the new documentation structure
4. Gather feedback from users and porters
5. Iterate based on feedback

