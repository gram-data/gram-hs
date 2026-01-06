# Data Model: Documentation Structure

**Feature**: 027-docs-reorganization  
**Date**: 2025-01-28

## Overview

This feature reorganizes documentation files and directories. The data model describes the documentation structure entities and their relationships.

## Entities

### User Guide

**Purpose**: Comprehensive user-facing documentation for the Pattern library

**Location**: `docs/guide/` (after reorganization)

**Attributes**:
- **Files**: 8 numbered markdown files (01-introduction.md through 08-gram-notation-reference.md)
- **Structure**: Sequential learning path from basic to advanced
- **Content**: Concepts, examples, use cases, reference material
- **Links**: Internal cross-references between guide sections

**Relationships**:
- Referenced by: Root README.md, reference documentation
- References: Other guide sections (relative links), Haddock documentation (absolute)

**State Transitions**:
- **Before**: Located at `docs/users/guide/`
- **After**: Located at `docs/guide/`
- **Transition**: Move entire directory using `git mv`

### Reference Documentation

**Purpose**: Porter-focused documentation including architecture, specifications, migration guides, and API porting guidance

**Location**: `docs/reference/` (consolidated location)

**Attributes**:
- **Core Files**: PORTING-GUIDE.md, ARCHITECTURE.md, SPECIFICATION.md, IMPLEMENTATION.md
- **Subdirectories**: 
  - `migration/` - Migration guides (rename-constructors.md)
  - `features/` - Feature specifications (core-pattern.md, typeclass-instances.md, graph-lens.md, gram-serialization.md, pattern-construction.md)
  - `semantics/` - Semantic specifications
- **Content**: Implementation details, porting guidance, API contracts

**Relationships**:
- Referenced by: Root README.md, user guide (for advanced topics)
- References: User guide (for library users), feature specs, migration guides

**State Transitions**:
- **Before**: Some files in `docs/users/migration/` and `docs/users/api/`
- **After**: All consolidated in `docs/reference/` with appropriate subdirectories
- **Transition**: Move files to appropriate subdirectories using `git mv`

### Migration Guide

**Purpose**: Documentation for migrating between library versions

**Location**: `docs/reference/migration/rename-constructors.md` (after reorganization)

**Attributes**:
- **File**: rename-constructors.md
- **Content**: Breaking changes, migration steps, code examples
- **Links**: May reference API documentation, examples

**Relationships**:
- Referenced by: PORTING-GUIDE.md, reference documentation
- References: API documentation, pattern construction guide

**State Transitions**:
- **Before**: Located at `docs/users/migration/rename-constructors.md`
- **After**: Located at `docs/reference/migration/rename-constructors.md`
- **Transition**: Move file using `git mv`, create `docs/reference/migration/` directory first if needed

### API Porting Guide

**Purpose**: Documentation for porting API functions to other languages

**Location**: `docs/reference/features/pattern-construction.md` (after reorganization)

**Attributes**:
- **File**: pattern-construction.md
- **Content**: Function signatures, porting guidance, language-specific recommendations
- **Links**: May reference other reference documentation

**Relationships**:
- Referenced by: PORTING-GUIDE.md, reference documentation
- References: Other feature specifications, migration guides

**State Transitions**:
- **Before**: Located at `docs/users/api/pattern-construction.md`
- **After**: Located at `docs/reference/features/pattern-construction.md`
- **Transition**: Move file using `git mv` to existing `docs/reference/features/` directory

### Redundant Directories (To Be Deleted)

**Purpose**: Inferior duplicates and redundant content

**Entities**:
1. **docs/users/guides/** - Inferior duplicates of numbered guide
   - Files: getting-started.md, pattern-construction.md, gram-serialization.md, graph-lens.md
   - **State**: Delete entire directory

2. **docs/users/examples/** - Redundant examples
   - Files: basic-patterns.md, graph-operations.md
   - **State**: Delete entire directory (examples already in guide)

3. **docs/users/api/** - Empty after moving pattern-construction.md
   - **State**: Delete directory

4. **docs/users/migration/** - Empty after moving rename-constructors.md
   - **State**: Delete directory

5. **docs/users/** - Empty after all moves and deletions
   - **State**: Delete directory

### Redundant README Files (To Be Deleted)

**Purpose**: Navigation files that duplicate directory structure

**Entities**:
1. **docs/users/guide/README.md** - Guide index (redundant with numbered files)
2. **docs/users/README.md** - User docs index (redundant with structure)
3. **docs/reference/README.md** - Reference index (redundant with structure)
4. **docs/design/README.md** - Design index (redundant with structure)

**State**: Delete all (keep only root `docs/README.md`)

## Link Relationships

### Internal Links

**Pattern**: `[text](relative/path/to/file.md)`

**Types**:
1. **Guide section links**: Links between numbered guide files (e.g., `[Introduction](01-introduction.md)`)
   - **Update Required**: None (relative paths within same directory remain valid)
   
2. **Cross-directory links**: Links from guide to reference or vice versa
   - **Update Required**: Yes (paths change with directory moves)
   
3. **Root README links**: Links from root README.md to documentation
   - **Update Required**: Yes (must point to new `docs/guide/01-introduction.md`)

### External Links

**Pattern**: Absolute URLs or paths from repo root

**Types**:
1. **Haddock documentation**: Absolute paths to generated docs
   - **Update Required**: None (absolute paths unchanged)
   
2. **GitHub links**: External references
   - **Update Required**: None (external links)

## Validation Rules

1. **File Existence**: All moved files must exist at target locations
2. **Directory Structure**: Target directories must exist before moves
3. **Link Resolution**: All relative markdown links must resolve to existing files
4. **No Orphans**: No files should reference deleted files/directories
5. **Git History**: All moves must preserve git history (use `git mv`)

## State Diagram

```
[Initial State]
docs/users/guide/ → [git mv] → docs/guide/
docs/users/migration/ → [git mv] → docs/reference/migration/
docs/users/api/ → [git mv] → docs/reference/features/
docs/users/guides/ → [delete] → (removed)
docs/users/examples/ → [delete] → (removed)
docs/users/api/ → [delete] → (removed)
docs/users/migration/ → [delete] → (removed)
docs/users/ → [delete] → (removed)

[Final State]
docs/guide/ (8 numbered files)
docs/reference/ (consolidated reference material)
```

## Notes

- All file operations preserve git history using `git mv`
- Link updates maintain relative path structure
- Directory structure is self-explanatory (no README files needed)
- External links may break but this is acceptable for improved structure

