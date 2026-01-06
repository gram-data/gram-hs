# Implementation Plan: Documentation Reorganization

**Branch**: `027-docs-reorganization` | **Date**: 2025-01-28 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/027-docs-reorganization/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Reorganize documentation structure to simplify navigation and remove redundancy. Move user guide from `docs/users/guide/` to `docs/guide/` for immediate discoverability. Consolidate all reference material (migration guides, API porting guidance) in `docs/reference/`. Remove redundant directories (`docs/users/guides/`, `docs/users/examples/`) and unnecessary README files. Update all internal links to reflect new structure. Use `git mv` to preserve file history throughout the reorganization.

## Technical Context

**Language/Version**: Markdown documentation files  
**Primary Dependencies**: Git (for `git mv` to preserve history)  
**Storage**: File system (documentation files)  
**Testing**: Link verification (checking all markdown links resolve correctly)  
**Target Platform**: Documentation repository (markdown files)  
**Project Type**: Documentation reorganization (no code changes)  
**Performance Goals**: N/A (documentation reorganization)  
**Constraints**: 
- Must preserve git history using `git mv` where possible
- Zero broken internal links after reorganization
- All file moves must be atomic and verifiable
- External links may break (acceptable trade-off)
**Scale/Scope**: 
- ~8 guide files to move
- ~2 reference files to move
- ~6 directories/files to delete
- ~10+ files to update with new links
- Total: ~26 file operations

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Phase 0 (Initial Check)

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Documentation structure is clear and self-explanatory. Directory names clearly indicate purpose (`docs/guide/` for user guide, `docs/reference/` for reference material). No code changes required.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ Link verification strategy ensures all internal links resolve correctly. Verification process checks all markdown files for broken relative links. No code tests needed (documentation reorganization).

- **Conceptual Consistency**: ✅ N/A - This is documentation reorganization, not code changes. Structure aligns with clear separation of user-facing vs. reference documentation.

- **Mathematical Clarity**: ✅ N/A - Documentation reorganization doesn't involve mathematical concepts.

- **Multi-Language Reference Alignment**: ✅ Documentation structure is language-agnostic. Clear separation between user guides and reference material benefits all language porters.

**Violations**: None. This is a documentation reorganization task that improves clarity and maintainability without adding complexity.

### Post-Phase 1 (After Design)

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Design maintains clear structure. All file operations use `git mv` to preserve history. Documentation structure is self-explanatory without redundant README files.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ Verification checklist created with comprehensive link verification strategy. All markdown links will be verified for correctness. Git history preservation verified.

- **Conceptual Consistency**: ✅ N/A - Documentation reorganization maintains clear separation of concerns.

- **Mathematical Clarity**: ✅ N/A - No mathematical concepts involved.

- **Multi-Language Reference Alignment**: ✅ Final structure benefits all language porters with clear reference material consolidation.

**Violations**: None. Design is complete and maintains all constitution principles.

## Project Structure

### Documentation (this feature)

```text
specs/027-docs-reorganization/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
docs/
├── README.md                    # Main docs index (keep, update links)
├── guide/                       # User guide (moved from docs/users/guide/)
│   ├── 01-introduction.md
│   ├── 02-basic-concepts.md
│   ├── 03-construction.md
│   ├── 04-basic-operations.md
│   ├── 05-typeclass-instances.md
│   ├── 06-advanced-morphisms.md
│   ├── 07-use-cases.md
│   └── 08-gram-notation-reference.md
└── reference/                   # Reference documentation (consolidated)
    ├── PORTING-GUIDE.md         # Existing (update links)
    ├── ARCHITECTURE.md           # Existing (update links)
    ├── SPECIFICATION.md          # Existing
    ├── IMPLEMENTATION.md         # Existing
    ├── migration/               # New subdirectory
    │   └── rename-constructors.md  # Moved from docs/users/migration/
    └── features/                # Existing subdirectory
        ├── core-pattern.md       # Existing
        ├── typeclass-instances.md # Existing
        ├── graph-lens.md        # Existing
        ├── gram-serialization.md # Existing
        └── pattern-construction.md # Moved from docs/users/api/
```

**Structure Decision**: 
- User guide moved to top level (`docs/guide/`) for immediate discoverability
- All reference material consolidated in `docs/reference/` with appropriate subdirectories
- Redundant `docs/users/` directory removed entirely
- README files removed from subdirectories (structure is self-explanatory)

## Complexity Tracking

> **No violations - documentation reorganization simplifies structure without adding complexity**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
