# Implementation Plan: Documentation Reorganization

**Branch**: `022-documentation-reorganization` | **Date**: 2025-01-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/022-documentation-reorganization/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Reorganize documentation to serve two distinct audiences: (1) Haskell library users who need usage-focused documentation, and (2) language porters who need reference documentation for accurate translation. Create separate documentation structures, porting guide with implementation roadmap, status markers on design documents, and integration of CLI tool documentation for testing.

## Technical Context

**Language/Version**: Markdown documentation (CommonMark/GitHub Flavored Markdown)  
**Primary Dependencies**: Git (version control), Markdown renderers (GitHub, local viewers)  
**Storage**: Git repository (markdown files in `docs/` directory structure)  
**Testing**: Manual review, link validation, documentation structure verification  
**Target Platform**: GitHub Pages, local markdown viewers, documentation generators  
**Project Type**: Documentation project (no code changes, only documentation reorganization)  
**Performance Goals**: N/A (documentation readability, not performance)  
**Constraints**: Must maintain backward compatibility for existing links, preserve historical context, ensure all documentation is accessible  
**Scale/Scope**: ~25 feature specs, ~7 design documents, multiple library READMEs, comprehensive reorganization into structured `docs/` hierarchy

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Phase 0 (Initial)

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Documentation structure is clear and self-organizing. All documentation sections have clear purposes and navigation. Public documentation (user-facing) is well-organized with examples.
- **Testing Standards (NON-NEGOTIABLE)**: ✅ Documentation can be verified through link checking, structure validation, and manual review. Success criteria are measurable (time-to-find metrics, 100% status markers). Documentation serves as executable specification for porters.
- **Conceptual Consistency**: ✅ Documentation preserves category-theoretic foundations in reference documentation. Architecture documentation maintains mathematical clarity. Design documents link to current implementations.
- **Mathematical Clarity**: ✅ Reference documentation maintains formal definitions and category-theoretic explanations. Architecture documentation preserves mathematical notation and concepts.
- **Multi-Language Reference Alignment**: ✅ Reference documentation is language-agnostic and focuses on concepts. Porting guide addresses cross-language considerations. Implementation patterns are described without Haskell-specific details.

### Post-Phase 1 (After Design)

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ Data model defines clear documentation entities with validation rules. Contracts specify required structure and content. Quickstart provides clear implementation guidance. All artifacts are well-documented.
- **Testing Standards (NON-NEGOTIABLE)**: ✅ Contracts define validation rules for structure and content. Success criteria are measurable and verifiable. Documentation structure can be validated programmatically.
- **Conceptual Consistency**: ✅ Data model preserves category-theoretic foundations in reference documentation entities. Architecture documentation entity maintains mathematical clarity. Design document entities link to current implementations.
- **Mathematical Clarity**: ✅ Reference documentation entities maintain formal definitions. Architecture entity preserves mathematical notation. Feature specification entities include formal definitions.
- **Multi-Language Reference Alignment**: ✅ Reference documentation entities are language-agnostic. Porting guide entity addresses cross-language considerations. Implementation pattern entities avoid Haskell-specific details.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/022-documentation-reorganization/
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
├── users/                    # User-facing documentation
│   ├── README.md            # User quick start
│   ├── api/                 # API reference (or links to generated docs)
│   ├── guides/              # Usage guides
│   │   ├── getting-started.md
│   │   ├── pattern-construction.md
│   │   ├── graph-lens.md
│   │   └── gram-serialization.md
│   └── examples/            # Working code examples
│
├── reference/               # Porter-facing documentation
│   ├── README.md            # Reference docs index
│   ├── ARCHITECTURE.md      # Core design principles
│   ├── SPECIFICATION.md     # Authoritative feature spec
│   ├── IMPLEMENTATION.md    # Implementation patterns
│   ├── PORTING-GUIDE.md     # Implementation roadmap
│   ├── semantics/           # Semantic specifications
│   │   ├── pattern-semantics.md
│   │   ├── graph-interpretation.md
│   │   └── gram-semantics.md
│   └── features/            # Feature-by-feature reference
│       ├── core-pattern.md
│       ├── typeclass-instances.md
│       ├── graph-lens.md
│       └── ...
│
├── design/                  # Design documents (preserved)
│   ├── README.md           # Design doc index with status
│   ├── implemented/         # Design docs for implemented features
│   │   ├── graph-lens.md    # ✅ Implemented (Feature 23)
│   │   └── ...
│   └── aspirational/        # Design docs for future features
│       ├── pattern-matching-dsl.md  # ⏳ Deferred
│       ├── zipper.md                # ⏳ Deferred
│       └── ...
│
└── history/                 # Historical development artifacts
    └── specs/               # Move current specs/ here (optional)
        ├── README.md        # Explains development sequence
        └── [001-025]/       # Preserved for historical reference

design/                      # Existing design/ directory (preserved, add status markers)
├── DESIGN.md                # Update with status markers
├── SEMANTICS.md             # Update with status markers
├── EXTENDED-SEMANTICS.md    # Update with status markers
├── graph-lens.md            # ✅ Implemented, link to reference
├── pattern-matching-dsl-design.md  # ⏳ Deferred
└── pattern-category.md      # Update with status markers

specs/                       # Existing specs/ directory (preserved, add README)
├── README.md                # Explains historical nature
└── [001-025]/               # Existing spec directories (preserved)

README.md                    # Update to be user-focused
```

**Structure Decision**: Documentation-only reorganization. No code changes. Structure separates user-facing (`docs/users/`) from reference (`docs/reference/`) documentation. Historical artifacts preserved with clear status markers. Design documents organized by implementation status.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations. Documentation reorganization maintains all constitution principles:
- Code Quality: Clear structure and organization
- Testing: Measurable success criteria and verifiable structure
- Conceptual Consistency: Preserves category-theoretic foundations
- Mathematical Clarity: Maintains formal definitions in reference docs
- Multi-Language Reference: Language-agnostic reference documentation
