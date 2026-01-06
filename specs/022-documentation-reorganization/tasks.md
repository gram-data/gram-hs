# Tasks: Documentation Reorganization

**Input**: Design documents from `/specs/022-documentation-reorganization/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: No test tasks - this is a documentation-only feature. Validation is through manual review and link checking.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Documentation files: `docs/` at repository root
- User documentation: `docs/users/`
- Reference documentation: `docs/reference/`
- Design documents: `design/` (existing) or `docs/design/`
- Historical specs: `specs/` (existing)

---

## Phase 1: Setup (Documentation Structure)

**Purpose**: Create documentation directory structure

- [x] T001 Create documentation directory structure: `docs/users/`, `docs/users/api/`, `docs/users/guides/`, `docs/users/examples/`
- [x] T002 [P] Create documentation directory structure: `docs/reference/`, `docs/reference/semantics/`, `docs/reference/features/`
- [x] T003 [P] Create documentation directory structure: `docs/design/` (index only - design docs stay in root `design/`)
- [x] T004 [P] Create documentation directory structure: `docs/history/`, `docs/history/specs/` (REMOVED - specs stay in root `specs/`)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core documentation structure that MUST be complete before user story documentation can be created

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Create `docs/reference/README.md` with reference documentation index and navigation
- [x] T006 Create `docs/users/README.md` placeholder structure (will be filled in US1)
- [x] T007 Create `docs/design/README.md` placeholder structure (will be filled in US3)

**Checkpoint**: Foundation ready - user story documentation can now begin

---

## Phase 3: User Story 1 - Find User-Facing Documentation (Priority: P1) 🎯 MVP

**Goal**: Create user-facing documentation structure that enables Haskell developers to quickly find installation instructions, usage guides, and API reference without encountering implementation or porting details.

**Independent Test**: A new user can find installation instructions and complete a basic example within 5 minutes of accessing the documentation. User can navigate to usage guides and API reference without seeing porting or implementation details.

### Implementation for User Story 1

- [x] T008 [US1] Create `docs/users/README.md` with installation instructions, quick start examples, and navigation to guides and API reference
- [x] T009 [P] [US1] Create `docs/users/guides/getting-started.md` with basic usage examples and common patterns
- [x] T010 [P] [US1] Create `docs/users/guides/pattern-construction.md` with examples of creating patterns
- [x] T011 [P] [US1] Create `docs/users/guides/graph-lens.md` with examples of using graph lens (if implemented)
- [x] T012 [P] [US1] Create `docs/users/guides/gram-serialization.md` with examples of serialization usage
- [x] T013 [P] [US1] Create `docs/users/examples/basic-patterns.md` with working code examples
- [x] T014 [P] [US1] Create `docs/users/examples/graph-operations.md` with graph lens examples (if applicable)
- [x] T015 [US1] Update root `README.md` to be user-focused with installation, quick start, and links to `docs/users/README.md`
- [x] T016 [US1] Add link to porting guide in root `README.md` (for porters, separate from user docs)
- [x] T017 [US1] Validate user documentation: verify no implementation details or porting information present in `docs/users/`
- [x] T018 [US1] Validate user documentation: verify all links work and examples are complete
- [x] T019 [US1] Git commit: "docs: implement user-facing documentation structure - US1"

**Checkpoint**: At this point, User Story 1 should be fully functional - users can find and use the documentation independently

---

## Phase 4: User Story 2 - Find Porting Reference Documentation (Priority: P1)

**Goal**: Create comprehensive reference documentation that enables language porters to find architecture, specifications, implementation guides, and porting roadmap with CLI tool integration.

**Independent Test**: A language porter can identify the implementation roadmap and understand dependencies within 10 minutes of accessing the porting guide. Porter can use CLI tool for testing within 15 minutes.

### Implementation for User Story 2

- [x] T020 [US2] Create `docs/reference/PORTING-GUIDE.md` with overview, implementation phases (Pattern → Subject → Gram), feature order, and getting started checklist
- [x] T021 [US2] Add CLI tool documentation section to `docs/reference/PORTING-GUIDE.md` covering building, generating test cases, canonical outputs, validation, and round-trip testing
- [x] T022 [P] [US2] Create `docs/reference/ARCHITECTURE.md` with core design principles, category-theoretic foundations, and key design decisions
- [x] T023 [P] [US2] Create `docs/reference/SPECIFICATION.md` with current feature specifications, implementation status matrix, API contracts, and behavioral specifications
- [x] T024 [P] [US2] Create `docs/reference/IMPLEMENTATION.md` with implementation patterns, language-agnostic algorithms, testing strategies, and common pitfalls
- [x] T025 [P] [US2] Create `docs/reference/features/core-pattern.md` with current specification for core Pattern type
- [x] T026 [P] [US2] Create `docs/reference/features/typeclass-instances.md` with current specification for typeclass instances
- [x] T027 [P] [US2] Create `docs/reference/features/graph-lens.md` with current specification for graph lens (Feature 23)
- [x] T028 [P] [US2] Create `docs/reference/features/gram-serialization.md` with current specification for gram serialization
- [x] T029 [P] [US2] Create `docs/reference/semantics/pattern-semantics.md` with pattern semantics specification
- [x] T030 [P] [US2] Create `docs/reference/semantics/graph-interpretation.md` with graph interpretation semantics
- [x] T031 [P] [US2] Create `docs/reference/semantics/gram-semantics.md` with gram notation semantics
- [x] T032 [US2] Update `docs/reference/README.md` with complete index, navigation, and links to all reference documentation
- [ ] T033 [US2] Validate reference documentation: verify all links work and current implementation status is accurate
- [ ] T034 [US2] Validate porting guide: verify CLI tool documentation is complete and actionable
- [ ] T035 [US2] Git commit: "docs: implement reference documentation and porting guide - US2"

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - users have usage docs, porters have reference docs

---

## Phase 5: User Story 3 - Understand Design Evolution (Priority: P2)

**Goal**: Add status markers to all design documents and organize them to clearly indicate which features are implemented versus aspirational, with links to current reference documentation.

**Independent Test**: Any design document clearly indicates implementation status. 100% of design documents have status markers. Implemented features link to current reference documentation.

### Implementation for User Story 3

- [x] T036 [US3] Create `docs/design/README.md` with index of all design documents, status table, and links to reference docs
- [x] T037 [P] [US3] Add status marker to `design/DESIGN.md`: "**Status**: ✅ Implemented (Core features) | ⏳ Planned (Future features) | 📝 Design Only (Conceptual)"
- [x] T038 [P] [US3] Add status marker to `design/graph-lens.md`: "**Status**: ✅ Implemented (Feature 23) | **Reference**: See `docs/reference/features/graph-lens.md`"
- [x] T039 [P] [US3] Add status marker to `design/pattern-matching-dsl-design.md`: "**Status**: ⏳ Deferred | **Note**: Predicate matching (Feature 9) provides similar functionality"
- [x] T040 [P] [US3] Add status marker to `design/SEMANTICS.md`: "**Status**: ✅ Implemented (Core semantics) | **Reference**: See `docs/reference/semantics/`"
- [x] T041 [P] [US3] Add status marker to `design/EXTENDED-SEMANTICS.md`: "**Status**: ✅ Implemented (Extended semantics) | **Reference**: See `docs/reference/semantics/gram-semantics.md`"
- [x] T042 [P] [US3] Add status marker to `design/pattern-category.md`: "**Status**: 📝 Design Only | **Note**: Conceptual design for category-theoretic interpretation"
- [x] T043 [US3] Optionally organize design documents: keep in `design/` at root with status markers (chosen approach - no need to move)
- [x] T044 [US3] Create `specs/README.md` explaining that specs are historical development artifacts, not current specifications, with links to `docs/reference/SPECIFICATION.md`
- [x] T045 [US3] Validate design documents: verify 100% have status markers
- [x] T046 [US3] Validate design documents: verify all implemented features link to reference docs
- [x] T047 [US3] Validate historical specs: verify `specs/README.md` clearly explains historical nature
- [x] T048 [US3] Git commit: "docs: add status markers to design documents and organize by implementation status - US3"

**Checkpoint**: All user stories should now be complete - users have usage docs, porters have reference docs, design evolution is clear

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final validation, link checking, and documentation polish

- [x] T049 [P] Validate all internal links in documentation structure (check all markdown files for broken links)
- [x] T050 [P] Verify all required entry points exist: `docs/users/README.md`, `docs/reference/PORTING-GUIDE.md`, `docs/reference/ARCHITECTURE.md`, `docs/design/README.md`, `specs/README.md`
- [x] T051 [P] Verify navigation structure: users can navigate user docs without seeing porting info, porters can navigate reference docs without seeing usage examples
- [x] T052 Update any external documentation references to point to new locations (if applicable)
- [x] T053 Verify success criteria: test that user can find installation in <5 min, porter can find roadmap in <10 min
- [x] T054 Verify CLI tool documentation is complete and actionable in porting guide
- [x] T055 Final documentation review: ensure no implementation details in user docs, all status markers present
- [x] T056 Git commit: "docs: finalize documentation reorganization"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User Story 1 (P1) and User Story 2 (P1) can proceed in parallel after Foundational
  - User Story 3 (P2) can start after Foundational but benefits from US1/US2 completion
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories (can run parallel with US1)
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - Benefits from US2 completion (needs reference docs to link to) but can start independently

### Within Each User Story

- Directory structure before content files
- Entry point READMEs before detailed guides
- Core documentation before examples
- Content creation before validation
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks (T001-T004) marked [P] can run in parallel
- All Foundational tasks (T005-T007) can run in parallel
- Once Foundational phase completes:
  - User Story 1 and User Story 2 can run in parallel (different directories)
  - Within US1: guides (T009-T012) and examples (T013-T014) can run in parallel
  - Within US2: architecture/spec/implementation (T022-T024) can run in parallel, feature specs (T025-T028) can run in parallel, semantics (T029-T031) can run in parallel
  - Within US3: status markers (T037-T042) can run in parallel
- Polish tasks (T049-T051) can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all guides for User Story 1 together:
Task: "Create docs/users/guides/getting-started.md"
Task: "Create docs/users/guides/pattern-construction.md"
Task: "Create docs/users/guides/graph-lens.md"
Task: "Create docs/users/guides/gram-serialization.md"

# Launch all examples for User Story 1 together:
Task: "Create docs/users/examples/basic-patterns.md"
Task: "Create docs/users/examples/graph-operations.md"
```

---

## Parallel Example: User Story 2

```bash
# Launch all core reference docs together:
Task: "Create docs/reference/ARCHITECTURE.md"
Task: "Create docs/reference/SPECIFICATION.md"
Task: "Create docs/reference/IMPLEMENTATION.md"

# Launch all feature specs together:
Task: "Create docs/reference/features/core-pattern.md"
Task: "Create docs/reference/features/typeclass-instances.md"
Task: "Create docs/reference/features/graph-lens.md"
Task: "Create docs/reference/features/gram-serialization.md"

# Launch all semantics docs together:
Task: "Create docs/reference/semantics/pattern-semantics.md"
Task: "Create docs/reference/semantics/graph-interpretation.md"
Task: "Create docs/reference/semantics/gram-semantics.md"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (directory structure)
2. Complete Phase 2: Foundational (entry point READMEs)
3. Complete Phase 3: User Story 1 (user-facing documentation)
4. **STOP and VALIDATE**: Test that a new user can find installation and complete example in <5 minutes
5. Deploy/document if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Validate independently → Deploy (MVP - users can use library)
3. Add User Story 2 → Validate independently → Deploy (porters can start porting)
4. Add User Story 3 → Validate independently → Deploy (design evolution clear)
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (user docs)
   - Developer B: User Story 2 (reference docs)
   - Developer C: User Story 3 (design status markers)
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **Git commit after each user story completion** (see tasks T019, T035, T048)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Documentation validation: manual review, link checking, structure verification
- No code changes - this is documentation-only reorganization

