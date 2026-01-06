# Tasks: Documentation Reorganization

**Input**: Design documents from `/specs/027-docs-reorganization/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: No tests requested - this is documentation reorganization (file operations and link updates)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Documentation files: `docs/` at repository root
- Guide files: `docs/guide/` (after reorganization)
- Reference files: `docs/reference/` with subdirectories

---

## Phase 1: Setup (Prerequisites & Preparation)

**Purpose**: Verify prerequisites and prepare target directories

- [X] T001 Verify existing documentation structure exists at docs/users/guide/
- [X] T002 Verify reference documentation structure exists at docs/reference/
- [X] T003 Create docs/reference/migration/ directory if it doesn't exist
- [X] T004 Verify docs/reference/features/ directory exists

**Checkpoint**: Prerequisites verified, target directories ready

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: N/A - This is documentation reorganization, no foundational infrastructure needed

**⚠️ NOTE**: No foundational phase needed for this feature. File operations can proceed directly.

---

## Phase 3: User Story 1 - Find User Documentation Easily (Priority: P1) 🎯 MVP

**Goal**: Move user guide from `docs/users/guide/` to `docs/guide/` and update all links to reflect new location

**Independent Test**: Verify that all 8 guide files are accessible at `docs/guide/`, root README.md points to new location, and all internal guide links resolve correctly

### Implementation for User Story 1

- [X] T005 [US1] Move docs/users/guide/ directory to docs/guide/ using `git mv docs/users/guide docs/guide`
- [X] T006 [US1] Verify all 8 guide files exist at docs/guide/ (01-introduction.md through 08-gram-notation-reference.md)
- [X] T007 [US1] Update root README.md to change link from docs/users/README.md to docs/guide/01-introduction.md
- [X] T008 [P] [US1] Check for old path references in docs/guide/01-introduction.md and update if needed
- [X] T009 [P] [US1] Check for old path references in docs/guide/02-basic-concepts.md and update if needed
- [X] T010 [P] [US1] Check for old path references in docs/guide/03-construction.md and update if needed
- [X] T011 [P] [US1] Check for old path references in docs/guide/04-basic-operations.md and update if needed
- [X] T012 [P] [US1] Check for old path references in docs/guide/05-typeclass-instances.md and update if needed
- [X] T013 [P] [US1] Check for old path references in docs/guide/06-advanced-morphisms.md and update if needed
- [X] T014 [P] [US1] Check for old path references in docs/guide/07-use-cases.md and update if needed
- [X] T015 [P] [US1] Check for old path references in docs/guide/08-gram-notation-reference.md and update if needed
- [X] T016 [US1] Verify all guide file links resolve correctly (no broken relative links)
- [X] T017 [US1] Git commit: "feat: move user guide to docs/guide/ and update links - US1"

**Checkpoint**: At this point, User Story 1 should be complete - guide files accessible at docs/guide/, root README updated, all links working

---

## Phase 4: User Story 2 - Find Reference Documentation Without Redundancy (Priority: P1)

**Goal**: Move migration and API porting guides to docs/reference/ subdirectories, update links, and delete redundant directories

**Independent Test**: Verify that migration guide is at docs/reference/migration/rename-constructors.md, API guide is at docs/reference/features/pattern-construction.md, all reference doc links updated, and redundant directories deleted

### Implementation for User Story 2

- [X] T018 [US2] Move docs/users/migration/rename-constructors.md to docs/reference/migration/rename-constructors.md using `git mv`
- [X] T019 [US2] Move docs/users/api/pattern-construction.md to docs/reference/features/pattern-construction.md using `git mv`
- [X] T020 [US2] Verify migration guide exists at docs/reference/migration/rename-constructors.md
- [X] T021 [US2] Verify API guide exists at docs/reference/features/pattern-construction.md
- [X] T022 [P] [US2] Delete docs/users/guides/ directory using `git rm -r docs/users/guides/`
- [X] T023 [P] [US2] Delete docs/users/examples/ directory using `git rm -r docs/users/examples/`
- [X] T024 [US2] Delete docs/users/api/ directory using `git rm -r docs/users/api/` (after moving pattern-construction.md)
- [X] T025 [US2] Delete docs/users/migration/ directory using `git rm -r docs/users/migration/` (after moving rename-constructors.md)
- [X] T026 [US2] Update docs/reference/PORTING-GUIDE.md to change user doc link from ../users/README.md to ../guide/01-introduction.md
- [X] T027 [P] [US2] Check docs/reference/ARCHITECTURE.md for old path references and update if needed
- [X] T028 [P] [US2] Check docs/reference/migration/rename-constructors.md for old path references and update if needed
- [X] T029 [P] [US2] Check docs/reference/features/pattern-construction.md for old path references and update if needed
- [X] T030 [US2] Verify all reference documentation links resolve correctly
- [X] T031 [US2] Git commit: "feat: consolidate reference documentation and delete redundant directories - US2"

**Checkpoint**: At this point, User Story 2 should be complete - reference files consolidated, redundant directories deleted, links updated

---

## Phase 5: User Story 3 - Navigate Documentation Without Redundant README Files (Priority: P2)

**Goal**: Delete redundant README files from subdirectories and verify navigation still works

**Independent Test**: Verify that docs/users/guide/README.md, docs/users/README.md, docs/reference/README.md, and docs/design/README.md are deleted, and that navigation works through direct file access

### Implementation for User Story 3

- [X] T032 [US3] Delete docs/users/guide/README.md using `git rm` (if not already deleted with directory move)
- [X] T033 [US3] Delete docs/users/README.md using `git rm` (if not already deleted with directory deletion)
- [X] T034 [US3] Delete docs/reference/README.md using `git rm`
- [X] T035 [US3] Delete docs/design/README.md using `git rm`
- [X] T036 [US3] Verify docs/users/guide/README.md does not exist
- [X] T037 [US3] Verify docs/users/README.md does not exist
- [X] T038 [US3] Verify docs/reference/README.md does not exist
- [X] T039 [US3] Verify docs/design/README.md does not exist
- [X] T040 [US3] Verify docs/README.md still exists (main index)
- [X] T041 [US3] Check for any links referencing deleted README files and remove/update them
- [X] T042 [US3] Delete docs/users/ directory using `git rm -r docs/users/` (after all moves and deletions complete)
- [X] T043 [US3] Verify docs/users/ directory does not exist
- [X] T044 [US3] Git commit: "feat: remove redundant README files and clean up directory structure - US3"

**Checkpoint**: At this point, User Story 3 should be complete - redundant README files deleted, directory structure clean

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final verification and cleanup

- [X] T045 Verify all guide files accessible at docs/guide/ (8 files total)
- [X] T046 Verify all reference files consolidated in docs/reference/
- [X] T047 Verify all redundant directories deleted (docs/users/guides/, docs/users/examples/, docs/users/api/, docs/users/migration/, docs/users/)
- [X] T048 Verify all redundant README files deleted
- [X] T049 Check for any remaining references to old paths (docs/users/) in all markdown files
- [X] T050 Verify git history preserved (check git log --follow for moved files)
- [X] T051 Run verification checklist from contracts/verification-checklist.md
- [X] T052 Verify directory structure matches plan.md structure
- [X] T053 Git commit: "docs: finalize documentation reorganization"

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: N/A - not applicable for this feature
- **User Stories (Phase 3+)**: Can proceed sequentially or in parallel where independent
  - User Story 1 (P1): Can start after Setup - Move guide directory
  - User Story 2 (P1): Can start after Setup - Move reference files (independent of US1)
  - User Story 3 (P2): Depends on US1 and US2 completion - Deletes remaining files/directories
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Setup - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Setup - Independent of US1 (different files)
- **User Story 3 (P2)**: Depends on US1 and US2 - Deletes files/directories after moves complete

### Within Each User Story

- File moves before link updates
- Link updates before verification
- Verification before commit
- Story complete before moving to next priority

### Parallel Opportunities

- Setup tasks T003-T004 can run in parallel
- User Story 1 link update tasks T008-T015 can run in parallel (different files)
- User Story 2 deletion tasks T022-T023 can run in parallel
- User Story 2 link update tasks T027-T029 can run in parallel (different files)
- User Stories 1 and 2 can be worked on in parallel (different files, no conflicts)

---

## Parallel Example: User Story 1

```bash
# Launch all link update tasks for User Story 1 together:
Task: "Check for old path references in docs/guide/01-introduction.md and update if needed"
Task: "Check for old path references in docs/guide/02-basic-concepts.md and update if needed"
Task: "Check for old path references in docs/guide/03-construction.md and update if needed"
Task: "Check for old path references in docs/guide/04-basic-operations.md and update if needed"
Task: "Check for old path references in docs/guide/05-typeclass-instances.md and update if needed"
Task: "Check for old path references in docs/guide/06-advanced-morphisms.md and update if needed"
Task: "Check for old path references in docs/guide/07-use-cases.md and update if needed"
Task: "Check for old path references in docs/guide/08-gram-notation-reference.md and update if needed"
```

---

## Parallel Example: User Story 2

```bash
# Launch deletion and link update tasks together:
Task: "Delete docs/users/guides/ directory using git rm -r"
Task: "Delete docs/users/examples/ directory using git rm -r"
Task: "Check docs/reference/ARCHITECTURE.md for old path references and update if needed"
Task: "Check docs/reference/migration/rename-constructors.md for old path references and update if needed"
Task: "Check docs/reference/features/pattern-construction.md for old path references and update if needed"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 3: User Story 1 (move guide, update links)
3. **STOP and VALIDATE**: Verify guide accessible at docs/guide/, links working
4. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup → Prerequisites ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Test independently → Deploy/Demo
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup together
2. Once Setup is done:
   - Developer A: User Story 1 (guide directory move and link updates)
   - Developer B: User Story 2 (reference file moves and deletions)
3. After US1 and US2 complete:
   - Developer C: User Story 3 (README deletions and cleanup)
4. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **Git commit after each user story completion** (see tasks T017, T031, T044)
- Stop at any checkpoint to validate story independently
- Use `git mv` for all file moves to preserve history
- Use `git rm` for all deletions to track in git
- Verify file operations before proceeding to link updates
- All link updates should maintain relative path structure where possible

