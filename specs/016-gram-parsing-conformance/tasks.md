# Tasks: Gram Parsing Conformance

**Feature Branch**: `016-gram-parsing-conformance`
**Feature Name**: Gram Parsing Conformance

## Phase 1: Setup
- [ ] T001 Initialize `tree-sitter-gram` submodule in `libs/gram/test-data/tree-sitter-gram`

## Phase 2: Foundation
- [ ] T002 Create `libs/gram/tests/Spec/CorpusSpec.hs` with module shell
- [ ] T003 Implement corpus file parsing logic in `libs/gram/tests/Spec/CorpusSpec.hs` to extract test cases (Name, Input, Expected)
- [ ] T004 Implement Hspec test generation in `libs/gram/tests/Spec/CorpusSpec.hs` to iterate over all corpus files
- [ ] T005 Register `Spec.CorpusSpec` in `libs/gram/tests/Test.hs`

## Phase 3: Full Syntax Compliance (P1)
**Goal**: Ensure parser handles all valid syntax defined in the standard Gram specification.
**Independent Test**: Run `cabal test` and verify zero failures for positive corpus cases.

- [ ] T006 [US1] Run initial corpus tests to identify syntax gaps and establish baseline failures
- [ ] T007 [US1] Analyze "Path" syntax failures and determine recursion/backtracking issues in `Gram.Parse`
- [ ] T008 [US1] Fix `parseRelationship` and `parsePath` in `libs/gram/src/Gram/Parse.hs` to support chained paths (e.g., `(a)-->(b)-->(c)`)
- [ ] T009 [US1] Address missing top-level syntax support (if any) in `fromGram` in `libs/gram/src/Gram/Parse.hs`
- [ ] T010 [US1] Verify all positive corpus test cases pass

## Phase 4: Parse Error Reporting (P2)
**Goal**: Ensure parser correctly rejects invalid syntax as defined by the corpus.
**Independent Test**: Run `cabal test` and verify zero failures for negative corpus cases (marked with `:error`).

- [ ] T011 [US2] Update `libs/gram/tests/Spec/CorpusSpec.hs` to handle `:error` tags in test cases
- [ ] T012 [US2] Implement assertion logic to verify parser returns `Left` for `:error` inputs
- [ ] T013 [US2] Verify all negative corpus test cases pass (i.e., parsing fails as expected)
- [ ] T014 [US2] [P] Improve error context in `libs/gram/src/Gram/Parse.hs` if valid inputs are being rejected or invalid inputs accepted incorrectly

## Phase 5: Polish
- [ ] T015 Run full test suite `cabal test` to ensure no regressions in existing tests
- [ ] T016 Document syntax support status and any known deviations in `libs/gram/SYNTAX_NOTES.md`

## Dependencies

1. **T001-T005** (Setup/Foundation) must be completed first to enable testing.
2. **T006** (Baseline) informs the fixes in **T007-T009**.
3. **T011-T013** (Error Reporting) can be done in parallel with **T008-T010** (Positive Compliance) once T004 is done, but logically follow syntax fixes.

## Parallel Execution Examples

- **US1 Fixes**: While one developer fixes Path syntax (T008), another can investigate other syntax gaps (T009).
- **Test Runner vs Parser**: T003/T004 (Runner) can be built while T008 (Parser Fix) is being researched.

## Implementation Strategy

1. **Foundation First**: We need the test harness working to see what's broken.
2. **Green Path**: Focus on getting valid syntax (positive tests) passing first.
3. **Red Path**: Ensure invalid syntax is rejected.

