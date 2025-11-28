# Tasks: Gram Serializer Updates

**Feature Branch**: `017-gram-serializer-updates`
**Feature Name**: Gram Serializer Updates

## Phase 1: Foundation & Edge Serialization
**Goal**: Enable serialization of single edge patterns `(a)-[r]->(b)`.

- [ ] T001 Analyze `Gram.Transform.hs` to define precise predicates for Edge and Walk patterns
- [ ] T002 Create `libs/gram/tests/Spec/Gram/SerializeSpec.hs` (or update if exists) with new test cases for Edge serialization
- [ ] T003 Implement `isEdgePattern` helper in `Gram.Serialize`
- [ ] T004 Update `serializePattern` in `Gram.Serialize` to handle Edge Patterns using path syntax `(a)-[r]->(b)`
- [ ] T005 Verify Edge serialization tests pass

## Phase 2: Walk Serialization
**Goal**: Enable serialization of chained walk patterns `(a)->(b)->(c)`.

- [ ] T006 Add test cases for Walk serialization to `SerializeSpec.hs`
- [ ] T007 Implement `isWalkPattern` helper in `Gram.Serialize`
- [ ] T008 Update `serializePattern` in `Gram.Serialize` to handle Walk Patterns using chained syntax
- [ ] T009 Verify Walk serialization tests pass

## Phase 3: Verification & Polish
**Goal**: Ensure round-trip correctness and no regressions.

- [ ] T010 Run full corpus round-trip tests (`Spec.Gram.CorpusSpec`) and verify failures are resolved
- [ ] T011 Address any regressions in non-path serialization
- [ ] T012 Update `libs/gram/SYNTAX_NOTES.md` to reflect full serialization support
- [ ] T013 Run full test suite `cabal test gram-test` to ensure 100% pass rate

