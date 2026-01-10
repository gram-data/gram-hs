# Tasks: Canonical JSON Pattern Representation

**Input**: Design documents from `/specs/029-canonical-json-pattern/`  
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are included as part of implementation (not TDD approach). Each user story includes test verification as final step.

**Organization**: Tasks are grouped by user story (P1 → P2 priorities) to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

Based on plan.md structure:
- **Library code**: `libs/gram/src/Gram/`
- **CLI code**: `apps/gramref-cli/src/Gramref/CLI/`
- **Tests**: `libs/gram/tests/Spec/Gram/` and `apps/gramref-cli/tests/Spec/Gramref/CLI/`
- **Test data**: `libs/gram/test-data/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and directory structure

- [x] T001 Create directory `libs/gram/src/Gram/Schema/` for schema generation modules
- [x] T002 Create directory `libs/gram/test-data/roundtrip/custom/` for custom roundtrip test cases
- [x] T003 Create symbolic link `libs/gram/test-data/roundtrip/corpus` → `libs/gram/test-data/` (tree-sitter-gram corpus)
- [x] T004 [P] Create test spec directory `libs/gram/tests/Spec/Gram/` if not exists
- [x] T005 [P] Create CLI test directory `apps/gramref-cli/tests/Spec/Gramref/CLI/` if not exists

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core JSON serialization that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T006 Move `apps/gramref-cli/src/Gramref/CLI/JSON.hs` to `libs/gram/src/Gram/JSON.hs` (refactor for library-level access)
- [x] T007 Update `libs/gram/gram.cabal` to expose `Gram.JSON` module
- [x] T008 Add `FromJSON` instance for `Pattern Subject` in `libs/gram/src/Gram/JSON.hs` (deserialization support)
- [x] T009 Add `FromJSON` instance for `Subject` in `libs/gram/src/Gram/JSON.hs`
- [x] T010 Add `FromJSON` instance for all `Value` types in `libs/gram/src/Gram/JSON.hs` (VInteger, VDecimal, VBoolean, VString, VSymbol, VTaggedString, VArray, VMap, VRange, VMeasurement)
- [x] T011 Update `apps/gramref-cli/src/Gramref/CLI/Output.hs` to import from `Gram.JSON` instead of `Gramref.CLI.JSON`
- [x] T012 Update `apps/gramref-cli/src/Gramref/CLI/Commands/Parse.hs` to import from `Gram.JSON`
- [x] T013 Remove old `apps/gramref-cli/src/Gramref/CLI/JSON.hs` file
- [x] T014 Run `timeout 60 cabal build all` to verify refactoring compiles without errors
- [x] T015 Run `timeout 60 cabal test gramref-cli` to verify existing CLI tests still pass after refactoring

**Checkpoint**: JSON serialization/deserialization available at library level - user story implementation can now begin

---

## Phase 3: User Story 2 - Convert Between Gram Notation and JSON (Priority: P1)

**Goal**: Enable bidirectional conversion between gram notation and canonical JSON format with data integrity

**Independent Test**: Convert pattern from gram → JSON → gram and verify semantic equivalence

**Dependencies**: Requires Phase 2 (JSON serialization/deserialization) complete

### Implementation for User Story 2

- [ ] T016 [P] [US2] Create test helper function `roundtripTest :: Pattern Subject -> Bool` in `libs/gram/tests/Spec/Gram/JSONSpec.hs` that verifies `fromJSON (toJSON p) == Success p`
- [ ] T017 [P] [US2] Add unit test for simple pattern roundtrip in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T018 [P] [US2] Add unit test for nested pattern roundtrip in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T019 [P] [US2] Add unit test for all value types (VInteger, VDecimal, VBoolean, VString) roundtrip in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T020 [P] [US2] Add unit test for complex value types (VSymbol, VTaggedString, VRange, VMeasurement) roundtrip in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T021 [P] [US2] Add unit test for array and map value types roundtrip in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T022 [US2] Add unit test for patterns with special characters in properties in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T023 [US2] Add unit test for empty properties, empty labels, anonymous subjects in `libs/gram/tests/Spec/Gram/JSONSpec.hs`
- [ ] T024 [US2] Update `libs/gram/gram.cabal` test-suite to include `JSONSpec`
- [ ] T025 [US2] Run `timeout 60 cabal test gram:test:gram-test` to verify all User Story 2 unit tests pass
- [ ] T026 [US2] Update `apps/gramref-cli/src/Gramref/CLI/Commands/Convert.hs` to support JSON as input format using `fromJSON` from `Gram.JSON`
- [ ] T027 [US2] Add CLI test in `apps/gramref-cli/tests/Spec/Gramref/CLI/ConvertSpec.hs` for JSON → gram conversion
- [ ] T028 [US2] Run `timeout 60 cabal test gramref-cli` to verify CLI conversion tests pass
- [ ] T029 [US2] Manual test: `echo '(a:Node {x: 1})' | cabal run gramref-cli -- parse --format json --value-only | cabal run gramref-cli -- convert --from json --to gram` (verify output)
- [ ] T030 [US2] Git commit: "feat(json): implement bidirectional gram/JSON conversion with roundtrip tests - US2"

**Checkpoint**: Gram ↔ JSON conversion working with unit test coverage ✅

---

## Phase 4: User Story 3 - Verify JSON Serialization Correctness (Priority: P1)

**Goal**: Automated roundtrip tests against entire tree-sitter-gram corpus ensuring 100% pass rate

**Independent Test**: Run roundtrip tests on corpus; verify all patterns successfully roundtrip without data loss

**Dependencies**: Requires User Story 2 (bidirectional conversion) complete

### Implementation for User Story 3

- [ ] T031 [US3] Create `libs/gram/tests/Spec/Gram/RoundtripSpec.hs` with hspec test structure
- [ ] T032 [US3] Implement corpus file discovery function `findCorpusFiles :: IO [FilePath]` in `RoundtripSpec.hs` to find all `.gram` test files in `test-data/`
- [ ] T033 [US3] Implement `testRoundtrip :: FilePath -> Spec` function that reads gram file, converts to JSON, back to gram, and verifies equivalence using `Gram.Parse.fromGram` and `Gram.Serialize.toGram`
- [ ] T034 [US3] Add QuickCheck property test `prop_roundtripPreservesStructure :: Pattern Subject -> Bool` in `RoundtripSpec.hs`
- [ ] T035 [US3] Add QuickCheck property test `prop_roundtripPreservesAllValueTypes` for each value type in `RoundtripSpec.hs`
- [ ] T036 [US3] Add specific test cases for edge cases: empty properties in `libs/gram/test-data/roundtrip/custom/empty-properties.gram`
- [ ] T037 [US3] Add specific test cases for edge cases: empty labels in `libs/gram/test-data/roundtrip/custom/empty-labels.gram`
- [ ] T038 [US3] Add specific test cases for edge cases: anonymous subjects in `libs/gram/test-data/roundtrip/custom/anonymous-subject.gram`
- [ ] T039 [US3] Add specific test cases for edge cases: deeply nested patterns in `libs/gram/test-data/roundtrip/custom/deep-nesting.gram`
- [ ] T040 [US3] Add specific test cases for edge cases: implicit root pattern in `libs/gram/test-data/roundtrip/custom/implicit-root.gram`
- [ ] T041 [US3] Update `libs/gram/gram.cabal` test-suite to include `RoundtripSpec`
- [ ] T042 [US3] Run `timeout 120 cabal test gram:test:gram-test --test-option="--match=Roundtrip"` to verify all roundtrip tests pass (may take longer for corpus)
- [ ] T043 [US3] Verify test output shows 100% corpus pass rate (document count and pass/fail stats)
- [ ] T044 [US3] Add CI check in `.github/workflows/` (if exists) or document in README that roundtrip tests must pass
- [ ] T045 [US3] Git commit: "test(json): add comprehensive roundtrip testing against tree-sitter-gram corpus - US3"

**Checkpoint**: 100% roundtrip test pass rate across entire corpus ✅

---

## Phase 5: User Story 1 - Access Canonical JSON Format Documentation (Priority: P1)

**Goal**: Developers can obtain JSON Schema document that formally specifies Pattern<Subject> structure

**Independent Test**: Run `gramref schema --format json-schema`; verify output validates against known good JSON; verify all value types defined

**Dependencies**: Requires User Story 2 (JSON serialization) complete; can proceed in parallel with User Story 3

### Implementation for User Story 1

- [ ] T046 [P] [US1] Create `libs/gram/src/Gram/Schema.hs` with module exports for schema generation
- [ ] T047 [P] [US1] Create `libs/gram/src/Gram/Schema/JSONSchema.hs` for JSON Schema generation
- [ ] T048 [US1] Implement `generatePatternSchema :: Value` function in `JSONSchema.hs` that constructs JSON Schema Draft 2020-12 document for Pattern
- [ ] T049 [US1] Add schema definitions for Subject in `generateSubjectDefinition :: Value`
- [ ] T050 [US1] Add schema definitions for Value types using `oneOf` with discriminators in `generateValueDefinition :: Value`
- [ ] T051 [US1] Add schema definitions for each complex value type (Symbol, TaggedString, Range, Measurement) in `JSONSchema.hs`
- [ ] T052 [US1] Add version information and metadata (`$schema`, `$id`, `version`) to generated schema in `JSONSchema.hs`
- [ ] T053 [US1] Update `libs/gram/gram.cabal` to expose `Gram.Schema` and `Gram.Schema.JSONSchema` modules
- [ ] T054 [US1] Create `apps/gramref-cli/src/Gramref/CLI/Commands/Schema.hs` for schema CLI command
- [ ] T055 [US1] Implement `SchemaOptions` data type with `format` field (JSONSchema | TypeScript | Rust) in `Schema.hs`
- [ ] T056 [US1] Implement `runSchema :: SchemaOptions -> IO ExitCode` that calls appropriate generator in `Schema.hs`
- [ ] T057 [US1] Add schema command parser in `apps/gramref-cli/src/Gramref/CLI.hs` to wire `schema` subcommand
- [ ] T058 [US1] Update `apps/gramref-cli/gramref-cli.cabal` to include `Commands.Schema` module
- [ ] T059 [US1] Create `libs/gram/tests/Spec/Gram/SchemaSpec.hs` for schema generation tests
- [ ] T060 [US1] Add test that generated JSON Schema is valid JSON in `SchemaSpec.hs`
- [ ] T061 [US1] Add test that generated schema includes all required fields ($schema, definitions, Pattern, Subject, Value) in `SchemaSpec.hs`
- [ ] T062 [US1] Add test that generated schema includes version information in `SchemaSpec.hs`
- [ ] T063 [US1] Add test that generated schema includes definitions for all 10 value types in `SchemaSpec.hs`
- [ ] T064 [US1] Update `libs/gram/gram.cabal` test-suite to include `SchemaSpec`
- [ ] T065 [US1] Run `timeout 60 cabal test gram:test:gram-test --test-option="--match=Schema"` to verify schema generation tests pass
- [ ] T066 [US1] Create CLI test in `apps/gramref-cli/tests/Spec/Gramref/CLI/SchemaCommandSpec.hs` for `gramref schema` command
- [ ] T067 [US1] Add test that `gramref schema --format json-schema` outputs valid JSON Schema in `SchemaCommandSpec.hs`
- [ ] T068 [US1] Run `timeout 60 cabal test gramref-cli --test-option="--match=Schema"` to verify CLI schema tests pass
- [ ] T069 [US1] Manual test: `cabal run gramref-cli -- schema --format json-schema > /tmp/schema.json` and verify output contains all expected definitions
- [ ] T070 [US1] Manual test: Use external validator (if available) to validate generated schema against JSON Schema meta-schema
- [ ] T071 [US1] Manual test: Generate JSON from test pattern and validate it against generated schema
- [ ] T072 [US1] Update `apps/gramref-cli/man/gramref.1` manpage to document `schema` command with `--format` option
- [ ] T073 [US1] Git commit: "feat(schema): add JSON Schema generation with gramref schema command - US1"

**Checkpoint**: JSON Schema generation working with CLI command ✅

---

## Phase 6: User Story 4 - Generate Type Definitions from Schema (Priority: P2)

**Goal**: Generate TypeScript and Rust type definitions from JSON Schema for downstream ports

**Independent Test**: Generate TypeScript/Rust types; compile in target language; verify they model Pattern<Subject> correctly

**Dependencies**: Requires User Story 1 (schema generation) complete

### Implementation for User Story 4

- [ ] T074 [P] [US4] Create `libs/gram/src/Gram/Schema/TypeScript.hs` for TypeScript type generation
- [ ] T075 [P] [US4] Create `libs/gram/src/Gram/Schema/Rust.hs` for Rust type generation
- [ ] T076 [US4] Implement `generateTypeScriptTypes :: Text` function in `TypeScript.hs` that generates TypeScript interfaces
- [ ] T077 [US4] Add TypeScript interface generation for Pattern in `TypeScript.hs`
- [ ] T078 [US4] Add TypeScript interface generation for Subject in `TypeScript.hs`
- [ ] T079 [US4] Add TypeScript discriminated union generation for Value types in `TypeScript.hs`
- [ ] T080 [US4] Add TypeScript type guard functions (isValueSymbol, isValueRange, etc.) in `TypeScript.hs`
- [ ] T081 [US4] Add JSDoc comments to generated TypeScript types in `TypeScript.hs`
- [ ] T082 [US4] Implement `generateRustTypes :: Text` function in `Rust.hs` that generates Rust structs
- [ ] T083 [US4] Add Rust struct generation for Pattern with serde derives in `Rust.hs`
- [ ] T084 [US4] Add Rust struct generation for Subject with serde derives in `Rust.hs`
- [ ] T085 [US4] Add Rust enum generation for Value types with #[serde(untagged)] in `Rust.hs`
- [ ] T086 [US4] Add Rust convenience constructors (new methods) for complex value types in `Rust.hs`
- [ ] T087 [US4] Update `libs/gram/gram.cabal` to expose `Gram.Schema.TypeScript` and `Gram.Schema.Rust` modules
- [ ] T088 [US4] Update `apps/gramref-cli/src/Gramref/CLI/Commands/Schema.hs` to support `--format typescript` option
- [ ] T089 [US4] Update `apps/gramref-cli/src/Gramref/CLI/Commands/Schema.hs` to support `--format rust` option
- [ ] T090 [US4] Add test in `libs/gram/tests/Spec/Gram/SchemaSpec.hs` that generated TypeScript is valid syntax (basic string checks)
- [ ] T091 [US4] Add test in `libs/gram/tests/Spec/Gram/SchemaSpec.hs` that generated Rust is valid syntax (basic string checks)
- [ ] T092 [US4] Add test that TypeScript output includes all expected interfaces (Pattern, Subject, Value types) in `SchemaSpec.hs`
- [ ] T093 [US4] Add test that Rust output includes all expected structs and enums in `SchemaSpec.hs`
- [ ] T094 [US4] Run `timeout 60 cabal test gram:test:gram-test --test-option="--match=Schema"` to verify type generation tests pass
- [ ] T095 [US4] Manual test: `cabal run gramref-cli -- schema --format typescript > /tmp/pattern.ts` and verify output
- [ ] T096 [US4] Manual test: `cabal run gramref-cli -- schema --format rust > /tmp/pattern.rs` and verify output
- [ ] T097 [US4] External validation: If TypeScript/tsc available, compile generated TypeScript types: `tsc --noEmit /tmp/pattern.ts`
- [ ] T098 [US4] External validation: If Rust/cargo available, create test crate and verify generated Rust compiles
- [ ] T099 [US4] Update `apps/gramref-cli/man/gramref.1` manpage to document `--format typescript` and `--format rust` options
- [ ] T100 [US4] Git commit: "feat(schema): add TypeScript and Rust type generation - US4"

**Checkpoint**: Type generation working for TypeScript and Rust ✅

---

## Phase 7: User Story 5 - Validate Custom JSON Output (Priority: P2)

**Goal**: Validate JSON output against canonical schema with clear error messages for discrepancies

**Independent Test**: Validate valid/invalid JSON against schema; verify validation passes/fails appropriately with helpful errors

**Dependencies**: Requires User Story 1 (schema generation) complete; can proceed in parallel with User Story 4

### Implementation for User Story 5

- [ ] T101 [US5] Add `validateJSON :: Value -> Value -> Either ValidationError ()` function in `libs/gram/src/Gram/JSON.hs` that validates JSON against schema
- [ ] T102 [US5] Implement schema validation logic: check required fields (value, elements) present in `validateJSON`
- [ ] T103 [US5] Implement schema validation logic: check field types match schema definitions in `validateJSON`
- [ ] T104 [US5] Implement schema validation logic: validate nested structures recursively in `validateJSON`
- [ ] T105 [US5] Implement schema validation logic: validate Value discriminated unions in `validateJSON`
- [ ] T106 [US5] Create custom `ValidationError` data type with field path, expected type, actual type in `libs/gram/src/Gram/JSON.hs`
- [ ] T107 [US5] Implement error message formatting for `ValidationError` that indicates which field failed in `Gram.JSON`
- [ ] T108 [US5] Create `apps/gramref-cli/src/Gramref/CLI/Commands/Validate.hs` (update existing or create new) for validation command
- [ ] T109 [US5] Update `runValidate :: ValidateOptions -> IO ExitCode` to support `--schema` option for schema file path in `Validate.hs`
- [ ] T110 [US5] Implement validation command that reads JSON file, loads schema, calls `validateJSON`, and reports results in `Validate.hs`
- [ ] T111 [US5] Add batch validation support: accept multiple JSON files and report summary in `Validate.hs`
- [ ] T112 [US5] Add test in `libs/gram/tests/Spec/Gram/JSONSpec.hs` that valid JSON passes validation
- [ ] T113 [US5] Add test that JSON missing required field fails validation with clear error in `JSONSpec.hs`
- [ ] T114 [US5] Add test that JSON with incorrect type fails validation with clear error in `JSONSpec.hs`
- [ ] T115 [US5] Add test that JSON with invalid Value discriminator fails validation in `JSONSpec.hs`
- [ ] T116 [US5] Add test that non-canonical formatting (different key order) passes validation in `JSONSpec.hs`
- [ ] T117 [US5] Run `timeout 60 cabal test gram:test:gram-test --test-option="--match=JSON"` to verify validation tests pass
- [ ] T118 [US5] Create CLI test in `apps/gramref-cli/tests/Spec/Gramref/CLI/ValidateSpec.hs` for validate command
- [ ] T119 [US5] Add test that `gramref validate valid.json --schema schema.json` exits with code 0 in `ValidateSpec.hs`
- [ ] T120 [US5] Add test that `gramref validate invalid.json --schema schema.json` exits with non-zero code in `ValidateSpec.hs`
- [ ] T121 [US5] Run `timeout 60 cabal test gramref-cli --test-option="--match=Validate"` to verify CLI validation tests pass
- [ ] T122 [US5] Manual test: Generate schema, create invalid JSON (missing field), run validation, verify error message is clear
- [ ] T123 [US5] Manual test: Generate schema, create invalid JSON (wrong type), run validation, verify error message indicates type mismatch
- [ ] T124 [US5] Manual test: Batch validation with mix of valid/invalid files, verify summary report
- [ ] T125 [US5] Update `apps/gramref-cli/man/gramref.1` manpage to document updated `validate` command with `--schema` option
- [ ] T126 [US5] Git commit: "feat(validate): add JSON validation against schema with clear error messages - US5"

**Checkpoint**: JSON validation working with helpful error messages ✅

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, performance, and cross-cutting improvements

- [ ] T127 [P] Update `docs/reference/features/gram-serialization.md` to document canonical JSON format
- [ ] T128 [P] Create new documentation file `docs/reference/features/canonical-json-format.md` with detailed format specification
- [ ] T129 [P] Add examples section to `docs/reference/features/canonical-json-format.md` showing all value types in JSON
- [ ] T130 [P] Update `README.md` at repository root to mention JSON Schema generation capability
- [ ] T131 [P] Update `libs/gram/README.md` (if exists) to document `Gram.JSON` and `Gram.Schema` modules
- [ ] T132 [P] Add Haddock documentation comments to all public functions in `libs/gram/src/Gram/JSON.hs`
- [ ] T133 [P] Add Haddock documentation comments to all public functions in `libs/gram/src/Gram/Schema.hs`
- [ ] T134 [P] Add Haddock documentation comments to all public functions in `libs/gram/src/Gram/Schema/JSONSchema.hs`
- [ ] T135 [P] Add Haddock documentation comments to all public functions in `libs/gram/src/Gram/Schema/TypeScript.hs`
- [ ] T136 [P] Add Haddock documentation comments to all public functions in `libs/gram/src/Gram/Schema/Rust.hs`
- [ ] T137 Generate Haddock documentation: `cabal haddock gram` and verify JSON/Schema modules are documented
- [ ] T138 Add code example in `libs/gram/src/Gram/JSON.hs` module documentation showing roundtrip usage
- [ ] T139 Add code example in `libs/gram/src/Gram/Schema.hs` module documentation showing schema generation usage
- [ ] T140 Performance test: Verify large pattern (1000+ nested elements) converts to/from JSON without stack overflow
- [ ] T141 Performance test: Verify schema generation completes in <1 second (measure with `time` command)
- [ ] T142 Performance test: Verify roundtrip test suite completes in <2 minutes total
- [ ] T143 Add `.cabal` file version bump to `0.2.0` in `libs/gram/gram.cabal` (minor version for new features)
- [ ] T144 Add `.cabal` file version bump to `0.2.0` in `apps/gramref-cli/gramref-cli.cabal`
- [ ] T145 Update `libs/gram/CHANGELOG.md` with new features: JSON serialization/deserialization, schema generation, type generation
- [ ] T146 Update `apps/gramref-cli/README.md` with examples of new `schema` command usage
- [ ] T147 Run full test suite: `timeout 120 cabal test all` to verify all tests pass across all packages
- [ ] T148 Run linter/code quality check: `hlint libs/gram/src/ apps/gramref-cli/src/` (if hlint available)
- [ ] T149 Verify quickstart examples from `specs/029-canonical-json-pattern/quickstart.md` actually work
- [ ] T150 Create example files in `examples/` directory: `examples/pattern-to-json.sh` showing conversion workflow
- [ ] T151 Create example files in `examples/` directory: `examples/schema-generation.sh` showing schema generation
- [ ] T152 Git commit: "docs: finalize canonical JSON pattern feature documentation and examples"
- [ ] T153 Git tag: `v0.2.0` (after all commits for this feature are complete)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 2 (Phase 3)**: Depends on Foundational (Phase 2) - First P1 story, provides bidirectional conversion
- **User Story 3 (Phase 4)**: Depends on User Story 2 completion - Builds on conversion for testing
- **User Story 1 (Phase 5)**: Depends on Foundational (Phase 2) - Can proceed in parallel with US2/US3
- **User Story 4 (Phase 6)**: Depends on User Story 1 completion - Builds on schema for type generation
- **User Story 5 (Phase 7)**: Depends on User Story 1 completion - Can proceed in parallel with US4
- **Polish (Phase 8)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Schema generation - depends only on Foundational, independent of other stories
- **User Story 2 (P1)**: Bidirectional conversion - depends only on Foundational, independent of other stories
- **User Story 3 (P1)**: Roundtrip testing - depends on US2 (needs bidirectional conversion to test)
- **User Story 4 (P2)**: Type generation - depends on US1 (needs schema to generate from)
- **User Story 5 (P2)**: Validation - depends on US1 (needs schema to validate against)

### Recommended Execution Order

1. **Phase 1: Setup** (T001-T005) - Quick setup
2. **Phase 2: Foundational** (T006-T015) - CRITICAL blocking phase
3. **Choice Point**: After Phase 2, can pursue different paths:
   - **Path A (Schema First)**: Phase 5 (US1) → Phase 6 (US4) & Phase 7 (US5) in parallel
   - **Path B (Conversion First)**: Phase 3 (US2) → Phase 4 (US3) → Phase 5 (US1) → Phase 6 (US4) & Phase 7 (US5)
   - **Recommended**: Path B (conversion first) provides immediate value and tests foundation before schema work

### Parallel Opportunities Within User Stories

**User Story 2** (Bidirectional Conversion):
- T016-T021 (all unit tests) can run in parallel - different test cases, same file

**User Story 1** (Schema Generation):
- T046-T047 (create module files) can run in parallel - different files
- T060-T063 (schema validation tests) can run in parallel - different test cases

**User Story 4** (Type Generation):
- T074-T075 (create TypeScript and Rust modules) can run in parallel - different files
- T090-T093 (type generation tests) can run in parallel - different test cases

**User Story 5** (Validation):
- T112-T116 (validation tests) can run in parallel - different test cases

**Polish Phase**:
- T127-T136 (all documentation tasks) can run in parallel - different files

---

## Parallel Example: User Story 2 (Bidirectional Conversion)

```bash
# After Phase 2 complete, can launch these tasks in parallel:
Task T016: Create roundtrip test helper in JSONSpec.hs
Task T017: Add simple pattern roundtrip test in JSONSpec.hs
Task T018: Add nested pattern roundtrip test in JSONSpec.hs
Task T019: Add value types roundtrip test in JSONSpec.hs
Task T020: Add complex value types roundtrip test in JSONSpec.hs
Task T021: Add array/map value types roundtrip test in JSONSpec.hs

# All write to same file but independent test cases - review/integrate after
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 + 3)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T015) - CRITICAL
3. Complete Phase 3: User Story 2 (T016-T030) - Bidirectional conversion
4. Complete Phase 4: User Story 3 (T031-T045) - Roundtrip testing
5. Complete Phase 5: User Story 1 (T046-T073) - Schema generation
6. **STOP and VALIDATE**: Test core functionality independently
7. Deploy/demo if ready

### Incremental Delivery

1. **Foundation** (Setup + Foundational) → JSON at library level ✅
2. **Add US2** (Conversion) → Gram ↔ JSON working ✅ Deploy/Demo
3. **Add US3** (Testing) → 100% corpus coverage ✅ Deploy/Demo
4. **Add US1** (Schema) → Formal specification available ✅ Deploy/Demo
5. **Add US4** (Types) → TypeScript/Rust generation ✅ Deploy/Demo
6. **Add US5** (Validation) → Complete validation story ✅ Deploy/Demo
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers after Foundational phase completes:

- **Developer A**: User Story 2 (Conversion) + User Story 3 (Testing) [sequential dependency]
- **Developer B**: User Story 1 (Schema) → User Story 4 (Types) [sequential dependency]
- **Developer C**: User Story 5 (Validation) [depends on US1]

Or single developer priority order: US2 → US3 → US1 → US4 & US5 in parallel → Polish

---

## Notes

- **[P] tasks**: Different files or independent test cases, no dependencies
- **[Story] label**: Maps task to specific user story for traceability
- **File paths**: All paths relative to repository root or explicitly stated
- **Git commits**: After each user story completion (T030, T045, T073, T100, T126, T152)
- **Test timeouts**: Always use `timeout` command to prevent hanging tests
- **Checkpoints**: Validate each user story independently before proceeding
- **Avoid**: Cross-story dependencies that break independence; vague tasks without file paths

---

## Testing Performance Guidelines

### Test Execution Timeouts

**CRITICAL**: Always use timeouts when running tests to prevent hanging:

- **Initial build/compile**: `timeout 60 cabal build` (60 seconds)
- **Unit/property tests**: `timeout 60 cabal test` (60 seconds for fast tests)
- **Roundtrip corpus tests**: `timeout 120 cabal test` (120 seconds for corpus - 130+ files)
- **Full test suite**: `timeout 180 cabal test all` (3 minutes for all packages)

### Test Performance Requirements

- **Unit tests**: Each test should complete in <100ms
- **Property tests**: Use small generators (max 100 cases), complete in <5 seconds
- **Roundtrip tests**: Entire corpus (<130 files) should complete in <2 minutes
- **Schema generation**: Should complete in <1 second
- **Full test suite**: All packages should complete in <3 minutes total

### Troubleshooting Slow Tests

If tests hang or take too long:

1. **Check for infinite recursion**: Especially in JSON FromJSON instances - verify base cases
2. **Check for ambiguous function calls**: Use qualified imports (`Prelude.foldl` vs `Data.List.foldl`)
3. **Check test data size**: Ensure QuickCheck uses `resize` to limit input size
4. **Check for strict evaluation**: Use `seq` or `deepseq` to force evaluation where needed
5. **Profile if needed**: `cabal test --enable-profiling` to identify bottlenecks

### Test Execution Commands

```bash
# Build with timeout:
timeout 60 cabal build all

# Test individual package:
timeout 60 cabal test gram:test:gram-test

# Test specific spec:
timeout 60 cabal test gram:test:gram-test --test-option="--match=JSON"

# Test all packages:
timeout 180 cabal test all

# Roundtrip tests (longer timeout for corpus):
timeout 120 cabal test gram:test:gram-test --test-option="--match=Roundtrip"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 3 minutes total
- Investigate any individual spec that takes >30 seconds
- Use `--test-show-details=streaming` to see progress

---

## Task Statistics

**Total Tasks**: 153
- Phase 1 (Setup): 5 tasks
- Phase 2 (Foundational): 10 tasks  
- Phase 3 (US2 - Conversion): 15 tasks
- Phase 4 (US3 - Testing): 15 tasks
- Phase 5 (US1 - Schema): 28 tasks
- Phase 6 (US4 - Types): 27 tasks
- Phase 7 (US5 - Validation): 26 tasks
- Phase 8 (Polish): 27 tasks

**Parallel Opportunities**: 47 tasks marked [P]

**Independent Test Criteria**:
- **US1**: Run `gramref schema --format json-schema` and validate output
- **US2**: Convert pattern gram → JSON → gram and verify equivalence
- **US3**: Run roundtrip tests on corpus and verify 100% pass rate
- **US4**: Generate TypeScript/Rust types and verify they compile
- **US5**: Validate JSON against schema and verify error messages

**Suggested MVP Scope**: User Stories 1, 2, and 3 (P1 priorities) = Core JSON support with schema and testing
