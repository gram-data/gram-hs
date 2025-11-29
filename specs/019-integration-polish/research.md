# Research: Integration and Polish

**Branch**: `019-integration-polish`

## Technical Decisions

### 1. API Export Strategy
**Decision**: Explicit export lists for `Pattern.Core` and `Pattern`.
**Rationale**: Prevents accidental leakage of internal helper functions. Ensures a stable API contract for consumers.
**Alternatives Considered**:
- *Implicit exports (`module Pattern.Core where`)*: Rejected. Exposes everything. Bad for libraries.
- *Internal module pattern (`Pattern.Internal`)*: Could be used, but `Pattern.Core` effectively serves this role if we carefully control exports. The current structure re-exports `Pattern.Core` via `Pattern`, which is standard "facade" pattern.

### 2. Documentation Standard
**Decision**: 100% Haddock coverage with `>>>` examples for all public functions.
**Rationale**: Essential for a reference implementation. Examples serve as doctests (conceptually) and teaching aids.

### 3. Test Coverage
**Decision**: Verify all Typeclass laws (Functor, Applicative, Monad, Comonad, Traversable, Semigroup, Monoid) via Property-Based Testing.
**Rationale**: "Rigorous semantics" requires mathematical verification, not just example-based testing.

## Validation Plan
1. **API Audit**: Manually verify export lists.
2. **Doc Check**: Use `cabal haddock` to ensure no warnings about missing docs.
3. **Law Check**: Verify `tests/Spec/Pattern/Properties.hs` covers all instances.

