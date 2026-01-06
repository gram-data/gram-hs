# TODO: Project Roadmap & History

**Approach**: Incremental, rigorous, foundation-first.
**Workflow**: Feature branches -> Speckit (`specify`, `plan`, `tasks`) -> Implementation -> Test -> Merge.
**Constitution**: All code must adhere to [Constitution](.specify/memory/constitution.md).

---

## 📜 History & Completed Work

### Core Pattern Library (`libs/pattern`)
Foundational implementation of the generic `Pattern v` recursive data structure.

- **Feature 1: Core Data Type**: Defined `Pattern v` with `value` and `elements`. Recursive tree structure verified.
- **Feature 2: Basic Typeclasses**: Implemented `Show` and `Eq` instances.
- **Feature 3: Construction Functions**: Added `pattern`, `patternWith`, `fromList`.
- **Feature 4: Functor**: `fmap` preserves structure while transforming values.
- **Feature 5: Foldable**: `foldr`, `foldl`, `toList` (flatten), `toTuple`.
- **Feature 6: Traversable**: `traverse`, `sequenceA` for effectful traversals.
- **Feature 7: Query Functions**: `length`, `size`, `depth`, `values`. Performance constraints met.
- **Feature 8: Advanced Typeclasses**:
  - `Ord`: Lexicographic ordering.
  - `Semigroup` / `Monoid`: Combination and Identity patterns.
  - `Hashable`: Structure-preserving hashing.
  - `Applicative`: Zip-like application of pattern functions to pattern values.
- **Feature 9: Predicate Matching**: `anyValue`, `allValues` (values); `filterPatterns`, `findPattern` (structure); `matches`, `contains`.
- **Feature 10: Comonad**: `extract` (focus), `duplicate` (context at every position), `extend`. Added `depthAt`, `sizeAt`, `indicesAt`.
- **Feature 19: Integration & Polish**: Explicit export lists, 100% Haddock documentation, property-based law verification, and final cleanup.
- **Feature 23: Graph Lens**: Interpretive graph views via predicate-based lens. Provides nodes, relationships, walks, navigation, and graph analysis operations. Enables multiple graph interpretations of the same Pattern through a minimal, elegant design.
- **Feature 28: Paramorphism**: Structure-aware folding via `para` function. Extends `Foldable` to provide structure-aware folding, just as `Comonad` extends `Functor`. Enables depth-weighted aggregations, element-count-aware operations, and nesting-level statistics. Includes comprehensive tests, documentation, and porting guidance.

### Gram Library (`libs/gram`)
Serialization and parsing for `Pattern Subject`.

- **Serialization**: `toGram` handles all value types, nested patterns, relationships, and anonymous subjects.
- **Parsing**: `fromGram` implements initial support for standard and extended values, relationships, and nesting.
- **Parsing Conformance**: Verified 100% pass rate against `tree-sitter-gram` corpus (Feature 16).
- **Validation**: `Gram.Validate` module implemented with duplicate definition, undefined reference, and arity checking.

### Subject Identity & Serialization (Feature 20)
Completed robust identity handling and round-trip capabilities.

- **Identity Preservation**: Implemented sequential ID generation for anonymous subjects to ensure round-trip stability.
- **Implicit Root**: Distinguishes between empty nodes `()` and implicit root containers `{}`.
- **Round-Trip Verification**: Validated structural equality after serialization/deserialization cycles against the full test corpus.

### Pattern Documentation (Feature 26)
Completed comprehensive user guide documentation.

- **User Guide**: Created progressive learning path from basic concepts through advanced morphisms
- **Gram Notation Reference**: Added appendix documenting special cases (nodes, annotations, relationships, references)
- **Use Cases**: Documented real-world applications including knowledge graphs, agentic systems, and design patterns

---

## 🗺️ Roadmap

**Status**: Foundation is complete. Remaining features are deferred until concrete use cases emerge. See `ROADMAP-ANALYSIS.md` for detailed analysis.

**Current Focus**: Validation and refinement of existing foundation rather than adding new features.

---

### 3. Pattern Morphisms
**Priority**: Deferred / Unlikely Needed
**Goal**: Basic morphisms if needed for core.

**Status**: `Functor` already provides structure-preserving transformations (`fmap`). `forget` would be trivial (`fmap (const ())`). No clear use case identified.

#### 12.1 Morphism Design
- [ ] **STOP and REVIEW**: Are morphisms needed yet? (Currently: No clear need - Functor covers this)
- [ ] Define `PatternMorphism` type synonym (only if abstraction proves valuable)
- [ ] Implement `homomorphism` if clearly needed (currently: `fmap` provides this)
- [ ] Implement `forget` if clearly needed (currently: `fmap (const ())` provides this)

---

### 3.5 Paramorphism for Structure-Aware Folding
**Priority**: ✅ Completed
**Goal**: Enable folding operations that have access to the full pattern structure at each position, not just values.

**Rationale**: Just as Comonad extends Functor to see structure (`extend` vs `fmap`), Paramorphism extends Foldable to see structure (`para` vs `foldr`). Patterns aren't just containers for values—the structure of the pattern *is information*. Paramorphism enables folding while preserving access to the original pattern subtree at each step, enabling structure-aware aggregations.

**Status**: ✅ **COMPLETED** - Implemented `para` function providing structure-aware folding. The folding function receives both the current pattern subtree and recursively computed results from children. Enables depth-weighted sums, element-count-aware aggregations, and nesting-level statistics. Comprehensive tests, documentation, and porting guidance included.

**Design Notes**:
- Paramorphism signature: `para :: (Pattern v -> [r] -> r) -> Pattern v -> r`
- The folding function receives: (1) the current pattern subtree, (2) the list of recursively computed results from children
- Enables structure-aware operations like: computing depth-aware sums, structure-preserving transformations during fold, context-dependent aggregations
- Relationship to Comonad: Comonad provides structure-aware *transformations* (`extend`), Paramorphism provides structure-aware *folding* (`para`)

#### 12.5 Paramorphism Implementation
- [x] **STOP and REVIEW**: Evaluate need for structure-aware folding beyond `Foldable` - ✅ Implemented
- [x] Research standard paramorphism patterns for recursive tree structures - ✅ Completed
- [x] Design `para :: (Pattern v -> [r] -> r) -> Pattern v -> r` function signature - ✅ Implemented
- [x] Consider: `paraMap :: (Pattern v -> [r] -> r) -> Pattern v -> Pattern r` (structure-preserving paramorphism) - ✅ Deferred (not needed for MVP)
- [x] Consider: `paraWith :: (Pattern v -> [r] -> r) -> Pattern v -> r` (with explicit accumulator) - ✅ Deferred (standard `para` sufficient)
- [x] Implement `para` function with recursive structure access - ✅ Completed
- [x] Write tests: verify paramorphism preserves structure information access - ✅ 25 unit tests + 6 property-based tests
- [x] Write tests: verify paramorphism laws (if applicable) - ✅ Property-based tests verify structure access, value access, order preservation
- [x] Write tests: compare with `Foldable` operations to demonstrate structure awareness - ✅ Comparison tests included
- [x] Write documentation: explain relationship to `Foldable` and `Comonad` - ✅ User guide and reference docs complete
- [x] Write documentation: provide examples showing structure-aware folding use cases - ✅ Examples in Haddock, user guide, and reference docs

---

### 4. Zipper for Interactive Navigation and Editing
**Priority**: Deferred / Evaluate Need
**Goal**: Efficient interactive navigation and editing (Up/Down/Left/Right).
**Reference**: See `design/DESIGN.md` (section "Zipper for Focus").

**Status**: Comonad instance (Feature 10) already provides context-aware operations (`extract`, `duplicate`, `extend`, `depthAt`, `sizeAt`, `indicesAt`). Zipper provides explicit parent/child navigation which may not be needed for foundational work. Only implement if a concrete editing/navigation use case emerges.

#### 13.1 Core Zipper Data Structure
- [ ] Review design document `design/DESIGN.md` (Zipper section)
- [ ] Implement `Zipper v` type with `focus` and `context` fields
- [ ] Implement `Context v` type with `parent`, `left`, `right`, and `above` fields
- [ ] Implement `fromPattern :: Pattern v -> Zipper v` (create zipper at root)
- [ ] Implement `toPattern :: Zipper v -> Pattern v` (reconstruct pattern from zipper)
- [ ] Write tests: verify zipper creation from patterns
- [ ] Write tests: verify pattern reconstruction from zipper
- [ ] Write tests: verify zipper preserves pattern structure and values

#### 13.2 Navigation Operations
- [ ] Implement `moveDown :: Int -> Zipper v -> Maybe (Zipper v)` (move to nth child)
- [ ] Implement `moveUp :: Zipper v -> Maybe (Zipper v)` (move to parent)
- [ ] Implement `moveLeft :: Zipper v -> Maybe (Zipper v)` (move to previous sibling)
- [ ] Implement `moveRight :: Zipper v -> Maybe (Zipper v)` (move to next sibling)
- [ ] Implement `moveToFirst :: Zipper v -> Maybe (Zipper v)` (move to first child)
- [ ] Implement `moveToLast :: Zipper v -> Maybe (Zipper v)` (move to last child)
- [ ] Implement `moveToRoot :: Zipper v -> Zipper v` (move to root)
- [ ] Implement `canMoveDown`, `canMoveUp`, `canMoveLeft`, `canMoveRight` predicates
- [ ] Write tests: verify all navigation operations work correctly
- [ ] Write tests: verify navigation handles edge cases (root, atomic patterns, boundaries)
- [ ] Write tests: verify navigation preserves pattern structure

#### 13.3 Context Access Operations
- [ ] Implement `parents :: Zipper v -> [v]` (list of parent values from immediate parent to root)
- [ ] Implement `ancestors :: Zipper v -> [Pattern v]` (list of parent Patterns from immediate parent to root)
- [ ] Implement `siblings :: Zipper v -> ([Pattern v], Pattern v, [Pattern v])` (left siblings, focus, right siblings)
- [ ] Implement `path :: Zipper v -> [Int]` (indices from root to focus)
- [ ] Implement `depth :: Zipper v -> Int` (depth of focus from root)
- [ ] Write tests: verify context access operations return correct values
- [ ] Write tests: verify `parents` and `ancestors` handle root correctly (empty lists)
- [ ] Write tests: verify context operations work at all nesting levels

#### 13.4 Editing Operations
- [ ] Implement `modifyFocus :: (Pattern v -> Pattern v) -> Zipper v -> Zipper v` (modify focused pattern)
- [ ] Implement `replaceFocus :: Pattern v -> Zipper v -> Zipper v` (replace focused pattern)
- [ ] Implement `insertBefore :: Pattern v -> Zipper v -> Zipper v` (insert before focus)
- [ ] Implement `insertAfter :: Pattern v -> Zipper v -> Zipper v` (insert after focus)
- [ ] Implement `insertChild :: Pattern v -> Zipper v -> Zipper v` (insert as first child)
- [ ] Implement `appendChild :: Pattern v -> Zipper v -> Zipper v` (append as last child)
- [ ] Implement `deleteFocus :: Zipper v -> Maybe (Zipper v)` (delete focus, move to parent or sibling)
- [ ] Implement `deleteChild :: Int -> Zipper v -> Maybe (Zipper v)` (delete nth child)
- [ ] Write tests: verify all editing operations work correctly
- [ ] Write tests: verify editing operations preserve pattern structure
- [ ] Write tests: verify editing operations handle edge cases (root deletion, empty patterns)
- [ ] Write tests: verify editing operations maintain zipper validity

#### 13.5 Zipper Query Operations
- [ ] Implement `isRoot :: Zipper v -> Bool` (check if at root)
- [ ] Implement `isAtomic :: Zipper v -> Bool` (check if focus is atomic)
- [ ] Implement `hasChildren :: Zipper v -> Bool` (check if focus has elements)
- [ ] Implement `childCount :: Zipper v -> Int` (number of children at focus)
- [ ] Implement `position :: Zipper v -> Maybe Int` (position among siblings, Nothing if root)
- [ ] Write tests: verify all query operations return correct values
- [ ] Write tests: verify query operations handle edge cases

#### 13.6 Integration and Utilities
- [ ] Implement `findFocus :: (Pattern v -> Bool) -> Pattern v -> Maybe (Zipper v)` (find first matching pattern)
- [ ] Implement `findAllFocus :: (Pattern v -> Bool) -> Pattern v -> [Zipper v]` (find all matching patterns)
- [ ] Implement `focusAt :: [Int] -> Pattern v -> Maybe (Zipper v)` (navigate to position by path)
- [ ] Consider: `Zipper` instances for common typeclasses (Functor, Foldable, Traversable)
- [ ] Consider: Conversion utilities between Zipper and Comonad operations
- [ ] Write tests: verify find operations work correctly
- [ ] Write tests: verify path-based navigation works correctly
- [ ] Write documentation: comprehensive examples showing zipper usage patterns
- [ ] Write documentation: relationship between Zipper and Comonad instances

---

### 5. Pattern Matching DSL (PatternExpr Library)
**Priority**: Deferred / Evaluate Need
**Goal**: Enable regex-like pattern matching expressions for Pattern structures.
**Reference**: See `design/pattern-matching-dsl-design.md`.

**Status**: Predicate matching (Feature 9) already provides `matches`, `contains`, `findPattern`, `filterPatterns`. Full DSL is extensive (3 layers, 6 phases). Only implement if predicate matching proves insufficient for real use cases.

#### 14.0 Design Validation and Exploration
- [ ] **STOP and REVIEW**: Review design document `design/pattern-matching-dsl-design.md` thoroughly
- [ ] **Validate Design Approach**: Analyze the layered architecture (PatternExpr → PathPattern → GraphPattern)
- [ ] **Consider Alternatives**: Research and evaluate alternative design approaches
- [ ] **Explore Toy Examples**: Create expressive toy examples to validate the design
- [ ] **Compare with Feature 9**: Evaluate relationship with predicate-based matching
- [ ] **Design Simplifications**: Identify what can be deferred or simplified
- [ ] **Create Design Summary**: Document the validated design approach
- [ ] **MANDATORY CHECKPOINT**: Present design validation to user for approval

#### 14.1 Pattern Expressions (Layer 1)
- [ ] Design `PatternExpr v` type with basic constructors (`PAny`, `PAtom`, `PSequence`)
- [ ] Design `Quantifier` type for repetition (`ZeroOrMore`, `OneOrMore`, `Exactly`, `Between`, etc.)
- [ ] Design combinators (`PThen`, `POr`, `PWhere`, `PBind`)
- [ ] Implement basic `PatternExpr` type and constructors
- [ ] Implement basic matching engine (`match`, `matchAll`, `matches`)
- [ ] Write tests: verify basic pattern matching on atomic patterns
- [ ] Write tests: verify basic pattern matching on sequences
- [ ] Write tests: verify combinators (sequential, alternative, predicates)

#### 14.2 Quantification and Advanced Matching
- [ ] Implement `PRepeat` with `Quantifier` support
- [ ] Implement backtracking for alternatives and quantifiers
- [ ] Implement variable binding (`PBind`) and capture groups
- [ ] Design `MatchResult` type with bindings and position information
- [ ] Implement `find` and `findAll` for finding patterns within larger patterns
- [ ] Write tests: verify quantifiers (`*`, `+`, `{m,n}`) work correctly
- [ ] Write tests: verify backtracking behavior
- [ ] Write tests: verify variable binding and capture groups
- [ ] Write tests: verify `find` and `findAll` operations
- [ ] Write tests: edge cases (empty patterns, deeply nested, complex quantifiers)

#### 14.3 Path Patterns (Layer 2)
- [ ] **STOP and REVIEW**: Evaluate need for path-level patterns beyond basic PatternExpr
- [ ] Design `PathPattern v` type for path-based matching
- [ ] Implement path-level predicates and constraints
- [ ] Implement intermediate node capturing in paths
- [ ] Implement path composition (`Seq`)
- [ ] Write tests: verify path pattern matching
- [ ] Write tests: verify path-level constraints
- [ ] Write tests: verify intermediate node capturing

#### 14.4 Graph Patterns (Layer 3)
- [ ] **STOP and REVIEW**: Evaluate need for non-linear graph patterns
- [ ] Design `GraphPattern v` type for multi-path patterns
- [ ] Implement equijoins (shared variables across paths)
- [ ] Implement logical operators (`AndGraph`, `OrGraph`)
- [ ] Write tests: verify graph pattern matching
- [ ] Write tests: verify equijoins work correctly
- [ ] Write tests: verify logical combinations

#### 14.5 Matching Engine and Optimization
- [ ] Implement pattern compilation (`compile :: PatternExpr v -> CompiledPattern v`)
- [ ] Implement `replace` and `replaceAll` for pattern transformation
- [ ] Add memoization for efficient matching
- [ ] Optimize common cases (early termination, indexing)
- [ ] Write tests: verify compilation improves performance
- [ ] Write tests: verify replace operations
- [ ] Write tests: verify performance targets (large patterns, complex expressions)

#### 14.6 Combinator Library and Surface Syntax
- [ ] Implement combinator library (`atom`, `sequence`, `zeroOrMore`, `oneOrMore`, etc.)
- [ ] Implement infix operators (`<~>`, `<|>`, `satisfying`, `as`)
- [ ] Implement derived combinators (`optional`, `listOf`, `atLeast`, `atMost`)
- [ ] Consider: QuasiQuoter for embedded pattern syntax (optional)
- [ ] Consider: Parser for Cypher-like syntax (optional)
- [ ] Write tests: verify all combinators work correctly
- [ ] Write tests: verify infix operators have correct precedence
- [ ] Write documentation: comprehensive examples using combinator library

---

### 6. Gram Notation and Parsed Pattern Gap Analysis
**Priority**: Medium / Evaluate Need
**Goal**: Investigate and address gaps between gram notation syntactic sugar and parsed Pattern structures.

**Rationale**: Gram notation provides syntactic sugar for special cases (nodes `(n:Person)`, annotations `@k("v") [:Target]`, relationships `(a)-[r:KNOWS]->(b)`, references). These map to standard Pattern structures, but there may be gaps in:
- How parsed gram notation constructs map to Pattern structures
- Whether all gram notation features are correctly parsed
- Whether round-trip serialization preserves syntactic sugar preferences
- Whether explicit construction functions exist for all gram notation constructs

**Status**: Documentation added (Feature 26) explaining gram notation special cases. Need to investigate implementation gaps.

#### 15.1 Gap Analysis
- [ ] **STOP and REVIEW**: Review gram notation specification and current parsing implementation
- [ ] Document all gram notation syntactic sugar constructs (nodes, annotations, relationships, references)
- [ ] Verify parsing correctly handles all syntactic sugar constructs
- [ ] Identify which constructs have explicit construction functions vs. requiring `pattern` function
- [ ] Test round-trip: parse gram → Pattern → serialize → verify structure preservation
- [ ] Document any parsing limitations or unsupported constructs
- [ ] Create gap analysis report: what works, what's missing, what needs improvement

#### 15.2 Construction Function Gaps
- [ ] Evaluate need for explicit node construction (currently: `point` covers this)
- [ ] Evaluate need for explicit annotation construction functions
- [ ] Evaluate need for explicit relationship construction functions (with direction support)
- [ ] Evaluate need for reference resolution utilities
- [ ] Design function signatures for any needed construction functions
- [ ] Implement construction functions if needed
- [ ] Write tests: verify construction functions match gram notation semantics
- [ ] Write documentation: update user guide with new construction functions

#### 15.3 Parsing Completeness
- [ ] Verify parser handles all gram notation constructs correctly
- [ ] Test edge cases: nested annotations, complex relationships, reference chains
- [ ] Verify parser preserves metadata (direction, properties) when available
- [ ] Test error handling for malformed gram notation
- [ ] Write tests: comprehensive parsing test suite for all syntactic sugar constructs
- [ ] Document parsing limitations and known issues

#### 15.4 Serialization Completeness
- [ ] Verify serializer can output gram notation syntactic sugar when appropriate
- [ ] Test round-trip: Pattern → gram → parse → Pattern (structure preservation)
- [ ] Evaluate whether serializer should prefer syntactic sugar vs. canonical form
- [ ] Implement serializer improvements if needed
- [ ] Write tests: verify serialization preserves structure and uses appropriate notation
- [ ] Write documentation: explain serialization behavior and notation choices
