# Advanced Morphisms: Category Theory Foundations

## Introduction

Patterns are built on solid mathematical foundations from category theory. Understanding these foundations helps you:
- Reason about Pattern operations mathematically
- Verify correctness of Pattern implementations
- Contribute to the Pattern library
- Port Patterns to other languages while preserving mathematical correctness

This section presents category theory concepts **intuitively first**, then provides **formal definitions** with examples connecting intuition to formalism.

## Morphisms: Structure-Preserving Transformations

### Intuitive Explanation

A **morphism** is a transformation that preserves structure. Think of it like translating a sentence from one language to another—the meaning (structure) is preserved, but the words (values) change.

For Patterns, morphisms preserve:
- **Element count**: Same number of elements
- **Nesting structure**: Same nesting depth
- **Element order**: Same sequence order

### Example: Functor as Morphism

The Functor instance provides a morphism that preserves pattern structure:

```haskell
import Pattern.Core (point, pattern, Pattern(..))
import Data.Functor (fmap)

-- Original pattern
original = pattern "root" [point "a", point "b"]

-- Transform values (morphism)
transformed = fmap (map toUpper) original
-- Pattern "ROOT" [Pattern "A" [], Pattern "B" []]

-- Structure preserved:
length original == length transformed  -- True (same element count)
depth original == depth transformed    -- True (same nesting)
```

**Gram notation:**
```gram
["root" | a, b]        -- Original
["ROOT" | A, B]        -- Transformed (structure preserved)
```

### Formal Definition

A **morphism** `f: A → B` in a category is a structure-preserving map between objects `A` and `B`.

For Patterns, a morphism `f: Pattern v → Pattern w` preserves:
- **Structure**: Element count, nesting depth, element order
- **Composition**: `f(g(x)) = (f ∘ g)(x)` for composable morphisms
- **Identity**: `f(id(x)) = id(f(x))` where `id` is the identity morphism

### Pattern Morphisms

Pattern operations that preserve structure are morphisms:

1. **Functor (`fmap`)**: Transforms values while preserving structure
2. **Natural transformations**: Transformations between functors
3. **Comonad operations**: Context-aware transformations preserving structure

## Natural Transformations: Transforming Functors

### Intuitive Explanation

A **natural transformation** transforms one functor into another while preserving the transformation structure. Think of it like converting between different representations of the same data—the underlying structure remains the same.

For Patterns, natural transformations transform Pattern operations while preserving their behavior.

### Example: `toList` as Natural Transformation

The `toList` function (from Foldable) is a natural transformation:

```haskell
import Data.Foldable (toList)
import Pattern.Core (point, pattern)

-- Pattern structure
pattern = pattern "root" [point "a", point "b"]

-- Natural transformation: Pattern → List
values = toList pattern
-- ["root", "a", "b"]
```

**Gram notation:**
```gram
["root" | a, b]  -- Natural transformation → ["root", "a", "b"]
```

This transformation is "natural" because it works consistently regardless of the pattern structure.

### Formal Definition

A **natural transformation** `η: F → G` between functors `F` and `G` is a family of morphisms `η_A: F(A) → G(A)` for each object `A`, such that for any morphism `f: A → B`:

```
η_B ∘ F(f) = G(f) ∘ η_A
```

This means the transformation commutes with functor operations.

### Pattern Natural Transformations

Common natural transformations for Patterns:

1. **`toList`**: Pattern → List (extracts all values)
2. **`extract`**: Pattern → Value (extracts decoration)
3. **`duplicate`**: Pattern → Pattern (Pattern) (creates contexts)

## Mathematical Laws: Ensuring Correctness

### Intuitive Explanation

Mathematical laws ensure that Pattern operations behave correctly and predictably. They're like rules that guarantee operations work as expected.

### Functor Laws

Functor operations must satisfy two laws:

#### Law 1: Identity

```haskell
fmap id == id
```

**Intuitive meaning**: Transforming with the identity function doesn't change anything.

**Example:**
```haskell
pattern = pattern "root" [point "a"]
fmap id pattern == pattern  -- True
```

**Gram notation:**
```gram
["root" | a]  -- fmap id preserves structure
```

#### Law 2: Composition

```haskell
fmap (f . g) == fmap f . fmap g
```

**Intuitive meaning**: Composing transformations is the same as transforming twice.

**Example:**
```haskell
pattern = pattern 10 [point 20]
f = (+1)
g = (*2)

fmap (f . g) pattern == fmap f (fmap g pattern)  -- True
-- Both: Pattern 21 [Pattern 41 []]
```

**Gram notation:**
```gram
[10 | 20]  -- fmap (f . g) == fmap f . fmap g
```

### Formal Definition

A **functor** `F: C → D` between categories `C` and `D` must satisfy:

1. **Identity preservation**: `F(id_A) = id_{F(A)}` for all objects `A`
2. **Composition preservation**: `F(f ∘ g) = F(f) ∘ F(g)` for all composable morphisms `f, g`

### Comonad Laws

Comonad operations must satisfy three laws:

#### Law 1: Extract-Extend

```haskell
extract . extend f == f
```

**Intuitive meaning**: Extracting from an extended pattern gives the function result.

**Example:**
```haskell
pattern = pattern "root" [point "elem"]
f = value  -- Function that extracts value

extract (extend f pattern) == f pattern  -- True
-- Both: "root"
```

#### Law 2: Extend-Extract

```haskell
extend extract == id
```

**Intuitive meaning**: Extending with extract is the identity.

**Example:**
```haskell
pattern = pattern "root" [point "elem"]
extend extract pattern == pattern  -- True
```

#### Law 3: Extend Composition

```haskell
extend f . extend g == extend (f . extend g)
```

**Intuitive meaning**: Composing extends is the same as extending with composition.

### Formal Definition

A **comonad** `(W, ε, δ)` consists of:
- Functor `W: C → C`
- Natural transformation `ε: W → Id` (extract)
- Natural transformation `δ: W → W²` (duplicate)

Satisfying:
1. **Left identity**: `ε_W ∘ δ = id_W`
2. **Right identity**: `W(ε) ∘ δ = id_W`
3. **Associativity**: `W(δ) ∘ δ = δ_W ∘ δ`

## How Patterns Relate to Other Categorical Structures

### Patterns as Functors

Patterns form a functor category where:
- **Objects**: Pattern types (`Pattern v` for different `v`)
- **Morphisms**: Value transformations (`fmap`)

### Patterns as Comonads

Patterns form a comonad where:
- **Extract**: Gets decoration value
- **Duplicate**: Creates pattern of contexts
- **Extend**: Applies context-aware functions

### Patterns and Natural Transformations

Pattern operations form natural transformations:
- `toList`: Pattern → List
- `extract`: Pattern → Value
- `duplicate`: Pattern → Pattern (Pattern)

## Porting Patterns: Preserving Mathematical Correctness

When porting Patterns to other languages, preserve mathematical correctness:

### Functor Laws

Ensure `fmap` satisfies:
1. `fmap id == id`
2. `fmap (f . g) == fmap f . fmap g`

### Comonad Laws

Ensure comonad operations satisfy:
1. `extract . extend f == f`
2. `extend extract == id`
3. `extend f . extend g == extend (f . extend g)`

### Testing Mathematical Laws

Write property-based tests to verify laws:

```haskell
-- Functor identity law
prop_functor_identity p = fmap id p == p

-- Functor composition law
prop_functor_composition p f g = 
  fmap (f . g) p == fmap f (fmap g p)

-- Comonad extract-extend law
prop_comonad_extract_extend p f = 
  extract (extend f p) == f p
```

## Paramorphism: Structure-Aware Folding

### Intuitive Explanation

**Paramorphism** extends `Foldable` to provide structure-aware folding, just as `Comonad` extends `Functor` to provide structure-aware transformation. While `Foldable` operations (`foldr`, `foldl`, `foldMap`) only provide access to values, paramorphism gives your folding function access to the full pattern structure at each position.

For Patterns, paramorphism enables:
- **Structure-aware aggregations**: Consider depth, element count, nesting level in addition to values
- **Context-dependent operations**: Adapt aggregation behavior based on structural properties
- **Structure-preserving transformations**: Access original structure while computing aggregated results

### Example: Depth-Weighted Sum

Paramorphism enables aggregations that consider structural context:

```haskell
import Pattern.Core (pattern, point, para)

-- Pattern structure
p = pattern 10 [point 5, point 3]

-- Foldable: Value-only folding
sumValues = foldr (+) 0 p  -- 18 (only values, no structure access)

-- Paramorphism: Structure-aware folding
depthWeightedSum = para (\pat rs -> value pat * depth pat + sum rs) p
-- Provides access to pattern structure (depth) and values
```

**Gram notation:**
```gram
[10 | 5, 3]  -- Pattern structure
-- Foldable: sums all values → 18
-- Paramorphism: weights by depth → 10*1 + 5*0 + 3*0 = 10
```

### Relationship to Foldable and Comonad

Understanding when to use each operation:

**Foldable** (value-only folding):
- Use when you only need values, not structural information
- Operations: `foldr`, `foldl`, `foldMap`, `toList`
- Example: Sum all values, extract values as list

**Paramorphism** (structure-aware folding):
- Use when you need structure-aware aggregations
- Operations: `para`
- Example: Depth-weighted sums, nesting-level statistics, element-count-based aggregations

**Comonad** (structure-aware transformation):
- Use when you need structure-aware transformation (not aggregation)
- Operations: `extend`, `duplicate`, `extract`
- Example: Transform values based on structural context, return Pattern structure

### Example: Comparing Operations

```haskell
import Pattern.Core (pattern, point, para)
import Data.Foldable (foldr, toList)
import Control.Comonad (extend)

p = pattern 10 [point 5, point 3]

-- Foldable: Value-only aggregation
foldr (+) 0 p  -- 18 (sums all values)

-- Paramorphism: Structure-aware aggregation
para (\pat rs -> value pat * depth pat + sum rs) p  -- 10 (depth-weighted)

-- Comonad: Structure-aware transformation
extend (\pat -> value pat * depth pat) p  
-- Pattern 10 [Pattern 0 [], Pattern 0 []] (transforms, returns Pattern)
```

### When to Use Paramorphism

Use paramorphism when you need:
- **Depth-aware operations**: Weight values by nesting depth
- **Element-count-aware aggregations**: Consider number of children in aggregation
- **Nesting-level statistics**: Compute statistics that depend on structural level
- **Context-dependent aggregations**: Adapt aggregation based on structural properties

### Formal Definition

**Paramorphism** is a recursion scheme that enables folding over recursive structures while preserving access to the original structure at each position.

For Patterns, paramorphism provides:
- **Folding function signature**: `(Pattern v -> [r] -> r) -> Pattern v -> r`
- **Structure access**: Folding function receives current pattern subtree
- **Child results**: Folding function receives recursively computed results from children
- **Structure-aware aggregation**: Enables aggregations considering structural properties

The relationship:
- **Foldable**: Provides value-only folding (`foldr`, `foldl`, `foldMap`)
- **Paramorphism**: Extends `Foldable` to provide structure-aware folding (`para`)
- **Comonad**: Provides structure-aware transformation (`extend`)

Just as `Comonad` extends `Functor` to see structure (`extend` vs `fmap`), Paramorphism extends `Foldable` to see structure (`para` vs `foldr`).

## Summary

- **Morphisms**: Structure-preserving transformations (Functor operations)
- **Natural transformations**: Transformations between functors (`toList`, `extract`, `duplicate`)
- **Mathematical laws**: Ensure correctness (Functor laws, Comonad laws)
- **Categorical structures**: Patterns form functors and comonads
- **Paramorphism**: Structure-aware folding extending `Foldable` (like `Comonad` extends `Functor`)
- **Porting guidance**: Preserve mathematical laws when porting to other languages

Understanding these foundations helps you reason about Patterns mathematically and ensure correctness.

## Next Steps

Explore real-world applications in [Use Cases](07-use-cases.md) to see how Patterns solve practical problems.

