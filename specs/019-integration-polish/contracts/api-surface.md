# API Contract: Pattern Library

## Module: Pattern

The `Pattern` module is the primary entry point. It re-exports functionality from `Pattern.Core`, `Pattern.Views`, `Pattern.Graph`, and `Pattern.Morphisms`.

### Type Signatures

```haskell
-- Core Type
data Pattern v = Pattern { value :: v, elements :: [Pattern v] }

-- Construction
pattern :: v -> Pattern v
patternWith :: v -> [Pattern v] -> Pattern v
fromList :: v -> [v] -> Pattern v

-- Query
length :: Pattern v -> Int
size :: Pattern v -> Int
depth :: Pattern v -> Int
values :: Pattern v -> [v]
flatten :: Pattern v -> [v]
toTuple :: Pattern v -> (v, [Pattern v])

-- Comonad / Context
extract :: Pattern v -> v
duplicate :: Pattern v -> Pattern (Pattern v)
extend :: (Pattern v -> w) -> Pattern v -> Pattern w
depthAt :: Pattern v -> Pattern Int
sizeAt :: Pattern v -> Pattern Int
indicesAt :: Eq v => Pattern v -> Pattern [Int]

-- Predicates
anyValue :: (v -> Bool) -> Pattern v -> Bool
allValues :: (v -> Bool) -> Pattern v -> Bool
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
matches :: Eq v => Pattern v -> Pattern v -> Bool
contains :: Eq v => Pattern v -> Pattern v -> Bool
```

### Laws and Invariants

- **Functor**: `fmap id == id`
- **Applicative**: `pure id <*> v == v`
- **Monad/Comonad**: Standard laws apply.
- **Traversable**: Standard laws apply.
- **Semigroup**: Associativity.
- **Monoid**: Identity element.

