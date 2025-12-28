# Typeclass Instances Feature Specification

**Status**: ✅ Implemented  
**Location**: `libs/pattern/src/Pattern/Core.hs`  
**Features**: Multiple typeclass instances (Features 2, 4, 5, 6, 8, 10)

## Overview

The Pattern type implements multiple typeclass instances that enable functional programming patterns and mathematical operations.

## Implemented Instances

### Show

```haskell
instance Show v => Show (Pattern v) where
  show (Pattern v []) = "Pattern " ++ show v ++ " []"
  show (Pattern v es) = "Pattern " ++ show v ++ " " ++ show es
```

**Status**: ✅ Implemented (Feature 2)  
**Manual instance** for readable display format.

### Eq

```haskell
instance Eq v => Eq (Pattern v) where
  (==) = -- Structural equality
```

**Status**: ✅ Implemented (Feature 2)  
**Derived instance** - two patterns are equal if values and elements are equal.

### Ord

```haskell
instance Ord v => Ord (Pattern v) where
  compare = -- Lexicographic ordering
```

**Status**: ✅ Implemented (Feature 8)  
**Lexicographic ordering**: Compare values first, then elements recursively.

### Functor

```haskell
instance Functor Pattern where
  fmap f (Pattern v els) = Pattern (f v) (fmap (fmap f) els)
```

**Status**: ✅ Implemented (Feature 4)  
**Laws**: Must satisfy functor laws (identity, composition)  
**Preserves**: Structure (same number of elements, same nesting)

### Foldable

```haskell
instance Foldable Pattern where
  foldr f z (Pattern v els) = f v (foldr (flip (foldr f)) z els)
  foldl f z (Pattern v els) = foldr (flip f) z (reverse (toList (Pattern v els)))
  toList = flatten
```

**Status**: ✅ Implemented (Feature 5)  
**Provides**: `foldr`, `foldl`, `foldMap`, `toList`, `flatten`

### Traversable

```haskell
instance Traversable Pattern where
  traverse f (Pattern v els) = Pattern <$> f v <*> traverse (traverse f) els
```

**Status**: ✅ Implemented (Feature 6)  
**Enables**: Effectful traversal (IO, validation, state)  
**Preserves**: Structure while sequencing effects

### Semigroup

```haskell
instance Semigroup v => Semigroup (Pattern v) where
  (<>) = -- Combine patterns by concatenating elements and combining values
```

**Status**: ✅ Implemented (Feature 8)  
**Combines**: Elements concatenated, values combined using value type's Semigroup

### Monoid

```haskell
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern mempty []
  mappend = (<>)
```

**Status**: ✅ Implemented (Feature 8)  
**Identity**: Pattern with `mempty` value and empty elements

### Hashable

```haskell
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt = -- Structure-preserving hashing
```

**Status**: ✅ Implemented (Feature 8)  
**Structure-preserving**: Equal patterns produce same hash

### Applicative

```haskell
instance Applicative Pattern where
  pure v = Pattern v []
  (<*>) = -- Zip-like application
```

**Status**: ✅ Implemented (Feature 8)  
**Zip-like**: Applies pattern of functions to pattern of values

### Comonad

```haskell
instance Comonad Pattern where
  extract (Pattern v _) = v
  duplicate p = -- Context at every position
  extend f p = fmap f (duplicate p)
```

**Status**: ✅ Implemented (Feature 10)  
**Context-aware**: Functions have access to structural context  
**Additional**: `depthAt`, `sizeAt`, `indicesAt`

## Typeclass Laws

All instances must satisfy their respective laws:

### Functor Laws
- **Identity**: `fmap id = id`
- **Composition**: `fmap (f . g) = fmap f . fmap g`

### Monoid Laws
- **Associativity**: `(a <> b) <> c = a <> (b <> c)`
- **Identity**: `mempty <> a = a = a <> mempty`

### Comonad Laws
- **Extract-Extend**: `extract . extend f = f`
- **Extend-Extract**: `extend extract = id`
- **Extend Composition**: `extend f . extend g = extend (f . extend g)`

## Test Coverage

Property-based tests verify all typeclass laws in `libs/pattern/tests/`.

## Language Portability

### Mapping to Other Languages

- **Rust**: Implement traits (`Functor`, `Foldable`, etc.)
- **C#/Java**: Use generic interfaces (`IFunctor<T>`, `IFoldable<T>`, etc.)
- **TypeScript**: Use generic interfaces or type classes
- **Python**: Use protocol classes or duck typing

### Law Verification

Always verify typeclass laws when porting:
- Write property-based tests
- Test with various value types
- Test edge cases (empty patterns, deeply nested)

## See Also

- **[Core Pattern](core-pattern.md)** - Core type specification
- **[Implementation](../IMPLEMENTATION.md)** - Implementation patterns
- **[Architecture](../ARCHITECTURE.md)** - Design principles

