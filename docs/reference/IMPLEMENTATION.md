# Implementation Guide

**Audience**: Language porters  
**Purpose**: Implementation patterns and guidance for porting

## Implementation Patterns

### Core Pattern Type

The Pattern type is a recursive data structure:

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  }
```

**Language-Specific Considerations**:
- **Statically typed**: Use generics/templates: `Pattern<T>` (C#/Java), `Pattern<T>` (Rust)
- **Dynamically typed**: Use runtime type checking
- **Recursive types**: May require `Box` (Rust) or similar for recursive references

### Typeclass/Trait Implementation

#### Functor Instance

```haskell
instance Functor Pattern where
  fmap f (Pattern v els) = Pattern (f v) (fmap (fmap f) els)
```

**Key Properties**:
- Preserves structure (same number of elements, same nesting)
- Transforms values only
- Must satisfy functor laws

**Language Mapping**:
- **Rust**: Implement `Functor` trait (if available) or `map` method
- **C#/Java**: Use generic `Map<T, U>` method
- **TypeScript**: Use generic `map<U>(f: (v: T) => U): Pattern<U>`

#### Foldable Instance

```haskell
instance Foldable Pattern where
  foldr f z (Pattern v els) = f v (foldr (flip (foldr f)) z els)
```

**Key Properties**:
- Aggregates values across entire structure
- Right-associative folding
- Provides `toList` for flattening

**Language Mapping**:
- **Rust**: Implement `Iterator` trait or `fold` method
- **C#/Java**: Use `Aggregate` or `reduce` methods
- **TypeScript**: Use `reduce` method

#### Traversable Instance

```haskell
instance Traversable Pattern where
  traverse f (Pattern v els) = Pattern <$> f v <*> traverse (traverse f) els
```

**Key Properties**:
- Effectful traversal (IO, validation, state)
- Preserves structure
- Sequences effects

**Language Mapping**:
- **Rust**: Use `Result` or `Option` for error handling
- **C#/Java**: Use `Optional` or `Either` types
- **TypeScript**: Use `Promise` or `Either` types

### Graph Lens Implementation

Graph Lens uses a minimal design:

```haskell
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
  , testNode     :: Pattern v -> Bool
  }
```

**Key Pattern**:
- Single predicate (`testNode`) determines all graph concepts
- Scope-bounded operations (only direct elements of `scopePattern`)
- Context captured at construction (pure predicates)

**Language Mapping**:
- **Rust**: Use closures: `GraphLens { scope, test_node: Box<dyn Fn(&Pattern) -> bool> }`
- **C#/Java**: Use function interfaces: `Func<Pattern, bool>`
- **TypeScript**: Use function types: `(p: Pattern) => boolean`

### Query Functions

Query functions traverse the pattern structure:

```haskell
length :: Pattern v -> Int
length (Pattern _ els) = length els

size :: Pattern v -> Int
size (Pattern _ els) = 1 + sum (map size els)

depth :: Pattern v -> Int
depth (Pattern _ []) = 0
depth (Pattern _ els) = 1 + maximum (map depth els)
```

**Performance Considerations**:
- `length`: O(1) - direct element count
- `size`: O(n) - must traverse entire structure
- `depth`: O(n) - must traverse to find maximum depth

### Testing Strategies

#### Unit Tests

Test each function independently:

```haskell
test "pattern construction" $ do
  let p = pattern "test"
  value p `shouldBe` "test"
  elements p `shouldBe` []
```

#### Property-Based Tests

Verify typeclass laws:

```haskell
prop_functor_identity p = fmap id p == p
prop_functor_composition p f g = fmap (f . g) p == (fmap f . fmap g) p
```

#### Integration Tests

Test cross-component functionality:

```haskell
test "round-trip serialization" $ do
  let original = patternWith "test" [pattern "a", pattern "b"]
  let serialized = toGram original
  let parsed = fromGram serialized
  parsed `shouldBe` Right original
```

## Common Pitfalls

### Recursive Type Definitions

**Problem**: Some languages require explicit boxing for recursive types.

**Solution**: Use `Box` (Rust), `Rc` (Rust), or similar for recursive references.

### Typeclass Laws

**Problem**: Implementing typeclass instances without verifying laws.

**Solution**: Always write property-based tests to verify laws hold.

### Memory Management

**Problem**: Recursive structures can cause memory issues in manual memory management.

**Solution**: Use reference counting or smart pointers where appropriate.

### Performance

**Problem**: Naive implementations can be slow for large patterns.

**Solution**: 
- Cache computed values where appropriate
- Use lazy evaluation if language supports it
- Consider memoization for expensive operations

## Language-Specific Patterns

### Rust

```rust
pub struct Pattern<T> {
    pub value: T,
    pub elements: Vec<Pattern<T>>,
}

impl<T> Functor for Pattern<T> {
    fn map<U, F>(self, f: F) -> Pattern<U>
    where
        F: Fn(T) -> U,
    {
        Pattern {
            value: f(self.value),
            elements: self.elements.into_iter().map(|e| e.map(&f)).collect(),
        }
    }
}
```

### TypeScript

```typescript
interface Pattern<T> {
  value: T;
  elements: Pattern<T>[];
}

function map<T, U>(p: Pattern<T>, f: (v: T) => U): Pattern<U> {
  return {
    value: f(p.value),
    elements: p.elements.map(e => map(e, f))
  };
}
```

### Java

```java
public class Pattern<T> {
    private final T value;
    private final List<Pattern<T>> elements;
    
    public <U> Pattern<U> map(Function<T, U> f) {
        return new Pattern<>(
            f.apply(value),
            elements.stream()
                .map(e -> e.map(f))
                .collect(Collectors.toList())
        );
    }
}
```

## See Also

- **[Architecture](ARCHITECTURE.md)** - Design principles
- **[Specification](SPECIFICATION.md)** - Feature specifications
- **[Porting Guide](PORTING-GUIDE.md)** - Implementation roadmap
- **[Feature Specifications](features/)** - Detailed feature specs

