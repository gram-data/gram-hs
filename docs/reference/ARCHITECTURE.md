# Architecture: Core Design Principles

**Audience**: Language porters  
**Purpose**: Understand the category-theoretic foundations and key design decisions

## Core Concept: Patterns as Decorated Sequences

A Pattern is conceptually a **decorated sequence**: the elements form the pattern itself, and the value provides decoration about that pattern.

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
```

**Key Insight**: The `elements` field IS the pattern - it contains the sequence that defines the pattern. The `value` field provides decoration about what kind of pattern it is.

### Sequence vs Tree: Two Complementary Views

**Primary Semantic (Conceptual)**: Patterns are decorated sequences where elements form the pattern itself. The sequence order is essential.

**Implementation Detail**: Patterns are implemented as recursive trees in memory. Each tree node stores a decoration (value) and contains the pattern elements as a list.

**Relationship**: The tree implementation supports the sequence semantic. Tree nodes store sequences (lists), tree traversal preserves order, and the recursive structure enables nested sequences.

## Design Principles

### 1. Schema-Lazy Design

Patterns don't commit to specific graph semantics at creation time. Interpretation happens through views, enabling:
- Multiple interpretations of the same pattern structure
- View composition and stacking
- Open-ended extensions for new graph-like interpretations

### 2. Category-Theoretic Foundation

Patterns are built on solid mathematical foundations from category theory:
- Functor instances that satisfy functor laws
- Natural transformations that satisfy naturality conditions
- Morphisms that preserve relevant structure
- Composition that is associative with identity elements

### 3. Multi-Language Translation

The design prioritizes clarity and mathematical correctness over language-specific optimizations, making it easier to translate to other languages while preserving conceptual consistency.

### 4. Decorated Sequence Semantics

Conceptually, patterns are decorated sequences where:
- Elements form the pattern itself
- Value provides decoration about the pattern
- Sequence order is essential
- Recursive nesting enables complex structures

## Pattern Structural Classifications

### Atomic Pattern
A pattern with no elements (`elements == []`). Fundamental building blocks.

### Singular Pattern
A pattern with exactly one element (`length (elements p) == 1`).

### Pattern with Elements
A pattern containing one or more pattern elements in sequence.

### Nested Pattern
A pattern containing patterns that themselves contain patterns, enabling arbitrary nesting depth.

## Graph Interpretations (Views)

Patterns can be **interpreted** as graph elements through different views. These are interpretations, not intrinsic properties.

### Nodes
**Definition**: A node is an atomic pattern - a pattern with no elements.

### Relationships
**Definition**: A relationship is a pattern with exactly two atomic patterns as elements.
- **Directed**: Element order encodes direction (Element[0] = source, Element[1] = target)
- **Undirected**: Element order is semantically irrelevant

### Walks
**Definition**: A walk is an ordered sequence of relationships where consecutive relationships share nodes.

## Graph Lens

Graph Lens provides an interpretive view of Pattern structures as graphs through a minimal design:
- **scopePattern**: Defines the boundary for graph operations
- **testNode**: Predicate determining which elements are nodes
- All other graph concepts (relationships, walks) derive from this single predicate

See `docs/reference/features/graph-lens.md` for complete specification.

## Category-Theoretic Perspective

### Patterns as a Free Structure

The Pattern type forms a category **Pat** where:
- Objects: Individual patterns
- Morphisms: Structural transformations preserving pattern shape

### Graph Views as Functors

Each graph view defines a functor F: **Pat** → **Graph_k** for some specific graph category.

## Key Properties

1. **Schema-lazy**: Patterns don't commit to specific graph semantics; interpretation happens in the view
2. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns
3. **Open-ended**: New views can be defined for any graph-like interpretation
4. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition

## Language Portability Considerations

### Type Systems
- **Statically typed**: Use generics/templates for `Pattern v`
- **Dynamically typed**: Use runtime type checking
- **Functional languages**: Leverage algebraic data types
- **OOP languages**: Consider sealed classes/interfaces

### Memory Management
- **GC languages**: Recursive structures work naturally
- **Manual memory**: Consider reference counting or smart pointers
- **Rust**: Use `Box<Pattern>` for recursive types

### Typeclass/Trait Implementation
- **Haskell typeclasses**: Map to traits (Rust), interfaces (Java/C#), protocols (Swift)
- **Laws**: Verify functor laws, monoid laws, etc.

## See Also

- **[Specification](SPECIFICATION.md)** - Current feature specifications
- **[Implementation](IMPLEMENTATION.md)** - Implementation patterns
- **[Porting Guide](PORTING-GUIDE.md)** - Implementation roadmap

