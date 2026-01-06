# Graph Lens Feature Specification

**Status**: ✅ Implemented (Feature 23)  
**Location**: `libs/pattern/src/Pattern/Graph.hs`  
**Design Doc**: `design/graph-lens.md` (historical)

## Overview

Graph Lens provides an interpretive view of Pattern structures as graphs. Rather than defining graph concepts as intrinsic properties, they emerge through the lens's interpretation.

## Core Design

### Graph Lens Type

```haskell
data GraphLens v = GraphLens
  { scopePattern :: Pattern v
  , testNode     :: Pattern v -> Bool
  }
```

**Components**:
- **scopePattern**: Defines the boundary for all graph operations
- **testNode**: Predicate determining which elements are nodes

### Design Principles

1. **Scope-bounded**: All operations only consider direct elements of `scopePattern`
2. **Single predicate foundation**: Only `testNode` is required
3. **Context at construction**: Predicate context captured when lens is created
4. **Interpretation, not intrinsic**: Graph structure is an interpretation

## Graph Operations

### Nodes

```haskell
nodes :: GraphLens v -> [Pattern v]
isNode :: GraphLens v -> Pattern v -> Bool
```

Nodes are direct elements of `scopePattern` that satisfy `testNode`.

### Relationships

```haskell
isRelationship :: GraphLens v -> Pattern v -> Bool
relationships :: GraphLens v -> [Pattern v]
source :: GraphLens v -> Pattern v -> Maybe (Pattern v)
target :: GraphLens v -> Pattern v -> Maybe (Pattern v)
reverseRel :: Pattern v -> Pattern v
```

Relationships are non-node patterns with exactly two node elements.

### Walks

```haskell
isWalk :: GraphLens v -> Pattern v -> Bool
walks :: GraphLens v -> [Pattern v]
walkNodes :: GraphLens v -> Pattern v -> [Pattern v]
```

Walks are non-node patterns whose elements are all relationships, where consecutive relationships share nodes.

### Navigation

```haskell
neighbors :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
incidentRels :: Eq v => GraphLens v -> Pattern v -> [Pattern v]
degree :: Eq v => GraphLens v -> Pattern v -> Int
```

### Graph Analysis

```haskell
connectedComponents :: Eq v => GraphLens v -> [[Pattern v]]
bfs :: Eq v => GraphLens v -> [Pattern v] -> Set (Pattern v) -> [Pattern v]
findPath :: Eq v => GraphLens v -> Pattern v -> Pattern v -> Maybe [Pattern v]
```

## Usage Examples

### Basic Graph

```haskell
-- Create graph
alice = pattern "alice"
bob = pattern "bob"
knows = patternWith "knows" [alice, bob]
graph = patternWith "graph" [alice, bob, knows]

-- Create lens
isAtomic (Pattern _ els) = null els
lens = GraphLens graph isAtomic

-- Query
nodes lens         -- [alice, bob]
relationships lens -- [knows]
```

### Meta-Graph (Relationships as Nodes)

```haskell
-- Relationships become nodes
r1 = patternWith "rel1" [pattern "a", pattern "b"]
r2 = patternWith "rel2" [pattern "b", pattern "c"]
depends = patternWith "depends" [r1, r2]
metaGraph = patternWith "meta" [r1, r2, depends]

-- Lens treating 2-element patterns as nodes
isRelNode (Pattern _ els) = length els == 2
metaLens = GraphLens metaGraph isRelNode

-- Query
nodes metaLens         -- [r1, r2]
relationships metaLens -- [depends]
```

## API Reference

See `libs/pattern/src/Pattern/Graph.hs` for complete API documentation.

## Test Coverage

Comprehensive tests in `libs/pattern/tests/Spec/Pattern/GraphSpec.hs`.

## Design Evolution

Graph Lens (Feature 23) implements the design described in `design/graph-lens.md` with the minimal, predicate-based approach.

## See Also

- **[Core Pattern](core-pattern.md)** - Core type specification
- **[Architecture](../ARCHITECTURE.md)** - Design principles
- **[Design Document](../../design/graph-lens.md)** - Historical design context

