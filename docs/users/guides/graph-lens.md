# Graph Lens

Graph Lens provides an interpretive view of Pattern structures as graphs. This enables you to work with patterns as graph structures (nodes, relationships, walks) through a minimal, elegant design.

## Overview

A Graph Lens interprets a Pattern as a graph by:
- Defining a scope (which pattern to interpret)
- Providing a predicate to identify nodes
- Deriving relationships and walks from the structure

## Basic Usage

```haskell
import Pattern.Core (pattern, patternWith)
import Pattern.Graph (GraphLens(..), nodes, relationships)

-- Create a simple graph pattern
alice = pattern "alice"
bob = pattern "bob"
knows = patternWith "knows" [alice, bob]
graph = patternWith "graph" [alice, bob, knows]

-- Create a lens that treats atomic patterns as nodes
isAtomic (Pattern _ els) = null els
atomicLens = GraphLens graph isAtomic

-- Query the graph
graphNodes = nodes atomicLens         -- [alice, bob]
graphRels = relationships atomicLens -- [knows]
```

## Creating a Graph Lens

A Graph Lens consists of:
- **scopePattern**: The pattern that defines the graph boundary
- **testNode**: A predicate that identifies which elements are nodes

```haskell
import Pattern.Graph (GraphLens(..))

-- Define what counts as a node
isAtomic (Pattern _ els) = null els

-- Create lens
lens = GraphLens myPattern isAtomic
```

## Querying Graphs

### Nodes

```haskell
import Pattern.Graph (nodes, isNode)

-- Get all nodes
allNodes = nodes lens

-- Check if a pattern is a node
isNode lens somePattern
```

### Relationships

```haskell
import Pattern.Graph (relationships, isRelationship, source, target)

-- Get all relationships
allRels = relationships lens

-- Check if a pattern is a relationship
isRelationship lens somePattern

-- Get source and target nodes
src = source lens relationship
tgt = target lens relationship
```

### Walks

```haskell
import Pattern.Graph (walks, isWalk, walkNodes)

-- Get all walks
allWalks = walks lens

-- Check if a pattern is a walk
isWalk lens somePattern

-- Get nodes in walk order
nodesInOrder = walkNodes lens walk
```

## Navigation

```haskell
import Pattern.Graph (neighbors, incidentRels, degree)

-- Find neighbors of a node
neighborsOfAlice = neighbors lens alice

-- Find all relationships involving a node
relsOfAlice = incidentRels lens alice

-- Get node degree
aliceDegree = degree lens alice
```

## Graph Analysis

```haskell
import Pattern.Graph (connectedComponents, bfs, findPath)

-- Find connected components
components = connectedComponents lens

-- Breadth-first search from a node
reachable = bfs lens [startNode] Set.empty

-- Find path between nodes
path = findPath lens startNode endNode
```

## Custom Node Predicates

You can define custom predicates to identify nodes:

```haskell
-- By value type
isPerson (Pattern v _) = case v of
  Person _ -> True
  _ -> False

-- By label prefix
hasLabelPrefix prefix (Pattern v _) = 
  prefix `isPrefixOf` show v

-- By structure
isSimpleNode (Pattern _ els) = length els <= 1
```

## Examples

### Simple Graph

```haskell
-- Create graph
alice = pattern "alice"
bob = pattern "bob"
charlie = pattern "charlie"
knows1 = patternWith "knows" [alice, bob]
knows2 = patternWith "knows" [bob, charlie]
graph = patternWith "graph" [alice, bob, charlie, knows1, knows2]

-- Create lens
lens = GraphLens graph (\p -> null (elements p))

-- Query
nodes lens        -- [alice, bob, charlie]
relationships lens -- [knows1, knows2]
```

### Meta-Graph (Relationships as Nodes)

```haskell
-- Relationships become nodes in a higher-level graph
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

## See Also

- **[Pattern Construction](pattern-construction.md)** - Creating patterns
- **[Examples](../examples/graph-operations.md)** - More graph examples
- **[Reference Documentation](../../reference/features/graph-lens.md)** - Complete specification

