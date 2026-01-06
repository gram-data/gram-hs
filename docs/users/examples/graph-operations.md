# Graph Operations Examples

Examples of using Graph Lens for graph operations.

## Basic Graph Lens

```haskell
import Pattern.Core (pattern, patternWith)
import Pattern.Graph (GraphLens(..), nodes, relationships)

-- Create a simple graph
alice = pattern "alice"
bob = pattern "bob"
knows = patternWith "knows" [alice, bob]
graph = patternWith "graph" [alice, bob, knows]

-- Create lens (atomic patterns are nodes)
isAtomic (Pattern _ els) = null els
lens = GraphLens graph isAtomic

-- Query graph
main = do
  putStrLn $ "Nodes: " ++ show (nodes lens)
  putStrLn $ "Relationships: " ++ show (relationships lens)
```

## Finding Neighbors

```haskell
import Pattern.Graph (neighbors, incidentRels, degree)

-- Find neighbors of a node
neighborsOfAlice = neighbors lens alice

-- Find all relationships involving a node
relsOfAlice = incidentRels lens alice

-- Get node degree
aliceDegree = degree lens alice
```

## Graph Traversal

```haskell
import Pattern.Graph (bfs, findPath)
import qualified Data.Set as Set

-- Breadth-first search from a node
reachable = bfs lens [startNode] Set.empty

-- Find path between nodes
case findPath lens startNode endNode of
  Just path -> -- Use the path
  Nothing -> -- No path exists
```

## Connected Components

```haskell
import Pattern.Graph (connectedComponents)

-- Find all connected components
components = connectedComponents lens

-- Each component is a list of nodes
mapM_ (putStrLn . show) components
```

## Custom Node Predicates

```haskell
-- By value prefix
isPersonNode (Pattern v _) = "person:" `isPrefixOf` v

-- By structure
isSimpleNode (Pattern _ els) = length els <= 1

-- Create lens with custom predicate
customLens = GraphLens graph isPersonNode
```

