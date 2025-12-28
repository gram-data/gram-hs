# Graph Interpretation Semantics

**Status**: ✅ Implemented (Feature 23)  
**Reference**: Graph Lens implementation

## Overview

Patterns can be **interpreted** as graph elements through Graph Lens. Graph structure is an interpretation, not an intrinsic property.

## Graph Elements

### Nodes
**Definition**: A node is an atomic pattern - a pattern with no elements.

### Relationships
**Definition**: A relationship is a pattern with exactly two atomic patterns as elements.

- **Directed**: Element order encodes direction (Element[0] = source, Element[1] = target)
- **Undirected**: Element order is semantically irrelevant

### Walks
**Definition**: A walk is an ordered sequence of relationships where consecutive relationships share nodes.

## Graph Lens

Graph Lens provides the interpretation mechanism:
- **scopePattern**: Defines graph boundary
- **testNode**: Predicate determining nodes
- All other graph concepts derive from this

## See Also

- **[Graph Lens](../features/graph-lens.md)** - Complete specification
- **[Architecture](../ARCHITECTURE.md)** - Design principles

