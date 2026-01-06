# Basic Pattern Examples

Working code examples for creating and working with patterns.

## Creating Atomic Patterns

```haskell
import Pattern.Core (pattern)

-- String atomic pattern
atomA :: Pattern String
atomA = pattern "A"

-- Integer atomic pattern
number :: Pattern Int
number = pattern 42

-- Boolean atomic pattern
flag :: Pattern Bool
flag = pattern True
```

## Creating Patterns with Elements

```haskell
import Pattern.Core (pattern, patternWith)

-- Create elements
alice = pattern "alice"
bob = pattern "bob"

-- Create pattern with elements
relationship = patternWith "knows" [alice, bob]

-- Multiple elements
group = patternWith "group" 
  [ pattern "alice"
  , pattern "bob"
  , pattern "charlie"
  ]
```

## Nested Patterns

```haskell
import Pattern.Core (pattern, patternWith)

-- Two-level nesting
nested = patternWith "root"
  [ patternWith "level1" [pattern "level2"]
  ]

-- Three-level nesting
deep = patternWith "root"
  [ patternWith "level1"
    [ patternWith "level2" [pattern "level3"]
    ]
  ]
```

## Querying Patterns

```haskell
import Pattern.Core (pattern, patternWith, length, size, depth, values)

-- Create a pattern
myPattern = patternWith "root"
  [ patternWith "a" [pattern "x", pattern "y"]
  , pattern "b"
  ]

-- Query properties
main = do
  putStrLn $ "Length: " ++ show (length myPattern)  -- 2
  putStrLn $ "Size: " ++ show (size myPattern)      -- 5
  putStrLn $ "Depth: " ++ show (depth myPattern)    -- 2
  putStrLn $ "Values: " ++ show (values myPattern)  -- ["root","a","x","y","b"]
```

## Working with Values and Elements

```haskell
import Pattern.Core (Pattern(..), pattern, patternWith)

-- Access value
getValue :: Pattern String -> String
getValue p = value p

-- Access elements
getElements :: Pattern String -> [Pattern String]
getElements p = elements p

-- Check if atomic
isAtomic :: Pattern v -> Bool
isAtomic p = null (elements p)

-- Example usage
example = do
  let atom = pattern "test"
  putStrLn $ "Value: " ++ value atom
  putStrLn $ "Is atomic: " ++ show (isAtomic atom)
```

## Building Graph Structures

```haskell
import Pattern.Core (pattern, patternWith)

-- Create nodes
alice = pattern "alice"
bob = pattern "bob"
charlie = pattern "charlie"

-- Create relationships
knows1 = patternWith "knows" [alice, bob]
knows2 = patternWith "knows" [bob, charlie]

-- Create graph container
graph = patternWith "graph" [alice, bob, charlie, knows1, knows2]
```

## Type Safety Examples

```haskell
import Pattern.Core (pattern, patternWith)

-- ✅ Valid: All patterns use String
valid = patternWith "root" [pattern "a", pattern "b"]

-- ❌ This won't compile: Mixed types
-- invalid = patternWith "root" [pattern "a", pattern 42]
```

