# Gram Serialization

The Gram library provides serialization and deserialization of Pattern Subject structures to and from gram notation text format.

## Overview

Gram notation is a text format for representing patterns with subjects (identity, labels, properties). The library handles conversion between Haskell data structures and gram notation.

## Basic Usage

### Serialization

```haskell
import Gram (toGram)
import Pattern.Core (pattern, patternWith)
import Subject.Core (Subject(..), Symbol(..))
import Data.Map (fromList, empty)
import qualified Data.Set as Set

-- Create a simple subject pattern
let s = Subject (Symbol "n") (Set.fromList ["Person"]) empty
let p = pattern s

-- Serialize to gram notation
toGram p
-- Result: "(n:Person)"
```

### Parsing

```haskell
import Gram (fromGram)
import Data.Either (either)

-- Parse gram notation
case fromGram "(n:Person)" of
  Right pattern -> -- Use the pattern
  Left err -> -- Handle parse error
```

## Subject with Properties

```haskell
import Gram (toGram)
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (VString, VInteger)
import Data.Map (fromList)
import qualified Data.Set as Set

-- Create subject with properties
let props = fromList [("name", VString "Alice"), ("age", VInteger 30)]
let s = Subject (Symbol "n") (Set.fromList ["Person"]) props
let p = pattern s

-- Serialize
toGram p
-- Result: "(n:Person {name:\"Alice\",age:30})"
```

## Anonymous Subjects

```haskell
-- Anonymous subject (empty Symbol)
let s = Subject (Symbol "") (Set.fromList ["Person"]) empty
let p = pattern s

toGram p
-- Result: "(:Person)" or "()"
```

## Relationships

```haskell
import Gram (toGram)
import Pattern.Core (pattern, patternWith)

-- Create relationship pattern
let alice = pattern (Subject (Symbol "a") (Set.fromList ["Person"]) empty)
let bob = pattern (Subject (Symbol "b") (Set.fromList ["Person"]) empty)
let knows = patternWith (Subject (Symbol "r") (Set.fromList ["KNOWS"]) empty) [alice, bob]

toGram knows
-- Result: "[r:KNOWS | (a:Person), (b:Person)]"
```

## Nested Patterns

```haskell
-- Nested pattern structure
let inner = patternWith "inner" [pattern "x", pattern "y"]
let outer = patternWith "outer" [inner, pattern "z"]

toGram outer
-- Result: "[outer | [inner | x, y], z]"
```

## Round-Trip Testing

```haskell
import Gram (toGram, fromGram)
import Data.Either (either)

-- Verify round-trip
let original = patternWith "test" [pattern "a", pattern "b"]
let serialized = toGram original
case fromGram serialized of
  Right parsed -> 
    if parsed == original
      then putStrLn "Round-trip successful!"
      else putStrLn "Round-trip failed"
  Left err -> putStrLn $ "Parse error: " ++ show err
```

## Error Handling

```haskell
import Gram (fromGram)
import Data.Either (either)

case fromGram invalidInput of
  Right pattern -> -- Success
  Left parseError -> 
    -- Handle error
    putStrLn $ "Parse failed: " ++ show parseError
```

## Value Types

Gram supports various value types:

- **Standard**: String, Integer, Boolean, Decimal
- **Extended**: Tagged strings, arrays, maps, ranges, measurements

```haskell
import Subject.Value (VString, VInteger, VBoolean, VArray)

-- Different value types
let stringVal = VString "text"
let intVal = VInteger 42
let boolVal = VBoolean True
let arrayVal = VArray [VString "a", VString "b"]
```

## See Also

- **[Pattern Construction](pattern-construction.md)** - Creating patterns
- **[Examples](../examples/)** - More serialization examples
- **[Reference Documentation](../../reference/semantics/gram-semantics.md)** - Complete semantics

