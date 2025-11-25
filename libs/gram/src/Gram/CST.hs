{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Gram.CST
  ( Gram(..)
  , Pattern(..)
  , PatternElement(..)
  , Path(..)
  , PathSegment(..)
  , Node(..)
  , Relationship(..)
  , Bracketed(..)
  , SubjectData(..)
  , Value(..)
  , RangeValue(..)
  , Identifier(..)
  , Symbol(..)
  ) where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Set (Set)
import qualified Subject.Core as Core
import qualified Subject.Value as CoreVal

-- | Top-level Gram structure
-- gram: optional(record) + repeat(pattern)
data Gram = Gram
  { gramRecord :: Maybe (Map String Value)
  , gramPatterns :: [Pattern]
  } deriving (Show, Eq, Generic)

-- | A pattern sequence
-- pattern: optional(annotations) + commaSep1(pattern_element)
data Pattern = Pattern
  { patternElements :: [PatternElement]
  } deriving (Show, Eq, Generic)

-- | Element of a pattern
-- pattern_element: subject | path
data PatternElement
  = PEPath Path
  | PEBracketed Bracketed
  deriving (Show, Eq, Generic)

-- | A path structure (node connected by relationships)
-- This represents the linearized path: (a)-[r1]->(b)<-[r2]-(c)
data Path = Path
  { pathStart :: Node
  , pathSegments :: [PathSegment]
  } deriving (Show, Eq, Generic)

-- | A segment of a path: a relationship and the next node
data PathSegment = PathSegment
  { segmentRel :: Relationship
  , segmentNode :: Node
  } deriving (Show, Eq, Generic)

-- | A node structure
-- node: (attributes?)
data Node = Node
  { nodeAttributes :: Maybe SubjectData
  } deriving (Show, Eq, Generic)

-- | A relationship structure
-- relationship: node + relationship_kind + path (in recursive definition)
-- In CST, we capture the arrow and the optional attributes
data Relationship = Relationship
  { relArrow :: String        -- The raw arrow string, e.g., "-->", "<==>", "-[...]->"
  , relAttributes :: Maybe SubjectData
  } deriving (Show, Eq, Generic)

-- | A bracketed pattern structure
-- subject: [attributes? | sub_pattern?]
-- Renamed from "Subject" to avoid confusion with the internal data type.
data Bracketed = Bracketed
  { bracketedAttributes :: Maybe SubjectData
  , bracketedNested :: [PatternElement] -- Nested patterns after pipe
  } deriving (Show, Eq, Generic)

-- | Subject data container (Identity, Labels, Properties)
-- Renamed from "Attributes" to align with semantics.
data SubjectData = SubjectData
  { dataIdentifier :: Maybe Identifier
  , dataLabels :: Set String
  , dataProperties :: Map String Value
  } deriving (Show, Eq, Generic)

-- | Identifiers
newtype Symbol = Symbol String
  deriving (Show, Eq, Ord, Generic)

data Identifier
  = IdentSymbol Symbol
  | IdentString String
  | IdentInteger Integer
  deriving (Show, Eq, Generic)

-- | Values (mirroring Subject.Value but local to CST if needed, 
-- or we can reuse Core types if they are purely data)
type Value = CoreVal.Value
type RangeValue = CoreVal.RangeValue
