-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Test.Hspec
import Pattern.Core (Pattern(..))

-- Custom type for testing
data Person = Person { personName :: String, personAge :: Int }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.Core" $ do
    
    describe "Leaf Patterns (User Story 1)" $ do
      
      describe "Creating leaf patterns with different value types" $ do
        
        it "creates a leaf pattern with string value" $ do
          let leaf = Pattern { value = "node1", elements = [] }
          value leaf `shouldBe` "node1"
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "creates a leaf pattern with integer value" $ do
          let leaf = Pattern { value = 42, elements = [] }
          value leaf `shouldBe` (42 :: Int)
          elements leaf `shouldBe` ([] :: [Pattern Int])
        
        it "creates a leaf pattern with custom type value" $ do
          let person = Person "Alice" 30
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
          elements leaf `shouldBe` ([] :: [Pattern Person])
      
      describe "Value field accessor" $ do
        
        it "returns the correct value for a leaf pattern with string" $ do
          let leaf = Pattern { value = "test", elements = [] }
          value leaf `shouldBe` "test"
        
        it "returns the correct value for a leaf pattern with integer" $ do
          let leaf = Pattern { value = 100, elements = [] }
          value leaf `shouldBe` (100 :: Int)
        
        it "returns the correct value for a leaf pattern with custom type" $ do
          let person = Person "Bob" 25
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
      
      describe "Elements field accessor" $ do
        
        it "returns empty list for leaf pattern" $ do
          let leaf = Pattern { value = "leaf", elements = [] }
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "returns empty list for leaf pattern with different value types" $ do
          let leafInt = Pattern { value = 42, elements = [] }
          let leafString = Pattern { value = "test", elements = [] }
          elements leafInt `shouldBe` ([] :: [Pattern Int])
          elements leafString `shouldBe` ([] :: [Pattern String])
      
      describe "Edge cases" $ do
        
        it "leaf pattern with explicitly empty list of children behaves correctly" $ do
          let leaf = Pattern { value = "node", elements = [] }
          value leaf `shouldBe` "node"
          elements leaf `shouldBe` ([] :: [Pattern String])
          null (elements leaf) `shouldBe` True
        
        it "multiple leaf patterns with same value type can be created independently" $ do
          let leaf1 = Pattern { value = "node1", elements = [] }
          let leaf2 = Pattern { value = "node2", elements = [] }
          value leaf1 `shouldBe` "node1"
          value leaf2 `shouldBe` "node2"
          elements leaf1 `shouldBe` ([] :: [Pattern String])
          elements leaf2 `shouldBe` ([] :: [Pattern String])
        
        it "leaf patterns with different value types are type-safe" $ do
          let leafString = Pattern { value = "text", elements = [] }
          let leafInt = Pattern { value = 123, elements = [] }
          value leafString `shouldBe` "text"
          value leafInt `shouldBe` (123 :: Int)
          -- Type system ensures they cannot be mixed
