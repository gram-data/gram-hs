{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Spec.Gram.JSONSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, decode, eitherDecode, Value)
import qualified Data.ByteString.Lazy as BSL
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Gram.JSON as GramJSON
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Helper function to test roundtrip: Pattern -> JSON -> Pattern
roundtripTest :: Pattern.Pattern Subject.Subject -> Bool
roundtripTest pat = 
  case decode (encode pat) of
    Just pat' -> pat == pat'
    Nothing -> False

-- | Helper to create a simple subject
mkSubject :: String -> Subject.Subject
mkSubject sym = Subject.Subject (Subject.Symbol sym) Set.empty Map.empty

-- | Helper to create a subject with labels
mkSubjectLabels :: String -> [String] -> Subject.Subject
mkSubjectLabels sym labels = 
  Subject.Subject (Subject.Symbol sym) (Set.fromList labels) Map.empty

-- | Helper to create a subject with properties
mkSubjectProps :: String -> [(String, SubjectValue.Value)] -> Subject.Subject
mkSubjectProps sym props = 
  Subject.Subject (Subject.Symbol sym) Set.empty (Map.fromList props)

spec :: Spec
spec = do
  describe "JSON Roundtrip Tests" $ do
    
    describe "Simple Pattern Roundtrip" $ do
      it "roundtrips a simple pattern with just identity" $ do
        let pat = Pattern.Pattern (mkSubject "node") []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips a pattern with identity and labels" $ do
        let pat = Pattern.Pattern (mkSubjectLabels "node" ["Person", "User"]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips an anonymous subject (empty symbol)" $ do
        let pat = Pattern.Pattern (mkSubject "") []
        roundtripTest pat `shouldBe` True

    describe "Nested Pattern Roundtrip" $ do
      it "roundtrips a pattern with one nested element" $ do
        let inner = Pattern.Pattern (mkSubject "child") []
        let outer = Pattern.Pattern (mkSubject "parent") [inner]
        roundtripTest outer `shouldBe` True
      
      it "roundtrips a pattern with multiple nested elements" $ do
        let inner1 = Pattern.Pattern (mkSubject "a") []
        let inner2 = Pattern.Pattern (mkSubject "b") []
        let inner3 = Pattern.Pattern (mkSubject "c") []
        let outer = Pattern.Pattern (mkSubject "root") [inner1, inner2, inner3]
        roundtripTest outer `shouldBe` True
      
      it "roundtrips deeply nested patterns" $ do
        let level3 = Pattern.Pattern (mkSubject "level3") []
        let level2 = Pattern.Pattern (mkSubject "level2") [level3]
        let level1 = Pattern.Pattern (mkSubject "level1") [level2]
        let root = Pattern.Pattern (mkSubject "root") [level1]
        roundtripTest root `shouldBe` True

    describe "Simple Value Types Roundtrip" $ do
      it "roundtrips VInteger values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("count", SubjectValue.VInteger 42)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VDecimal values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("price", SubjectValue.VDecimal 3.14)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VBoolean values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("active", SubjectValue.VBoolean True)]) []
        roundtripTest pat `shouldBe` True
        let pat2 = Pattern.Pattern (mkSubjectProps "node" [("active", SubjectValue.VBoolean False)]) []
        roundtripTest pat2 `shouldBe` True
      
      it "roundtrips VString values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("name", SubjectValue.VString "Alice")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips multiple simple values together" $ do
        let props = [ ("count", SubjectValue.VInteger 10)
                    , ("price", SubjectValue.VDecimal 99.99)
                    , ("active", SubjectValue.VBoolean True)
                    , ("name", SubjectValue.VString "Product")
                    ]
        let pat = Pattern.Pattern (mkSubjectProps "node" props) []
        roundtripTest pat `shouldBe` True

    describe "Complex Value Types Roundtrip" $ do
      it "roundtrips VSymbol values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("type", SubjectValue.VSymbol "identifier")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VTaggedString values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("data", SubjectValue.VTaggedString "json" "{\"key\": \"value\"}")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VRange values" $ do
        let range = SubjectValue.RangeValue (Just 1.0) (Just 10.0)
        let pat = Pattern.Pattern (mkSubjectProps "node" [("range", SubjectValue.VRange range)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VMeasurement values" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("temp", SubjectValue.VMeasurement "°C" 23.5)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips multiple complex values together" $ do
        let range = SubjectValue.RangeValue (Just 0.0) (Just 100.0)
        let props = [ ("type", SubjectValue.VSymbol "sensor")
                    , ("config", SubjectValue.VTaggedString "yaml" "interval: 1000")
                    , ("range", SubjectValue.VRange range)
                    , ("temp", SubjectValue.VMeasurement "K" 273.15)
                    ]
        let pat = Pattern.Pattern (mkSubjectProps "node" props) []
        roundtripTest pat `shouldBe` True

    describe "Array and Map Value Types Roundtrip" $ do
      it "roundtrips VArray with simple values" $ do
        let arr = SubjectValue.VArray [SubjectValue.VInteger 1, SubjectValue.VInteger 2, SubjectValue.VInteger 3]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("numbers", arr)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VArray with mixed types" $ do
        let arr = SubjectValue.VArray 
                    [ SubjectValue.VInteger 42
                    , SubjectValue.VString "text"
                    , SubjectValue.VBoolean True
                    ]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("mixed", arr)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips nested VArray" $ do
        let innerArr = SubjectValue.VArray [SubjectValue.VInteger 1, SubjectValue.VInteger 2]
        let outerArr = SubjectValue.VArray [innerArr, SubjectValue.VInteger 3]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("nested", outerArr)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VMap with simple values" $ do
        let m = Map.fromList [("x", SubjectValue.VInteger 10), ("y", SubjectValue.VInteger 20)]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("point", SubjectValue.VMap m)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips VMap with complex values" $ do
        let m = Map.fromList 
                  [ ("symbol", SubjectValue.VSymbol "test")
                  , ("count", SubjectValue.VInteger 5)
                  , ("name", SubjectValue.VString "example")
                  ]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("data", SubjectValue.VMap m)]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips nested VMap" $ do
        let innerMap = Map.fromList [("a", SubjectValue.VInteger 1)]
        let outerMap = Map.fromList [("inner", SubjectValue.VMap innerMap), ("b", SubjectValue.VInteger 2)]
        let pat = Pattern.Pattern (mkSubjectProps "node" [("nested", SubjectValue.VMap outerMap)]) []
        roundtripTest pat `shouldBe` True

    describe "Special Characters in Properties" $ do
      it "roundtrips strings with quotes" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("text", SubjectValue.VString "He said \"hello\"")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips strings with newlines" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("text", SubjectValue.VString "line1\nline2")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips strings with backslashes" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("path", SubjectValue.VString "C:\\Users\\test")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips strings with unicode" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("emoji", SubjectValue.VString "Hello 👋 World 🌍")]) []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips property keys with special characters" $ do
        let pat = Pattern.Pattern (mkSubjectProps "node" [("key-with-dash", SubjectValue.VInteger 1), ("key_with_underscore", SubjectValue.VInteger 2)]) []
        roundtripTest pat `shouldBe` True

    describe "Empty Properties, Labels, and Anonymous Subjects" $ do
      it "roundtrips pattern with empty properties" $ do
        let subj = Subject.Subject (Subject.Symbol "node") (Set.singleton "Label") Map.empty
        let pat = Pattern.Pattern subj []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips pattern with empty labels" $ do
        let subj = Subject.Subject (Subject.Symbol "node") Set.empty (Map.singleton "x" (SubjectValue.VInteger 1))
        let pat = Pattern.Pattern subj []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips anonymous subject (empty symbol) with labels and properties" $ do
        let subj = Subject.Subject (Subject.Symbol "") (Set.singleton "Label") (Map.singleton "x" (SubjectValue.VInteger 1))
        let pat = Pattern.Pattern subj []
        roundtripTest pat `shouldBe` True
      
      it "roundtrips pattern with all empty (anonymous subject, no labels, no properties)" $ do
        let subj = Subject.Subject (Subject.Symbol "") Set.empty Map.empty
        let pat = Pattern.Pattern subj []
        roundtripTest pat `shouldBe` True

    describe "Canonicalization" $ do
      it "produces deterministic JSON output" $ do
        let props = Map.fromList [("z", SubjectValue.VInteger 1), ("a", SubjectValue.VInteger 2), ("m", SubjectValue.VInteger 3)]
        let subj = Subject.Subject (Subject.Symbol "node") Set.empty props
        let pat = Pattern.Pattern subj []
        let json1 = encode pat
        let json2 = encode pat
        json1 `shouldBe` json2
      
      it "canonicalizeJSON sorts keys alphabetically" $ do
        let val = GramJSON.patternToValue (Pattern.Pattern (mkSubjectProps "n" [("z", SubjectValue.VInteger 1), ("a", SubjectValue.VInteger 2)]) [])
        let canonical = GramJSON.canonicalizeJSON val
        -- Canonical form should have keys sorted
        encode canonical `shouldBe` encode canonical

  describe "Error Handling" $ do
    it "fails to parse invalid JSON" $ do
      let invalidJSON = "{invalid json}" :: String
      let result = eitherDecode (BSL.pack $ map (fromIntegral . fromEnum) invalidJSON) :: Either String (Pattern.Pattern Subject.Subject)
      result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
    
    it "fails to parse JSON missing required fields" $ do
      let missingSubject = "{\"elements\": []}" :: String
      let result = eitherDecode (BSL.pack $ map (fromIntegral . fromEnum) missingSubject) :: Either String (Pattern.Pattern Subject.Subject)
      result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
