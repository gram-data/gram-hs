{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Commands.Schema
  ( SchemaOptions(..)
  , SchemaFormat(..)
  , schemaOptions
  , runSchema
  ) where

import Options.Applicative
import qualified Gram.Schema.JSONSchema as Schema
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import System.Exit (ExitCode(..))

-- | Supported schema formats
data SchemaFormat
  = JSONSchema    -- ^ JSON Schema (Draft 2020-12)
  | TypeScript    -- ^ TypeScript type definitions (future)
  | Rust          -- ^ Rust type definitions with serde (future)
  deriving (Show, Eq)

-- | Options for schema generation command
data SchemaOptions = SchemaOptions
  { schemaFormat :: SchemaFormat  -- ^ Output format
  } deriving (Show)

-- | Command-line parser for schema options
schemaOptions :: Parser SchemaOptions
schemaOptions = SchemaOptions
  <$> option (maybeReader parseFormat)
      ( long "format"
     <> metavar "FORMAT"
     <> value JSONSchema
     <> help "Output format: json-schema, typescript, or rust (default: json-schema)"
      )

-- | Parse format string to SchemaFormat
parseFormat :: String -> Maybe SchemaFormat
parseFormat "json-schema" = Just JSONSchema
parseFormat "typescript" = Just TypeScript
parseFormat "rust" = Just Rust
parseFormat _ = Nothing

-- | Run the schema generation command
runSchema :: SchemaOptions -> IO ExitCode
runSchema opts = do
  case schemaFormat opts of
    JSONSchema -> do
      let schema = Schema.generatePatternSchema
      let jsonBytes = encodePretty schema
      BSL.putStr jsonBytes
      return ExitSuccess
    
    TypeScript -> do
      putStrLn "Error: TypeScript schema generation not yet implemented"
      putStrLn "Use --format json-schema for now"
      return (ExitFailure 3)
    
    Rust -> do
      putStrLn "Error: Rust schema generation not yet implemented"
      putStrLn "Use --format json-schema for now"
      return (ExitFailure 3)
