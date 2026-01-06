{-# LANGUAGE OverloadedStrings #-}
module Gramref.CLI.Output
  ( formatOutput
  , formatError
  ) where

import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..))
import qualified Gramref.CLI.JSON as JSON
import qualified Gram.Serialize as Gram
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Data.Text.IO as TIO

formatOutput :: OutputFormat -> OutputOptions -> Pattern.Pattern Subject.Subject -> IO ()
formatOutput FormatJSON opts pat = TIO.putStrLn (JSON.patternToJSON opts pat)
formatOutput FormatGram _ pat = putStrLn (Gram.toGram pat)
formatOutput FormatDebug _ pat = print pat

formatError :: OutputFormat -> OutputOptions -> String -> IO ()
formatError FormatJSON opts err = TIO.putStrLn (JSON.errorToJSON opts err)
formatError FormatGram _ err = putStrLn ("Error: " ++ err)
formatError FormatDebug _ err = putStrLn ("Error: " ++ err)

