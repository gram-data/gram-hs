{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gramref.CLI (runCLI)
import System.Exit (exitWith)

main :: IO ()
main = do
  exitCode <- runCLI
  exitWith exitCode

