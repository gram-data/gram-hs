{-# LANGUAGE OverloadedStrings #-}
-- | Module: Gram.Schema
-- 
-- Provides schema generation for Pattern<Subject> in various formats.
-- 
-- This module exports schema generators for different target formats,
-- enabling downstream language ports to validate their implementations.
--
-- @since 0.1.0
module Gram.Schema
  ( -- * Re-exports
    module Gram.Schema.JSONSchema
  ) where

import Gram.Schema.JSONSchema
