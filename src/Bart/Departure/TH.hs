{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bart.Departure.TH
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Bart.Departure.TH
  ( bartLensRules
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Control.Lens
import           Control.Lens.TH

-------------------------------------------------------------------------------
-- Lens rules
-------------------------------------------------------------------------------

bartLensRules :: LensRules
bartLensRules = defaultRules
  & lensIso   .~ addSuffix
  & lensField .~ addSuffix
  where
  addSuffix [] = Nothing
  addSuffix xs = Just (xs ++ "L")
