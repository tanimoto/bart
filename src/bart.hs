{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


-------------------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Main
  ( main
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Bart.Departure.Types
import           Bart.Departure.Proc

import           Network                   (withSocketsDo)

import qualified System.Console.CmdArgs    as Cmd


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
  opts <- Cmd.cmdArgs options
  bart opts
