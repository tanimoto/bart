{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-cse #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bart.Departure.Types
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Bart.Departure.Types
--  (
--  ) where
  where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Bart.Departure.TH      (bartLensRules)

import           Control.Lens

import           Data.Data

import           Data.Default           (Default (..))

import           Data.Monoid            (Monoid (..), (<>))

import           Data.Text              (Text)

import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as Cmd


-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

data Depart = Depart
  { departUri      :: Text
  , departDate     :: Text
  , departTime     :: Text
  , departStations :: [Station]
  , departMessage  :: Text
  }
  deriving (Eq, Show, Data, Typeable)

instance Default Depart where
  def = Depart
    { departUri      = mempty
    , departDate     = mempty
    , departTime     = mempty
    , departStations = mempty
    , departMessage  = mempty
    }

data Station = Station
  { stationName :: Text
  , stationAbbr :: Text
  , stationEtds :: [Etd]
  }
  deriving (Eq, Show, Data, Typeable)

instance Default Station where
  def = Station
    { stationName = mempty
    , stationAbbr = mempty
    , stationEtds = mempty
    }

data Etd = Etd
  { etdDest :: Text
  , etdAbbr :: Text
  , etdEsts :: [Estimate]
  }
  deriving (Eq, Show, Data, Typeable)

instance Default Etd where
  def = Etd
    { etdDest = mempty
    , etdAbbr = mempty
    , etdEsts = mempty
    }

data Estimate = Estimate
  { estMinutes   :: Text
  , estPlatform  :: Text
  , estDirection :: Text
  , estLength    :: Text
  , estColor     :: Text
  , estHexColor  :: Text
  , estBikeFlag  :: Text
  }
  deriving (Eq, Show, Data, Typeable)

instance Default Estimate where
  def = Estimate
    { estMinutes   = mempty
    , estPlatform  = mempty
    , estDirection = mempty
    , estLength    = mempty
    , estColor     = mempty
    , estHexColor  = mempty
    , estBikeFlag  = mempty
    }

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLensesWith bartLensRules ''Depart
makeLensesWith bartLensRules ''Station
makeLensesWith bartLensRules ''Etd
makeLensesWith bartLensRules ''Estimate


-------------------------------------------------------------------------------
-- Command Line Options
-------------------------------------------------------------------------------

data Options
  = Departure
    { station :: [String]
    }
  deriving (Eq, Show, Data, Typeable)

options :: Options
options
   = Cmd.modes [departure &= Cmd.auto]
  &= Cmd.program "bart"

departure :: Options
departure = Departure
  { station = Cmd.def &= Cmd.typ "STATION" &= Cmd.args
  }


-------------------------------------------------------------------------------
-- Results
-------------------------------------------------------------------------------

data Result = Result
  { resDest  :: Text
  , resColor :: Text
  , resMins  :: [Text]
  }
  deriving (Eq, Show)

instance Default Result where
  def = Result
    { resDest  = mempty
    , resColor = mempty
    , resMins  = mempty
    }

makeLensesWith bartLensRules ''Result
