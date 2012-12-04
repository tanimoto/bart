{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards    #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bart.Departure.Proc
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Bart.Departure.Proc
  ( bart
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Bart.Departure.Parser
import           Bart.Departure.Types
import           Bart.Departure.HTTP

import           Control.Monad                (when)
import           Control.Monad.Trans.Class    (MonadTrans (lift))
import           Control.Monad.IO.Class       (MonadIO (liftIO))

import           Control.Lens                 hiding (left)

import           Control.Error

import           Data.Default                 (Default (..))

import           Data.Conduit                 (Conduit, Pipe, ResourceT, Sink,
                                               Source, ($$), ($=), (=$))
import qualified Data.Conduit                 as C

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T

import qualified Network.HTTP.Conduit         as H


-------------------------------------------------------------------------------
-- Processing
-------------------------------------------------------------------------------

formatResult :: Result -> Text
formatResult Result {..} =
  T.intercalate " "
    [ resDest
    , resColor
    , T.intercalate ", " resMins
    , "min"
    ]

bart :: Options -> IO ()
bart Departure {..} = do
  res <- runEitherT $ do
    -- Check if user provided a station
    stn <- flip tryHead station "Error: provide a station"

    -- Download the departure data
    man <- lift $ H.newManager H.def
    may <- lift $ C.runResourceT $ do
                  getDepart man $ T.pack stn
    dep <- flip tryJust may "Error: could not download data"

    -- Parse the departure data
    return $ map parse $ dep
              ^. departStationsL
              ^. to (headDef def)
              ^. stationEtdsL
  -- Display the results
  case res of
    Left msg -> errLn msg
    Right xs -> mapM_ (T.putStrLn . formatResult) xs

  where
  parse Etd {..} = def
    & resDestL  .~ etdAbbr
    & resColorL .~ (headDef def etdEsts ^. estColorL)
    & resMinsL  .~ (map estMinutes etdEsts)
