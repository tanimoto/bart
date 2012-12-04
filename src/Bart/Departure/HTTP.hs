{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bart.Departure.HTTP
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Bart.Departure.HTTP
  ( reqDepart
  , getDepart
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Bart.Departure.Parser
import           Bart.Departure.Types

import           Control.Monad.IO.Class       (MonadIO (liftIO))

import           Control.Error

import           Control.Exception            (try)

import           Data.Default                 (Default (..))

import           Data.Monoid                  (Monoid (..), (<>))

import           Data.Conduit                 (Conduit, Pipe, ResourceT, Sink,
                                               Source, ($$), ($=), (=$))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T

import           Text.XML.Stream.Parse

import qualified Network.HTTP.Conduit         as H
import qualified Network.HTTP.Conduit.Browser as H
import qualified Network.HTTP.Types           as H


-------------------------------------------------------------------------------
-- HTTP
-------------------------------------------------------------------------------

bartKey :: ByteString
bartKey = "MW9S-E7SL-26DU-VV8V"

reqDepart :: Text -> IO (H.Request (C.ResourceT IO))
reqDepart station = do
  req <- H.parseUrl "http://api.bart.gov/api/etd.aspx"
  return $ req { H.queryString = H.renderQuery False
    [ ("cmd", Just "etd")
    , ("orig", Just $ T.encodeUtf8 station)
    , ("plat", Nothing)
    , ("dir", Nothing)
    , ("key", Just bartKey)
    ]}

getXml :: H.Manager -> H.Request (ResourceT IO) -> ResourceT IO L.ByteString
getXml man req = do
  res <- H.browse man $ H.makeRequestLbs req
  return $ H.responseBody res

getDepart :: H.Manager -> Text -> ResourceT IO (Maybe Depart)
getDepart man station = do
  req <- liftIO $ reqDepart station
  xml <- getXml man req
  dep <- parseLBS def xml $$ parseDepart
  return dep
