{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bart.Departure.Parser
-- Copyright  : (c) 2012 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Bart.Departure.Parser
  ( parseDepart
  , parseStation
  , parseEtd
  , parseEstimate
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Bart.Departure.Types

import           Control.Applicative          (Applicative (..), (<$>))
import           Control.Monad                (join)

import           Control.Monad.IO.Class       (MonadIO (liftIO))

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
import qualified Data.ByteString.Char8        as BC

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Data.XML.Types

import           Text.XML.Stream.Parse


-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

ptag
  :: C.MonadThrow m
  => Name
  -> Pipe Event Event o u m a
  -> Pipe Event Event o u m (Maybe a)
ptag name p = tagNoAttr name p

text
  :: C.MonadThrow m
  => Name
  -> Pipe Event Event o u m (Maybe Text)
text name = ptag name content

withDef :: (Monad m, Default a) => m (Maybe a) -> m a
withDef f = do
  may <- f
  case may of
    Nothing  -> return def
    Just res -> return res

parseDepart
  :: C.MonadThrow m
  => Pipe Event Event o u m (Maybe Depart)
parseDepart = do
  withDef $ ptag "root" $ do
    uri  <- text "uri"
    date <- text "date"
    time <- text "time"
    stns <- many parseStation
    msg  <- text "message"
    return $ Depart
      <$> uri
      <*> date
      <*> time
      <*> Just stns
      <*> msg

parseStation
  :: C.MonadThrow m
  => Pipe Event Event o u m (Maybe Station)
parseStation = do
  withDef $ ptag "station" $ do
    name <- text "name"
    abbr <- text "abbr"
    etds <- many parseEtd
    return $ Station
      <$> name
      <*> abbr
      <*> Just etds

parseEtd
  :: C.MonadThrow m
  => Pipe Event Event o u m (Maybe Etd)
parseEtd = do
  withDef $ ptag "etd" $ do
    dest <- text "destination"
    abbr <- text "abbreviation"
    ests <- many parseEstimate
    return $ Etd
      <$> dest
      <*> abbr
      <*> Just ests

parseEstimate
  :: C.MonadThrow m
  => Pipe Event Event o u m (Maybe Estimate)
parseEstimate = do
  withDef $ ptag "estimate" $ do
    minu <- text "minutes"
    plat <- text "platform"
    dir  <- text "direction"
    len  <- text "length"
    col  <- text "color"
    hex  <- text "hexcolor"
    bike <- text "bikeflag"
    return $ Estimate
      <$> minu
      <*> plat
      <*> dir
      <*> len
      <*> col
      <*> hex
      <*> bike
