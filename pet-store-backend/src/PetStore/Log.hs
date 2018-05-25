{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module PetStore.Log where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as IO
import           Data.Time.Clock.System
import           Data.Time.Format
import           Servant                    (NoContent)
import Control.Monad.Freer

class MonadLog m where
  mlog :: (ToJSON a) => a -> m ()


withinLog :: (MonadLog m, Monad m, ToJSON a, ToJSON b) => a -> m b -> m b
withinLog start act = do
  mlog start
  res <- act
  mlog res
  pure res

instance ToJSON NoContent where
  toJSON _ = Null

instance (MonadIO m) => MonadLog m where
  mlog a = liftIO $ do
    ts <- systemToUTCTime <$> getSystemTime
    IO.putStrLn $ encode $ object [ "timestamp" .= formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) ts
                                  , "message" .= a
                                  ]

-- withinLog' :: (Member MonadLog m, ToJSON a, ToJSON b) => a -> Eff m b -> Eff m b
-- withinLog' start act = do
--   mlog start
--   res <- act
--   mlog res
--   pure res

data LogEffect r where
  Log :: (ToJSON a) => a -> LogEffect ()

runLog :: (Member IO m) => Eff (LogEffect : m) x -> Eff m x
runLog = interpret eval where
  -- why do we need the type signature here?
  eval :: (Member IO n) => LogEffect y -> Eff n y
  eval (Log a) = do
    ts <- send $ systemToUTCTime <$> getSystemTime
    send $ IO.putStrLn $ encode $ object [
                                  "timestamp" .= formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) ts
                                  , "message" .= a
                                  ]
