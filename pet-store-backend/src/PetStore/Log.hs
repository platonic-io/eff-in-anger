{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module PetStore.Log where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as IO
import           Data.Time.Clock.System
import           Data.Time.Format
import           Servant                    (NoContent)
import Control.Monad.Freer
import qualified Control.Monad.Freer       as Freer

instance ToJSON NoContent where
  toJSON _ = Null

withinLog' :: (ToJSON v, ToJSON b, Member LogEffect effs)
           => v
           -> Eff effs b -> Eff effs b
withinLog' start act = do
  mlog start
  res <- act
  mlog res
  pure res

data LogEffect r where
  Log :: (ToJSON a) => a -> LogEffect ()

mlog :: (Member LogEffect effs, ToJSON a )
     =>  a -> Eff effs ()
mlog = Freer.send . Log

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
