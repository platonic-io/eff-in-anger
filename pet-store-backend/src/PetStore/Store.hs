{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | A backend store based on event sourcing
--
--  * Events affecting store are persisted as a stream of events into a file
--  * State is cached in memory
--
module PetStore.Store where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Freer     (Eff, Member, interpret)
import qualified Control.Monad.Freer     as Freer
import           Data.Aeson              (eitherDecode, encode)
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           PetStore.Messages
import           PetStore.State
import qualified System.IO               as IO
import           System.IO.Error

data StoreDB = StoreDB { storedb   :: MVar Store
                       , eventSink :: IO.Handle
                       }

data StoreEff a where
  Persist :: (Store -> Output) -> StoreEff Output

persist :: (Member StoreEff effs)
        => (Store -> Output) -> Eff effs Output
persist = Freer.send . Persist

runStore :: (Member IO m)
         => StoreDB -> Eff (StoreEff : m) x -> Eff m x
runStore StoreDB{storedb, eventSink} = interpret eval where
  eval :: (Member IO n) => StoreEff y -> Eff n y
  eval (Persist upd) = do
    Freer.send $ modifyMVar storedb $ \ store@Store{..} -> do
      let event = upd store
      hPutStrLn eventSink (decodeUtf8 $ encode event)
      IO.hFlush eventSink
      pure (apply event store, event)

resetStore :: (Member IO m) => StoreDB -> Eff m ()
resetStore StoreDB{storedb,eventSink} = do
  Freer.send $ modifyMVar_  storedb $ \ Store{..} -> do
    IO.hSeek eventSink IO.AbsoluteSeek 0
    IO.hSetFileSize eventSink 0
    pure $ Store [] Map.empty

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  store  <- catchup h $ Store [] Map.empty
  StoreDB <$> newMVar store <*> pure h

catchup :: IO.Handle -> Store -> IO Store
catchup hdl store =
  (readAndApplyOneEvent hdl store >>= catchup hdl)
  `catch` \ (e :: IOException) -> if isEOFError e
                                  then pure store
                                  else throwIO e

readAndApplyOneEvent :: IO.Handle -> Store -> IO Store
readAndApplyOneEvent eventSink store = do
  serializedEvent <- hGetLine eventSink
  either
    (throwIO . userError . (\ e -> "failed to decode event " <> show serializedEvent <> ": " <> e))
    (pure . flip apply store)
    $ eitherDecode (encodeUtf8 serializedEvent)
