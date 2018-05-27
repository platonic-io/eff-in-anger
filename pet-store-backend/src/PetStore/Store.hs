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
import           Control.Monad.Freer       (Eff, Member, interpret)
import qualified Control.Monad.Freer       as Freer
import           Control.Monad.Reader
import           Control.Monad.Trans       (MonadIO (..))
import           Data.Aeson                (eitherDecode, encode)
import qualified Data.List                 as List
import           Data.Map                  (member)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Lazy.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO         (hGetLine, hPutStrLn)
import           PetStore.Log
import           PetStore.Messages
import qualified System.IO                 as IO
import           System.IO.Error

data Store = Store { storedPets :: [ Pet ]
                   , baskets    :: Map.Map User [ Pet ]
                   , eventSink  :: IO.Handle
                   }

runEvent :: (Member IO m) => StoreDB -> Eff (Event : m) x -> Eff m x
runEvent storedb = interpret eval where
  eval :: (Member IO n) => Event y -> Eff n y
  eval event = withinLog' event $ do
    Freer.send $ modifyMVar storedb $ \ store@Store{..} -> do
      --hPutStrLn eventSink (decodeUtf8 $ encode event)
      IO.hFlush eventSink
      pure (apply event store, undefined)

  apply :: Event y -> Store -> Store
  apply (PetAdded' pet) store                = store { storedPets = pet : storedPets store }
  apply (PetRemoved' pet) store              = store { storedPets = List.delete pet $ storedPets store }
  apply (UserLoggedIn' user) store
    | not (user `member` baskets store)       = store { baskets = Map.insert user [] (baskets store) }
    | otherwise                               = store
  apply (AddedToBasket' user pet) store      = store { storedPets = List.delete pet $ storedPets store
                                                      , baskets = Map.adjust (pet:) user $ baskets store }
  apply (RemovedFromBasket' user pet) store = store { storedPets = pet : storedPets store
                                                      , baskets = Map.adjust (List.delete pet) user $ baskets store }
  apply (CheckedOutBasket' user _ _) store       = store { baskets = Map.adjust (const []) user $ baskets store }
  apply (UserLoggedOut' user) store          = store { storedPets = putbackUserBasket (storedPets store)
                                                      , baskets = Map.delete user $ baskets store }
    where
      putbackUserBasket pets =
        case Map.lookup user (baskets store) of
          Nothing -> pets
          Just ps -> pets <> ps

  apply UserBasket'{} store                    = store
  apply Pets'{} store                          = store
  apply Error'{} store                         = store

send' :: (Member IO m) => StoreDB -> Input -> Eff m Output
send' storedb input = withinLog' input $ Freer.send $ modifyMVar storedb $ \ store@Store{..} -> do
  let event = act input store
  hPutStrLn eventSink (decodeUtf8 $ encode event)
  IO.hFlush eventSink
  pure (apply event store, event)

runCommand :: (Member LogEffect m, Member Event m, Member IO m) => StoreDB -> Eff (Command : m) x -> Eff m x
runCommand storedb eff = do
  Store (storedPets::[Pet]) baskets _ <- Freer.send $ readMVar storedb
  let
    -- why do we need the type signature here?
    eval :: (Member IO n, Member Event n) => Command y -> Eff n y
    eval (Add' pet) -- = send' storedb $ Add pet
      | (pet :: Pet) `notElem` (storedPets::[Pet]) = Freer.send $ PetAdded' pet
      | otherwise                = Freer.send $ Error' PetAlreadyAdded
    eval (Remove' pet) = send' storedb $ Remove pet
    eval (UserLogin' user) = send' storedb $ UserLogin user
    eval (AddToBasket' user pet ) = send' storedb $ AddToBasket user pet
    eval (RemoveFromBasket' user pet) = send' storedb $ RemoveFromBasket user pet
    eval (CheckoutBasket' user payment) = send' storedb $ CheckoutBasket user payment
    eval (CheckoutFailed' output) = pure output
    eval (UserLogout' user) = send' storedb $ UserLogout user
    eval ListPets' = send' storedb $ ListPets
    eval (GetUserBasket' user) = send' storedb $ GetUserBasket user
  interpret eval eff

resetStore :: (Member IO m) => StoreDB -> Eff m ()
resetStore db = do
  Freer.send $ modifyMVar_  db $ \ Store{..} -> do
    IO.hSeek eventSink IO.AbsoluteSeek 0
    IO.hSetFileSize eventSink 0
    pure $ Store [] Map.empty eventSink

act :: Input -> Store -> Output
act Add{pet}  Store{storedPets}
  | pet `notElem` storedPets = PetAdded pet
  | otherwise                = Error PetAlreadyAdded

act Remove{pet}  Store{storedPets}
  | pet `notElem` storedPets = Error PetDoesNotExist
  | otherwise                = PetRemoved pet

-- Actually a Query not a Command
act ListPets          Store{storedPets}
  = Pets storedPets

act UserLogin { user }               _
  = UserLoggedIn user

act AddToBasket { user, pet }        Store{storedPets, baskets}
  | not (user `member` baskets) = Error UserNotLoggedIn
  | pet `notElem` storedPets    = Error PetDoesNotExist
  | otherwise                   = AddedToBasket user pet

act RemoveFromBasket { user, pet}    Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | fmap (pet `notElem`) userBasket
    == Just True                     = Error PetNotInBasket
  | otherwise                        = RemovedFromBasket user pet

  where
    userBasket = Map.lookup user baskets

act GetUserBasket { user } Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | otherwise                    = UserBasket user userBasket
  where
    userBasket     = fromJust $ Map.lookup user baskets

act CheckoutBasket { user, payment } Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | otherwise                    = CheckedOutBasket user payment basketAmount
  where
    userBasket     = fromJust $ Map.lookup user baskets
    basketAmount   = foldr (+) 0 $ fmap petPrice userBasket

act UserLogout { user }              Store{baskets}
  | not (user `member` baskets)      = Error UserNotLoggedIn
  | otherwise                        = UserLoggedOut user


apply :: Output -> Store -> Store
apply PetAdded { pet } store                = store { storedPets = pet : storedPets store }
apply PetRemoved { pet } store              = store { storedPets = List.delete pet $ storedPets store }
apply UserLoggedIn { user} store
  | not (user `member` baskets store)       = store { baskets = Map.insert user [] (baskets store) }
  | otherwise                               = store
apply AddedToBasket { user, pet }store      = store { storedPets = List.delete pet $ storedPets store
                                                    , baskets = Map.adjust (pet:) user $ baskets store }
apply RemovedFromBasket { user, pet } store = store { storedPets = pet : storedPets store
                                                    , baskets = Map.adjust (List.delete pet) user $ baskets store }
apply CheckedOutBasket { user } store       = store { baskets = Map.adjust (const []) user $ baskets store }
apply UserLoggedOut { user } store          = store { storedPets = putbackUserBasket (storedPets store)
                                                    , baskets = Map.delete user $ baskets store }
  where
    putbackUserBasket pets =
      case Map.lookup user (baskets store) of
        Nothing -> pets
        Just ps -> pets <> ps

apply UserBasket{} store                    = store
apply Pets{} store                          = store
apply Error{} store                         = store

type StoreDB = MVar Store

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  let store = Store [] Map.empty h
  catchup store >>= newMVar

catchup :: Store -> IO Store
catchup store =
  (readAndApplyOneEvent store >>= catchup)
  `catch` \ (e :: IOException) -> if isEOFError e
                                  then pure store
                                  else throwIO e

readAndApplyOneEvent :: Store -> IO Store
readAndApplyOneEvent store@Store{eventSink} = do
  serializedEvent <- hGetLine eventSink
  either
    (throwIO . userError . (\ e -> "failed to decode event " <> show serializedEvent <> ": " <> e))
    (pure . flip apply store)
    $ eitherDecode (encodeUtf8 serializedEvent)
