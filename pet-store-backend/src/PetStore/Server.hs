-- keep
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PetStore.Server where

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar)
import           Control.Monad.Except      (ExceptT (..), join)
import           Control.Monad.Freer       (Eff, Member, interpret, runM, send)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Data.Aeson                (FromJSON, ToJSON, eitherDecode,
                                            encode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           GHC.Generics              (Generic)
import           Network.Wai.Handler.Warp  (run)
import           Servant                   hiding (throwError)
-- /keep

decodePets :: Either String [ByteString] -> Either String [Pet]
decodePets = join . fmap (traverse eitherDecode)

data Store = Store {
  collections :: Map.Map Key [ByteString]
}

startServer :: Int -> IO ()
startServer port = do
  store <- newMVar $ Store $ Map.empty
  let
    runEff :: Eff '[BusinessLogicEffect, StorageEffect, Error ServantErr, IO] x -> IO (Either ServantErr x)
    runEff = runM . runError . runStorage store . runBusinessLogic

    runServant :: IO (Either ServantErr x) -> Handler x
    runServant = Handler . ExceptT

    runServer = runServant . runEff

    application = serve signature $ hoistServer signature (runServer) implementation

    signature :: Proxy PetStoreSignature
    signature = Proxy

    implementation = addPetHandler

    addPetHandler :: (Member BusinessLogicEffect m) => Pet -> Eff m [Pet]
    addPetHandler = send . AddPet

  run port application

runBusinessLogic :: (Member StorageEffect m, Member (Error ServantErr) m) => Eff (BusinessLogicEffect : m) x -> Eff m x
runBusinessLogic = interpret eval where
  eval :: (Member StorageEffect n, Member (Error ServantErr) n) => BusinessLogicEffect a -> Eff n a
  eval (AddPet pet) = do
    pets <- decodePets <$> (send $ AddDocument "pets" $ encode pet)
    case pets of
      Right p -> pure p
      _       -> throwError $ err500

runStorage :: (Member IO m) => MVar Store -> Eff (StorageEffect : m) x -> Eff m x
runStorage store' = interpret eval where
  eval :: (Member IO n) => StorageEffect a -> Eff n a
  eval (AddDocument key bytes) = send $ modifyMVar store' $
    \Store{collections} ->
      let
        existing = maybe [] id $ Map.lookup key collections
        updated = bytes : existing
      in pure ( Store{collections = Map.insert key updated collections}, Right updated )

type PetStoreSignature = AddPetRoute

type AddPetRoute = "pets"  :> ReqBody '[JSON] Pet :> Post '[JSON] [Pet] -- keep/

data Pet = Pet{
  petName :: String
} deriving (Generic, FromJSON, ToJSON, Show, Eq)

data BusinessLogicEffect a where
  AddPet :: Pet -> BusinessLogicEffect [Pet]

data StorageEffect a where
  AddDocument :: Key -> ByteString -> StorageEffect (Either String [ByteString])

type Key = String


