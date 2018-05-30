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
import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Freer       (Eff, Member, interpret, runM, send)
import           Control.Monad.Freer.Error (Error, runError)
import           Data.Aeson                (FromJSON, ToJSON, encode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           GHC.Generics              (Generic)
import           Network.Wai.Handler.Warp  (run)
import           Servant
-- /keep

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

    addPetHandler :: (Member BusinessLogicEffect m) => Pet -> Eff m String
    addPetHandler = send . AddPet

  run port application

runBusinessLogic :: (Member StorageEffect m) => Eff (BusinessLogicEffect : m) x -> Eff m x
runBusinessLogic = interpret eval where
  eval :: (Member StorageEffect n) => BusinessLogicEffect a -> Eff n a
  eval (AddPet pet) = do
    send $ AddDocument "pets" $ encode pet
    pure $ "got: " ++ petName pet

runStorage :: (Member IO m) => MVar Store -> Eff (StorageEffect : m) x -> Eff m x
runStorage store' = interpret eval where
  eval :: (Member IO n) => StorageEffect a -> Eff n a
  eval (AddDocument key bytes) = send $ modifyMVar store' $
    \Store{collections} -> pure (Store{collections = Map.update (\c -> Just $ bytes : c) key collections}, () )

type PetStoreSignature = AddPetRoute

type AddPetRoute = "pets"  :> ReqBody '[JSON] Pet :> Post '[JSON] String -- keep/

data Pet = Pet{
  petName :: String
} deriving (Generic, FromJSON, ToJSON)

data BusinessLogicEffect a where
  AddPet :: Pet -> BusinessLogicEffect String

data StorageEffect a where
  AddDocument :: Key -> ByteString -> StorageEffect ()

type Key = String


