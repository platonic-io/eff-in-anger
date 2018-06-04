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

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar, readMVar)
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

data Database = Database{
  documents :: [ByteString]
}

startServer :: IO ()
startServer = do
  dbMVar <- newMVar $ Database []
  let
    handlers = addPet
    application = serve routes $ hoistServer routes (effToServant dbMVar) handlers
  run 1234 application

effToIO :: MVar Database -> Eff '[PetStoreEffect, DatabaseEffect, Error ServantErr, IO] x -> IO ( Either ServantErr x )
effToIO dbMVar = runM . runError . runInMemoryDatabase dbMVar . runPetStore

ioToServant :: IO ( Either ServantErr x ) -> Handler x
ioToServant = Handler . ExceptT

effToServant :: MVar Database -> Eff '[PetStoreEffect, DatabaseEffect, Error ServantErr, IO] x -> Handler x
effToServant dbMVar = ioToServant . effToIO dbMVar

data Pet = Pet{
  petName :: String
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

data DatabaseEffect a where
  AddDocument :: ByteString -> DatabaseEffect ()
  ListDocuments :: DatabaseEffect (Either String [ByteString])

addDocument :: (Member DatabaseEffect m) => ByteString -> Eff m ()
addDocument = send . AddDocument

listDocuments :: (Member DatabaseEffect m) => Eff m (Either String [ByteString])
listDocuments = send ListDocuments

data PetStoreEffect a where
  AddPet :: Pet -> PetStoreEffect [Pet]

addPet :: (Member PetStoreEffect m) => Pet -> Eff m [Pet]
addPet = send . AddPet

runInMemoryDatabase :: (Member IO m) => MVar Database -> Eff (DatabaseEffect : m) x -> Eff m x
runInMemoryDatabase dbMVar = interpret eval where
  eval :: (Member IO n) => DatabaseEffect a -> Eff n a
  eval (AddDocument document) = send $ modifyMVar dbMVar $
    \ db -> pure ( db{ documents = document : documents db }, () )
  eval ListDocuments = send $ Right . documents <$> readMVar dbMVar

runPetStore :: (Member (Error ServantErr) m, Member DatabaseEffect m) => Eff (PetStoreEffect : m) x -> Eff m x
runPetStore = interpret eval where
  eval :: (Member (Error ServantErr) n, Member DatabaseEffect n) => PetStoreEffect a -> Eff n a
  eval (AddPet pet) = do
    addDocument $ encode pet
    pets <- decodePets <$> listDocuments
    case pets of
      Right pets' -> pure pets'
      Left err -> throwError err500

type Routes = AddPetRoute
type AddPetRoute = "pets" :> ReqBody '[JSON] Pet :> Post '[JSON] [Pet]

routes :: Proxy Routes
routes = Proxy

decodePets :: Either String [ByteString] -> Either String [Pet]
decodePets = join . fmap (traverse eitherDecode)

