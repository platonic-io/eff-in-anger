{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

import           Control.Monad.Except
import           Control.Monad.Freer (runM, Eff, Member)
import           Control.Monad.Freer.Error (Error, runError)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Default
import           GHC.MVar
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           PetStore.Api
import           PetStore.Config
import           PetStore.Handler
import           PetStore.Log
import           PetStore.Messages
import           PetStore.Payment.Api
import           PetStore.Payment.Types
import           PetStore.Store
import           PetStore.Swagger
import           Servant

startServer :: ServerConfig -> IO ()
startServer conf@ServerConfig{..} = do
  mlog' $ object [ "action" .= ("start" :: String), "configuration" .= conf ]
  store <- makeStore
  paymentClient <- makeClient paymentHost paymentPort
  logger <- doLog operationMode
  void $ run listeningPort $ logger $ server store operationMode paymentClient
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      myApi paymentClient = listPets' :<|> addPet' :<|> removePet' :<|> login' :<|> logout' :<|> addToBasket' :<|> removeFromBasket' :<|> checkout' paymentClient :<|> listBasket'

      runCustom :: (Member IO m) => StoreDB -> Eff (Command : LogEffect : m) x -> Eff m x
      runCustom store = runLog . runEvent store . runCommand store

      runEff :: Eff '[Error ServantErr, IO] x -> IO (Either ServantErr x)
      runEff = runM . runError -- . runLog

      runServant :: IO (Either ServantErr x) -> Handler x
      runServant = Handler . ExceptT

      runServer store = runServant . runEff . runCustom store
      -- runServer store = Handler . flip runReaderT store

      server store Prod paymentClient = serve petStoreApi $ hoistServer petStoreApi (runServer store) (prodHandler paymentClient)
      server store Dev paymentClient = serve devPetStoreApi $ hoistServer devPetStoreApi (runServer store) (devHandler paymentClient store)

      prodHandler paymentClient = listPets' :<|> addPet' :<|> removePet' :<|> login' :<|> logout' :<|> addToBasket' :<|> removeFromBasket' :<|> checkout' paymentClient :<|> listBasket'
      devHandler  paymentClient store = prodHandler paymentClient :<|> reset' store :<|> pure petStoreSwagger
