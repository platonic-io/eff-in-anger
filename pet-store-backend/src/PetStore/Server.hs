{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

import           Control.Monad.Except
import           Control.Monad.Freer (runM, Eff, Member)
import           Control.Monad.Freer.Error (Error, runError)
import           Data.Aeson
import           Data.Default
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           PetStore.Api
import           PetStore.Config
import           PetStore.Handler
import           PetStore.Log
import           PetStore.Payment.Api
import           PetStore.Store
import           PetStore.Command
import           PetStore.Swagger
import           Servant

startServer :: ServerConfig -> IO ()
startServer conf@ServerConfig{..} = do
  runM $ runLog $ mlog $ object [ "action" .= ("start" :: String), "configuration" .= conf ]
  store <- makeStore
  paymentClient <- makeClient paymentHost paymentPort
  logger <- doLog operationMode
  void $ run listeningPort $ logger $ server store operationMode paymentClient
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      runCustom :: (Member IO m) => PaymentClient -> StoreDB -> Eff (Command : StoreEff : LogEffect : m) x -> Eff m x
      runCustom paymentClient store = runLog . runStore store . runCommand paymentClient

      runEff :: Eff '[Error ServantErr, IO] x -> IO (Either ServantErr x)
      runEff = runM . runError -- . runLog

      runServant :: IO (Either ServantErr x) -> Handler x
      runServant = Handler . ExceptT

      runServer paymentClient store = runServant . runEff . runCustom paymentClient store
      -- runServer store = Handler . flip runReaderT store

      server store Prod paymentClient = serve petStoreApi $ hoistServer petStoreApi (runServer paymentClient store) prodHandler
      server store Dev paymentClient = serve devPetStoreApi $ hoistServer devPetStoreApi (runServer paymentClient store) (devHandler store)

      prodHandler = listPets' :<|> addPet' :<|> removePet' :<|> login' :<|> logout' :<|> addToBasket' :<|> removeFromBasket' :<|> checkout' :<|> listBasket'
      devHandler  store = prodHandler :<|> reset' store :<|> pure petStoreSwagger
