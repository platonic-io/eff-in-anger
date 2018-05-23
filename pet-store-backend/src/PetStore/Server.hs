{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PetStore.Server where

import           Control.Monad.Except
import           Control.Monad.Reader
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
import           PetStore.Swagger
import           Servant

startServer :: ServerConfig -> IO ()
startServer conf@ServerConfig{..} = do
  mlog $ object [ "action" .= ("start" :: String), "configuration" .= conf ]
  store <- makeStore
  paymentClient <- makeClient paymentHost paymentPort
  logger <- doLog operationMode
  void $ run listeningPort $ logger $ server store operationMode paymentClient
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      runServer store = Handler . flip runReaderT store

      server store Prod paymentClient = serve petStoreApi $ hoistServer petStoreApi (runServer store) (prodHandler paymentClient)
      server store Dev  paymentClient = serve devPetStoreApi $ hoistServer devPetStoreApi (runServer store) (devHandler paymentClient)

      prodHandler paymentClient = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout paymentClient :<|> listBasket
      devHandler  paymentClient = prodHandler paymentClient :<|> reset :<|> pure petStoreSwagger
