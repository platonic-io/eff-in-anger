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

type MyApi = Foo :<|> Bar :<|> ListPets

apiSig :: Proxy MyApi
apiSig = Proxy

startServer :: ServerConfig -> IO ()
startServer conf@ServerConfig{..} = do
  mlog $ object [ "action" .= ("start" :: String), "configuration" .= conf ]
  store <- makeStore
  paymentClient <- makeClient paymentHost paymentPort
  logger <- doLog operationMode
  void $ run listeningPort $ logger $ server store operationMode paymentClient
    where
      doLog _ = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      myApi = foo :<|> bar :<|> listPets'

      handleCustom :: (Member IO m) => Eff (LogEffect : m) x -> Eff m x
      handleCustom = runLog

      handleEff :: Eff '[Error ServantErr, IO] x -> IO (Either ServantErr x)
      handleEff = runM . runError -- . runLog

      handleServant :: IO (Either ServantErr x) -> Handler x
      handleServant = Handler . ExceptT

      handle = handleServant . handleEff . handleCustom

      server _ _ _ = serve apiSig $ hoistServer apiSig handle myApi

      -- runServer store = Handler . flip runReaderT store

      -- server store Prod paymentClient = serve petStoreApi $ hoistServer petStoreApi (runServer store) (prodHandler paymentClient)
      -- server store Dev  paymentClient = serve devPetStoreApi $ hoistServer devPetStoreApi (runServer store) (devHandler paymentClient)

      -- prodHandler paymentClient = listPets :<|> addPet :<|> removePet :<|> login :<|> logout :<|> addToBasket :<|> removeFromBasket :<|> checkout paymentClient :<|> listBasket
      -- devHandler  paymentClient = prodHandler paymentClient :<|> reset :<|> pure petStoreSwagger
