-- keep
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}


module PetStore.Server where

import           Control.Monad.Except
import           Control.Monad.Freer (runM, Eff, Member, send)
import           Control.Monad.Freer.Error (runError, Error)
import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
-- /keep

startServer :: Int -> IO ()
startServer port = do
  run port application
  where
    runEff :: Eff '[Error ServantErr, IO] x -> IO (Either ServantErr x)
    runEff = runM . runError

    runServant :: IO (Either ServantErr x) -> Handler x
    runServant = Handler . ExceptT

    runServer :: Eff '[Error ServantErr, IO] x -> Handler x
    runServer = runServant . runEff

    application = serve signature $ hoistServer signature (runServer) implementation

    signature :: Proxy PetStoreSignature
    signature = Proxy

    implementation = addPetHandler

    addPetHandler :: Pet -> Eff m ()
    addPetHandler _ = pure ()

type PetStoreSignature = AddPetRoute

type AddPetRoute = "pets"  :> ReqBody '[JSON] Pet :> Post '[JSON] () -- keep/

data Pet = Pet String
  deriving (Generic, FromJSON)
