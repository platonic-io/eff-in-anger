{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module PetStore.Config where

import           Data.Aeson
import           GHC.Generics


data ServerMode = Prod | Dev
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data ServerConfig = ServerConfig { operationMode :: ServerMode
                                 , listeningPort :: Int
                                 , paymentHost   :: String
                                 , paymentPort   :: Int
                                 }
                  deriving (Eq, Show, Generic, ToJSON, FromJSON)
