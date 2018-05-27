{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module PetStore.Api where

import           Data.Swagger
import           Data.Text         (pack, unpack)
import           PetStore.Messages
import           Servant


type ListPets         = "pets"  :> Get '[JSON] Output
type AddPet           = "pets"  :> ReqBody '[JSON] Pet :> Post '[JSON] Output
type RemovePet        = "pets"  :> ReqBody '[JSON] Pet :> Delete '[JSON] Output
type Login            = "users" :> Capture "user" User :> Put '[JSON] Output
type Logout           = "users" :> Capture "user" User :> Delete '[JSON] Output
type AddToBasket      = "users" :> Capture "user" User :> "basket" :> ReqBody '[JSON] Pet :> Put '[JSON] Output
type RemoveFromBasket = "users" :> Capture "user" User :> "basket" :> ReqBody '[JSON] Pet :> Delete '[JSON] Output
type Checkout         = "users" :> Capture "user" User :> "basket" :> ReqBody '[JSON] Payment :> Post '[JSON] Output
type ListBasket       = "users" :> Capture "user" User :> "basket" :> Get '[JSON] Output

type PetStoreApi = ListPets :<|> AddPet :<|> RemovePet :<|> Login :<|> Logout :<|> AddToBasket :<|> RemoveFromBasket :<|> Checkout :<|> ListBasket

type Reset            = "reset" :> Delete '[JSON] NoContent

type DevPetStoreApi = PetStoreApi :<|> Reset :<|> "swagger.json" :> Get '[JSON] Swagger

petStoreApi :: Proxy PetStoreApi
petStoreApi = Proxy

devPetStoreApi :: Proxy DevPetStoreApi
devPetStoreApi = Proxy

instance FromHttpApiData User where
  parseQueryParam = Right . User . unpack

instance ToHttpApiData User where
  toQueryParam (User u) = pack u
