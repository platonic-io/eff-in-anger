{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
module PetStore.Handler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Monoid          ((<>))
import           PetStore.Log
import           PetStore.Messages
import           PetStore.Payment.Api
import           PetStore.Store
import           Servant

type PetServer m a =
  (MonadLog m, MonadReader StoreDB m, MonadError ServantErr m, MonadIO m) => m a

listPets :: PetServer m Output
listPets =  ask >>= send ListPets

addPet :: Pet -> PetServer m Output
addPet pet =  ask >>= send (Add pet)

removePet :: Pet -> PetServer m Output
removePet pet =  ask >>= send (Remove pet)

reset :: PetServer m NoContent
reset =  ask >>= resetStore >> pure NoContent

login            :: User -> PetServer m Output
login user =  ask >>= send (UserLogin user)

logout           :: User -> PetServer m Output
logout user =  ask >>= send (UserLogout user)

addToBasket      :: User -> Pet -> PetServer m Output
addToBasket user pet =  ask >>= send (AddToBasket user pet)

removeFromBasket :: User -> Pet -> PetServer m Output
removeFromBasket user pet =  ask >>= send (RemoveFromBasket user pet)

checkout         :: PaymentClient -> User -> Payment -> PetServer m Output
checkout paymentClient user payment = do
  store <- ask
  res <- liftIO $ paymentClient payment
  case res of
    PaymentResult _ True -> send (CheckoutBasket user payment) store
    _                    -> mlog ("failed to validate payment" <> show res) >> pure (Error InvalidPayment)

listBasket       :: User -> PetServer m Output
listBasket user = ask >>= send (GetUserBasket user)
