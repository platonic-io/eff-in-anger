{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
module PetStore.Handler where

import           Control.Monad.Except
import           Control.Monad.Freer       (Eff, Member, runM)
import qualified Control.Monad.Freer       as Freer
import           Control.Monad.Freer.Error (Error, runError)
import           Control.Monad.Reader
import           Data.Monoid               ((<>))
import           PetStore.Log
import           PetStore.Messages
import           PetStore.Payment.Api
import           PetStore.Store
import           Servant

type PetServer' m a =
  (Member Command m) => Eff m a

listPets' :: PetServer' m Output
listPets' = do
  Freer.send ListPets'

addPet' :: Pet -> PetServer' m Output
addPet' pet =  Freer.send (Add' pet)

removePet' :: Pet -> PetServer' m Output
removePet' pet =  Freer.send (Remove' pet)

reset' :: (Member IO m) => StoreDB -> PetServer' m NoContent
reset' db = resetStore db >> pure NoContent

login'            :: User -> PetServer' m Output
login' user =  Freer.send (UserLogin' user)

logout'           :: User -> PetServer' m Output
logout' user =  Freer.send (UserLogout' user)

addToBasket'      :: User -> Pet -> PetServer' m Output
addToBasket' user pet =  Freer.send (AddToBasket' user pet)

removeFromBasket' :: User -> Pet -> PetServer' m Output
removeFromBasket' user pet =  Freer.send (RemoveFromBasket' user pet)

checkout'         :: (Member IO m) => PaymentClient -> User -> Payment -> PetServer' m Output
checkout' paymentClient user payment = do
  res <- Freer.send $ paymentClient payment
  case res of
    PaymentResult _ True -> Freer.send (CheckoutBasket' user payment)
    _                    -> do
      Freer.send $ mlog' ("failed to validate payment" <> show res)
      Freer.send $ CheckoutFailed' $ Error InvalidPayment

listBasket'       :: User -> PetServer' m Output
listBasket' user = Freer.send (GetUserBasket' user)
