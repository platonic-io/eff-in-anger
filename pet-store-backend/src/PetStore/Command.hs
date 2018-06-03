{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- | A backend store based on event sourcing
--
--  * Events affecting store are persisted as a stream of events into a file
--  * State is cached in memory
--
module PetStore.Command where

import           Control.Monad.Freer  (Eff, Member, interpret)
import qualified Control.Monad.Freer  as Freer
import           Data.Monoid
import           PetStore.Log
import           PetStore.Messages    (Output, Payment, PaymentResult (..), Pet,
                                       PetStoreError (..), User)
import qualified PetStore.Messages    as M
import           PetStore.Payment.Api
import           PetStore.State
import           PetStore.Store
import           Servant


data Command a where
  CheckoutBasket :: { user :: User, payment :: Payment } -> Command Output
  Add :: { pet :: Pet } -> Command Output
  Remove :: { pet :: Pet } -> Command Output
  UserLogin :: { user :: User } -> Command Output
  AddToBasket :: { user :: User, pet :: Pet } -> Command Output
  RemoveFromBasket :: { user :: User, pet :: Pet } -> Command Output
  UserLogout :: { user :: User } -> Command Output
  ListPets :: Command Output
  GetUserBasket :: { user :: User } -> Command Output


runCommand :: (Member StoreEff m, Member StoreEff m, Member LogEffect m, Member IO m) => PaymentClient -> Eff (Command : m) x -> Eff m x
runCommand paymentClient eff =   interpret eval eff
  where

    eval :: (Member StoreEff n, Member LogEffect n, Member IO n)
         => Command y -> Eff n y
    eval (CheckoutBasket{user,payment}) = do
      res <- Freer.send $ paymentClient payment
      case res of
        PaymentResult _ True -> persist (act co)
        _                    -> do
          mlog ("failed to validate payment" <> show res)
          pure $ M.Error InvalidPayment
      where
        co = M.CheckoutBasket user payment
    eval (Add pet) = sendToStore (M.Add pet)
    eval (Remove pet) = sendToStore (M.Remove pet)
    eval (UserLogin user) = sendToStore (M.UserLogin user)
    eval (AddToBasket user pet) = sendToStore (M.AddToBasket user pet)
    eval (RemoveFromBasket user pet) = sendToStore (M.RemoveFromBasket user pet)
    eval (UserLogout user) = sendToStore (M.UserLogout user)
    eval ListPets = sendToStore M.ListPets
    eval (GetUserBasket user) = sendToStore (M.GetUserBasket user)

    sendToStore input = withinLog' input $ persist (act input)


reset :: (Member IO m) => StoreDB -> Eff m NoContent
reset db = resetStore db >> pure NoContent
