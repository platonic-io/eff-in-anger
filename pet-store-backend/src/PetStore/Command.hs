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
import           PetStore.Messages
import           PetStore.Payment.Api
import           PetStore.State
import           PetStore.Store


data Command a where
  Command :: Input -> Command Output

runCommand :: (Member StoreEff m, Member StoreEff m, Member LogEffect m, Member IO m) => PaymentClient -> Eff (Command : m) x -> Eff m x
runCommand paymentClient eff =   interpret eval eff
  where

    eval :: (Member StoreEff n, Member LogEffect n, Member IO n)
         => Command y -> Eff n y
    eval (Command  co@CheckoutBasket{..}) = withinLog' co $ do
      res <- Freer.send $ paymentClient payment
      case res of
        PaymentResult _ True -> persist (act co)
        _                    -> do
          mlog ("failed to validate payment" <> show res)
          pure $ Error InvalidPayment
    eval (Command input)  = withinLog' input $ persist (act input)
