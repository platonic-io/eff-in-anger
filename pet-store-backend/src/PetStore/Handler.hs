{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
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

type Foo = "foo"  :> Get '[JSON] String
type Bar = "bar" :> Get '[JSON] String
type ListPets = "listPets" :> Get '[JSON] Output

foo :: (Member IO m) => Eff m [Char]
foo = pure "foo"

bar :: Eff m [Char]
bar = pure "bar"

listPets' :: (Member LogEffect m) => Eff m Output
listPets' = do
  Freer.send $ Log ("hello" :: String)
  pure $ Error PetAlreadyAdded -- send ListPets

type PetServer m a =
  (MonadLog m, MonadReader StoreDB m, MonadError ServantErr m, MonadIO m) => m a

listPets :: PetServer m Output
listPets =  send ListPets

addPet :: Pet -> PetServer m Output
addPet pet =  send (Add pet)

removePet :: Pet -> PetServer m Output
removePet pet =  send (Remove pet)

reset :: PetServer m NoContent
reset =  resetStore >> pure NoContent

login            :: User -> PetServer m Output
login user =  send (UserLogin user)

logout           :: User -> PetServer m Output
logout user =  send (UserLogout user)

addToBasket      :: User -> Pet -> PetServer m Output
addToBasket user pet =  send (AddToBasket user pet)

removeFromBasket :: User -> Pet -> PetServer m Output
removeFromBasket user pet =  send (RemoveFromBasket user pet)

checkout         :: PaymentClient -> User -> Payment -> PetServer m Output
checkout paymentClient user payment = do
  res <- liftIO $ paymentClient payment
  case res of
    PaymentResult _ True -> send (CheckoutBasket user payment)
    _                    -> mlog ("failed to validate payment" <> show res) >> pure (Error InvalidPayment)

listBasket       :: User -> PetServer m Output
listBasket user = send (GetUserBasket user)
