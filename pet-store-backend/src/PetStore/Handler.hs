{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
module PetStore.Handler where


import           Control.Monad.Freer (Eff, Member, send)
import           PetStore.Command
import           PetStore.Messages
import           PetStore.Store
import           Servant

type PetServer' m a =
  (Member Command m) => Eff m a

listPets' :: PetServer' m Output
listPets' = do
  send $ Command ListPets

addPet' :: Pet -> PetServer' m Output
addPet' pet =  send $ Command (Add pet)

removePet' :: Pet -> PetServer' m Output
removePet' pet =  send $ Command (Remove pet)

reset' :: (Member IO m) => StoreDB -> PetServer' m NoContent
reset' db = resetStore db >> pure NoContent

login'            :: User -> PetServer' m Output
login' user =  send $ Command (UserLogin user)

logout'           :: User -> PetServer' m Output
logout' user =  send $ Command (UserLogout user)

addToBasket'      :: User -> Pet -> PetServer' m Output
addToBasket' user pet =  send $ Command (AddToBasket user pet)

removeFromBasket' :: User -> Pet -> PetServer' m Output
removeFromBasket' user pet =  send $ Command (RemoveFromBasket user pet)

checkout'         :: (Member IO m) => User -> Payment -> PetServer' m Output
checkout' user payment = send $ Command (CheckoutBasket user payment)

listBasket'       :: User -> PetServer' m Output
listBasket' user = send $ Command (GetUserBasket user)
