{-# LANGUAGE NamedFieldPuns #-}
module PetStore.State where

import qualified Data.List         as List
import           Data.Map          (member)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)
import           Data.Monoid       ((<>))
import           PetStore.Messages


data Store = Store { storedPets :: [ Pet ]
                   , baskets    :: Map.Map User [ Pet ]
                   }


act :: Input -> Store -> Output
act Add{pet}  Store{storedPets}
  | pet `notElem` storedPets = PetAdded pet
  | otherwise                = Error PetAlreadyAdded

act Remove{pet}  Store{storedPets}
  | pet `notElem` storedPets = Error PetDoesNotExist
  | otherwise                = PetRemoved pet

-- Actually a Query not a Command
act ListPets          Store{storedPets}
  = Pets storedPets

act UserLogin { user }               _
  = UserLoggedIn user

act AddToBasket { user, pet }        Store{storedPets, baskets}
  | not (user `member` baskets) = Error UserNotLoggedIn
  | pet `notElem` storedPets    = Error PetDoesNotExist
  | otherwise                   = AddedToBasket user pet

act RemoveFromBasket { user, pet}    Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | fmap (pet `notElem`) userBasket
    == Just True                     = Error PetNotInBasket
  | otherwise                        = RemovedFromBasket user pet

  where
    userBasket = Map.lookup user baskets

act GetUserBasket { user } Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | otherwise                    = UserBasket user userBasket
  where
    userBasket     = fromJust $ Map.lookup user baskets

act CheckoutBasket { user, payment } Store{baskets}
  | not (user `member` baskets)  = Error UserNotLoggedIn
  | otherwise                    = CheckedOutBasket user payment basketAmount
  where
    userBasket     = fromJust $ Map.lookup user baskets
    basketAmount   = foldr (+) 0 $ fmap petPrice userBasket

act UserLogout { user }              Store{baskets}
  | not (user `member` baskets)      = Error UserNotLoggedIn
  | otherwise                        = UserLoggedOut user




apply :: Output -> Store -> Store
apply PetAdded { pet } store                = store { storedPets = pet : storedPets store }
apply PetRemoved { pet } store              = store { storedPets = List.delete pet $ storedPets store }
apply UserLoggedIn { user} store
  | not (user `member` baskets store)       = store { baskets = Map.insert user [] (baskets store) }
  | otherwise                               = store
apply AddedToBasket { user, pet }store      = store { storedPets = List.delete pet $ storedPets store
                                                    , baskets = Map.adjust (pet:) user $ baskets store }
apply RemovedFromBasket { user, pet } store = store { storedPets = pet : storedPets store
                                                    , baskets = Map.adjust (List.delete pet) user $ baskets store }
apply CheckedOutBasket { user } store       = store { baskets = Map.adjust (const []) user $ baskets store }
apply UserLoggedOut { user } store          = store { storedPets = putbackUserBasket (storedPets store)
                                                    , baskets = Map.delete user $ baskets store }
  where
    putbackUserBasket pets =
      case Map.lookup user (baskets store) of
        Nothing -> pets
        Just ps -> pets <> ps

apply UserBasket{} store                    = store
apply Pets{} store                          = store
apply Error{} store                         = store
