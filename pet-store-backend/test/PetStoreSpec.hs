{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PetStoreSpec where

import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import           Control.Monad.Freer     (Eff, Member, interpret, runM, send)
import qualified Data.Map                as Map
import           PetStore.Server
import           Test.Hspec

spec :: Spec
spec = describe "pet store tests" $ do
  it "add pet" $ do
    store <- newMVar $ Store Map.empty
    actual <- runM $ runStorage store $ runBusinessLogic  $ send $ AddPet $ Pet "test-pet"
    actual `shouldBe` "got: test-pet"
