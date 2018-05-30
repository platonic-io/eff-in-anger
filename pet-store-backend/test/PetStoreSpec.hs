{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PetStoreSpec where

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar)
import           Control.Monad.Freer       (Eff, Member, interpret, runM, send)
import           Control.Monad.Freer.Error (runError)
import qualified Data.Map                  as Map
import           PetStore.Server
import           Test.Hspec
import           Servant

spec :: Spec
spec = describe "pet store tests" $ do
  it "add pet" $ do
    store <- newMVar $ Store Map.empty
    let testPet = Pet "test-pet"
    actual :: Either ServantErr [Pet] <- runM $ runError $ runStorage store $ runBusinessLogic $ send $ AddPet $ testPet
    actual `shouldBe` (Right [testPet])
