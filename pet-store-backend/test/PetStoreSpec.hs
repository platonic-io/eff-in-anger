{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
  let testPet = Pet "test-pet"

  it "add pet to MVar store" $ do
    store <- newMVar $ Store Map.empty

    actual :: Either ServantErr [Pet] <- runM $ runError $ runStorage store $ runBusinessLogic $ send $ AddPet $ testPet
    actual `shouldBe` (Right [testPet])

  it "add pet with store failure" $ do
    let
      runFailingStore :: Eff (StorageEffect : m) x -> Eff m x
      runFailingStore = interpret $ \(AddDocument _ _) -> pure $ Left  "store failed"

    actual :: Either ServantErr [Pet] <- runM $ runError $ runFailingStore $ runBusinessLogic $ send $ AddPet $ testPet
    actual `shouldBe` (Left err500)


