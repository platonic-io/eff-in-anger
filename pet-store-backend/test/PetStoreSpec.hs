{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module PetStoreSpec where

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar)
import           Control.Monad.Freer       (Eff, Member, interpret, runM, send)
import           Control.Monad.Freer.Error (runError, Error)
import qualified Data.Map                  as Map
import           PetStore.Server
import           Servant
import           Test.Hspec

runFailingDatabase :: Eff (DatabaseEffect : m) x -> Eff m x
runFailingDatabase = interpret eval where
  eval :: DatabaseEffect a -> Eff n a
  eval ListDocuments = pure $ Left "db failed"
  eval (AddDocument document) = pure ()

effToIOWithFailingDb :: Eff '[PetStoreEffect, DatabaseEffect, Error ServantErr, IO] x -> IO ( Either ServantErr x )
effToIOWithFailingDb = runM . runError . runFailingDatabase . runPetStore

spec :: Spec
spec = describe "pet store tests" $ do
  let
    testPet1 = Pet "test-pet-1"
    testPet2 = Pet "test-pet-2"
  it "test addPet" $ do
    dbMVar <- newMVar $ Database []
    res <- effToIO dbMVar $ do
      addPet testPet1
      addPet testPet2
    res `shouldBe` Right [testPet2, testPet1]

  it "test addPet with failing database" $ do
    res <- effToIOWithFailingDb $ do
      addPet testPet1
    res `shouldBe` Left err500
