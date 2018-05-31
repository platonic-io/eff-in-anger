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
  it "" $ do
    pending
