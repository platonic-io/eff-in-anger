{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PetStore.Server where

import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar)
import           Control.Monad.Except      (ExceptT (..), join)
import           Control.Monad.Freer       (Eff, Member, interpret, runM, send)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Data.Aeson                (FromJSON, ToJSON, eitherDecode,
                                            encode)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           GHC.Generics              (Generic)
import           Network.Wai.Handler.Warp  (run)
import           Servant                   hiding (throwError)

-- decodePets :: Either String [ByteString] -> Either String [Pet]
-- decodePets = join . fmap (traverse eitherDecode)

