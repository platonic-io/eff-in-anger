{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           PetStore.Config
import           PetStore.Server
import           System.Environment

main :: IO ()
main = do
  [devMode, port, payHost, payPort ] <- getArgs
  startServer $ ServerConfig (read devMode) (read port) payHost (read payPort)
