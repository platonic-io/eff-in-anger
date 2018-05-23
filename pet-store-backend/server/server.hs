{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           PetStore.Config
import           PetStore.Server
import           System.Environment

main :: IO ()
main = do
  [devMode, port, paymentHost, paymentPort ] <- getArgs
  startServer $ ServerConfig (read devMode) (read port) paymentHost (read paymentPort)
