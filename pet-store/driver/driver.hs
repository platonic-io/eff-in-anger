{-# LANGUAGE ViewPatterns #-}

import           PetStore.Driver
import           System.Environment

main :: IO ()
main = do
  [read -> numThreads, serverHost, serverPort] <- getArgs -- to connect to server
  runTestDriver numThreads serverHost (read serverPort)
