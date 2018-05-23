#!/usr/bin/env stack
-- stack runhaskell --package async --package unix --

{-|
A simple script to build and run all components of the ''One Log'' talk
-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

import           Color
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad            (void)
import           Log
import           Process
import           System.Environment       (getArgs)
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run"]   -> doRun
    ["build"] -> doBuild
    _         -> putStrLn "expecting one of 'run' or 'build'"


doBuild :: IO ()
doBuild = do
  runProc "mvn" [ "install" ] "pet-store-payment"
  runProc "stack" [ "test",  "--fast" ]  "."
  runProc "stack" [ "install",  "--fast" ]  "."


doRun :: IO ()
doRun = do
  q  <- newChan
  t  <- tailLogs q stdout

  let procs = [ ( "java",  [ "-jar" , "./pet-store-payment/target/pet-store-payment-1.0-SNAPSHOT.jar", "server", "payment-conf.yaml" ],  ".")
              , ( "pet-store-server", [ "Dev" , "9090", "localhost", "8080" ], ".")
              ]

  ps <- concat <$> mapM (\ ((name, args, dir), clr) -> spawnProc clr q name args dir) (zip procs genColors)

  void $ waitAnyCancel (t:ps)
