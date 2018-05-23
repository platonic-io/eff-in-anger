{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Process where


import           Color
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Exception        (IOException, catch, finally, throw,
                                           throwIO, try)
import           Control.Monad            (forever, void)
import           Data.Aeson
import qualified Data.ByteString.Char8    as BS8
import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid
import qualified Data.Text.Lazy           as Text
import           Log
import           System.Exit
import           System.IO
import           System.IO.Error          (isEOFError)
import           System.Process


runProc :: FilePath -> [ String ] -> FilePath -> IO ()
runProc fp arg dir = do
  putStrLn $ "> " <> fp <> " " <> unwords arg
  res <- createProcess ((proc fp arg) { cwd = Just dir }) >>= \ ( _,_,_, h) -> waitForProcess h
  case res of
    ExitSuccess       -> pure ()
    e@(ExitFailure _) -> exitWith e


spawnProc :: Color -> LogQueue -> FilePath -> [ String ] -> FilePath -> IO [ Async () ]
spawnProc col queue fp arg dir = do
  let name = Text.pack fp

      readOutput :: Handle -> IO (Async ())
      readOutput hd = async $
        forever ((do
                    l <- LBS.fromStrict <$> BS8.hGetLine hd
                    writeChan queue (logEntry name col l))
                 `catch` (\ e  ->
                             if isEOFError e
                             then throw e
                             else writeChan queue (logEntry name col $
                                                   encode $
                                                   object [ "process" .=  name
                                                          , "error" .= show e ])))

  procStart <- try (createProcess ((proc fp arg) { cwd = Just dir, std_in = Inherit, std_out = CreatePipe, std_err = CreatePipe }))

  case procStart of
    Right (_, Just aout, Just aerr, aProc) -> do
      outtid <- readOutput aout
      errtid <- readOutput aerr
      writeChan queue $ logEntry "driver" (Color 200 10 10) (encode $ object [ "message" .= Text.pack ("starting " <> fp <> " " <> unwords arg) ])

      p <- async $ void (waitForProcess aProc) `finally` terminateProcess aProc
      return $ [p, outtid, errtid]
    Left (e :: IOException) -> throwIO $ userError $ "exception launching process " <> fp <> ": " <> show e
    _      -> throwIO $ userError $ "unexpected result from launching process " <> fp
