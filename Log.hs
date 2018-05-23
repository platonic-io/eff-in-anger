{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Log where

import           Color
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Data.Aeson
import           Data.Bifunctor           (bimap)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy           as Text
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO        as TextIO
import           System.IO

-- |Entries that can be fed to a `LogQueue`
data LogEntry = LogEntry Message
              -- ^A log message originating from some service
              | EndOfLog
              -- ^A /Poison Pill/  to stop the logging thread
  deriving (Eq, Show)

logEntry :: Text.Text -> Color -> LBS.ByteString -> LogEntry
logEntry o col c = LogEntry $ Message o col c

data Message = Message { origin :: Text.Text
                       , color  :: Color
                       , msg    :: LBS.ByteString
                       }
             deriving (Eq, Show)

type LogQueue  = Chan LogEntry

safeDecodeUtf8 :: LBS.ByteString -> Text.Text
safeDecodeUtf8 = decodeUtf8With lenientDecode

tailLogs :: LogQueue
         -> Handle
         -> IO (Async ())

tailLogs logSource logSink = do
  let doLog = do
        entry <- readChan logSource
        case entry of
          LogEntry out -> do
            let
              jsonMsg :: Either Text.Text Value
              jsonMsg = jsonFromText $ msg out

              fullMsg = either
                        (const $ object ["node" .= origin out, "log" .= object [ "message" .= (safeDecodeUtf8 $ msg out :: Text.Text)]])
                        (\ m ->  object ["node" .= origin out, "log" .= m ])
                        jsonMsg

              msgString = colorise (color out) $ jsonToText fullMsg
            TextIO.hPutStrLn logSink msgString
            doLog

          EndOfLog ->
            pure ()

  async doLog


jsonFromText :: (FromJSON a) => LBS.ByteString -> Either Text.Text a
jsonFromText = bimap Text.pack id . eitherDecode

jsonToText :: (ToJSON a) => a -> Text.Text
jsonToText = decodeUtf8 . encode
