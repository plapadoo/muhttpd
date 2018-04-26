{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative       ((<*>))
import           Control.Monad             (when)
import           Data.Bool                 (Bool)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import           Data.CaseInsensitive      (original)
import           Data.Function             (($), (.))
import           Data.Functor              ((<$>))
import           Data.Int                  (Int)
import           Data.Monoid               (mempty, (<>))
import           Data.String               (String)
import           Network.HTTP.Types.Header (Header)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai               (Request, requestBody,
                                            requestHeaders, responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           Options.Applicative       (Parser, auto, execParser, fullDesc,
                                            header, help, helper, info, long,
                                            option, progDesc, switch, value,
                                            (<**>))
import           Prelude                   (undefined)
import           System.IO                 (IO)

data Settings = Settings {
      settingsPort    :: Int
    , settingsHeaders :: Bool
  }

settingsParser :: Parser Settings
settingsParser =
  Settings
      <$> option auto
          ( long "listen-port"
         <> help "Which port to listen on"
         <> value 8080)
      <*>
          switch
          ( long "with-headers"
         <> help "Whether to prepend request headers")

description :: String
description = "Listen on a port, output to standard output or call a handler"

parseSettings :: IO Settings
parseSettings = execParser opts
  where opts = info (settingsParser <**> helper)
         ( fullDesc
         <> progDesc description
         <> header "muhttpd - Unix-philosophy httpd" )

requestHeadersFlat :: Request -> BS.ByteString
requestHeadersFlat = BS8.unlines . (headerToBS <$>) . requestHeaders
  where headerToBS :: Header -> BS.ByteString
        headerToBS (name, v) = original name <> ": " <> v

main :: IO ()
main = do
  settings <- parseSettings
  run (settingsPort settings) $ \request respond -> do
    body <- requestBody request
    when (settingsHeaders settings) (BS8.putStrLn (requestHeadersFlat request))
    BS8.putStrLn body
    respond (responseLBS status200 mempty mempty)

