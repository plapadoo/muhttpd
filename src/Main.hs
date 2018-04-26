{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import           Data.Function             (($))
import           Data.Functor              ((<$>))
import           Data.Int                  (Int)
import           Data.Monoid               (mempty, (<>))
import           Data.String               (String)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai               (requestBody, responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           Options.Applicative       (Parser, auto, execParser, fullDesc,
                                            header, help, helper, info, long,
                                            option, progDesc, value, (<**>))
import           Prelude                   (undefined)
import           System.IO                 (IO)

data Settings = Settings {
    settingsPort :: Int
  }

settingsParser :: Parser Settings
settingsParser =
  Settings
      <$> option auto
          ( long "listen-port"
         <> help "Which port to listen on"
         <> value 8080)

description :: String
description = "Listen on a port, output to standard output or call a handler"

parseSettings :: IO Settings
parseSettings = execParser opts
  where opts = info (settingsParser <**> helper)
         ( fullDesc
         <> progDesc description
         <> header "muhttpd - Unix-philosophy httpd" )

main :: IO ()
main = do
  settings <- parseSettings
  run (settingsPort settings) $ \request respond -> do
    body <- requestBody request
    BS8.putStrLn body
    respond (responseLBS status200 mempty mempty)

