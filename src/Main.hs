{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative        (optional, (<*>))
import           Data.Bool                  (Bool)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.CaseInsensitive       (original)
import           Data.Foldable              (fold)
import           Data.Function              (($), (.))
import           Data.Functor               ((<$>))
import           Data.Int                   (Int)
import           Data.Maybe                 (Maybe (Just, Nothing))
import           Data.Monoid                (mempty, (<>))
import           Data.String                (String)
import           Network.HTTP.Types.Header  (Header)
import           Network.HTTP.Types.Status  (Status, mkStatus, status200,
                                             status400, status401, status402,
                                             status403, status404, status405,
                                             status406, status407, status408,
                                             status409, status410, status411,
                                             status412, status413, status414,
                                             status415, status416, status417,
                                             status422, status428, status429,
                                             status431, status500, status501,
                                             status502, status503, status504,
                                             status505, status511)
import           Network.Wai                (Request, rawPathInfo,
                                             rawQueryString, requestBody,
                                             requestHeaders, responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           Options.Applicative        (Parser, auto, execParser, fullDesc,
                                             header, help, helper, info, long,
                                             option, progDesc, strOption,
                                             switch, value, (<**>))
import           Prelude                    ()
import           System.Exit                (ExitCode (..))
import           System.IO                  (IO, hClose, stderr)
import           System.Process             (CreateProcess,
                                             StdStream (CreatePipe),
                                             createProcess, proc, std_err,
                                             std_in, std_out, waitForProcess)

data Settings = Settings {
      settingsPort    :: Int
    , settingsHeaders :: Bool
    , settingsHandler :: Maybe String
  }

settingsParser :: Parser Settings
settingsParser =
  Settings
      <$> option auto (long "listen-port" <> help "Which port to listen on" <> value 8080)
      <*> switch (long "with-headers" <> help "Whether to prepend request headers")
      <*> optional (strOption $ long "handler" <> help "Pass the request content to this program's standard input")

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

exitCodeToHttp :: ExitCode -> Status
exitCodeToHttp ExitSuccess      = status200
exitCodeToHttp (ExitFailure 1)  = status401
exitCodeToHttp (ExitFailure 2)  = status402
exitCodeToHttp (ExitFailure 3)  = status403
exitCodeToHttp (ExitFailure 4)  = status404
exitCodeToHttp (ExitFailure 5)  = status405
exitCodeToHttp (ExitFailure 6)  = status406
exitCodeToHttp (ExitFailure 7)  = status407
exitCodeToHttp (ExitFailure 8)  = status408
exitCodeToHttp (ExitFailure 9)  = status409
exitCodeToHttp (ExitFailure 10) = status410
exitCodeToHttp (ExitFailure 11) = status411
exitCodeToHttp (ExitFailure 12) = status412
exitCodeToHttp (ExitFailure 13) = status413
exitCodeToHttp (ExitFailure 14) = status414
exitCodeToHttp (ExitFailure 15) = status415
exitCodeToHttp (ExitFailure 16) = status416
exitCodeToHttp (ExitFailure 17) = status417
exitCodeToHttp (ExitFailure 22) = status422
exitCodeToHttp (ExitFailure 28) = status428
exitCodeToHttp (ExitFailure 29) = status429
exitCodeToHttp (ExitFailure 31) = status431
exitCodeToHttp (ExitFailure 40) = status400
exitCodeToHttp (ExitFailure 50) = status500
exitCodeToHttp (ExitFailure 51) = status501
exitCodeToHttp (ExitFailure 52) = status502
exitCodeToHttp (ExitFailure 53) = status503
exitCodeToHttp (ExitFailure 54) = status504
exitCodeToHttp (ExitFailure 55) = status505
exitCodeToHttp (ExitFailure 56) = status511
exitCodeToHttp (ExitFailure v)  = mkStatus v mempty

tailSafe :: [a] -> Maybe [a]
tailSafe []     = Nothing
tailSafe (_:xs) = Just xs

main :: IO ()
main = do
  settings <- parseSettings
  run (settingsPort settings) $ \request respond -> do
    body <- requestBody request
    let headers = requestHeadersFlat request
        output = if settingsHeaders settings
                 then headers <> "\n" <> body
                 else body
    case settingsHandler settings of
      Nothing -> do
        BS8.putStrLn output
        respond (responseLBS status200 mempty mempty)
      Just handler -> do
        let pathInfo :: String
            pathInfo = BS8.unpack (rawPathInfo request)
            queryString :: String
            queryString = fold . tailSafe . BS8.unpack . rawQueryString $ request
            processArgs :: [String]
            processArgs = [pathInfo, queryString]
            processParams :: CreateProcess
            processParams = (proc handler processArgs){
              std_out = CreatePipe,
              std_err = CreatePipe,
              std_in = CreatePipe
              }
        (Just hin, Just hout, Just herr, procHandle) <- createProcess processParams
        BS8.hPutStrLn hin output
        hClose hin
        responseBody <- BSL8.hGetContents hout
        errors <- BSL8.hGetContents herr
        BSL8.hPutStr stderr errors
        exitCode <- waitForProcess procHandle
        let responseHeaders = mempty
            exitCodeHttp = exitCodeToHttp exitCode
        respond (responseLBS exitCodeHttp responseHeaders responseBody)

