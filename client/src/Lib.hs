{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( someFunc
    ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (join, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Proxy
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.Time.Clock              --(UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           Data.Semigroup                     ((<>))
import           Distribution.PackageDescription.TH
import           Git.Embed
import           GHC.Generics
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Options.Applicative
import           Servant
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           API
import           ClientAPI

import           System.Process
import           System.IO

data CacheObject = CacheObject  { cacheName        :: String
                      , cacheContent               :: String
                      , cacheTime                  :: String
                      }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

class PrintResponse a where
  resp :: Show a => a -> String

instance PrintResponse ResponseData where
  resp r = "Response is a single value: " ++ response r

instance PrintResponse [Message] where
  resp [] = "No messages."
  resp [x] = "Response is a single message: " ++ message x
  resp rs = "Response is an array with messages: " ++ (DL.intercalate ", " $ map message rs)

instance PrintResponse [ResponseData] where
  resp rs = "Response is an array with values: " ++ (DL.intercalate ", " $ map response rs)

instance PrintResponse Bool where
  resp True =  "Response is a boolean : Totally!"
  resp False = "Response is a boolean : Like No Way!"

doReportCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< envs h p)

doCall f h p = SC.runClientM f =<< envs h p

upload :: String -> String -> Maybe String -> Maybe String -> IO ()
upload fileName dirName h dirPort = do
  let filePath = dirName ++ fileName
  lock <- doCall (lockFile $ Message filePath "name" ) h (Just (show 8040))
  case lock of
    Right True -> do
      address <- doCall (postDirectory $ Message fileName dirName) h dirPort
      case address of
        Left err -> putStrLn "Something went wrong"
        Right resp -> case resp of
            [] -> putStrLn $ "No such directory: " ++ dirName
            [x@(Message fsIP fsPort)] -> do
              contents <- readFile fileName
              doReportCall (postFile $ Message filePath contents) (Just fsIP) (Just fsPort)
              doReportCall (unlockFile $ Message filePath "name") h (Just (show 8040))
    otherwise -> putStrLn "locked"

download :: String -> String -> Maybe String -> Maybe String -> IO ()
download fileName dirName h dirPort = do
  let filePath = dirName ++ fileName
  address <- doCall (getDirectory $ Message fileName dirName) h dirPort
  case address of
    Left err -> putStrLn "Something went wrong"
    Right response -> case response of
        [] -> putStrLn $ "No such directory: " ++ dirName
        [x@(DirectoryResponse _ fsIP fsPort time)] -> do
          checkCache <- withMongoDbConnection $ find (select ["cacheName" =: filePath] "CLIENT_CACHE") >>= drainCursor
          let cache = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe CacheObject) checkCache
          case cache of
            [] -> do
              file <- doCall (getFile (Just filePath)) (Just fsIP) (Just fsPort)
              case file of
                Left err -> putStrLn "error retrieving file..."
                Right (Message _ text:_) -> do
                  putStrLn "downloaded new file"
                  let cacheRef = (CacheObject filePath text time)
                  withMongoDbConnection $ upsert (select ["cacheName" =: filePath] "CLIENT_CACHE") $ toBSON cacheRef
                  writeFile fileName text
            ((CacheObject cacheName cacheText cacheTime):_) -> do
              let diff = realToFrac (diffUTCTime (read time) (read cacheTime)) :: Double
              let checkTimeStamp = realToFrac diff > 0
              case checkTimeStamp of
                True -> do
                  file <- doCall (getFile (Just filePath)) (Just fsIP) (Just fsPort)
                  case file of
                    Left err -> putStrLn "error retrieving file..."
                    Right (Message _ text:_) -> do
                      putStrLn "downloaded new file"
                      let cacheRef = (CacheObject filePath text time)
                      withMongoDbConnection $ upsert (select ["cacheName" =: filePath] "CLIENT_CACHE") $ toBSON cacheRef
                      writeFile fileName text
                otherwise -> do
                  putStrLn "downloading from cache"
                  writeFile fileName cacheText

someFunc :: IO ()
someFunc = do
  upload "test.txt" "server" (Just "localhost") (Just "8000")
  download "test.txt" "server" (Just "localhost") (Just "8000")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")

envs :: Maybe String -> Maybe String -> IO SC.ClientEnv
envs host port = SC.ClientEnv <$> newManager defaultManagerSettings
                             <*> (SC.BaseUrl <$> pure SC.Http
                                             <*> (host <?> usehaskellHost)
                                             <*> (read <$> (port <?> usehaskellPort))
                                             <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   -- | The url endpoint for contactingt the use-haskell service
   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   -- | The neo4j port
   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  pipe <- connect (host "127.0.0.1")
  ret <- runResourceT $ liftIO $ access pipe master (pack "USEHASKELLDB") act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

