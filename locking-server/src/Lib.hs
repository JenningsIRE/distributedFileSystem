{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           API
import           ServerAPI

import qualified Data.ByteString.Char8        as BS
import           System.Process
import           System.IO

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  let settings = setPort 8040 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy LockAPI
api = Proxy

deriving instance FromBSON Bool
deriving instance ToBSON   Bool

data Lock = Lock { path :: String
                 , state :: Bool
                 , owner :: String
                 }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

server :: Server LockAPI
server = lockFile
    :<|> unlockFile
  where
    lockFile :: Message -> Handler Bool
    lockFile msg@(Message path name) = liftIO $ do
      withMongoDbConnection $ do
        docs <- find (select ["path" =: path] "LOCK_RECORD") >>= drainCursor
        let lock = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case lock of
          ((Lock _ True _):_) -> liftIO $ do
            warnLog "is locked"
            return False -- islocked
          ((Lock _ False _):_) -> liftIO $ do
              warnLog "file sucessfully locked"
              withMongoDbConnection $ upsert (select ["path" =: path] "LOCK_RECORD") $ toBSON $ (Lock path True name)
              return True -- file sucessfully locked
          [] -> liftIO $ do -- file does not exist
              warnLog "file does not exist"
              withMongoDbConnection $ upsert (select ["path" =: path] "LOCK_RECORD") $ toBSON $ (Lock path True name)
              return True

    unlockFile :: Message -> Handler Bool
    unlockFile msg@(Message path name) = liftIO $ do
      withMongoDbConnection $ do
        docs <- find (select ["path" =: path] "LOCK_RECORD") >>= drainCursor
        let lock = catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        case lock of
          ((Lock _ True owner):_) -> liftIO $ do
            case (name == owner) of
              True -> do
                warnLog "true"
                withMongoDbConnection $ upsert (select ["path" =: path] "LOCK_RECORD") $ toBSON $ (Lock path False name)
                return True
              False -> do
                warnLog $ "Hey! " ++ name ++ "doesn't own " ++ path
                return False
          ((Lock _ False _):_) -> liftIO $ do
            warnLog "file is already unlocked"
            return True -- file is already unlocked
          [] -> liftIO $ do
            warnLog "file does not exist"
            return False -- file does not exist

-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
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

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def