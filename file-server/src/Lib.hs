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
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           UseHaskellAPI
import           Lib

-- data Message = Message { name    :: String
--                        , message :: String
--                        } deriving (Generic, FromJSON, ToBSON, FromBSON)

-- deriving instance FromBSON String  -- we need these as BSON does not provide
-- deriving instance ToBSON   String


-- data ResponseData = ResponseData { response :: String
--                                  } deriving (Generic, ToJSON, FromJSON,FromBSON)

type APIS = "postFile"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "getFile"              :> QueryParam "name" String :> Get '[JSON] [Message]

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy APIS
api = Proxy

server :: Server APIS
server = postFile
    :<|> getFile

postFile :: Message -> Handler Bool
postFile msg@(Message key _) = liftIO $ do
  warnLog $ "Storing message under key " ++ key ++ "."

  withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

  return True  -- as this is a simple demo I'm not checking anything

getFile :: Maybe String -> Handler [Message]
getFile (Just key) = liftIO $ do
  warnLog $ "Searching for value for key: " ++ key

  withMongoDbConnection $ do
    docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
    --warnLog $ "retrieved data: " ++ show docs
    return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

getFile Nothing = liftIO $ do
  warnLog  "No key for searching."
  return  ([] :: [Message])



