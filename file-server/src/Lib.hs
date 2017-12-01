{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


startApp :: IO ()
startApp = do
  print "starting..."
  run 8080 app

app :: Application
app = serve api server2

api :: Proxy API
api = Proxy

type API = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

isaac :: User
isaac = User 1 "Isaac" "Newton"

albert :: User
albert = User 2 "Albert" "Einstein"

users :: [User]
users = [isaac, albert]

server2 :: Server API
server2 = return users
     :<|> return albert
     :<|> return isaac