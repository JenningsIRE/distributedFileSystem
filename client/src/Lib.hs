{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad                      (join, when)
import           Data.List
import           Data.Proxy
import           Data.Semigroup                     ((<>))
import           Distribution.PackageDescription.TH
import           Git.Embed
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Options.Applicative
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
--import           UseHaskellAPI
--import           UseHaskellAPIClient
import           API
import           ClientAPI

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
  resp rs = "Response is an array with messages: " ++ (intercalate ", " $ map message rs)

instance PrintResponse [ResponseData] where
  resp rs = "Response is an array with values: " ++ (intercalate ", " $ map response rs)

instance PrintResponse Bool where
  resp True =  "Response is a boolean : Totally!"
  resp False = "Response is a boolean : Like No Way!"

doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)

doPostFile :: String -> String -> Maybe String -> Maybe String -> IO ()
doPostFile n m  = doCall $ postFile $ Message n m

doGetFile :: String -> Maybe String -> Maybe String -> IO ()
doGetFile s  = doCall $ getFile $ Just s

--doStoreMessage :: String -> String -> Maybe String -> Maybe String -> IO ()
--doStoreMessage n m  = doCall $ storeMessage $ Message n m

--doSearchMessage :: String -> Maybe String -> Maybe String -> IO ()
--doSearchMessage s  = doCall $ searchMessage $ Just s


someFunc :: IO ()
someFunc = do
  doPostFile "1" "this is a test" (Just "localhost") (Just "8080")
  doGetFile "1" (Just "localhost") (Just "8080")
  --doStoreMessage "1" "this is a test" (Just "localhost") (Just "8080")
  --doSearchMessage "1" (Just "localhost") (Just "8080")


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

env :: Maybe String -> Maybe String -> IO SC.ClientEnv
env host port = SC.ClientEnv <$> newManager defaultManagerSettings
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

