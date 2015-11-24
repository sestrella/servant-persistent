{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module GitHub
    ( startApp
    ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Database.Persist.Sql
import Database.Persist.Sqlite
import GitHub.Organization.API
import GitHub.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = do
  config <- getConfig
  run 8080 $ app config

getConfig :: IO Config
getConfig = Config <$> getConnectionPool

getConnectionPool :: IO ConnectionPool
getConnectionPool = runStdoutLoggingT $ createSqlitePool "github" 1

app :: Config -> Application
app = serve gitHubAPI . readServer

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

readServer :: Config -> Server GitHubAPI
readServer = undefined

readerToEither :: Config -> GitHubT :~> EitherT ServantErr IO
readerToEither Config{..} = Nat $ \x -> runReaderT x connectionPool

server :: ServerT GitHubAPI GitHubT
server = organizationAPI
