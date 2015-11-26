{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module GitHub
    ( startApp
    ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Organization
  name String
  deriving Generic

Repository
  name String
  organizationId OrganizationId
  deriving Generic
|]

instance ToJSON Organization
instance ToJSON Repository

instance FromJSON Organization
instance FromJSON Repository

instance FromText (Key Organization) where
  fromText = Just . OrganizationKey . SqlBackendKey . Prelude.read . show

instance FromText (Key Repository) where
  fromText = Just . RepositoryKey . SqlBackendKey . Prelude.read . show

type Create a  = ReqBody '[JSON] a :> Post '[JSON] a
type Read a    = Capture "id" (Key a) :> Get '[JSON] a
type ReadAll a = Get '[JSON] [a]
type Update a  = Capture "id" (Key a) :> ReqBody '[JSON] a :> Put '[JSON] ()
type Delete a  = Capture "id" (Key a) :> Servant.Delete '[JSON] ()

type Crud a = Create a
         :<|> GitHub.Read a
         :<|> ReadAll a
         :<|> GitHub.Update a
         :<|> GitHub.Delete a

type GitHub = "organizations" :> Crud Organization
         :<|> "repositories" :> Crud Repository
type GitHubT = ReaderT ConnectionPool (EitherT ServantErr IO)

class (PersistEntity a, SqlBackend ~ PersistEntityBackend a) => HasCrud a where
  create :: a -> GitHubT a
  create organization = do
    entity <- runDB $ insertEntity organization
    return $ entityVal entity

  read :: Key a -> GitHubT a
  read entityId = do
    maybeEntity <- runDB $ get entityId
    case maybeEntity of
      Nothing     -> lift $ left err404
      Just entity -> return entity

  readAll :: GitHubT [a]
  readAll = do
    entities <- runDB $ selectList [] []
    return $ map entityVal entities

  update :: Key a -> a -> GitHubT ()
  update entityId = runDB . replace entityId

  delete :: Key a -> GitHubT ()
  delete = runDB . Database.Persist.Sqlite.delete

instance HasCrud Organization
instance HasCrud Repository

startApp :: IO ()
startApp = do
  pool <- createPool
  runSqlPool (runMigration migrateAll) pool
  run 8080 $ app pool

createPool :: IO ConnectionPool
createPool = runStdoutLoggingT $ createSqlitePool "db" 1

app :: ConnectionPool -> Application
app = serve gitHub . readServer

gitHub :: Proxy GitHub
gitHub = Proxy

readServer :: ConnectionPool -> Server GitHub
readServer pool = enter (runReaderTNat pool) server

server :: ServerT GitHub GitHubT
server = crud :<|> crud

crud :: HasCrud a => (a -> GitHubT a)
                :<|> (Key a -> GitHubT a)
                :<|> GitHubT [a]
                :<|> (Key a -> a -> GitHubT ())
                :<|> (Key a -> GitHubT ())
crud = create
  :<|> GitHub.read
  :<|> readAll
  :<|> GitHub.update
  :<|> GitHub.delete

runDB :: SqlPersistT IO a -> GitHubT a
runDB query = ask >>= liftIO . runSqlPool query
