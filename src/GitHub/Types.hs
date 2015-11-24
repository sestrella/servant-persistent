{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module GitHub.Types where

import Control.Monad.Reader
import Control.Monad.Trans.Either
import Database.Persist.Sql
import GitHub.Organization.Types
import Servant

type GitHubAPI = "organizations" :> OrganizationAPI

type GitHubT = ReaderT ConnectionPool (EitherT ServantErr IO)

data Config = Config { connectionPool :: ConnectionPool }
