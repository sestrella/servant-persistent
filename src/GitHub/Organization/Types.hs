{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module GitHub.Organization.Types where

import Data.Aeson
import GHC.Generics
import Servant

type OrganizationAPI = Post '[JSON] Organization
                  :<|> Capture "id" Int :> Get '[JSON] Organization

data Organization = Organization
                    deriving Generic

instance ToJSON Organization
