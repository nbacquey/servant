{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Servant.Server.Internal.Auth where

import           Control.Monad.Trans.Except         (ExceptT)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)

import           Servant.Server.Internal.ServantErr (ServantErr)

-- * General Auth

-- | Specify the type of data returned after we've authenticated a request.
-- quite often this is some `User` datatype.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
type family AuthServerData a :: *

-- | Handlers for AuthProtected resources
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
newtype AuthHandler r usr = AuthHandler
  { unAuthHandler :: r -> ExceptT ServantErr IO usr }
  deriving (Generic, Typeable)

-- | NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthHandler :: (r -> ExceptT ServantErr IO usr) -> AuthHandler r usr
mkAuthHandler = AuthHandler
