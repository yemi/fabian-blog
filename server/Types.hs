{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime())

import Database.MongoDB (Document, (=:))

import Opaleye (Column(), PGText, PGInt4, PGTimestamptz)

import DB.Utils

import Servant
import Servant.HTML.Blaze
import Servant.Server.Internal

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

type API = "api" :> "posts" :> Get '[JSON] [BlogPost] :<|>
           "api" :> "post" :> Authorized (ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost) :<|>
           "api" :> "post" :> Capture "slug" String :> Get '[JSON] BlogPost :<|>
           "api" :> "user" :> Authorized (ReqBody '[JSON] User :> Post '[JSON] User) :<|>
           "api" :> "user" :> "login" :> ReqBody '[JSON] LoginReq :> Post '[JSON] LoginToken :<|>
           "api" :> "user" :> Authorized ("logout" :> Post '[JSON] ()) :<|>
           "static" :> Raw :<|>
           "admin" :> Get '[HTML] Html :<|>
           Get '[HTML] Html

data BlogPost' a b c d e = BlogPost
  { bpId :: a
  , bpTitle :: b
  , bpSlug :: c
  , bpBody :: d
  , bpCreatedOn :: e
  } deriving (Show, Eq, Generic)
  
type BlogPost = BlogPost' Int Text Text Text UTCTime
type BlogPostSetColumn = BlogPost' (Maybe (Column PGInt4)) 
                                   (Column PGText) 
                                   (Column PGText) 
                                   (Column PGText) 
                                   (Column PGTimestamptz)
                                   
type BlogPostGetColumn = BlogPost' (Column PGInt4) 
                                   (Column PGText) 
                                   (Column PGText) 
                                   (Column PGText) 
                                   (Column PGTimestamptz)

instance ToJSON BlogPost
instance FromJSON BlogPost

data User' a b c d = User 
  { uId :: a
  , uUsername :: b
  , uPassword :: c
  , uEmail :: d
  } deriving (Show, Eq, Generic)

type User = User' Int Text Text Text
type UserSetColumn = User' (Maybe (Column PGInt4)) 
                           (Column PGText) 
                           (Column PGText) 
                           (Column PGText)
                           
type UserGetColumn = User' (Column PGInt4) 
                           (Column PGText) 
                           (Column PGText) 
                           (Column PGText)

instance ToJSON User
instance FromJSON User

data LoginToken' a b = LoginToken 
  { ltTokenKey :: a 
  , ltCreatedOn :: b
  } deriving (Show, Eq, Generic)

type LoginToken = LoginToken' Text UTCTime 
type LoginTokenColumn = LoginToken' (Column PGText) (Column PGTimestamptz)

instance ToJSON LoginToken
instance FromJSON LoginToken
instance ToText LoginToken
instance FromText LoginToken

data LoginReq = LoginReq
  { username :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON LoginReq
instance ToJSON LoginReq

type Authorized t = Header "Authorization" LoginToken :> t

type Slug = String

documentToUser :: Document -> User
documentToUser doc = do
  let uUsername = getText "username" doc
  let uPassword = getText "password" doc
  User 12 uUsername uPassword "test@te.se"

--loginTokenToDocument :: LoginToken -> Document
--loginTokenToDocument (LoginToken token) = ["token" =: token]

