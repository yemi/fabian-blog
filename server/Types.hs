{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

import Database.MongoDB (Document, (=:))

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

data BlogPost = BlogPost
  { title :: Text
  , slug :: Text
  , body  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON BlogPost
instance FromJSON BlogPost

data User = User
  { uUsername :: Text
  , uPassword :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

newtype LoginToken = LoginToken Text
  deriving (ToJSON, FromJSON, FromText, ToText, Ord, Eq)

data LoginReq = LoginReq
  { lrUsername :: Text
  , lrPassword :: Text
  } deriving (Generic)

instance FromJSON LoginReq
instance ToJSON LoginReq

type Authorized t = Header "Authorization" LoginToken :> t

blogPostToDocument :: BlogPost -> Document
blogPostToDocument BlogPost {..} =
  ["title" =: title, "slug" =: slug, "body" =: body]

documentToBlogPost :: Document -> BlogPost
documentToBlogPost doc = do
  let title = getText "title" doc
  let slug = getText "slug" doc
  let body = getText "body" doc
  BlogPost title slug body

userToDocument :: User -> Document
userToDocument User {..} =
  ["username" =: uUsername, "password" =: uPassword]

documentToUser :: Document -> User
documentToUser doc = do
  let username = getText "username" doc
  let password = getText "password" doc
  User username password
  
loginTokenToDocument :: LoginToken -> Document
loginTokenToDocument (LoginToken token) = ["token" =: token]