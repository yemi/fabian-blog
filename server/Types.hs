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

import Opaleye (Column(), PGText, PGInt4, PGTimestamptz)

import Servant
import Servant.HTML.Blaze
import Servant.Server.Internal

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

type AppAPI = Get '[HTML] Html
         :<|> "admin" :> Get '[HTML] Html
         :<|> "api" :> "posts" :> PostAPI
         :<|> "api" :> "users" :> UserAPI
         :<|> "static" :> Raw

type PostAPI = Get '[JSON] [BlogPost]
          :<|> Authorized (ReqBody '[JSON] NewBlogPost :> Post '[JSON] BlogPost)

type UserAPI = Authorized (ReqBody '[JSON] NewUser :> Post '[JSON] User)
          :<|> "login" :> ReqBody '[JSON] LoginReq :> Post '[JSON] Text
          :<|> Authorized ("logout" :> Post '[JSON] ())

data NewBlogPost = NewBlogPost
  { nbpTitle :: Text
  , nbpBody :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON NewBlogPost
instance FromJSON NewBlogPost

data BlogPost' a b c d e f = BlogPost
  { bpId :: a
  , bpTitle :: b
  , bpSlug :: c
  , bpBody :: d
  , bpCreatedBy :: e
  , bpCreatedOn :: f
  } deriving (Show, Eq, Generic)

type BlogPost = BlogPost' Int Text Text Text Int UTCTime
type BlogPostSetColumn = BlogPost' (Maybe (Column PGInt4))
                                   (Column PGText)
                                   (Column PGText)
                                   (Column PGText)
                                   (Column PGInt4)
                                   (Maybe (Column PGTimestamptz))

type BlogPostGetColumn = BlogPost' (Column PGInt4)
                                   (Column PGText)
                                   (Column PGText)
                                   (Column PGText)
                                   (Column PGInt4)
                                   (Column PGTimestamptz)

instance ToJSON BlogPost
instance FromJSON BlogPost

data NewUser = NewUser
  { nuUsername :: Text
  , nuPassword :: Text
  , nuEmail :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON NewUser
instance FromJSON NewUser

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

data LoginReq = LoginReq
  { username :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON LoginReq
instance ToJSON LoginReq

type Authorized t = Header "Authorization" Text :> t

type Slug = String
