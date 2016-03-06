{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           GHC.Generics                (Generic)

import           Data.Aeson                  (FromJSON, ToJSON, Value (Object),
                                              object, parseJSON, toJSON, (.:),
                                              (.=))
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime ())

import           Opaleye                     (Column (), PGInt4, PGText,
                                              PGTimestamptz)

import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Internal

import           Text.Blaze.Html5            hiding (object)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes

 -- Misc

type Slug = String


-- API

type AppAPI = Get '[HTML] Html
        :<|> "admin" :> Get '[HTML] Html
        :<|> "api" :> "posts" :> PostAPI
        :<|> "api" :> "users" :> UserAPI
        :<|> "static" :> Raw

type PostAPI = Get '[JSON] [BlogPost]
          :<|> Authorized (ReqBody '[JSON] NewBlogPost :> Post '[JSON] BlogPost)

type UserAPI = Authorized (ReqBody '[JSON] NewUser :> Post '[JSON] User)
          :<|> "login" :> ReqBody '[JSON] LoginReq :> Post '[JSON] LoginRes


-- Blog Post

data NewBlogPost = NewBlogPost
  { nbpTitle :: Text
  , nbpBody  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON NewBlogPost
instance FromJSON NewBlogPost

data BlogPost' a b c d e f = BlogPost
  { bpId        :: a
  , bpTitle     :: b
  , bpSlug      :: c
  , bpBody      :: d
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


-- User

data NewUser = NewUser
  { nuUsername :: Text
  , nuPassword :: Text
  , nuEmail    :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON NewUser
instance FromJSON NewUser

data PublicUser = PublicUser
  { puId       :: Int
  , puUsername :: Text
  , puEmail    :: Text
  } deriving (Show, Eq)

instance ToJSON PublicUser where
  toJSON (PublicUser id username email) =
    object ["id" .= id, "username" .= username, "email" .= email]

instance FromJSON PublicUser where
  parseJSON (Object v) = PublicUser
    <$> v .: "id"
    <*> v .: "username"
    <*> v .: "email"
  parseJSON _ = mempty

data User' a b c d = User
  { uId       :: a
  , uUsername :: b
  , uPassword :: c
  , uEmail    :: d
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


--Authorization

data LoginReq = LoginReq
  { username :: Text
  , password :: Text
  } deriving (Generic)

instance FromJSON LoginReq
instance ToJSON LoginReq

data LoginRes = LoginRes
  { jwt  :: Text
  , user :: PublicUser
  } deriving (Show, Eq, Generic)

instance ToJSON LoginRes
instance FromJSON LoginRes

type Authorized t = Header "Authorization" Text :> t

