{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API where

import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)

import qualified Crypto.Hash.SHA256 as SHA256

import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (isJust)
import Data.Functor (($>))

import Servant hiding (JSON)

import Web.JWT (Algorithm(HS256), encodeSigned, secret, def, stringOrURI,
                intDate, iss, sub, iat)

import Types
import DB

getBlogPosts :: EitherT ServantErr IO [BlogPost]
getBlogPosts = liftIO $ queryBlogPosts

createBlogPost :: Maybe JWT -> BlogPost -> EitherT ServantErr IO BlogPost
createBlogPost mt blogPost = do
  checkAuth mt
  liftIO $ insertBlogPost blogPost
  return blogPost

getBlogPost :: Slug -> EitherT ServantErr IO BlogPost
getBlogPost slug = do
  blogPost <- liftIO $ queryBlogPost slug
  case blogPost of
    Just blogPost' -> return blogPost'
    Nothing -> left $ err503 { errBody = "No blog posts found with the provided slug." }

createUser :: Maybe JWT -> User -> EitherT ServantErr IO User
createUser mt User {..} = do
  checkAuth mt
  let uPassword' = decodeLatin1 . SHA256.hash . encodeUtf8 $ uPassword
  let user = User uId uUsername uPassword' uEmail
  liftIO $ insertUser user
  return user

logIn :: LoginReq -> EitherT ServantErr IO Text
logIn LoginReq {..} = do
  user <- liftIO $ queryUserByUsername username
  case user of
    Just (User {..}) -> do
      let password' = decodeLatin1 . SHA256.hash . encodeUtf8 $ password
      if password' == uPassword then do
        now <- liftIO getPOSIXTime
        let claimsSet = def { iss = stringOrURI "Fabe"
                            , sub = stringOrURI uUsername
                            , iat = intDate now
                            }
        let key = secret "Fabianz secret"
        return $ encodeSigned HS256 key claimsSet
      else left $ ServantErr 400 "Username/password pair did not match" "" []
    Nothing -> left $ ServantErr 400 "User not found" "" []

logOut :: Maybe JWT -> EitherT ServantErr IO ()
logOut mt = do
  checkAuth mt
  maybe (return ()) (\_ -> return ()) mt

checkAuth :: Maybe JWT -> EitherT ServantErr IO ()
checkAuth = maybe unauthorized runCheck
  where
  runCheck token = do
    unless False unauthorized
  unauthorized =
    left $ ServantErr 401 "You are not authenticated. Please sign-in" "" []

api :: Proxy AppAPI
api = Proxy
