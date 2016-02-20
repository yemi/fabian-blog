{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API where

import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)

import qualified Crypto.Hash.SHA256 as SHA256

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (isJust)
import Data.Functor (($>))

import Servant hiding (JSON)

import Web.JWT (Algorithm(HS256), JWT, VerifiedJWT, claims, decodeAndVerifySignature, encodeSigned,
                stringOrURIToText, secret, def, stringOrURI, intDate, iss, sub, iat)

import Types
import DB

getBlogPosts :: EitherT ServantErr IO [BlogPost]
getBlogPosts = liftIO $ queryBlogPosts

createBlogPost :: Maybe Text -> NewBlogPost -> EitherT ServantErr IO BlogPost
createBlogPost mjwt blogPost = do
  jwt <- checkAuth mjwt
  let Just userId = fmap (read . unpack . stringOrURIToText) $ sub $ claims jwt
  liftIO $ insertBlogPost userId blogPost

getBlogPost :: Slug -> EitherT ServantErr IO BlogPost
getBlogPost slug = do
  blogPost <- liftIO $ queryBlogPost slug
  case blogPost of
    Just blogPost' -> return blogPost'
    Nothing -> left $ err503 { errBody = "No blog posts found with the provided slug." }

createUser :: Maybe Text -> NewUser -> EitherT ServantErr IO User
createUser jwt NewUser {..} = do
  checkAuth jwt
  let nuPassword' = decodeLatin1 . SHA256.hash . encodeUtf8 $ nuPassword
  liftIO $ insertUser $ NewUser nuUsername nuPassword' nuEmail

logIn :: LoginReq -> EitherT ServantErr IO Text
logIn LoginReq {..} = do
  user <- liftIO $ queryUserByUsername username
  case user of
    Just (User {..}) -> do
      let password' = decodeLatin1 . SHA256.hash . encodeUtf8 $ password
      if password' == uPassword then do
        now <- liftIO getPOSIXTime
        let claimsSet = def { iss = stringOrURI "Fabe"
                            , sub = stringOrURI . pack . show $ uId
                            , iat = intDate now
                            }
        return $ encodeSigned HS256 (secret "Fabianz secret") claimsSet
      else left $ ServantErr 400 "Username/password pair did not match" "" []
    Nothing -> left $ ServantErr 400 "User not found" "" []

logOut :: Maybe Text -> EitherT ServantErr IO ()
logOut mt = do
  checkAuth mt
  maybe (return ()) (\_ -> return ()) mt

checkAuth :: Maybe Text -> EitherT ServantErr IO (JWT VerifiedJWT)
checkAuth = maybe unauthorized (runCheck . decodeAndVerifySignature (secret "Fabianz secret"))
  where
  runCheck Nothing = unauthorized
  runCheck (Just verifiedJWT) = return verifiedJWT
  unauthorized =
    left $ ServantErr 401 "You are not authenticated. Please sign-in" "" []

api :: Proxy AppAPI
api = Proxy
