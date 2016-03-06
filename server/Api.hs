{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module API where

import           Control.Monad              (unless)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Either (EitherT, left)

import           Crypto.Hash.SHA256         (hash)

import           Data.Functor               (($>))
import           Data.Maybe                 (isJust)
import           Data.Text                  (Text, pack, unpack)
import           Data.Text.Encoding         (decodeLatin1, encodeUtf8)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import           Servant                    hiding (JSON)

import           Web.JWT                    (Algorithm (HS256), JWT,
                                             JWTClaimsSet, Secret, VerifiedJWT,
                                             claims, decodeAndVerifySignature,
                                             def, encodeSigned, iat, intDate,
                                             iss, secret, stringOrURI,
                                             stringOrURIToText, sub)

import           DB
import           Types

encrypt :: Text -> Text
encrypt = decodeLatin1 . hash . encodeUtf8

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
  liftIO $ insertUser $ NewUser nuUsername (encrypt nuPassword) nuEmail

jwtSecret :: Secret
jwtSecret = secret "Fabianz secret"

makeClaimsSet :: Int -> IO JWTClaimsSet
makeClaimsSet uid = do
  now <- getPOSIXTime
  return $ def { iss = stringOrURI "Fabe"
               , sub = stringOrURI . pack . show $ uid
               , iat = intDate now
               }

makeLoginRes :: User -> IO LoginRes
makeLoginRes User {..} = do
  claimsSet <- makeClaimsSet uId
  return $ LoginRes
    { jwt = encodeSigned HS256 jwtSecret claimsSet
    , user = PublicUser uId uUsername uEmail
    }

login :: LoginReq -> EitherT ServantErr IO LoginRes
login LoginReq {..} = do
  user <- liftIO $ queryUserByUsername username
  case user of
    Just user' -> do
      if encrypt password == uPassword user'
        then liftIO $ makeLoginRes user'
        else left $ ServantErr 400 "Username/password pair did not match" "" []
    Nothing -> left $ ServantErr 400 "User not found" "" []

checkAuth :: Maybe Text -> EitherT ServantErr IO (JWT VerifiedJWT)
checkAuth = maybe unauthorized (runCheck . decodeAndVerifySignature jwtSecret)
  where
  runCheck Nothing = unauthorized
  runCheck (Just verifiedJWT) = return verifiedJWT
  unauthorized =
    left $ ServantErr 401 "You are not authenticated. Please sign-in" "" []

api :: Proxy AppAPI
api = Proxy

