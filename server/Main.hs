{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Servant
import Servant.HTML.Blaze

import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)

import qualified Crypto.Hash.SHA256 as SHA256

import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Maybe (isJust)
import Data.Functor (($>))
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5.Attributes as A

import Database.MongoDB ((=:), insert, select, findOne, find, deleteOne, rest)

import DB
import DB.Utils

import Types

api :: Proxy API
api = Proxy

server :: Server API
server = getBlogPosts :<|> 
         createBlogPost :<|> 
         getBlogPost :<|> 
         createUser :<|> 
         logIn :<|> 
         logOut :<|> 
         serveDirectory "client" :<|> 
         adminPage :<|> 
         startPage

app :: Application
app =  logStdout $ serve api server

getBlogPosts :: EitherT ServantErr IO [BlogPost]
getBlogPosts = do
  docs <- liftIO . runMongo $ rest =<< find (select [] "blogPosts")
  return $ documentToBlogPost <$> docs

createBlogPost :: Maybe LoginToken -> BlogPost -> EitherT ServantErr IO BlogPost
createBlogPost mt blogPost = do
  checkAuth mt
  let doc = blogPostToDocument blogPost
  liftIO . runMongo $ insert "blogPosts" doc
  return blogPost

getBlogPost :: String -> EitherT ServantErr IO BlogPost
getBlogPost slug = do
  doc <- liftIO . runMongo . findOne $ select ["slug" =: slug] "blogPosts"
  case doc of
    Just doc' -> return $ documentToBlogPost doc'
    Nothing -> left $ err503 { errBody = "No blog posts found with the provided id." }

createUser :: Maybe LoginToken -> User -> EitherT ServantErr IO User
createUser mt User {..} = do
  checkAuth mt
  let uPassword' = decodeLatin1 . SHA256.hash . encodeUtf8 $ uPassword
  let user = User uUsername uPassword'
  let doc = userToDocument user
  liftIO . runMongo . insert "users" $ doc
  return user

logIn :: LoginReq -> EitherT ServantErr IO LoginToken
logIn LoginReq {..} = do
  doc <- liftIO . runMongo . findOne $ select ["username" =: username] "users"
  case doc of
    Just doc' -> do
      let password' = decodeLatin1 . SHA256.hash . encodeUtf8 $ password
      let User {..} = documentToUser doc'
      if password' == uPassword then do
        uuid <- liftIO UUID.nextRandom
        let token = LoginToken (UUID.toText uuid)
        let doc = loginTokenToDocument token
        liftIO . runMongo $ insert "validTokens" doc
        return token
      else left $ ServantErr 400 "Username/password pair did not match" "" []
    Nothing -> left $ ServantErr 400 "User not found" "" []

logOut :: Maybe LoginToken -> EitherT ServantErr IO ()
logOut mt = do
  checkAuth mt
  maybe (return ()) logOutAction mt
  where
  logOutAction (LoginToken token) =
    liftIO . runMongo . deleteOne $ select ["token" =: token] "validTokens"

checkAuth :: Maybe LoginToken -> EitherT ServantErr IO ()
checkAuth = maybe unauthorized runCheck
  where
  runCheck (LoginToken token) = do
    doc <- liftIO . runMongo . findOne $ select ["token" =: token] "validTokens"
    let isValidToken = isJust doc
    unless isValidToken unauthorized
  unauthorized =
    left $ ServantErr 401 "You are not authenticated. Please sign-in" "" []

renderPage' :: Maybe H.Html -> H.Html -> H.Html
renderPage' head body =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "x-ua-compatible" ! A.content "ie=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title "Fabian Rios"
      maybe mempty id head
    H.body body

renderPage :: H.Html -> H.Html
renderPage body = renderPage' Nothing body

renderBlogPost :: BlogPost -> H.Html
renderBlogPost BlogPost {..} =
  H.div $ do
    H.h2 $ H.toMarkup title
    H.p $ H.toMarkup body

renderBlogPosts :: [BlogPost] -> H.Html
renderBlogPosts posts =
  H.div $ do
    foldMap renderBlogPost posts

startPage :: EitherT ServantErr IO H.Html
startPage = do
  docs <- liftIO . runMongo $ rest =<< find (select [] "blogPosts")
  let blogPosts = renderBlogPosts $ documentToBlogPost <$> docs
  return $ renderPage blogPosts

adminPage :: EitherT ServantErr IO H.Html
adminPage = return . renderPage $ do
  H.div ! A.id "admin" $ ""
  H.script ! A.src "/static/dist/main.js" $ ""

main :: IO ()
main = do
  putStrLn "Running on port 8080"
  run 8080 app
