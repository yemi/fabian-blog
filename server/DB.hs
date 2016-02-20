{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module DB where

import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)

import Control.Arrow (returnA)

import Data.Int (Int64())
import Data.Profunctor.Product (p4)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)

import qualified Crypto.Hash.SHA256 as SHA256

import Opaleye
  ( Table(Table)
  , QueryRunner()
  , Column()
  , Query()
  , PGText
  , PGInt4
  , required
  , optional
  , queryTable
  , runInsertReturning
  )
import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgString, pgStrictText, pgInt4, pgUTCTime)
import Opaleye.Operators (restrict, (.==))

import Types

connection :: IO Connection
connection = connectPostgreSQL "dbname='fabian_blog'"

runInsertReturning' :: Default QueryRunner getColumns a =>
                      Table setColumns getColumns ->
                      setColumns ->
                      IO [a]
runInsertReturning' table columns = do
  conn <- connection
  runInsertReturning conn table columns id

-- Users

$(makeAdaptorAndInstance "pUser" ''User')

usersTable :: Table UserSetColumn UserGetColumn
usersTable = Table "users" $
  pUser User { uId = optional "user_id"
             , uUsername = required "username"
             , uPassword = required "password"
             , uEmail = required "email"
             }

usersQuery :: Query UserGetColumn
usersQuery = queryTable usersTable

queryUsers :: IO [User]
queryUsers = flip runQuery usersQuery =<< connection

userByUsernameQuery :: Text -> Query UserGetColumn
userByUsernameQuery username = proc () -> do
  row@(User _ uUsername _ _) <- usersQuery -< ()
  restrict -< uUsername .== pgStrictText username
  returnA -< row

queryUserByUsername :: Text -> IO (Maybe User)
queryUserByUsername username = do
  user <- flip runQuery (userByUsernameQuery username) =<< connection
  return $ case user of
    [] -> Nothing
    [user] -> Just user

insertUser :: NewUser -> IO User
insertUser NewUser {..} = do
  [user] <- runInsertReturning' usersTable $
    User Nothing
         (pgStrictText nuUsername)
         (pgStrictText nuPassword)
         (pgStrictText nuEmail)
  return user

-- Blog posts

$(makeAdaptorAndInstance "pBlogPost" ''BlogPost')

blogPostsTable :: Table BlogPostSetColumn BlogPostGetColumn
blogPostsTable = Table "blog_posts" $
  pBlogPost BlogPost { bpId = optional "blog_post_id"
                     , bpTitle = required "title"
                     , bpSlug = required "slug"
                     , bpBody = required "body"
                     , bpCreatedBy = required "created_by"
                     , bpCreatedOn = optional "created_on"
                     }

blogPostsQuery :: Query BlogPostGetColumn
blogPostsQuery = queryTable blogPostsTable

queryBlogPosts :: IO [BlogPost]
queryBlogPosts = flip runQuery blogPostsQuery =<< connection

blogPostQuery :: Slug -> Query BlogPostGetColumn
blogPostQuery slug = proc () -> do
  row@(BlogPost _ _ bpSlug _ _ _) <- blogPostsQuery -< ()
  restrict -< bpSlug .== pgString slug
  returnA -< row

queryBlogPost :: Slug -> IO (Maybe BlogPost)
queryBlogPost slug = do
  blogPosts <- flip runQuery (blogPostQuery slug) =<< connection
  return $ case blogPosts of
    [] -> Nothing
    [blogPost] -> Just blogPost

insertBlogPost :: Int -> NewBlogPost -> IO BlogPost
insertBlogPost userId NewBlogPost {..} = do
  [blogPost] <- runInsertReturning' blogPostsTable $
    BlogPost Nothing
             (pgStrictText nbpTitle)
             (pgStrictText nbpTitle) --TODO hyphenate title
             (pgStrictText nbpBody)
             (pgInt4 userId)
             Nothing
  return blogPost
