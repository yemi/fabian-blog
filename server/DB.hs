{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module DB where

import qualified Database.MongoDB as MongoDB
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)

import Control.Arrow (returnA)

import Data.Int (Int64())
import Data.Profunctor.Product (p4)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)

import qualified Crypto.Hash.SHA256 as SHA256

import Opaleye 
  ( Table(Table)
  , Column()
  , Query()
  , PGText
  , PGInt4
  , required
  , optional
  , queryTable
  , runInsert
  )
import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgString, pgStrictText, pgInt4, pgUTCTime)
import Opaleye.Operators (restrict, (.==))

import Types

dbName = "blog"

runMongo :: MongoDB.Action IO a -> IO a
runMongo action = do
  pipe <- MongoDB.connect (MongoDB.host "127.0.0.1")
  val <- MongoDB.access pipe MongoDB.master dbName action
  MongoDB.close pipe
  return val

connection :: IO Connection
connection = connectPostgreSQL "dbname='fabian_blog'"

runInsert' :: Table setColumns getColumns -> setColumns -> IO Int64
runInsert' table columns = do
  conn <- connection
  runInsert conn table columns

-- Users

$(makeAdaptorAndInstance "pUser" ''User')

usersTable :: Table UserSetColumn UserGetColumn
usersTable = Table "users" $ 
  pUser User { uId = optional "id"
             , uUsername = required "username"
             , uPassword = required "password"
             , uEmail = required "email" 
             }

usersQuery :: Query UserGetColumn
usersQuery = queryTable usersTable

runUsersQuery :: Query UserGetColumn -> IO [User]
runUsersQuery query = flip runQuery query =<< connection

insertUser :: User -> IO Int64
insertUser User {..} = runInsert' usersTable $ 
  User Nothing 
       (pgStrictText uUsername) 
       (pgStrictText uPassword) 
       (pgStrictText uEmail)

-- Blog posts

$(makeAdaptorAndInstance "pBlogPost" ''BlogPost')

blogPostsTable :: Table BlogPostSetColumn BlogPostGetColumn
blogPostsTable = Table "blog_posts" $ 
  pBlogPost BlogPost { bpId = optional "id"
                     , bpTitle = required "title"
                     , bpSlug = required "slug"
                     , bpBody = required "body"
                     , bpCreatedAt = required "created_at"
                     }

blogPostsQuery :: Query BlogPostGetColumn
blogPostsQuery = queryTable blogPostsTable

queryBlogPosts :: IO [BlogPost]
queryBlogPosts = flip runQuery blogPostsQuery =<< connection

blogPostQuery :: Slug -> Query BlogPostGetColumn
blogPostQuery slug = proc () -> do
  row@(BlogPost _ _ bpSlug _ _) <- blogPostsQuery -< ()
  restrict -< bpSlug .== pgString slug 
  returnA -< row

queryBlogPost :: Slug -> IO (Maybe BlogPost)
queryBlogPost slug = do
  blogPosts <- flip runQuery (blogPostQuery slug) =<< connection
  return $ case blogPosts of
    [] -> Nothing
    [blogPost] -> Just blogPost

insertBlogPost :: BlogPost -> IO Int64
insertBlogPost BlogPost {..} = runInsert' blogPostsTable $ 
  BlogPost Nothing 
           (pgStrictText bpTitle) 
           (pgStrictText bpSlug) 
           (pgStrictText bpBody) 
           (pgUTCTime bpCreatedAt) 

