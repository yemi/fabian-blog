{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module DB where

import qualified Database.MongoDB as MongoDB
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)

import Data.Profunctor.Product (p4)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Opaleye 
  ( Table(Table)
  , Column()
  , Query()
  , PGText
  , PGInt4
  , required
  , optional
  , queryTable
  )
import Opaleye.RunQuery (runQuery)

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

$(makeAdaptorAndInstance "pUser" ''User')

usersTable :: Table UserColumn UserColumn
usersTable = Table "usersTable" $ pUser User { uId = required "user_id"
                                             , uUsername = required "username"
                                             , uPassword = required "password"
                                             , uEmail = required "email" }

usersQuery :: Query UserColumn
usersQuery = queryTable usersTable

runUsersQuery :: Query UserColumn -> IO [User]
runUsersQuery query = do 
  conn <- connection
  runQuery conn query

--insertUser :: User


