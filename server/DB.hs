{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}
module DB where

import Database.MongoDB (Action, access, close, connect, host, master)

dbName = "blog"

runMongo :: Action IO a -> IO a
runMongo action = do
  pipe <- connect (host "127.0.0.1")
  val <- access pipe master dbName action
  close pipe
  return val
