{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module DB.Utils where

import Database.MongoDB (Action, Document, Document, Field, Value, Val, access,
                        close, connect, delete, exclude, find, count,
                        host, insertMany, master, project, rest,
                        select, sort, aggregate, (=:), (!?), typed, value,
                        ObjectId, Label, valueAt, modify, look)

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Text (Text, pack)

getText :: Label -> Document -> Text
getText label = pack . typed . (valueAt label)

getInteger :: Label -> Document -> Integer
getInteger label = typed . (valueAt label)

getObjId :: Document -> ObjectId
getObjId = typed . (valueAt "_id")

getSecondaryObjId :: Label -> Document -> ObjectId
getSecondaryObjId label = typed . (valueAt label)

lookupString :: Label -> Document -> Maybe String
lookupString label document =
  document !? label

lookupInteger :: Label -> Document -> Maybe Integer
lookupInteger label document =
  document !? label

lookupSecondaryObjId :: Label -> Document -> Maybe ObjectId
lookupSecondaryObjId label document =
  document !? label
