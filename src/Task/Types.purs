module Hasyr.Task.Types where

import Prelude

import Data.Argonaut (Json, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.JSDate (isValid, parse, toDateTime)
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Unsafe (unsafePerformEffect)

type Meta =
  { note :: Maybe String
  , dueDate :: Maybe DateTime
  }

type TaskId = Int

type Task =
  { id :: TaskId
  , name :: String
  , meta :: Meta
  , createdAt :: DateTime
  -- , updatedAt :: DateTime
  }

type Tasks = Array Task

dateTimeFromString :: String -> Either String DateTime
dateTimeFromString str = unsafePerformEffect $ do
  jsDate <- parse str
  pure $ if isValid jsDate
    then toDateTime jsDate # note "Invalid date format"
    else Left "Invalid date format"

decodeMeta :: Json -> Either String Meta
decodeMeta json = do
  obj <- decodeJson json
  note <- obj .:? "note"
  dueDate' <- obj .:? "dueDate"
  dueDate <- case dueDate' of
    Nothing -> pure Nothing
    Just str -> Just <$> dateTimeFromString str
  pure { note, dueDate }

decodeTask :: Json -> Either String Task
decodeTask json = do
  obj <- decodeJson json
  id <- obj .: "id"
  name <- obj .: "name"
  meta' <- obj .:? "meta"
  meta <- case meta' of
    Nothing -> pure { note: Nothing, dueDate: Nothing }
    Just metaJson -> decodeMeta metaJson
  createdAt <- dateTimeFromString =<< obj .: "createdAt"
  pure $ { id, name, meta, createdAt }

decodeTasks :: Json -> Either String Tasks
decodeTasks = do
  lmap ("Couldn't decode Array (" <> _)
  <<< (traverseWithIndex f <=< decodeJArray)
  where
    msg i m = "Failed at index " <> show i <> "): " <> m
    f i = lmap (msg i) <<< decodeTask
