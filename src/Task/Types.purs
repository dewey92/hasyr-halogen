module Hasyr.Task.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Bifunctor (lmap)
import Data.DateTime as DT
import Data.Either (Either(..), note)
import Data.JSDate (fromDateTime, isValid, parse, toDateTime, toUTCString)
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Unsafe (unsafePerformEffect)

newtype DateTime = DateTime DT.DateTime

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

instance decodeJsonDateTime :: DecodeJson DateTime where
  decodeJson json = decodeJson json >>= fromString where
    fromString :: String -> Either String DateTime
    fromString str = unsafePerformEffect $ do
      jsDate <- parse str
      pure $ if isValid jsDate
        then DateTime <$> (toDateTime jsDate # note "Invalid date format")
        else Left "Invalid date format"

instance encodeJsonDateTime :: EncodeJson DateTime where
  encodeJson (DateTime dt) = fromDateTime dt # toUTCString # encodeJson

decodeMeta :: Json -> Either String Meta
decodeMeta json = do
  obj <- decodeJson json
  note <- obj .:? "note"
  dueDate <- obj .:? "dueDate"
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
  createdAt <- obj .: "createdAt"
  pure $ { id, name, meta, createdAt }

decodeTasks :: Json -> Either String Tasks
decodeTasks = do
  lmap ("Couldn't decode Array (" <> _)
  <<< (traverseWithIndex f <=< decodeJArray)
  where
    msg i m = "Failed at index " <> show i <> "): " <> m
    f i = lmap (msg i) <<< decodeTask
