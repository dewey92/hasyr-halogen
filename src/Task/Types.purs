module Hasyr.Task.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, encodeJson)
import Data.Argonaut.Decode.Struct.Tolerant as AT
import Data.DateTime as DT
import Data.Either (Either(..), note)
import Data.JSDate (fromDateTime, isValid, parse, toDateTime, toUTCString)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
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
  }

type Tasks = Array Task

dateTimeFromString :: String -> Either String DateTime
dateTimeFromString str
  | jsDate <- unsafePerformEffect $ parse str
  , isValid jsDate = DateTime <$> toDateTime jsDate # note "Invalid date format"
  | otherwise      = Left "Invalid date format"

dateTimeToString :: DateTime -> String
dateTimeToString (DateTime dt) = fromDateTime dt # toUTCString

instance decodeJsonDateTime :: DecodeJson DateTime where
  decodeJson = AT.decodeJson >=> dateTimeFromString

instance encodeJsonDateTime :: EncodeJson DateTime where
  encodeJson = encodeJson <<< dateTimeToString

decodeMeta :: Json -> Either String Meta
decodeMeta = AT.decodeJson

decodeTask :: Json -> Either String Task
decodeTask = AT.decodeJsonPer
  { meta: AT.decodeJson >=> decodeMeta }

decodeTasks :: Json -> Either String Tasks
decodeTasks = AT.decodeJson >=> traverse decodeTask
