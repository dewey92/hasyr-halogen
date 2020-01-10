module Hasyr.Task.Apis where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as Format
import Data.Argonaut (Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Tasks)

tasksFromJson :: Json -> Either String Tasks
tasksFromJson = decodeJson

getTasksFromFakeServer :: Aff (Either String Tasks)
getTasksFromFakeServer = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"
  rawResult <- AX.get Format.json fakeUrl
  delay (Milliseconds 3000.0) -- not too fast, I want to show loading indicator

  pure $ do
    { body } <- lmap AX.printError rawResult
    tasks <- tasksFromJson body
    pure tasks

class Monad m <= ManageTasks m where
  getAllTasks :: m (Either String Tasks)

instance manageTasksAppM :: ManageTasks AppM where
  getAllTasks = liftAff getTasksFromFakeServer

instance manageTasksHalogenM :: ManageTasks m => ManageTasks (HalogenM st act cs msg m) where
  getAllTasks = lift getAllTasks