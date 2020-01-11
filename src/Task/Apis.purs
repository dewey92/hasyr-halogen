module Hasyr.Task.Apis where

import Prelude

import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as Format
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (HalogenM, lift, liftAff)
import Hasyr.AppM (AppM)
import Hasyr.Task.Types (Tasks, Task)

taskFromJson :: Json -> Either String Task
taskFromJson = decodeJson

tasksFromJson :: Json -> Either String Tasks
tasksFromJson = decodeJson

fakeServerGetTasks :: Aff (Either String Tasks)
fakeServerGetTasks = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"
  rawResult <- AX.get Format.json fakeUrl
  delay (Milliseconds 3000.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    tasks <- tasksFromJson body
    pure tasks

fakeServerAddTask :: String -> Aff (Either String Task)
fakeServerAddTask taskName = do
  let fakeUrl = "https://my-json-server.typicode.com/dewey92/hasyr-halogen/tasks"
  let payload = Json $ encodeJson { name: taskName }
  rawResult <- AX.post Format.json fakeUrl (Just payload)
  delay (Milliseconds 3000.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    tasks <- taskFromJson body
    pure tasks

class Monad m <= ManageTasks m where
  getAllTasks :: m (Either String Tasks)
  addTask :: String -> m (Either String Task)

instance manageTasksAppM :: ManageTasks AppM where
  getAllTasks = liftAff fakeServerGetTasks
  addTask = liftAff <<< fakeServerAddTask

instance manageTasksHalogenM :: ManageTasks m => ManageTasks (HalogenM st act cs msg m) where
  getAllTasks = lift getAllTasks
  addTask = lift <<< addTask
