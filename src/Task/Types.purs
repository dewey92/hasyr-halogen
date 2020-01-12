module Hasyr.Task.Types where

type TaskId = Int
type Task = { id :: TaskId, name :: String }
type Tasks = Array Task
