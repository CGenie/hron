module Hron.Base where

import Data.Time


data Task = Task {
    time :: UTCTime,
    command :: String,
    finished :: Bool
} deriving (Show)


execute :: Task -> Task
execute task = task { finished = True }


shouldFinish :: Task -> IO Bool
shouldFinish task = do
    currentTime <- getCurrentTime
    let diff = diffUTCTime (time task) currentTime
    return $ and [
            not $ finished task,
            diff <= 0.0
        ]


taskNow :: String -> IO Task
taskNow command = do
    currentTime <- getCurrentTime
    return $ Task {
        time = currentTime
      , command = command
      , finished = False
    }


taskInSeconds :: Integer -> String -> IO Task
taskInSeconds seconds command = do
    currentTime <- getCurrentTime
    return $ Task {
        time = addUTCTime (fromInteger seconds) currentTime 
      , command = command
      , finished = False
    }
