module Hron.Main (loop)
where

import Hron.Base

import Control.Monad (mapM)
import Control.Concurrent (threadDelay)

import Data.Time


loop :: [Task] -> IO ()
loop tasks = do
    sff <- mapM shouldFinish tasks
    let sf = zip tasks sff
    let toExecute = map fst $ filter snd sf
    let executed = map execute toExecute
    if (length executed > 0)
        then print $ show executed
        else return ()

    print "Delay"
    threadDelay (1 * 1000000)

    loop $ map fst $ filter (not . snd) sf
