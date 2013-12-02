import Hron.Base
import Hron.Main (loop)

main = do
    task <- taskNow "some task"
    t <- taskInSeconds 3 "some other task"

    loop [task, t]
