module StopWatch where
import FRP.Yampa
import Data.IORef
import Data.Time.Clock



data Stopwatch = Stopwatch { zero :: UTCTime
                           , prev :: UTCTime }

startStopWatch :: UTCTime -> Stopwatch
startStopWatch now = Stopwatch now now

storeStopwatch :: IO (IORef Stopwatch)
storeStopwatch = getCurrentTime >>= (newIORef . startStopWatch)

diffTime :: (IORef Stopwatch) -> IO (DTime,DTime)
diffTime ref = do
    now <- getCurrentTime
    (Stopwatch zero prev) <- readIORef ref
    writeIORef ref (Stopwatch zero now)
    
    let dt = realToFrac (diffUTCTime now prev)
        timePassed = realToFrac (diffUTCTime now zero)
    
    return (dt, timePassed)