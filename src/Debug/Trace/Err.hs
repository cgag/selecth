module Debug.Trace.Err
(trace)
where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe
import System.IO (stderr)

trace :: Text -> a -> a
trace msg x = unsafePerformIO $ do
    TIO.hPutStrLn stderr msg
    return x
