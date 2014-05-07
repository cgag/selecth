import Control.Monad

import System.IO
import System.Process
import System.Console.ANSI

main :: IO ()
main = do
  tty <- openFile "/dev/tty" ReadWriteMode
  hSetBuffering tty NoBuffering

  {-let procSpec = shell "stty raw -echo cbreak"-}
  let procSpec = shell "stty -echo cbreak"
  _ <- createProcess procSpec

  hSetSGR tty [SetColor Background Vivid White]
  hHideCursor tty

  {-inputFiles <- hGetContents stdin -}
  {-putStrLn inputFiles-}

  {-replicateM_ 10 $ hPutChar tty '\n'-}
  {-hCursorUp tty 5-}
  loop tty
  where
    loop tty = do 
        x <- hGetChar tty 
        hPutChar tty x 
        {-hCursorForward tty 1-}
        hCursorDown tty 1
        if x == 'x' then
          return ()
        else
          loop tty
