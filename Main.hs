import Control.Monad

import System.IO
import System.Process
import System.Exit
import System.Console.ANSI

configureTty :: Handle -> IO ()
configureTty tty = do
    -- LineBuffering by default requires hitting enter to see anything
    hSetBuffering tty NoBuffering 

    -- this will close the stdin handle, we need to use a second handle to 
    -- /dev/tty
    ttyTmpHandle <- openFile "/dev/tty" ReadMode
    let procSpec = (shell "stty -echo cbreak") { 
                          std_in  = UseHandle ttyTmpHandle
                        , std_out = CreatePipe
                        , std_err = CreatePipe
                    }
  
    (_, _, _, processHandle) <- createProcess procSpec
    exitCode <- waitForProcess processHandle 
    case exitCode of
        (ExitFailure code) -> error $ "Failed to configure tty (Exit code: "
                                          ++ show code ++ ")"
        ExitSuccess -> return ()

{-restoreTTY :: IO ()-}

main :: IO ()
main = do
  -- tty <- openFile "/dev/tty" ReadWriteMode
  tty <- openFile "/dev/tty" ReadWriteMode
  configureTty tty

  hSetSGR tty [SetColor Background Vivid White]
  {-hHideCursor tty-}

  putStrLn "hello"

  inputFiles <- getContents 
  putStrLn inputFiles

  {-replicateM_ 10 $ hPutChar tty '\n'-}
  {-hCursorUp tty 5-}

  loop tty
  where
    loop tty = do 
        x <- hGetChar tty 
        hCursorForward tty 1
        hCursorDown tty 1
        hPutChar tty x 
        if x == 'x' 
        then do
          hClose tty
          return ()
        else
          loop tty
