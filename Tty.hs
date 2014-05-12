module Tty where

import System.IO 
import System.Process
import System.Exit

import System.Console.ANSI
import System.Console.Terminal.Size 

saneTty :: IO ()
saneTty = ttyCommand "stty sane"

configureTty :: Handle -> IO ()
configureTty tty = do
    -- LineBuffering by default requires hitting enter to see anything
    hSetBuffering tty NoBuffering 
    {-hSetSGR tty [SetColor Background Vivid White]-}
    ttyCommand  "stty raw -echo cbreak"

writeLine :: Handle -> String -> IO ()
writeLine tty line = do
    clearCurrentLine tty
    hPutStr tty line
    hCursorDown tty 1

withCursorHidden :: Handle -> IO () -> IO ()
withCursorHidden tty action = do
    hHideCursor tty 
    action
    hShowCursor tty 

writeLines :: Handle -> [String] -> IO ()
writeLines tty lns = do
    mapM_ (writeLine tty) lns
    hCursorUp tty (length lns)


unsafeSize :: (Integral n) => Handle -> IO (Window n)
unsafeSize h = do
    mWindow <- hSize h 
    case mWindow of
        Just x -> return x
        Nothing -> error "couldn't get window size"

{-clearLines :: Handle -> Int -> IO ()-}
{-clearLines tty n = do -}
    {-writeLines tty (replicate n "")-}
    {-hCursorUp tty n-}

clearCurrentLine :: Handle -> IO ()
clearCurrentLine tty = do
    Window _ w <- unsafeSize tty 
    hSetCursorColumn tty 0
    hPutStr tty $ replicate w ' '
    hSetCursorColumn tty 0

ttyCommand :: String -> IO ()
ttyCommand command = do
    -- this will close the stdin handle, we need to use a second handle to 
    -- /dev/tty
    ttyTmpHandle <- openFile "/dev/tty" ReadMode
    let procSpec = (shell command) { std_in = UseHandle ttyTmpHandle }
    (_, _, _, processHandle) <- createProcess procSpec
    exitCode <- waitForProcess processHandle 
    case exitCode of
        ExitFailure code -> error $ "Failed to configure tty (Exit code: "
                                      ++ show code ++ ")"
        ExitSuccess -> return ()
