{-# LANGUAGE TupleSections #-}

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

withCursorHidden :: Handle -> IO () -> IO ()
withCursorHidden tty action = do
    hHideCursor tty 
    action
    hShowCursor tty 

writeLine :: Handle -> Int -> (String, SGR) -> IO ()
writeLine tty maxLen (line, sgr) = do
    hSetSGR tty [sgr]
    clearCurrentLine tty maxLen
    hPutStr tty (take maxLen line)
    hCursorDown tty 1

writeLines :: Handle -> [(String, SGR)] -> IO ()
writeLines tty lns = do
    Window _ w <- unsafeSize tty 
    mapM_ (writeLine tty w) lns
    hCursorUp tty (length lns)

unsafeSize :: (Integral n) => Handle -> IO (Window n)
unsafeSize h = do
    mWindow <- hSize h 
    case mWindow of
        Just x -> return x
        Nothing -> error "couldn't get window size"

clearCurrentLine :: Handle -> Int -> IO ()
clearCurrentLine tty winWidth = do
    hSetCursorColumn tty 0
    hPutStr tty $ replicate winWidth ' '
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
