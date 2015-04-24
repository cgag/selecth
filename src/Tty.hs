{-# LANGUAGE TupleSections #-}

module Tty where

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.IO                 as T
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           System.Exit
import           System.IO
import           System.Process

import           System.Console.ANSI
import           System.Console.Terminal.Size

saneTty :: IO ()
saneTty = ttyCommand "stty sane"

configureTty :: Handle -> IO ()
configureTty tty = do
    -- LineBuffering by default requires hitting enter to see anything
    hSetBuffering tty NoBuffering
    ttyCommand  "stty raw -echo cbreak"

withCursorHidden :: Handle -> IO () -> IO ()
withCursorHidden tty action = do
    hHideCursor tty
    action
    hShowCursor tty

writeLine :: Handle -> Int -> (Text, SGR) -> IO ()
writeLine tty maxLen (line, sgr) = do
    let line' = T.take maxLen line
    let (start, end) = (2, 4)
    let (left, rightTmp) = T.splitAt start line'
    let (middle, right)  = T.splitAt (end - start) rightTmp

    clearCurrentLine tty
    hSetSGR tty [sgr]
    T.hPutStr tty left
    hSetSGR tty [SetColor Foreground Vivid Red]
    T.hPutStr tty middle
    hSetSGR tty [Reset]
    T.hPutStr tty right
    hCursorDown tty 1

-- TODO: resizing is nice, but how does calling size repeatedly
-- affect performance?
-- TODO: Can we combine them to a single large string and
-- only write once?
writeLines :: Handle -> Vector (Text, SGR) -> IO ()
writeLines tty lns = do
    (_, w) <- unsafeSize tty
    V.mapM_ (writeLine tty w) lns
    hCursorUp tty (V.length lns)

unsafeSize :: Handle -> IO (Int, Int)
unsafeSize tty = do
    mWindow <- hSize tty
    case mWindow of
        Just (Window h w) -> return (h, w)
        Nothing -> error "couldn't get window size"

clearCurrentLine :: Handle -> IO ()
clearCurrentLine tty = do
    hClearLine tty
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
        ExitFailure code -> error $ "Failed to run tty command "
                                    ++ command
                                    ++ "(Exit code: "
                                    ++ show code
                                    ++ ")"
        ExitSuccess -> return ()
