{-# LANGUAGE OverloadedStrings #-}

-- TODO: no prelude at all
import Prelude hiding (lookup)

import Control.Monad

import Data.Map
import Data.Maybe
import Data.Monoid
import Data.List.Split 

-- Try with just strings, benchmark, then try with Text?
import qualified Data.Text as T

import System.IO 
import System.Process
import System.Exit

import System.Console.ANSI
import System.Console.Terminal.Size 

data KeyPress = CtrlC | Enter | PlainChar Char

data Search = Search  
    { query :: T.Text 
    , choices :: [T.Text]} deriving Show

render :: Search -> T.Text
render (Search {query=q, choices=cs}) = "> " <> q <> "\n" <> T.unlines cs

specialChars :: Map Char KeyPress
specialChars = fromList [ ('\ETX', CtrlC)
                        , ('\r',   Enter)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (PlainChar c) (lookup c specialChars)

-- TODO: Handle exceptions, ensure tty gets restored to default settings
-- TODO: Ensure handle to tty is closed?  See what GB ensures.

configureTty :: Handle -> IO ()
configureTty tty = do
    -- LineBuffering by default requires hitting enter to see anything
    hSetBuffering tty NoBuffering 
    {-hSetSGR tty [SetColor Background Vivid White]-}
    ttyCommand  "stty raw -echo cbreak"

saneTty :: IO ()
saneTty = ttyCommand "stty sane"

{-restoreTTY :: IO ()-}

handleInput :: Handle -> IO ()
handleInput tty = do
    x <- hGetChar tty 
    case charToKeypress x of
        CtrlC -> hCursorDown tty 1 >> putStrLn "Quit :(" 
        Enter -> hCursorDown tty 1 >> putStrLn "Done" 
        PlainChar c -> do
            hPutChar tty c
            handleInput tty


unsafeSize :: (Integral n) => Handle -> IO (Window n)
unsafeSize h = do
    mWindow <- hSize h 
    case mWindow of
        Just x -> return x
        Nothing -> error "couldn't get window size"

linesToShow :: Int
linesToShow = 10

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty

    hHideCursor tty
    {-hSetCursorPosition tty 1 0-}

    hSetCursorColumn tty 0

    replicateM_ (linesToShow + 1) $ hPutChar tty '\n'
    hCursorUp tty (linesToShow + 1)

    hPutStr tty "> QUERY"

    hCursorDown tty 1
    hSetCursorColumn tty 0

    
    inputFiles <- getContents 

    mapM_ (\wordList -> do

              replicateM_ linesToShow $ do
                  hPutStr tty "                            " 
                  hCursorDown tty 1 
                  hSetCursorColumn tty 0
              
              hCursorUp tty linesToShow

              mapM_ (\word -> do
                        hPutStr tty word
                        hSetCursorColumn tty 0
                        hCursorDown tty 1) 
                    wordList

              hCursorUp tty linesToShow
          ) 
          $ chunksOf linesToShow $ drop 400 $ take 10000 $ lines inputFiles 

    hCursorDown tty linesToShow

    handleInput tty

    hSetCursorColumn tty 0

    saneTty
    hClose tty

ttyCommand :: String -> IO ()
ttyCommand command = do
    -- this will close the stdin handle, we need to use a second handle to 
    -- /dev/tty
    ttyTmpHandle <- openFile "/dev/tty" ReadMode
    let procSpec = (shell command) { std_in = UseHandle ttyTmpHandle }
    (_, _, _, processHandle) <- createProcess procSpec
    exitCode <- waitForProcess processHandle 
    case exitCode of
        (ExitFailure code) -> error $ "Failed to configure tty (Exit code: "
                                          ++ show code ++ ")"
        ExitSuccess -> return ()
