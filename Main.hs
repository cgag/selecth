{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
{-import Control.Concurrent (threadDelay)-}

import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.List.Split 

-- Try with just strings, benchmark, then try with Text?  {-import qualified Data.Text as T-}

import System.IO 
import System.Process
import System.Exit

import System.Console.ANSI
import System.Console.Terminal.Size 

data KeyPress = CtrlC | Enter | Backspace | PlainChar Char

data Search = Search  
    { query :: String
    , choices :: [String]
    , selection :: Int } deriving Show

clearCurrentLine :: Handle -> IO ()
clearCurrentLine tty = do
    Window _ w <- unsafeSize tty 
    hSetCursorColumn tty 0
    hPutStr tty $ replicate w ' '
    hSetCursorColumn tty 0

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

-- just match anything that contains all the characters for now
matches :: String -> [String] -> [String]
matches q = filter (\s -> all (`elem` map toLower s) 
                              (map toLower q))

{-TODO: this shiould return string and then writelines should be called on it-}
render :: Handle -> Search -> IO ()
render tty (Search {query=q, choices=cs}) = withCursorHidden tty $ do
    let queryLine = "> " <> q 
    mapM_ (writeLines tty . (queryLine:)) $ chunksOf choicesToShow $ pad $ matches q cs
    hSetCursorColumn tty 0
    hCursorForward tty (length queryLine)
  where 
      pad xs = xs ++ replicate (choicesToShow - length xs) " "

clearLines :: Handle -> Int -> IO ()
clearLines tty n = do 
    writeLines tty (replicate n "")
    hCursorUp tty n

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL',   Backspace)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (PlainChar c) (M.lookup c specialChars)

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

dropLast :: String -> String
dropLast = reverse . drop 1 . reverse

handleInput :: Handle -> Search -> IO ()
handleInput tty search = do
    x <- hGetChar tty 
    case charToKeypress x of
        CtrlC -> hCursorDown tty 1 >> putStrLn "Quit :(" 
        Enter -> hCursorDown tty choicesToShow 
                 >> hSetCursorColumn tty 0 
                 >> saneTty
                 >> hPutStr tty "\n"
                 >> putStrLn (query search)
        Backspace -> do 
            let newSearch = search { query = dropLast $ query search }
            render tty newSearch
            handleInput tty newSearch

        PlainChar c -> do
            let newSearch = search { query = query search ++ [c] }
            render tty newSearch
            handleInput tty newSearch


unsafeSize :: (Integral n) => Handle -> IO (Window n)
unsafeSize h = do
    mWindow <- hSize h 
    case mWindow of
        Just x -> return x
        Nothing -> error "couldn't get window size"

choicesToShow :: Int
choicesToShow = 10

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM (take 10 . lines) getContents

    let initSearch = Search { query="", choices=initialChoices, selection=0 }

    handleInput tty initSearch

    {-saneTty-}
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
        ExitFailure code -> error $ "Failed to configure tty (Exit code: "
                                      ++ show code ++ ")"
        ExitSuccess -> return ()
