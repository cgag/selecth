{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
{-import Control.Concurrent (threadDelay)-}

import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

-- Try with just strings, benchmark, then try with Text?  {-import qualified Data.Text as T-}

import System.IO 
import System.Exit

import System.Console.ANSI

import Tty

{-TODO: implement withTty that handles restoring tty state-}

data KeyPress = CtrlC | Enter | Backspace | PlainChar Char

data Search = Search  
    { query :: String
    , choices :: [String]
    , selection :: Int } deriving Show

data Action = NewSearch Search | Choice String | Abort

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL',   Backspace)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (PlainChar c) (M.lookup c specialChars)


-- just match anything that contains all the characters for now
matches :: String -> [String] -> [String]
matches q = filter (\s -> all (`elem` map toLower s) 
                              (map toLower q))

{-TODO: this shiould return [string] and then writelines should be called on it-}
render :: Search -> (String, [String])
render (Search {query=q, choices=cs}) = 
    let queryLine = "> " <> q
        matchLines = pad (take choicesToShow $ matches q cs)  
    in (queryLine, queryLine : matchLines)
  where 
    pad xs = xs ++ replicate (choicesToShow - length xs) " "


-- TODO: Handle exceptions, ensure tty gets restored to default settings
-- TODO: Ensure handle to tty is closed?  See what GB ensures.
configureTty :: Handle -> IO ()
configureTty tty = do
    -- LineBuffering by default requires hitting enter to see anything
    hSetBuffering tty NoBuffering 
    {-hSetSGR tty [SetColor Background Vivid White]-}
    ttyCommand  "stty raw -echo cbreak"

{-restoreTTY :: IO ()-}

dropLast :: String -> String
dropLast = reverse . drop 1 . reverse

writeSelection :: Handle -> String -> IO ()
writeSelection tty choice = do
    hCursorDown tty choicesToShow 
    hSetCursorColumn tty 0 
    saneTty
    hPutStr tty "\n"
    putStrLn choice
    exitSuccess

abort :: Handle -> IO ()
abort tty = do
    hCursorDown tty 1 
    putStrLn "Quit :(" 
    exitSuccess

handleInput :: Char -> Search -> Action
handleInput inputChar search = 
    case charToKeypress inputChar of
        CtrlC -> Abort
        Enter -> Choice (head $ matches (query search) (choices search))
        Backspace   -> NewSearch search { query = dropLast $ query search }
        PlainChar c -> NewSearch search { query = query search ++ [c] }

choicesToShow :: Int
choicesToShow = 10

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM (take 10 . lines) getContents

    let initSearch = Search { query="", choices=initialChoices, selection=0 }

    _ <- loop tty initSearch 

    saneTty -- duplicated in writeSelection
    hClose tty 
  where
    loop tty search = do
      x <- hGetChar tty 
      case handleInput x search of
          Abort -> abort tty
          Choice c -> writeSelection tty c
          NewSearch newSearch -> do
              let (queryLine, renderedLines) = render newSearch
              withCursorHidden tty $ writeLines tty renderedLines
              hSetCursorColumn tty (length queryLine)
              loop tty newSearch
