{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
{-import Control.Parallel.Strategies-}

{-import Control.Concurrent (threadDelay)-}

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Data.Function (on)

-- Try with just strings, benchmark, then try with Text?  {-import qualified Data.Text as T-}

import System.IO 
import System.Exit

import System.Console.ANSI

import Tty
import Score

{-TODO: implement withTty that handles restoring tty state-}

data KeyPress = CtrlC | Enter | Invisible | Backspace | PlainChar Char

data Search = Search  
    { query :: String
    , choices :: [String]
    , selection :: Int } deriving Show

data RenderedSearch = RenderedSearch 
    {
      queryString :: String
    , renderedLines :: [String]
    }

data Choice = Choice 
    { finalMatches :: [String]
    , matchIndex :: Int
    }

data Action = NewSearch Search | MakeChoice Choice | Abort | Ignore


specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL', Backspace)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (if isPrint c then PlainChar c else Invisible) 
                             (M.lookup c specialChars)

matches :: String -> [String] -> [String]
matches qry chs = take choicesToShow 
                  $ map fst 
                  $ filter (\(_,cScore) -> cScore > 0) 
                  $ sortBy (flip compare `on` snd) scoredChoices
  where 
      scoredChoices =  map (\choice -> (choice, score qry choice)) chs
                       {-`using` parListChunk 10000 rdeepseq-}

-- Seach -> (QueryString, renderedLines)
render :: Search -> RenderedSearch
render (Search {query=q, choices=cs}) = 
    let queryLine = "> " <> q
        matchLines = pad (take choicesToShow $ matches q cs)  
    in RenderedSearch queryLine $ queryLine : matchLines
  where 
    pad xs = xs ++ replicate (choicesToShow - length xs) " "

draw :: Handle -> RenderedSearch -> IO ()
draw tty rendered = do
    withCursorHidden tty $ 
        writeLines tty (renderedLines rendered)
    hSetCursorColumn tty $ length $ queryString rendered

-- TODO: Handle exceptions, ensure tty gets restored to default settings
-- TODO: Ensure handle to tty is closed?  See what GB ensures.
{-restoreTTY :: IO ()-}

dropLast :: String -> String
dropLast = reverse . drop 1 . reverse

writeSelection :: Handle -> Choice -> IO ()
writeSelection tty choice = do
    hCursorDown tty $ length $ finalMatches choice
    hSetCursorColumn tty 0 
    saneTty
    hPutStr tty "\n"
    putStrLn (finalMatches choice !! matchIndex choice) -- DANGEROUS 
    exitSuccess

abort :: Handle -> IO ()
abort tty = do
    hCursorDown tty 1 
    saneTty -- duplicated in writeSelection
    putStrLn "Quit :(" 
    exitSuccess

handleInput :: Char -> Search -> Action
handleInput inputChar search = 
    case charToKeypress inputChar of
        CtrlC -> Abort
        Enter -> MakeChoice Choice { 
                              finalMatches = matches (query search) (choices search)
                            , matchIndex = selection search
                            } 
        Backspace   -> NewSearch search { query = dropLast $ query search }
        PlainChar c -> NewSearch search { query = query search ++ [c] }
        Invisible -> Ignore

choicesToShow :: Int
choicesToShow = 10

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM lines getContents

    let initSearch = Search { query="", choices=initialChoices, selection=0 }
    draw tty $ render initSearch

    -- maybe this should return Abort | Quit?, ExitSuccess + sanetty duped
    _ <- eventLoop tty initSearch 

    hClose tty 
  where
    eventLoop tty search = do
      x <- hGetChar tty 
      case handleInput x search of
          Abort -> abort tty
          MakeChoice c -> writeSelection tty c
          Ignore -> eventLoop tty search
          NewSearch newSearch -> do
              draw tty $ render newSearch
              eventLoop tty newSearch
