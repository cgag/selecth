{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Parallel.Strategies

{-import Control.Concurrent (threadDelay)-}

import Data.List
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Function (on)

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

data Choice = Choice 
    { 
        finalMatches :: [String]
      , matchIndex :: Int
    }

data Action = NewSearch Search | MakeChoice Choice | Abort

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL',   Backspace)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (PlainChar c) (M.lookup c specialChars)

hasMatch :: String -> String -> Bool
hasMatch [] _ = True
hasMatch _ [] = False
hasMatch (q:restOfQuery) choice = 
  case elemIndex q choice of
    Just i -> hasMatch restOfQuery (drop (i+1) choice)
    Nothing -> False

score :: String -> String -> Int
score q choice
    | null q      = 1
    | null choice = 0
    | otherwise = 
        let lowerQ      = map toLower q
            lowerChoice = map toLower choice
        in if hasMatch lowerQ lowerChoice 
           then 1 
           else 0

matches :: String -> [String] -> [String]
matches qry chs = take choicesToShow $ 
                  map fst $ 
                  filter (\(_,cScore) -> cScore > 0) $
                  sortBy (flip compare `on` snd) scoredChoices
  where 
      scoredChoices =  map (\choice -> (choice, score qry choice)) chs
                       {-`using` parListChunk 10000 rdeepseq-}

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

choicesToShow :: Int
choicesToShow = 10

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM lines getContents

    let initSearch = Search { query="", choices=initialChoices, selection=0 }

    _ <- eventLoop tty initSearch 

    saneTty -- duplicated in writeSelection
    hClose tty 
  where
    eventLoop tty search = do
      x <- hGetChar tty 
      case handleInput x search of
          Abort -> abort tty
          MakeChoice c -> writeSelection tty c
          NewSearch newSearch -> do
              let (queryLine, renderedLines) = render newSearch
              withCursorHidden tty $ writeLines tty renderedLines
              hSetCursorColumn tty (length queryLine)
              eventLoop tty newSearch
