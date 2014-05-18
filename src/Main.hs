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

import System.IO 
import System.Exit

import System.Console.ANSI

import Tty
import Score

{-TODO: need to be sure to not draw off the edge of the scree-}
{-TODO: implement withTty that handles restoring tty state-}
{-TODO: invert colors on current seleciton-}
{-TODO: support actually changing selected index-}
{-TODO: Tons of time in GC, check for space leaks: -}
  {-http://www.haskell.org/haskellwiki/Performance/GHC#Measuring_performance-}

data KeyPress = CtrlC | Enter | Invisible | Backspace | PlainChar Char

data Search = Search  
    { query :: String
    , choices :: [String]
    , selection :: Int } deriving Show

data RenderedSearch = RenderedSearch 
    {
      queryString :: String
    , renderedLines :: [(String, SGR)]
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
                  $ sortBy (flip compare `on` snd) 
                  $ scoreAll qry chs 

render :: Search -> RenderedSearch
render (Search {query=q, choices=cs, selection=selIndex}) = 
    let queryLine = "> " <> q
        matchLines = pad (take choicesToShow $ matches q cs)  
        renderedMatchLines = map (\(m, i) -> 
                                    if i == selIndex
                                    then (m, SetSwapForegroundBackground True)
                                    else (m, Reset))
                             $ zip matchLines [0..]
    in RenderedSearch queryLine $ (queryLine, Reset) : renderedMatchLines
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
    -- !!DANGEROUS , this fails is there are no matches
    putStrLn (finalMatches choice !! matchIndex choice) 
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
choicesToShow = 20

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM lines getContents

    -- create room for choices and query line
    replicateM_ choicesToShow $ hPutStr tty "\n"
    hCursorUp tty (choicesToShow + 1)

    let initSearch = Search { query="", choices=initialChoices, selection=0 }
    draw tty $ render initSearch

    -- maybe this should return Abort | Quit?, ExitSuccess + sanetty duplicated
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
