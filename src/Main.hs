{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import           Data.Char           (isPrint)
import           Data.Function       (on)
import           Data.List           (sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Safe (atMay)

import           System.Exit         (exitSuccess, exitFailure)
import           System.IO           (Handle, IOMode (..), hClose, hGetChar,
                                      hPutStr, openFile)

import           System.Console.ANSI

import           Score
import           Tty

{-TODO: handle CtrlC properly -}
{-TODO: implement withTty that handles restoring tty state-}
{-TODO: getting unweildy passing around currMatchCount and choicesToShow-}
{-TODO: Try using threadscope-}
{-TODO: Tons of time in GC, check for space leaks: -}
  {-http://www.haskell.org/haskellwiki/Performance/GHC#Measuring_performance-}

{-TODO:  Do this refactor: -- "...return Abort | Quit?, sanetty duplicated " -}

data KeyPress = CtrlC | CtrlN | CtrlP | Enter
                | Invisible | Backspace | PlainChar Char

data Search = Search
    { query     :: String
    , choices   :: [String]
    , selection :: Int } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: String
    , renderedLines :: [(String, SGR)]
    , matchCount    :: Int
    }

data Choice = Choice
    { finalMatches :: [String]
    , matchIndex   :: Int
    }

data Action = NewSearch Search | MakeChoice Choice | Abort | Ignore

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL', Backspace)
                          , ('\SO',  CtrlN)
                          , ('\DLE', CtrlP)]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (if isPrint c then PlainChar c else Invisible)
                             (M.lookup c specialChars)

matches :: String -> [String] -> [String]
matches qry chs = map fst
                  $ filter (\(_,cScore) -> cScore > 0)
                  $ sortBy (flip compare `on` snd)
                  $ scoreAll qry chs

render :: Search -> Int -> RenderedSearch
render (Search {query=q, choices=cs, selection=selIndex}) choicesToShow =
    let queryLine = "> " <> q
        matched = take choicesToShow $ matches q cs
        matchLines = pad matched
        renderedMatchLines = map (\(m, i) ->
                                    if i == selIndex
                                    then (m, SetSwapForegroundBackground True)
                                    else (m, Reset))
                             $ zip matchLines [0..]
    in RenderedSearch { queryString = queryLine
                      , renderedLines = (queryLine, Reset) : renderedMatchLines
                      , matchCount = length matched }
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

writeSelection :: Handle -> Choice -> Int -> IO ()
writeSelection tty choice choicesToShow = do
    hCursorDown tty $ length $ take choicesToShow $ finalMatches choice
    hSetCursorColumn tty 0
    saneTty
    hPutStr tty "\n"
    case finalMatches choice `atMay` matchIndex choice of
        Just match -> putStrLn match >> exitSuccess
        Nothing -> exitFailure

abort :: Handle -> IO ()
abort tty = do
    {-hCursorUp tty 1-}
    hSetCursorColumn tty 0
    writeLines tty [("", Reset)]
    saneTty -- duplicated in writeSelection
    {-hPutStr tty "\n"-}
    exitFailure

handleInput :: Char -> Search -> Int -> Action
handleInput inputChar search currMatchCount =
    case charToKeypress inputChar of
        CtrlC -> Abort
        Enter -> MakeChoice Choice {
                              finalMatches = matches (query search)
                                                     (choices search)
                            , matchIndex = selection search
                            }
        Backspace   -> NewSearch search { query = dropLast $ query search
                                        , selection = 0 }
        PlainChar c -> NewSearch search { query = query search ++ [c]
                                        , selection = 0 }
        CtrlN -> NewSearch search { selection = min (selection search + 1)
                                                    (currMatchCount - 1) }
        CtrlP -> NewSearch search { selection = max (selection search - 1) 0 }
        Invisible -> Ignore


main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty
    initialChoices <- liftM lines getContents

    (winHeight, _) <- unsafeSize tty
    let choicesToShow = min 20 (winHeight - 1)

    -- create room for choices and query line
    replicateM_ (choicesToShow + 1) $ hPutStr tty "\n"
    hCursorUp tty (choicesToShow + 1)

    let initSearch = Search { query="", choices=initialChoices, selection=0 }
    draw tty $ render initSearch choicesToShow

    -- maybe this should return Abort | Quit?, ExitSuccess + sanetty duplicated
    _ <- eventLoop tty initSearch 0 choicesToShow

    hClose tty
  where
    eventLoop :: Handle -> Search -> Int -> Int -> IO ()
    eventLoop tty search currMatchCount choicesToShow = do
      x <- hGetChar tty
      case handleInput x search currMatchCount of
          Abort -> abort tty
          MakeChoice c -> writeSelection tty c choicesToShow
          Ignore -> eventLoop tty search currMatchCount choicesToShow
          NewSearch newSearch -> do
              let rendered = render newSearch choicesToShow
              draw tty rendered
              eventLoop tty newSearch (matchCount rendered) choicesToShow
