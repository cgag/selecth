{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import           Data.Char           (isPrint)
import           Data.Function       (on)
import           Data.List           (sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Safe                (atMay)

import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (Handle, IOMode (..), hClose, hGetChar,
                                      hPutStr, openFile)

import           System.Console.ANSI

import           Score
import           Tty

{- TODO: implement soething like withTty that handles restoring tty state -}
{- TODO: getting unweildy passing around currMatchCount and choicesToShow-}
{-TODO: look into resource monad for ensuring tty gets closed? -}

data KeyPress = CtrlC | CtrlN | CtrlP | CtrlW | Enter
                | Invisible | Backspace | PlainChar Char

data Search = Search
    { query     :: String
    , choices   :: [String]
    , selection :: Int
    } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: String
    , renderedLines :: [(String, SGR)]
    , matchCount    :: Int
    }

data Choice = Choice
    { finalMatches :: [String]
    , matchIndex   :: Int
    }

data Action       = SearchAction SearchAction | ExitAction ExitAction
data SearchAction = NewSearch Search | Ignore
data ExitAction   = Abort | MakeChoice Choice

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL', Backspace)
                          , ('\SO',  CtrlN)
                          , ('\DLE', CtrlP)
                          , ('\ETB', CtrlW)]

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
                                    if i == selIndex && m /= " " -- don't highlight pad line
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

dropLastWord :: String -> String
dropLastWord = reverse . dropWhile (/=' ') . reverse

writeSelection :: Handle -> Choice -> Int -> IO ()
writeSelection tty choice choicesToShow = do
    hCursorDown tty $ length $ take choicesToShow $ finalMatches choice
    hPutStr tty "\n"
    case finalMatches choice `atMay` matchIndex choice of
        Just match -> putStrLn match >> exitSuccess
        Nothing -> exitFailure

handleInput :: Char -> Search -> Int -> Action
handleInput inputChar search currMatchCount =
    case charToKeypress inputChar of
        CtrlC -> ExitAction Abort
        Enter -> ExitAction $ MakeChoice Choice {
                                finalMatches = matches (query search)
                                                       (choices search)
                              , matchIndex = selection search
                              }
        Backspace   -> SearchAction $ NewSearch search
            { query = dropLast $ query search
            , selection = 0 }
        PlainChar c -> SearchAction $ NewSearch search
            { query = query search ++ [c]
            , selection = 0 }
        CtrlN -> SearchAction $ NewSearch search
            { selection = min (selection search + 1)
                              (currMatchCount - 1) }
        CtrlP -> SearchAction $ NewSearch search
            { selection = max (selection search - 1) 0 }
        CtrlW -> SearchAction $ NewSearch search
            { query = dropLastWord (query search)
            , selection = 0 }
        Invisible -> SearchAction Ignore

{-withTty :: (Handle -> IO ()) -> IO ()-}
{-withTty f = do -}
    {-tty <- openFile "/dev/tty" ReadWriteMode-}
    {-configureTty tty-}
    {-f tty-}
    {-saneTty-}

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

    eventLoop tty initSearch 0 choicesToShow

  where
    eventLoop :: Handle -> Search -> Int -> Int -> IO ()
    eventLoop tty search currMatchCount choicesToShow = do
      x <- hGetChar tty
      case handleInput x search currMatchCount of
          ExitAction eaction -> do
            hSetCursorColumn tty 0
            saneTty
            case eaction of
                Abort -> hClose tty >> exitFailure
                MakeChoice c -> writeSelection tty c choicesToShow
            hClose tty
          SearchAction saction -> do
              let newSearch = case saction of
                                  Ignore -> search
                                  NewSearch s -> s
              let rendered = render newSearch choicesToShow
              draw tty rendered
              eventLoop tty newSearch (matchCount rendered) choicesToShow
