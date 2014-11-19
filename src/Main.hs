{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import qualified Data.ByteString          as B
import           Data.Char                (isPrint)
import           Data.Function            (on)
import           Data.List                (sortBy)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.IO             as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as Err
import           Safe                     (atMay)

import           System.Exit              (exitFailure, exitSuccess)
import           System.IO                (Handle
                                          , IOMode (..)
                                          , hClose
                                          , hGetChar
                                          , openFile)

import           System.Console.ANSI

import           Score
import           Tty


{- TODO: implement something like withTty that handles restoring tty state -}
{- TODO: getting unweildy passing around currMatchCount and choicesToShow-}
{- TODO: look into resource monad for ensuring tty gets closed? -}

prompt :: T.Text
prompt = "> "

data KeyPress = CtrlC
              | CtrlN
              | CtrlP
              | CtrlW
              | Enter
              | Invisible
              | Backspace
              | PlainChar Char

data Search = Search
    { query     :: T.Text
    , choices   :: [T.Text]
    , selection :: Int
    } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: Text
    , renderedLines :: [(Text, SGR)]
    , matchCount    :: Int
    }

data Choice = Choice
    { finalMatches :: [T.Text]
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

matches :: T.Text -> [T.Text] -> [T.Text]
matches qry chs = map fst
                  . filter (\(_,cScore) -> cScore > 0)
                  . sortBy (flip compare `on` snd)
                  $ scoreAll qry chs

render :: Search -> Int -> RenderedSearch
render (Search {query=q, choices=cs, selection=selIndex}) choicesToShow =
    let queryLine  = prompt <> q
        matched    = take choicesToShow $ matches q cs
        matchLines = pad choicesToShow matched
        renderedMatchLines = map (\(m, i) ->
                                    -- don't highlight pad line
                                    if i == selIndex && m /= " "
                                    then (m, SetSwapForegroundBackground True)
                                    else (m, Reset))
                             $ zip matchLines [0..]
    in RenderedSearch { queryString   = queryLine
                      , renderedLines = (queryLine, Reset) : renderedMatchLines
                      , matchCount    = length matched }
  where
    pad n xs = xs ++ replicate (n - length xs) " "

draw :: Handle -> RenderedSearch -> IO ()
draw tty rendered = do
    withCursorHidden tty $
        writeLines tty (renderedLines rendered)
    hSetCursorColumn tty . T.length $ queryString rendered

-- TODO: Handle exceptions, ensure tty gets restored to default settings
-- TODO: Ensure handle to tty is closed?  See what GB ensures.
{-restoreTTY :: IO ()-}

dropLast :: T.Text -> T.Text
dropLast = T.dropEnd 1

dropLastWord :: T.Text -> T.Text
dropLastWord = T.reverse . T.dropWhile (/= ' ') . T.reverse

writeSelection :: Handle -> Choice -> Int -> IO ()
writeSelection tty choice choicesToShow = do
    hCursorDown tty . length . take choicesToShow $ finalMatches choice
    T.hPutStr tty "\n"
    case finalMatches choice `atMay` matchIndex choice of
        Just match -> T.putStrLn match
        Nothing -> error "Failed to write selection."

handleInput :: Char -> Search -> Int -> Action
handleInput inputChar search currMatchCount =
    case charToKeypress inputChar of
        CtrlC -> ExitAction Abort
        Enter -> ExitAction $ MakeChoice Choice {
                                finalMatches = matches (query search)
                                                       (choices search)
                              , matchIndex = selection search
                              }
        Backspace -> SearchAction $ NewSearch search
            { query = dropLast $ query search
            , selection = 0 }
        PlainChar c -> SearchAction $ NewSearch search
            { query = query search <> T.singleton c
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
    -- TODO: can't quit if there's nothing on stdin
    contents <- B.getContents
    let initialChoices = T.lines . TE.decodeUtf8With Err.ignore $ contents

    (winHeight, _) <- unsafeSize tty
    let choicesToShow = min 20 (winHeight - 1)

    -- create room for choices and query line
    let linesToDraw = choicesToShow + 1

    -- TODO: this should probably be in Tty module
    replicateM_ linesToDraw $ T.hPutStr tty "\n"
    hCursorUp tty linesToDraw

    let initSearch = Search { query="", choices=initialChoices, selection=0 }
    draw tty $ render initSearch choicesToShow

    eventLoop tty initSearch choicesToShow choicesToShow

  where
    eventLoop :: Handle -> Search -> Int -> Int -> IO ()
    eventLoop tty search currMatchCount choicesToShow = do
      x <- hGetChar tty
      case handleInput x search currMatchCount of
          ExitAction eaction -> do
              hSetCursorColumn tty 0
              saneTty
              case eaction of
                  Abort -> hCursorDown tty (currMatchCount + 1)
                           >> hClose tty
                           >> exitFailure
                  MakeChoice c -> writeSelection tty c choicesToShow
                                  >> hClose tty
                                  >> exitSuccess
          SearchAction saction -> do
              let newSearch = case saction of
                                  Ignore -> search
                                  NewSearch s -> s
              let rendered = render newSearch choicesToShow
              draw tty rendered
              eventLoop tty newSearch
                            (matchCount rendered)
                            choicesToShow
