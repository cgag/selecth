{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.ST

import qualified Data.ByteString              as B
import           Data.Char                    (isPrint)
import           Data.Function                (on)
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Encoding.Error     as Err
import           Data.Text.IO                 as T
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VI

import           System.Exit                  (exitFailure, exitSuccess)
import           System.IO                    (Handle, IOMode (..), hClose,
                                               hGetChar, openFile)

import           System.Console.ANSI

import           Score
import           Tty

{- TODO: implement something like withTty that handles restoring tty state -}
{- TODO: getting unweildy passing around currMatchCount and choicesToShow-}
{- TODO: look into resource monad for ensuring tty gets closed? -}

prompt :: Text
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
    { query     :: !Text
    , choices   :: !(Vector Text)
    , selection :: !Int
    , matches   :: !(Vector Text)
    } deriving Show

data SelecthState = SelecthState
    { s_search            :: !Search
    , s_choicesToShow     :: !Int
    , s_currentMatchCount :: !Int
    , s_scoreMemo :: M.Map Text (Vector Text)
    } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: !Text
    , renderedLines :: !(Vector (Text, SGR))
    , matchCount    :: !Int
    }

data Choice = Choice
    { finalMatches :: !(Vector Text)
    , matchIndex   :: !Int
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
                          , ('\ETB', CtrlW)
                          ]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (if isPrint c then PlainChar c else Invisible)
                             (M.lookup c specialChars)

findMatches :: Text -> Vector Text -> Vector Text
findMatches qry chs = V.map fst
                  . sortImmutableVec (flip compare `on` snd)
                  . V.filter (\(_,cScore) -> cScore > 0)
                  $ scoreAll qry chs
  where
    sortImmutableVec f v = runST $ do
                              vec <- V.thaw v
                              VI.sortBy f vec
                              V.freeze vec

render :: Search -> Int -> RenderedSearch
render (Search {query=q, matches=matched, selection=selIndex}) csToShow =
    RenderedSearch { queryString   = queryLine
                   , renderedLines = V.cons (queryLine, Reset)
                                            renderedMatchLines
                   , matchCount    = V.length matched }
  where
    renderedMatchLines = swapBackground selIndex
                           . V.map (\m -> (m, Reset))
                           $ matchLines
    matched'    = V.take csToShow matched
    matchLines = pad csToShow matched'
    queryLine  = prompt <> q
    swapBackground i vec =
      V.update
          vec
          (V.singleton
              (selIndex, (fst (vec V.! i), SetSwapForegroundBackground True)))
    pad n xs = xs <> V.replicate (n - V.length xs) " "

draw :: Handle -> RenderedSearch -> IO ()
draw tty rendered = do
    withCursorHidden tty $
        writeLines tty (renderedLines rendered)
    hSetCursorColumn tty . T.length $ queryString rendered

-- TODO: Handle exceptions, ensure tty gets restored to default settings
-- TODO: Ensure handle to tty is closed?  See what GB ensures.
{-restoreTTY :: IO ()-}

dropLast :: Text -> Text
dropLast = T.dropEnd 1

dropLastWord :: Text -> Text
dropLastWord = T.reverse . T.dropWhile (/= ' ') . T.reverse

writeSelection :: Handle -> Choice -> Int -> IO ()
writeSelection tty choice csToShow = do
    hCursorDown tty . V.length . V.take csToShow $ finalMatches choice
    T.hPutStr tty "\n"
    case finalMatches choice V.!? matchIndex choice of
        Just match -> T.putStrLn match
        Nothing -> error "Failed to write selection."

handleInput :: Char -> Search -> Int -> Action
handleInput inputChar search choicesToShow =
    case charToKeypress inputChar of
        CtrlC -> ExitAction Abort
        Enter -> ExitAction $ MakeChoice Choice {
                                finalMatches = findMatches (query search)
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
            { selection = mod (selection search + 1) choicesToShow }
        CtrlP -> SearchAction $ NewSearch search
            { selection = mod (selection search - 1) choicesToShow}
        -- CtrlW -> SearchAction $ NewSearch search
        --     { query = dropLastWord (query search)
        --     , selection = 0 }
        Invisible -> SearchAction Ignore

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty

    (winHeight, _) <- unsafeSize tty
    let choicesToShow = min 20 (winHeight - 1)

    -- TODO: can't quit if there's nothing on stdin
    contents <- B.getContents
    let initialChoices = V.fromList . T.lines . TE.decodeUtf8With Err.ignore $ contents

    -- create room for choices and query line
    let linesToDraw = choicesToShow + 1

    -- TODO: this should probably be in Tty module
    replicateM_ linesToDraw $ T.hPutStr tty "\n"
    hCursorUp tty linesToDraw

    let initSearch = Search { query=""
                            , choices=initialChoices
                            , selection=0
                            , matches=(findMatches "" initialChoices)}
        rendered = render initSearch choicesToShow
    draw tty rendered

    eventLoop tty SelecthState { s_search = initSearch
                               , s_currentMatchCount = matchCount rendered
                               , s_choicesToShow = choicesToShow
                               , s_scoreMemo = M.empty}
  where
    eventLoop :: Handle -> SelecthState -> IO ()
    eventLoop tty (SelecthState srch csToShow currMatchCount memo) = do
      x <- hGetChar tty
      case handleInput x srch csToShow of
          ExitAction eaction -> do
              hSetCursorColumn tty 0
              saneTty
              case eaction of
                  Abort -> hCursorDown tty (currMatchCount + 1)
                           >> hClose tty
                           >> exitFailure
                  MakeChoice c -> writeSelection tty c csToShow
                                  >> hClose tty
                                  >> exitSuccess
          SearchAction saction -> do
              let newSearch = case saction of
                                  Ignore -> srch
                                  NewSearch s -> s
                  -- results of the last search become choices for this search
                  memo' = case M.lookup (query newSearch) memo of
                            Just _  -> memo
                            Nothing -> M.insert (query newSearch)
                                                (findMatches (query newSearch)
                                                             (matches srch))
                                                memo
                  matched = fromMaybe (error "Memoization is broken")
                                      (M.lookup (query newSearch) memo')
                  newSearch' = newSearch { matches = matched }
                  rendered = render newSearch' csToShow
              draw tty rendered
              eventLoop tty (SelecthState newSearch'
                                          csToShow
                                          (matchCount rendered)
                                          memo')
