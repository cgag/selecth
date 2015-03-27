{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.ST

import qualified Data.ByteString              as B
import           Data.Char                    (isPrint, isSpace)
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

import Debug.Trace

{- TODO: implement something like withTty that handles restoring tty state -}
{- TODO: getting unweildy passing around currMatchCount and choicesToShow-}
{- TODO: look into resource monad for ensuring tty gets closed? -}
{- TODO: state monad? passing around matches and memo tables is cumbersome -}

prompt :: Text
prompt = "> "

data KeyPress = CtrlC
              | CtrlN
              | CtrlP
              | CtrlW
              | CtrlH
              | CtrlU
              | Enter
              | Invisible
              | Backspace
              | PlainChar Char

type Memo = M.Map Text (Vector Text)

data Search = Search
    { query     :: !Text
    , choices   :: !(Vector Text)
    , selection :: !Int
    , matches   :: !(Vector Text)
    } deriving Show

data SelecthState = SelecthState
    { s_search            :: !Search
    , s_choicesToShow     :: !Int
    , s_scoreMemo :: Memo
    } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: !Text
    , renderedLines :: !(Vector (Text, SGR))
    } deriving Show

data Choice = Choice
    { finalMatches :: !(Vector Text)
    , matchIndex   :: !Int
    }

data Action       = SearchAction SearchAction | ExitAction ExitAction

data SearchAction = Extend Text
                  | DropWord
                  | DropChar
                  | Clear
                  | SelectUp
                  | SelectDown
                  | Ignore

data ExitAction   = Abort
                  | MakeChoice Search

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL', Backspace)
                          , ('\BS',  CtrlH)
                          , ('\NAK',  CtrlU)
                          , ('\SO',  CtrlN)
                          , ('\DLE', CtrlP)
                          , ('\ETB', CtrlW)
                          ]

charToKeypress :: Char -> KeyPress
charToKeypress c = fromMaybe (if isPrint c then PlainChar c else Invisible)
                             (M.lookup c specialChars)

findMatches :: Memo -> Text -> Vector Text -> (Vector Text, Memo)
findMatches memo qry chs =
    case M.lookup qry memo of
       Just mtchs -> (mtchs, memo)
       Nothing -> (getMatches qry chs, M.insert qry (getMatches qry chs) memo)
  where
    getMatches q cs = V.map fst
                        . sortImmutableVec (flip compare `on` snd)
                        . V.filter (\(_,cScore) -> cScore > 0)
                        $ scoreAll q cs

    sortImmutableVec f v = runST $ do
                              vec <- V.thaw v
                              VI.sortBy f vec
                              V.freeze vec

  -- memo' = case M.lookup (query newSearch) memo of
  --           Just _  -> memo
  --           Nothing -> M.insert (query newSearch)
  --                               (findMatches (query newSearch)
  --                                            (matches srch))
  --                               memo
  -- matched = fromMaybe (error "Memoization is broken")
  --                     (M.lookup (query newSearch) memo')
  -- newSearch' = newSearch { matches = matched }

render :: Search -> Int -> RenderedSearch
render (Search {query=q, matches=matched, selection=selIndex}) csToShow =
    RenderedSearch { queryString   = queryLine
                   , renderedLines = V.cons (queryLine, Reset)
                                            renderedMatchLines
                   }
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
-- dropLastWord = T.reverse . T.dropWhile (/= ' ') . T.reverse
dropLastWord = T.stripEnd . T.dropWhileEnd (not . isSpace)

writeSelection :: Handle -> Search -> Int -> IO ()
writeSelection tty (Search {matches=matches', selection=sel}) csToShow = do
    hCursorDown tty . V.length . V.take csToShow $ matches'
    T.hPutStr tty "\n"
    case matches' V.!? sel of
        Just match -> T.putStrLn match
        Nothing -> error "Failed to write selection."

handleInput :: Char -> Search -> Action
handleInput inputChar search =
    case charToKeypress inputChar of
        Enter     -> ExitAction (MakeChoice search)
        CtrlC     -> ExitAction Abort
        CtrlN     -> SearchAction SelectDown
        CtrlP     -> SearchAction SelectUp
        CtrlU     -> SearchAction Clear
        CtrlW     -> SearchAction DropWord
        CtrlH     -> SearchAction DropChar
        Backspace -> SearchAction DropChar
        Invisible -> SearchAction Ignore
        PlainChar c -> SearchAction $ Extend (T.singleton c)

buildSearch ::SearchAction -> Search -> Int -> Memo -> (Search, Memo)
buildSearch action search choicesToShow memo =
    case action of
        Extend qAddition -> (search', memo')
          where
            search' = search { query   = q'
                             , matches = matches'
                             , selection = 0 }
            q' = query search <> qAddition
            (matches', memo') = findMatches memo q' (matches search)
        DropWord -> ( search { query = q'
                            , matches = fromMemo q' memo
                            , selection = 0}
                    , memo)
          where q' = dropLastWord (query search)
        DropChar -> (search { query = q'
                            , matches = fromMemo q' memo
                            , selection = 0 }
                    , memo)
          where q' = dropLast (query search)
        Clear -> (search { query = "", selection = 0 }, memo)
        SelectDown -> ( search { selection = mod (selection search + 1)
                                                 (min choicesToShow
                                                      (V.length (matches search))) }
                      , memo)
        SelectUp -> (search { selection = mod (selection search - 1)
                                              (min choicesToShow
                                                   (V.length (matches search))) }
                    , memo)
        Ignore -> (search, memo)
  where
    fromMemo q m = case M.lookup q m of
                     Nothing -> trace ("memo error on : " <> T.unpack q) $ error "Memoization error"
                     Just ms -> ms


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

    let (initMatches, initMemo) = (findMatches M.empty "" initialChoices)
    let initSearch = Search { query=""
                            , choices=initialChoices
                            , selection=0
                            , matches=initMatches
                            }

    let rendered = render initSearch choicesToShow
    draw tty rendered

    eventLoop tty SelecthState { s_search = initSearch
                               , s_choicesToShow = choicesToShow
                               , s_scoreMemo = initMemo
                               }
 where
    eventLoop :: Handle -> SelecthState -> IO ()
    eventLoop tty (SelecthState srch csToShow memo) = do
      x <- hGetChar tty
      case handleInput x srch of
          ExitAction eaction -> do
              hSetCursorColumn tty 0
              saneTty
              case eaction of
                  Abort -> hCursorDown tty (V.length (matches srch) + 1)
                           >> hClose tty
                           >> exitFailure
                  MakeChoice c -> writeSelection tty c csToShow
                                  >> hClose tty
                                  >> exitSuccess
          SearchAction saction -> do
              let (search', memo') = buildSearch saction srch csToShow memo
              draw tty (render search' csToShow)
              eventLoop tty (SelecthState search'
                                          csToShow
                                          memo')
