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
                                               openFile, hGetChar, hReady)

import           System.Console.ANSI

import           Score
import           Tty

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
    }

instance Show Search where
  show _ = "[search]"

data SelecthState = SelecthState
    { s_search        :: !Search
    , s_choicesToShow :: !Int
    , s_scoreMemo     :: !Memo
    } deriving Show

data RenderedSearch = RenderedSearch
    { queryString   :: !Text
    , renderedLines :: !(Vector (Text, SGR))
    } deriving Show


data Action = SearchAction !SearchAction | ExitAction !ExitAction
  deriving Show


data SearchAction = Extend !Text
                  | DropWord
                  | DropChars !Int
                  | Clear
                  | SelectPrev
                  | SelectNext
                  | Ignore
  deriving Show

data ExitAction   = Abort
                  | MakeChoice !Search
  deriving Show

specialChars :: M.Map Char KeyPress
specialChars = M.fromList [ ('\ETX', CtrlC)
                          , ('\r',   Enter)
                          , ('\DEL', Backspace)
                          , ('\BS',  CtrlH)
                          , ('\NAK', CtrlU)
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
       Nothing    -> (newMatches, M.insert qry newMatches memo)
  where
    newMatches = getMatches qry chs
    getMatches q cs = V.map fst
                        . sortImmutableVec (flip compare `on` snd)
                        . V.filter (\(_,cScore) -> cScore > 0)
                        $ scoreAll q cs
    sortImmutableVec f v = runST $ do
                              vec <- V.thaw v
                              VI.sortBy f vec
                              V.freeze vec

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
    matched'   = V.take csToShow matched
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

dropLastWord :: Text -> Text
dropLastWord = T.stripEnd . T.dropWhileEnd (not . isSpace)

-- TODO: ugh, this should indeed have used choices to show,
-- there can easily be 100s of matches.
writeSelection :: Handle -> Search -> IO ()
writeSelection tty (Search {matches=matches', selection=sel}) = do
    hCursorDown tty (V.length matches')
    T.hPutStr tty "\n"
    T.putStrLn $ fromMaybe (error "Failed to write selection.")
                           (matches' V.!? sel)

handleInput :: Text -> Search -> [Action]
handleInput inputText search = T.foldr buildActions [] inputText
  where
    buildActions c xs = actionFor c : xs
    actionFor c =
      case charToKeypress c of
          Enter     -> ExitAction (MakeChoice search)
          CtrlC     -> ExitAction Abort
          CtrlN     -> SearchAction SelectNext
          CtrlP     -> SearchAction SelectPrev
          CtrlU     -> SearchAction Clear
          CtrlW     -> SearchAction DropWord
          CtrlH     -> SearchAction (DropChars 1)
          Backspace -> SearchAction (DropChars 1)
          Invisible -> SearchAction Ignore
          PlainChar char -> SearchAction $ Extend (T.singleton char)

collapseActions :: [Action] -> [Action]
collapseActions = foldr collapse []
  where
    collapse (SearchAction (Extend t)) (SearchAction (Extend t'):as) =
        SearchAction (Extend (t <> t')) : as
    collapse (SearchAction (DropChars n)) (SearchAction (DropChars m):as) =
        SearchAction (DropChars $ n + m) : as
    collapse nextA as = nextA : as

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
        DropWord    -> memoSearch $ dropLastWord (query search)
        DropChars n -> memoSearch $ T.dropEnd n (query search)
        Clear       -> if T.null (query search)
                       then (search, memo) -- why isn't this being hit?
                       else memoSearch ""
        Ignore     -> (search, memo)
        SelectNext -> moveSel 1
        SelectPrev -> moveSel (-1)
  where
    memoSearch q =  let (matches', memo') = findMatches memo q (choices search)
                    in  (search { query = q
                                , matches = matches'
                                , selection = 0}
                        , memo')
    moveSel n = ( search { selection = if possibleSelections > 0
                                       then mod (selection search + n)
                                                possibleSelections
                                       else selection search }
                , memo)
    possibleSelections = min choicesToShow (V.length (matches search))

main :: IO ()
main = do
    tty <- openFile "/dev/tty" ReadWriteMode
    configureTty tty

    (winHeight, _) <- unsafeSize tty
    let choicesToShow = min 20 (winHeight - 1)

    -- TODO: can't quit if there's nothing on stdin
    contents <- B.getContents
    let initialChoices = V.fromList
                         . T.lines
                         . TE.decodeUtf8With Err.ignore
                         $ contents

    -- create room for choices and query line
    let linesToDraw = choicesToShow + 1

    -- TODO: this should probably be in Tty module
    replicateM_ linesToDraw $ T.hPutStr tty "\n"
    hCursorUp tty linesToDraw

    let (initMatches, initMemo) = findMatches M.empty "" initialChoices
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
      x <- getBuffered tty
      let actions = collapseActions (handleInput (T.pack x) srch)
      forM_ actions $ \action ->
        case action of
            SearchAction saction -> do
                let (search', memo') = buildSearch saction srch csToShow memo
                draw tty (render search' csToShow)
                eventLoop tty (SelecthState search' csToShow memo')
            ExitAction eaction -> do
                hSetCursorColumn tty 0
                saneTty
                case eaction of
                    Abort -> hCursorDown tty (V.length (matches srch) + 1)
                             >> hClose tty
                             >> exitFailure
                    MakeChoice s -> writeSelection tty s
                                    >> hClose tty
                                    >> exitSuccess

    getBuffered :: Handle -> IO String
    getBuffered h = do
      x <- hGetChar h
      ready <- hReady h
      if ready
         then do
           y <- getBuffered h
           return (x : y)
         else return [x]
