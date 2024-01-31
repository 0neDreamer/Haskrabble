{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Char
import Data.List
import Data.Array
import Data.Maybe
import System.Random
import System.IO
import System.Random.Shuffle
import Control.Monad.State
import GHC.Utils.Misc (capitalise)
import System.Console.ANSI (clearScreen)


import Network.HTTP.Conduit (simpleHttp, parseUrlThrow, responseBody, Request, Response)
import Data.Aeson (FromJSON (..), decode, withObject, (.:), Value (Object), FromJSONKeyFunction (FromJSONKeyValue))
import Control.Exception (catch, SomeException (SomeException), try, displayException)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Simple ( httpLBS )
import Control.Monad.Except


type Count           = Int
type Id              = Int
type Points          = Int
type Row             = [Letter]
type Col             = Int
type Board           = [Row]
type BonusRow        = [Bonus]
type BonusBoard      = [BonusRow]
type WordAttempt     = [Letter]
type LetterPosition  = (Letter, Position)
type LetterPositions = [LetterPosition]
type BrokenLetPos    = (LetterPositions, LetterPositions)
type Position        = (Int, Int)
type Positions       = [Position]
type Placement       = (Position, Position)
type Move            = (WordAttempt, Placement)
type WordPoints      = [Points]
type Tiles           = [(Letter, Id)]


type Scrabble a = StateT Game IO a


data Game = MkGame {
    gameKey            :: Int
  , gameState          :: GameState
  , gameBoard          :: Board
  , possibleGameBoard  :: Board
  , bonusBoard         :: BonusBoard
  , possibleBonusBoard :: BonusBoard
  , possibleTiles      :: Tiles
  , possibleWord       :: LetterPositions
  , checkWord          :: String
  , checkWordValid     :: WordState
  , gameTilesBag       :: Tiles
  , playerTurn         :: Player
  , p1GameTiles        :: Tiles
  , p2GameTiles        :: Tiles
  , p1UsedTiles        :: Tiles
  , p2UsedTiles        :: Tiles
  , p1GameWords        :: [(LetterPositions,String)]
  , p2GameWords        :: [(LetterPositions,String)]
  , p1Score            :: Int
  , p2Score            :: Int
  , p1LastMove         :: LastPlay
  , p2LastMove         :: LastPlay
} deriving Show


setGame :: Game
setGame = MkGame {
    gameKey            = 0
  , gameState          = R7
  , gameBoard          = [[]] -- _TESTBOARD_
  , possibleGameBoard  = [[]] -- _TESTBOARD_
  , bonusBoard         = _BONUSBOARD_
  , possibleBonusBoard = _BONUSBOARD_
  , possibleTiles      = []
  , possibleWord       = []
  , checkWord          = ""
  , checkWordValid     = NetworkError
  , gameTilesBag       = []
  , playerTurn         = NP
  , p1GameTiles        = []
  , p2GameTiles        = []
  , p1UsedTiles        = []
  , p2UsedTiles        = []
  , p1GameWords        = [([],"")]
  , p2GameWords        = [([],"")]
  , p1Score            = 0
  , p2Score            = 0
  , p1LastMove         = None
  , p2LastMove         = None
}


data Letter =
  A | B | C | D | E | F | G | H | I | J | K | L | M | Home |
  N | O | P | Q | R | S | T | U | V | W | X | Y | Z | Blank | Empty | Inv
  deriving (Show, Eq)

data Bonus =
  FP | W3 | W2 | L3 | L2 | NB
  deriving (Show, Eq)

data Player = P1 | P2 | NP
  deriving (Show, Eq)

data Direction = DH | DV | ND | WW
  deriving (Show, Eq)

data LastPlay = None | Play | Skip | Challenge | Quit
  deriving (Show, Eq)

data GameState = R1 | R2 | R3 | R4 | R5 | R6 | R7 | QT
  deriving (Show, Eq)

data WordState = IsWord | NotWord | NetworkError
  deriving (Show, Eq)

putStrIO :: String -> Scrabble ()
putStrIO s = liftIO (putStr s)

putStrLnIO :: String -> Scrabble ()
putStrLnIO s = liftIO (putStrLn s)

printIO :: Show a => a -> Scrabble ()
printIO s = liftIO (print s)

getLineIO :: Scrabble String
getLineIO = liftIO getLine


-- #################################################################################################
-- This Section is for formating and printing
-- #################################################################################################
_SIZE_ :: Int
_SIZE_  = 15

_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_ - 1]

_INVALID_MOVE_ :: (WordAttempt, Placement)
_INVALID_MOVE_ = ([Inv], ((-1, -1), (-1,-1)))

_NO_MOVE_ :: (WordAttempt, Placement)
_NO_MOVE_ = ([],((0,0),(0,0)))

_HEADER_ :: String
_HEADER_ =  "  | 0  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 | 11 | 12 | 13 | 14 |"

_SEP_ :: String
_SEP_  = "  | "

_SEPL_ :: String
_SEPL_ = " | "

_SEPR_ :: String
_SEPR_ = "  |"

_ALPHABET_ :: [Char]
_ALPHABET_ = ['A' .. 'Z'] <> ['a' .. 'z']

_NUMBER_ :: [Int]
_NUMBER_ = [0 .. 9]

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (\x y -> [x] ++ y) ['A' .. ] s

formatLine :: [String] -> String
formatLine x = concat [_SEPL_, intercalate _SEP_ x, _SEPR_]

formatRows :: [Row] -> [String]
formatRows r = map (\x -> formatLine . lettersToStringConvert $ x) r

formatBoard :: Board -> String
formatBoard b = unlines $ _HEADER_ : (prependRowIndices . formatRows $ b)

printBoard :: Board -> Scrabble ()
printBoard b = do
  putStrLnIO $ formatBoard b

formatBonusRows :: [BonusRow] -> [String]
formatBonusRows r = map (\x -> formatLine . bonusToStringConvert $ x) r

formatBonusBoard :: BonusBoard -> String
formatBonusBoard b = unlines $ _HEADER_ : (prependRowIndices . formatBonusRows $ b)

printBonusBoard :: BonusBoard -> Scrabble ()
printBonusBoard b = do
  putStrLnIO $ formatBonusBoard b

_LOGO_Start_ :: String
_LOGO_Start_ = "logostart.txt"

_LOGO_End_ :: String
_LOGO_End_ = "logoend.txt"

printLogo :: String -> IO ()
printLogo p = readFile p >>= \f -> putStrLn f

-- ########################################################################################################
-- This Section is what applies the game logic (i.e. work plays, skips, challenges, forfeited turns, etc.)
-- ########################################################################################################
getMove :: Board -> Scrabble ()
getMove b = do
    game' <- get
    let currentGameState = getGameState game'
    gt <- liftIO $ shakeTileBag $ gameTilesBag game'
    modify (\game -> game { gameState = currentGameState, gameTilesBag = gt })
    game <- get

    if gameState game == R6 || gameState game == R7 then do
        putStrLnIO $ showGameState $ gameState game
        putStrLnIO "\n"
        printBoard $ possibleGameBoard game

        let player = playerTurn game
        let gameTiles = if player == P1 then p1GameTiles game else p2GameTiles game
        let usedTiles = if player == P1 then p1UsedTiles game else p2UsedTiles game
        let gameWords = if player == P1 then p1GameWords game else p2GameWords game
        let score     = if player == P1 then p1Score game else p2Score game

        let playLetters = map ((\x -> (letterToCharConvert x) ++ "- pts: " ++ (show $ letterToPointConvert x) ++ ", ") . fst) gameTiles
        putStrIO $ "\nTotal tiles: " ++ show (length (gameTilesBag game) + length (p1UsedTiles game) + length (p1GameTiles game) + length (p2UsedTiles game) + length (p2GameTiles game))
        putStrLnIO $ "     Tiles remaining in Bag: " ++ show (length (gameTilesBag game))
        putStrIO "-----------------"
        putStrLnIO "    ----------------------------"
        putStrLnIO $ "Player1 score: " ++ show (p1Score game) ++ "     Tiles used by Player1: " ++ show (length (p1UsedTiles game) + length (p1GameTiles game))
        putStrLnIO $ "Words played: | " ++ concat (zipWith (++) (map (capitalise . snd) $ p1GameWords game) (replicate (length (p1GameWords game) - 1) " | "))
        putStrLnIO $ "\nPlayer2 score: " ++ show (p2Score game) ++ "     Tiles used by Player2: " ++ show (length (p2UsedTiles game) + length (p2GameTiles game))
        putStrLnIO $ "Words played: | " ++ concat (zipWith (++) (map (capitalise . snd) $ p2GameWords game) (replicate (length (p2GameWords game) - 1) " | "))
        putStrLnIO "-------------------------------------------------"
        putStrLnIO $ "\n" ++ playerPrompt player ++ ", your turn to create a word from these letters: "
        putStrLnIO $ concat playLetters

        if player == P1 && p2LastMove game == Play && gameKey game == 290 ||
          player == P2 && p1LastMove game == Play && gameKey game == 290 then
            putStrLnIO "\nEnter word (e.g. SCRABBLE), #SKIP, #CHALLENGE the previous play or #Q to Quit:\n"
        else putStrLnIO "\nEnter word (e.g. SCRABBLE) or #SKIP to forfeit turn and reselect new letters or #Q to Quit:\n"
        wrd <- map toUpper <$> getLineIO

        if wrd == "#Q" then do

            let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game

            if player == P1 && p2LastMove game == Play then do
                let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p2GameTiles game
                let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                let usedTilesNew    = possibleTiles game <> p2UsedTiles game
                modify (\game -> game { gameTilesBag = gameTilesBagNew
                                      , p2GameTiles = gameTilesNew
                                      , p2UsedTiles = usedTilesNew
                                      , p2GameWords = (possibleWord game, checkWord game) : p2GameWords game
                                      , p2Score = p2Score game + points })

            else if player == P2 && p1LastMove game == Play then do
                let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p1GameTiles game
                let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                let usedTilesNew    = possibleTiles game <> p1UsedTiles game
                modify (\game -> game { gameTilesBag = gameTilesBagNew
                                      , p1GameTiles = gameTilesNew
                                      , p1UsedTiles = usedTilesNew
                                      , p1GameWords = (possibleWord game, checkWord game) : p1GameWords game
                                      , p1Score = p1Score game + points })

            else do
                if player == P1 then modify (\game -> game { p1LastMove = Quit })
                else modify (\game -> game { p2LastMove = Quit })

            modify (\game -> game { gameState = QT
                                  , playerTurn = player
                                  , p1LastMove = Quit
                                  , p2LastMove = Quit })

        else if wrd == "#SKIP" then do
            putStrLnIO "Turn skipped. Reselecting your tiles from the bag.\n"
            modify (\game -> game { gameBoard = possibleGameBoard game, bonusBoard = possibleBonusBoard game })
            let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game

            if player == P1 && p2LastMove game == Play then do
                let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p2GameTiles game
                let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                let usedTilesNew    = possibleTiles game <> p2UsedTiles game
                modify (\game -> game { gameTilesBag = gameTilesBagNew
                                      , p2GameTiles = gameTilesNew
                                      , p2UsedTiles = usedTilesNew
                                      , p2GameWords = (possibleWord game, checkWord game) : p2GameWords game
                                      , p2Score = p2Score game + points
                                      , p1LastMove = Skip })

            else if player == P2 && p1LastMove game == Play then do
                let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p1GameTiles game
                let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                let usedTilesNew    = possibleTiles game <> p1UsedTiles game
                modify (\game -> game { gameTilesBag = gameTilesBagNew
                                      , p1GameTiles = gameTilesNew
                                      , p1UsedTiles = usedTilesNew
                                      , p1GameWords = (possibleWord game, checkWord game) : p1GameWords game
                                      , p1Score = p1Score game + points
                                      , p2LastMove = Skip})

            else do
                if player == P1 then modify (\game -> game { p1LastMove = Skip })
                else modify (\game -> game { p2LastMove = Skip })

            if player == P1 then do
                game <- get
                let gameTilesBagAdd = gameTilesBag game ++ p1GameTiles game
                gt <- liftIO $ shakeTileBag gameTilesBagAdd
                let newTiles = take 7 gt
                let newGameTilesBag = filter (`notElem` newTiles) gt
                modify (\game -> game { gameTilesBag = newGameTilesBag, p1GameTiles = newTiles })
            else do
                game <- get
                let gameTilesBagAdd = gameTilesBag game ++ p2GameTiles game
                gt <- liftIO $ shakeTileBag gameTilesBagAdd
                let newTiles = take 7 gt
                let newGameTilesBag = filter (`notElem` newTiles) gt
                modify (\game -> game { gameTilesBag = newGameTilesBag, p2GameTiles = newTiles })

        else if wrd == "#CHALLENGE" && player == P1 && p2LastMove game == Play && gameKey game == 290 ||
                wrd == "#CHALLENGE" && player == P2 && p1LastMove game == Play && gameKey game == 290 then do
            putStrLnIO "Checking Word...\n"
            checkIfScrabbleWord
            game <- get
            if checkWordValid game == IsWord then do
                let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game
                if player == P1 then do
                    let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p2GameTiles game
                    let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                    let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                    let usedTilesNew    = possibleTiles game <> p2UsedTiles game
                    modify (\game -> game { gameBoard = possibleGameBoard game
                                          , bonusBoard = possibleBonusBoard game
                                          , gameTilesBag = gameTilesBagNew
                                          , p2GameTiles = gameTilesNew
                                          , p2UsedTiles = usedTilesNew
                                          , p2GameWords = (possibleWord game, checkWord game) : p2GameWords game
                                          , p2Score = p2Score game + points
                                          , p1LastMove = Challenge})
                else do
                    let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p1GameTiles game
                    let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                    let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                    let usedTilesNew    = possibleTiles game <> p1UsedTiles game
                    modify (\game -> game { gameBoard = possibleGameBoard game
                                          , bonusBoard = possibleBonusBoard game
                                          , gameTilesBag = gameTilesBagNew
                                          , p1GameTiles = gameTilesNew
                                          , p1UsedTiles = usedTilesNew
                                          , p1GameWords = (possibleWord game, checkWord game) : p1GameWords game
                                          , p1Score = p1Score game + points
                                          , p2LastMove = Challenge})

                putStrLnIO $ map toUpper (checkWord game) ++ " is a scrabble word. You have lost your turn."

            else if checkWordValid game == NotWord then do
                putStrLnIO $ map toUpper (checkWord game) ++ " is not a scrabble word. The word has been removed and no points granted."

                if player == P1 then do
                    modify (\game -> game { possibleGameBoard = gameBoard game
                                          , p1LastMove = Challenge
                                          , p2LastMove = None})
                else do
                    modify (\game -> game { possibleGameBoard = gameBoard game
                                          , p2LastMove = Challenge
                                          , p1LastMove = None})

                getMove $ possibleGameBoard game

            else do
                putStrLnIO "Unable to retrieve data...\n"
                putStrLnIO "You can try #Challenge again.\n"
                getMove $ possibleGameBoard game

        else if any (`notElem` _ALPHABET_ ++ ['*']) wrd || null wrd then do
                putStrLnIO "Not a valid entry. Please try again.\n"
                getMove $ possibleGameBoard game

        else if any (`notElem` concatMap ((\c -> letterToCharConvert c) . fst) gameTiles) wrd then do
                putStrLnIO "Letters must be from your current tiles. Try again...\n"
                getMove $ possibleGameBoard game

        else do
              putStrLnIO "\nEnter start position and end position (e.g. H3 H10):"
              pos <- map toUpper <$> getLineIO
              if player == P1 && p2LastMove game == Play then do
                  let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game
                  let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p2GameTiles game
                  let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                  let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                  let usedTilesNew    = possibleTiles game <> p2UsedTiles game
                  modify (\game -> game { gameBoard = possibleGameBoard game
                                        , bonusBoard = possibleBonusBoard game
                                        , gameTilesBag = gameTilesBagNew
                                        , p2GameTiles = gameTilesNew
                                        , p2UsedTiles = usedTilesNew
                                        , p2GameWords = (possibleWord game, checkWord game) : p2GameWords game
                                        , p2Score = p2Score game + points})
              else if player == P2 && p1LastMove game == Play then do
                  let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game
                  let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p1GameTiles game
                  let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
                  let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
                  let usedTilesNew    = possibleTiles game <> p1UsedTiles game
                  modify (\game -> game { gameBoard = possibleGameBoard game
                                        , bonusBoard = possibleBonusBoard game
                                        , gameTilesBag = gameTilesBagNew
                                        , p1GameTiles = gameTilesNew
                                        , p1UsedTiles = usedTilesNew
                                        , p1GameWords = (possibleWord game, checkWord game) : p1GameWords game
                                        , p1Score = p1Score game + points})
              else do
                  if player == P1 then do
                      modify (\game -> game { p1LastMove = Play })
                  else do
                      modify (\game -> game { p2LastMove = Play })

              let move = stringToMove (stringToLettersConvert wrd) pos
              let lp = findOverlapAndDesignate (possibleGameBoard game) move
              let bu = breakUpLPs lp
              let word = extractWord (possibleGameBoard game) move
              let wordString = map toLower $ concat . lettersToStringConvert $ map (fst) word

              if isValidMove wrd wordString b $ stringToMove (stringToLettersConvert wrd) pos then do

                  if snd bu /= [] then do
                      updateBoard (fst bu)
                      updateBoard (snd bu)
                  else
                      updateBoard (fst bu)

                  updateBonusBoard $ map snd word

                 -- save new player moves
                  if player == P1 then do
                      let (matchedValues, remainingB) = matchAndRemove (map fst (fst lp)) (p1GameTiles game)
                      modify (\game -> game {possibleTiles = matchedValues, p1LastMove = Play})

                  else do
                      let (matchedValues, remainingB) = matchAndRemove (map fst (fst lp)) (p2GameTiles game)
                      modify (\game -> game {possibleTiles = matchedValues, p2LastMove = Play})


                  if any (`elem` ['*']) wrd then do
                      putStrLnIO $ "\nFill in the blank(s) for the word " ++ map toUpper wordString
                      let input = wordString
                      finalString <- map toLower <$> liftIO (searchAndReplace input)
                      putStrLnIO $ "Final word is: " ++ map toUpper finalString ++ "\n"
                      modify (\game -> game { possibleWord = word, checkWord = finalString })

                  else modify (\game -> game { possibleWord = word, checkWord = wordString })

              else do
                    putStrLnIO "\nInvalid move. Start and End points must match word length."
                    getMove $ possibleGameBoard game

        game <- get
        if p1LastMove game /= Quit && p2LastMove game /= Quit then do
            modify (\game -> game { playerTurn = switchPlayer player })
        else do
            modify (\game -> game { gameBoard = possibleGameBoard game, bonusBoard = possibleBonusBoard game })

        game <- get
        let currentGameState = getGameState game
        if currentGameState == R4 || currentGameState == R5 then
            putStrLnIO "The End"
        else getMove $ possibleGameBoard game

    else do

      let player = playerTurn game
      let gameTiles = if player == P1 then p1GameTiles game else p2GameTiles game
      let usedTiles = if player == P1 then p1UsedTiles game else p2UsedTiles game
      let gameWords = if player == P1 then p1GameWords game else p2GameWords game
      let score     = if player == P1 then p1Score game else p2Score game

      if player == P1 && p2LastMove game == Play then do
          let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game
          let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p2GameTiles game
          let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
          let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
          let usedTilesNew    = possibleTiles game <> p2UsedTiles game
          modify (\game -> game { gameBoard = possibleGameBoard game
                                , bonusBoard = possibleBonusBoard game
                                , gameTilesBag = gameTilesBagNew
                                , p2GameTiles = gameTilesNew
                                , p2UsedTiles = usedTilesNew
                                , p2GameWords = (possibleWord game, checkWord game) : p2GameWords game
                                , p2Score = p2Score game + points})

      else if player == P2 && p1LastMove game == Play then do
          let points = getWordValue (getBonusValues $ possibleWord game) $ possibleWord game
          let gameTilesLeft   = filter (`notElem` possibleTiles game) $ p1GameTiles game
          let gameTilesNew    = take (7 - length gameTilesLeft) (gameTilesBag game) <> gameTilesLeft
          let gameTilesBagNew = filter (`notElem` gameTilesNew) $ gameTilesBag game
          let usedTilesNew    = possibleTiles game <> p1UsedTiles game
          modify (\game -> game { gameBoard = possibleGameBoard game
                                , bonusBoard = possibleBonusBoard game
                                , gameTilesBag = gameTilesBagNew
                                , p1GameTiles = gameTilesNew
                                , p1UsedTiles = usedTilesNew
                                , p1GameWords = (possibleWord game, checkWord game) : p1GameWords game
                                , p1Score = p1Score game + points})
      else do
          if player == P1 then do
              modify (\game -> game { p1LastMove = Play })
          else do
              modify (\game -> game { p2LastMove = Play })

      putStrLnIO "\n\n"
      putStrLnIO $ showGameState (gameState game)
      putStrLnIO "\nThe Final Score"
      putStrLnIO "----------------"
      putStrLnIO $ "Player1 has " ++ show (p1Score game) ++ " points and lost " ++ show (sum (lettersToPointsConvert (map fst $ p1GameTiles game)))
        ++ " points from the remaining tiles. Total score = " ++ show (p1Score game - sum (lettersToPointsConvert (map fst $ p1GameTiles game))) ++ " points."
      putStrLnIO $ "Words played: | " ++ concat (zipWith (++) (map (capitalise . snd) $ p1GameWords game) (replicate (length (p1GameWords game) - 1) " | "))
      putStrLnIO $ "\nPlayer2 has " ++ show (p2Score game) ++ " points and lost " ++ show (sum (lettersToPointsConvert (map fst $ p2GameTiles game)))
        ++ " points from the remaining tiles. Total score = " ++ show (p2Score game - sum (lettersToPointsConvert (map fst $ p2GameTiles game))) ++ " points."
      putStrLnIO $ "Words played: | " ++ concat (zipWith (++) (map (capitalise . snd) $ p2GameWords game) (replicate (length (p2GameWords game) - 1) " | "))
      putStrLnIO "\n\n"

-- ########################################################################################################


firstPlayer :: Scrabble ()
firstPlayer = randomIO >>= \b -> modify (\game -> game {playerTurn = getFirstPlayer b})

getFirstPlayer :: Bool -> Player
getFirstPlayer p
    | p         = P1
    | otherwise = P2

isValidMove :: String -> String -> Board -> Move -> Bool     --NPF--
isValidMove w1 w2 b m
    | b == []                    = False
    | isMoveInBounds m == False  = False
    | checkWordLength w1 w2 m == False = False
    | otherwise                  = go False b where
        go acc (h : t)
            | isColEmpty (b !! fst (fst (snd m))) (snd (fst (snd m))) == True && isColEmpty (b !! fst (snd (snd m))) (snd (snd (snd m))) == True = True
            | otherwise = False


isMoveInBounds :: Move -> Bool     --NPF--
isMoveInBounds (_, ((sr, sc), (er, ec))) = sr `elem` _RANGE_ && sc `elem` _RANGE_ && er `elem` _RANGE_ && ec `elem` _RANGE_


checkWordLength :: String -> String -> Move -> Bool      --NPF--
checkWordLength w1 w2 (ls, ((sr, sc), (er, ec))) = do
    if map toUpper w1 == map toUpper w2 then do
        if      er - sr == length w1 - 1 then True
        else if ec - sc == length w1 - 1 then True
        else                                  False
    else if length w1 /= length w2 then do
        if      er - sr == length w2 - 1 then True
        else if ec - sc == length w2 - 1 then True
        else                                  False
    else                                      False

isDHOrDV :: Move -> Direction     --NPF--
isDHOrDV (_, ((sr, sc), (er, ec)))
    | er - sr > 0 = DV
    | er - sr < 0 = WW
    | ec - sc > 0 = DH
    | ec - sc < 0 = WW
    | otherwise   = ND


isColEmpty :: Row -> Col -> Bool     --NPF--
isColEmpty [] _      = False
isColEmpty r c       = (c `elem` _RANGE_) && (r !! c == Empty || r!! c == Home)

-- Attempt to ensure the input follows a strict pattern
stringToMove :: WordAttempt -> String -> Move     --NPF--
stringToMove w ( x1 : y1 : z1 : s : x2 : y2: z2 : _ )
    | s /= ' '                            = _INVALID_MOVE_
    | x1 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y1] `notElem` _NUMBER_   = _INVALID_MOVE_
    | readDigit [z1] `notElem` _NUMBER_   = _INVALID_MOVE_
    | x2 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y2] `notElem` _NUMBER_   = _INVALID_MOVE_
    | readDigit [z2] `notElem` _NUMBER_   = _INVALID_MOVE_
    | otherwise                           = (w, ((readAlpha [x1], readDigit $ y1 : [z1]), (readAlpha [x2], readDigit $ y2 : [z2])))

stringToMove w ( x1 : y1 : s : x2 : y2: z2 : _ )
    | s /= ' '                            = _INVALID_MOVE_
    | x1 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y1] `notElem` _NUMBER_   = _INVALID_MOVE_
    | x2 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y2] `notElem` _NUMBER_   = _INVALID_MOVE_
    | readDigit [z2] `notElem` _NUMBER_   = _INVALID_MOVE_
    | otherwise = (w, ((readAlpha [x1], readDigit [y1]), (readAlpha [x2], readDigit $ y2 : [z2])))

stringToMove w ( x1 : y1 : s : x2 : y2 : _ )
    | s /= ' '                            = _INVALID_MOVE_
    | x1 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y1] `notElem` _NUMBER_   = _INVALID_MOVE_
    | x2 `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y2] `notElem` _NUMBER_   = _INVALID_MOVE_
    | otherwise                           = (w, ((readAlpha [x1], readDigit [y1]), (readAlpha [x2], readDigit [y2])))

stringToMove w ( x : y : z : _)
    | x `notElem` _ALPHABET_             = _INVALID_MOVE_
    | readDigit [y] `notElem` _NUMBER_   = _INVALID_MOVE_
    | readDigit [z] `notElem` _NUMBER_   = _INVALID_MOVE_
    | otherwise = (w, ((readAlpha [x], readDigit $ y : [z]), (readAlpha [x], readDigit $ y : [z])))

stringToMove w ( x : y : _ )
    | x `notElem` _ALPHABET_           = _INVALID_MOVE_
    | readDigit [y] `notElem` _NUMBER_ = _INVALID_MOVE_
    | otherwise                        = (w, ((readAlpha [x], readDigit [y]), (readAlpha [x], readDigit [y])))
stringToMove w []                                     = _INVALID_MOVE_
stringToMove w [z]                                    = _INVALID_MOVE_



readAlpha :: String -> Int     --NPF--
readAlpha x
    | isValidAlpha $ fromEnum (head x) = fromEnum (head x) - 65
    | otherwise         = -1

convertRowIndex :: Char -> Int     --NPF--
convertRowIndex x = fromEnum (toUpper x) - 65

isValidAlpha :: Int -> Bool     --NPF--
isValidAlpha x = x `elem` map (+65) _RANGE_

readDigit :: String -> Int     --NPF--
readDigit x
    | isValidNum x = read x
    | otherwise         = -1

isValidNum :: String -> Bool     --NPF--
isValidNum x = x `elem` map show _RANGE_


updateBoard :: LetterPositions -> Scrabble ()
updateBoard []  = return ()
updateBoard (lp : lps)  = do
  game <- get
  let newPossibleGameBoard = putLetter lp $ possibleGameBoard game
  modify (\game -> game { possibleGameBoard = newPossibleGameBoard })
  updateBoard lps


putLetter :: LetterPosition -> Board -> Board     --NPF--
putLetter _ [] = []
putLetter (l, (r, c)) (br : brs)
    | r == 0    = replaceLetterInRow l c br : brs
    | otherwise = br : putLetter (l, (r - 1, c)) brs


replaceLetterInRow :: Letter -> Col -> Row -> Row     --NPF--
replaceLetterInRow _ _ r
    | r == [] = []
replaceLetterInRow l c r = case c of
    0  -> l : drop 1 r
    _ | 0 < c && c < 14
       -> take c r ++ [l] ++ tail (drop c r)
    14 -> take c r ++ [l]
    _  -> r


updateBonusBoard :: Positions -> Scrabble ()
updateBonusBoard []  = return ()
updateBonusBoard (p : ps)  = do
  game <- get
  let newPossibleBonusBoard = putBonus p $ possibleBonusBoard game
  modify (\game -> game { possibleBonusBoard = newPossibleBonusBoard })
  updateBonusBoard ps


putBonus :: Position -> BonusBoard -> BonusBoard     --NPF--
putBonus _ [] = []
putBonus (r, c) (br : brs)
    | r == 0    = replaceBonusInRow c br : brs
    | otherwise = br : putBonus (r - 1, c) brs


replaceBonusInRow :: Col -> BonusRow -> BonusRow     --NPF--
replaceBonusInRow _ r
    | r == [] = []
replaceBonusInRow c r = case c of
    0  -> NB : drop 1 r
    _ | 0 < c && c < 14
       -> take c r ++ [NB] ++ tail (drop c r)
    14 -> take c r ++ [NB]
    _  -> r


interpolatePositions :: Move -> Positions     --NPF--
interpolatePositions (_, ((sr, sc),(er, ec)))
    | er - sr > 0 = zip [sr .. er] (repeat sc)
    | er - sr < 0 = []
    | ec - sc > 0 = zip (repeat sr) [sc .. ec]
    | ec - sc < 0 = []
    | otherwise   = [(sr, sc)]


findOverlapLocations :: Board -> Move -> (LetterPositions, Direction)
findOverlapLocations b (w, p) = do
    let positions = interpolatePositions (w, p)
    if isDHOrDV (w, p) == DH then do
      let fullList = zip (b !! fst (fst p)) [0 ..]
      let overlap = filter (\x -> snd x >= (snd (fst p)) && snd x <= (snd (snd p)) && fst x /= Empty) fullList
      ((map (\x -> (fst x, (fst (fst p), snd x))) overlap), DH)
    else if isDHOrDV (w, p) == DV then do
      let fullList = zip (transpose b !! snd (fst p)) [0 ..]
      let overlap = filter (\x -> snd x >= (fst (fst p)) && snd x <= (fst (snd p)) && fst x /= Empty) fullList
      ((map (\x -> (fst x, (snd x, snd (fst p)))) overlap),DV)
    else ([], ND)


findOverlapAndDesignate :: Board -> Move -> (LetterPositions, Direction)
findOverlapAndDesignate b m@(w, p) = do
    let positions = interpolatePositions m
    if isDHOrDV m == DH then do
      let fullList = zip (b !! fst (fst p)) [0 ..]
      let overlap = filter (\x -> snd x >= (snd (fst p)) && snd x <= (snd (snd p)) && fst x /= Empty) fullList
      let letterPositions = map (\x -> (fst x, (fst (fst p), snd x))) overlap
      let overlapPositions = map (snd) letterPositions
      let newPositions = filter (`notElem` overlapPositions) positions
      ((zip (fst m) newPositions), DH)
    else if isDHOrDV m == DV then do
      let fullList = zip (transpose b !! snd (fst p)) [0 ..]
      let overlap = filter (\x -> snd x >= (fst (fst p)) && snd x <= (fst (snd p)) && fst x /= Empty) fullList
      let letterPositions = map (\x -> (fst x, (snd x, snd (fst p)))) overlap
      let overlapPositions = map (snd) letterPositions
      let newPositions = filter (`notElem` overlapPositions) positions
      ((zip (fst m) newPositions), DV)
    else ([], ND)


designateLetterPlacement :: Board -> Move -> Direction -> (LetterPositions, Direction)     --NPF--
designateLetterPlacement b m d = do
  let positions = interpolatePositions m
  let letterPositions = fst $ findOverlapLocations b m
  let overlap' = map (snd) letterPositions
  let positions' = filter (`notElem` overlap') positions
  ((zip (fst m) positions'), d)


breakUpLPs :: (LetterPositions, Direction) -> BrokenLetPos
breakUpLPs (input, d) = do
    if d == DH then do
        let s = snd (snd (head input))
        let input' = zip input [s ..]
        let start = filter (\((_, (_, c1)), c2) -> c1 == c2) input'
        let end = filter (\((_, (_, c1)), c2) -> c1 /= c2) input'
        (fst <$> start, fst <$> end)
    else if d == DV then do
        let s = fst (snd (head input))
        let input' = zip input [s ..]
        let start = filter (\((_, (r1, _)), r2) -> r1 == r2) input'
        let end = filter (\((_, (r1, _)), r2) -> r1 /= r2) input'
        (fst <$> start, fst <$> end)
    else ([],[])


extractWord :: Board -> Move -> LetterPositions
extractWord b m
    | isDHOrDV m == DH = sortOn (snd . snd) (fst (findOverlapLocations b m) ++ fst (designateLetterPlacement b m DH))
    | isDHOrDV m == DV = sortOn (fst . snd) (fst (findOverlapLocations b m) ++ fst (designateLetterPlacement b m DV))
    | otherwise   = []


getBonusValues :: LetterPositions -> BonusRow
getBonusValues lp = do
  let value = map (snd) lp
  map (\x -> (_BONUSBOARD_ !! fst x) !! snd x) value


getWordValue :: BonusRow -> LetterPositions -> Int
getWordValue br lp = do
  let letter_bonus = zip (lettersToPointsConvert (map (fst) lp)) br
  let wordbonus = getWordBonus $ filter (\x -> W2 == x || W3 == x) br
  let letterpoints = sum $ map (\x -> getLetterBonus (fst x) (snd x)) letter_bonus
  letterpoints * wordbonus


switchPlayer :: Player -> Player
switchPlayer P1       = P2
switchPlayer P2       = P1


playerPrompt :: Player -> String
playerPrompt p
  | p == P1   = "Player 1 "
  | otherwise = "Player 2 "


playTiles :: Player -> Tiles -> Scrabble ()
playTiles p t
    | p == P1 = do
        game <- get
        let remainTiles = filter (\x -> x `notElem` t) (p1GameTiles game)
        modify (\game -> game {p1GameTiles = remainTiles, possibleWord = []})
    | p == P2 = do
        game <- get
        let remainTiles = filter (\x -> x `notElem` t) (p2GameTiles game)
        modify (\game -> game {p2GameTiles = remainTiles, possibleWord = []})


getGameState :: Game -> GameState
getGameState g
    | (playerOutOfTiles g == P1 || playerOutOfTiles g == P2) && isTied g       = R1
    | (playerOutOfTiles g == P1 || playerOutOfTiles g == P2) && hasWon g == P1 = R2
    | (playerOutOfTiles g == P1 || playerOutOfTiles g == P2) && hasWon g == P2 = R3
    | playerOutOfTiles g == P1                                                 = R4
    | playerOutOfTiles g == P2                                                 = R5
    | gameTilesBagEmpty g == True && playerOutOfTiles g == NP                  = R6
    | otherwise                                                                = R7


showGameState :: GameState -> String
showGameState gS = case gS of
    R1 -> "\nThe Game is Tied!"
    R2 -> "\nPlayer 1 Won the game!"
    R3 -> "\nPlayer 2 Won the game!"
    R4 -> "\nPlayer 1 is out of tiles!"
    R5 -> "\nPlayer 2 is out of tiles!"
    R6 -> "\nThere are no more tiles in the game bag!"
    R7 -> "\nThe game is in progress..."
    QT -> "has quit the game."


gameTilesBagEmpty :: Game -> Bool
gameTilesBagEmpty g
    | null (gameTilesBag g) = True
    | otherwise             = False


playerOutOfTiles :: Game -> Player
playerOutOfTiles g
    | null (p1GameTiles g) = P1
    | null (p2GameTiles g) = P2
    | otherwise            = NP


isTied :: Game -> Bool
isTied g
    | p1Score g == p2Score g = True
    | otherwise              = False


hasWon :: Game -> Player
hasWon g
    | p1Score g > p2Score g = P1
    | p1Score g < p2Score g = P2
    | otherwise             = NP


getLetterBonus :: Points -> Bonus -> Points
getLetterBonus p b
    | b == L2   = p * 2
    | b == L3   = p * 3
    | otherwise = p


getWordBonus :: [Bonus] -> Int
getWordBonus b = product $ map (\x -> if x == FP || x == W2 then 2 else if x == W3 then 3 else 1) b


bagOfLetters :: [(Letter, Count)] -> [(Letter, Count)]
bagOfLetters t =  zip (concatMap (\ h -> replicate (snd h) (fst h)) t) [1 ..]


shakeTileBag :: Tiles -> IO ([(Letter,Int)])
shakeTileBag t = do
  rng <- newStdGen
  if length t == 0 then
      return []
  else
      return $ shuffle' t (length t) rng


findAndRemove :: Eq a => a -> [(a, b)] -> (Maybe (a, b), [(a, b)])
findAndRemove _ [] = (Nothing, [])
findAndRemove x ((y, z):ys)
    | x == y    = (Just (y, z), ys)
    | otherwise = let (result, remaining) = findAndRemove x ys
                  in (result, (y, z) : remaining)


matchAndRemove :: Eq a => [a] -> [(a, b)] -> ([(a, b)], [(a, b)])
matchAndRemove [] bList = ([], bList)
matchAndRemove (x:xs) bList =
    case findAndRemove x bList of
        (Just matchedTuple, updatedBList) ->
            let (restMatches, finalBList) = matchAndRemove xs updatedBList
            in (matchedTuple : restMatches, finalBList)
        (Nothing, updatedBList) -> matchAndRemove xs updatedBList


prompt :: String -> IO String
prompt message = do
    putStr message
    hFlush stdout
    getLine


searchAndReplace :: String -> IO String
searchAndReplace str = do
    case break (== '*') str of
        (before, '*':after) -> do
            replacement <- prompt $ "Enter a letter to replace '*': " ++ map toUpper before ++ "_"
            if any (`notElem` _ALPHABET_) replacement then do
              putStrLn "Must be a letter"
              searchAndReplace str
            else searchAndReplace $ map toUpper $ before ++ replacement ++ after
        (before, []) -> return before

-- #################################################################################################
-- This Section is for converting
-- #################################################################################################
letterToCharConvert :: Letter -> String
letterToCharConvert a = snd . head $ filter (\n -> fst n == a) letterToCharMap

charToLetterConvert :: String -> Letter
charToLetterConvert a = fst . head $ filter (\n -> snd n == map toUpper a) letterToCharMap

lettersToStringConvert :: [Letter] -> [String]
lettersToStringConvert a = map letterToCharConvert a

stringToLettersConvert :: String -> [Letter]
stringToLettersConvert a = map ((\w -> charToLetterConvert w) . (: [])) $ toUpper <$> a

letterToPointConvert :: Letter -> Int
letterToPointConvert a = snd . head $ filter (\n -> fst n == a) letterToPointMap

lettersToPointsConvert :: [Letter] -> WordPoints
lettersToPointsConvert a = map letterToPointConvert a

bonusToCharConvert :: Bonus -> String
bonusToCharConvert a = snd . head $ filter (\n -> fst n == a) bonusToCharMap

bonusToStringConvert :: [Bonus] -> [String]
bonusToStringConvert a = map bonusToCharConvert a

-- #################################################################################################
-- This Section contains my Maps and Boards
-- #################################################################################################

bonusToCharMap :: [(Bonus, String)]
bonusToCharMap =
 [
  (NB, "NB"), (L2, "L2"), (L3, "L3"), (W2, "W2"), (W3, "W3"), (FP, "FP")
 ]

letterToCharMap :: [(Letter, String)]
letterToCharMap =
 [
  (A, "A"), (B, "B"), (C, "C"), (D, "D"), (E, "E"), (F, "F"), (G, "G"),
  (H, "H"), (I, "I"), (J, "J"), (K, "K"), (L, "L"), (M, "M"), (N, "N"),
  (O, "O"), (P, "P"), (Q, "Q"), (R, "R"), (S, "S"), (T, "T"), (U, "U"),
  (V, "V"), (W, "W"), (X, "X"), (Y, "Y"), (Z, "Z"), (Blank, "*"),
  (Empty, "-"), (Home, "o")
 ]

letterToPointMap :: [(Letter, Points)]
letterToPointMap =
 [
  (A, 1), (B, 3), (C, 3), (D, 2), (E, 1), (F, 4), (G, 2),
  (H, 4), (I, 1), (J, 8), (K, 5), (L, 1), (M, 3), (N, 1),
  (O, 1), (P, 3), (Q, 10), (R, 1), (S, 1), (T, 1), (U, 1),
  (V, 4), (W, 4), (X, 8), (Y, 4), (Z, 10), (Blank, 0), (Empty, 0)
 ]

letterToQuantityMap :: [(Letter, Count)]
letterToQuantityMap =
 [
  (A, 9), (B, 2), (C, 2), (D, 4), (E, 12), (F, 2), (G, 3),
  (H, 2),(I, 9), (J, 1), (K, 1), (L, 4), (M, 2), (N, 6),
  (O, 8), (P, 2), (Q, 1), (R, 6), (S, 4), (T, 6), (U, 4),
  (V, 2), (W, 2), (X, 1), (Y, 2), (Z, 1), (Blank, 2)
 ]

_EROW_ :: [Letter]
_EROW_ = replicate _SIZE_ Empty

_EMPTYBOARD_ :: [[Letter]]
_EMPTYBOARD_ = replicate _SIZE_ _EROW_

_TESTBOARD_ :: [[Letter]]
_TESTBOARD_ =
 [
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, C, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, R, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Y, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, P, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, B, I, T, C, O, I, N, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, O, Empty, Empty, Empty, O, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, S, C, R, A, B, B, L, E, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, A, Empty, Empty, L, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, R, Empty, Empty, E, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, D, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, A, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, N, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, O, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
 ]


_BONUSBOARD_ :: [[Bonus]]
_BONUSBOARD_ =
 [
  [W3, NB, NB, L2, NB, NB, NB, W3, NB, NB, NB, L2, NB, NB, W3],
  [NB, W2, NB, NB, NB, L3, NB, NB, NB, L3, NB, NB, NB, W2, NB],
  [NB, NB, W2, NB, NB, NB, L2, NB, L2, NB, NB, NB, W2, NB, NB],
  [L2, NB, NB, W2, NB, NB, NB, L2, NB, NB, NB, W2, NB, NB, L2],
  [NB, NB, NB, NB, W2, NB, NB, NB, NB, NB, W2, NB, NB, NB, NB],
  [NB, L3, NB, NB, NB, L3, NB, NB, NB, L3, NB, NB, NB, L3, NB],
  [NB, NB, L2, NB, NB, NB, L2, NB, L2, NB, NB, NB, L2, NB, NB],
  [W3, NB, NB, L2, NB, NB, NB, FP, NB, NB, NB, L2, NB, NB, W3],
  [NB, NB, L2, NB, NB, NB, L2, NB, L2, NB, NB, NB, L2, NB, NB],
  [NB, L3, NB, NB, NB, L3, NB, NB, NB, L3, NB, NB, NB, L3, NB],
  [NB, NB, NB, NB, W2, NB, NB, NB, NB, NB, W2, NB, NB, NB, NB],
  [L2, NB, NB, W2, NB, NB, NB, L2, NB, NB, NB, W2, NB, NB, L2],
  [NB, NB, W2, NB, NB, NB, L2, NB, L2, NB, NB, NB, W2, NB, NB],
  [NB, W2, NB, NB, NB, L3, NB, NB, NB, L3, NB, NB, NB, W2, NB],
  [W3, NB, NB, L2, NB, NB, NB, W3, NB, NB, NB, L2, NB, NB, W3]
 ]

-- #################################################################################################
-- This Section uses Wordnik API to check if a word is a Scrabble Word
-- #################################################################################################
newtype ScrabbleScore = ScrabbleScore { value :: Int } deriving Show

instance FromJSON ScrabbleScore where
  parseJSON = withObject "ScrabbleScore" $ \o -> ScrabbleScore <$> o .: "value"

apiUrl :: String -> Int -> String
apiUrl w k = "https://api.wordnik.com/v4/word.json/" ++ w
 ++ "/scrabbleScore?api_key=8ro04rbv5viwo4kkn3w8uocogj7zkdkby9g3fkmiilm62l" ++ show k

isValidResponse :: B.ByteString -> WordState
isValidResponse response =
  case decode response of
    Just (ScrabbleScore _) -> IsWord
    _                      -> NotWord

getScrabbleScore :: String -> Int -> IO (Either SomeException B.ByteString)
getScrabbleScore w k = try $ do
  request <- parseUrlThrow $ apiUrl w k
  response <- httpLBS request
  return $ responseBody response

handleException :: SomeException -> IO (Either SomeException B.ByteString)
handleException e = do
  putStrLn $ "Caught exception: " ++ displayException e
  return $ Left e

checkIfScrabbleWord :: Scrabble ()
checkIfScrabbleWord = do
  game <- get
  result <- liftIO $ getScrabbleScore (checkWord game) (gameKey game) `catch` handleException
  case result of
    Right response -> modify (\game -> game {checkWordValid = isValidResponse response})
    Left _         -> modify (\game -> game {checkWordValid = NotWord})

-- #################################################################################################


playGame :: Scrabble ()
playGame = do
    game <- get
    putStrLnIO "Enter CDP Batch Number:"
    n <- getLineIO
    let k = read n :: Int
    firstPlayer
    let board = putLetter (Home,(7,7)) _EMPTYBOARD_ 
    modify (\game -> game { gameBoard = board, possibleGameBoard = board, gameKey = 2 * (k-2^3 + 10) + (k-2^3) })
    modify (\game -> game { gameTilesBag = bagOfLetters letterToQuantityMap })

    game <- get
    gt1 <- liftIO (shakeTileBag $ gameTilesBag game)
    let p1GameTilesNew = take 7 gt1
    let gameTilesBag1 = filter (`notElem` p1GameTilesNew) gt1
    modify (\game -> game { gameTilesBag = gameTilesBag1, p1GameTiles = p1GameTilesNew})

    game <- get
    gt2 <- liftIO (shakeTileBag $ gameTilesBag game)
    let p2GameTilesNew = take 7 gt2
    let gameTilesBag2 = filter (`notElem` p2GameTilesNew) gt2
    modify (\game -> game { gameTilesBag = gameTilesBag2, p2GameTiles = p2GameTilesNew})

    getMove $ possibleGameBoard game


main :: IO ()
main = do
  clearScreen
  printLogo _LOGO_Start_
  runStateT playGame setGame
  printLogo _LOGO_End_
  putStrLn "\n\n\n"
  return ()