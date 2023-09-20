module Lib
  ( Player (..),
    play,
    startingPlayer,
    Board (..),
    moves,
    move,
    Movement (..),
    initialBoard,
    home,
    on,
    piecesIn,
    simulate,
    Game (..),
  )
where

import Data.List (nub)
import System.Random
  ( Random (randomR),
    RandomGen,
    StdGen,
    mkStdGen,
  )

data Player = Red | Green | Blue | Yellow deriving (Show, Eq)

data Movement = Movement {start :: Int, end :: Int} deriving (Show, Eq)

type Positions = (Int, Int, Int, Int)

initialPositions :: Positions
initialPositions = (0, 0, 0, 0)

initialBoard :: Board
initialBoard = Board initialPositions initialPositions initialPositions initialPositions

data Board = Board {red :: Positions, green :: Positions, yellow :: Positions, blue :: Positions} deriving (Show, Eq)

data Game = Game {boards :: [Board], gen :: StdGen, player :: Player, doubles :: Int, lastMove :: Movement, numMoves :: Int} deriving (Show, Eq)

players :: [Player]
players = [Red, Green, Blue, Yellow]

next :: Player -> Player
next Red = Green
next Green = Yellow
next Yellow = Blue
next Blue = Red

startingPlayer :: Int -> Player
startingPlayer seed = players !! rand
  where
    n = length players
    (rand, _) = randomR (0, n - 1) (mkStdGen seed)

play :: Int -> Player
play = startingPlayer

initMove :: Movement
initMove = Movement 0 1

on :: Int -> Positions -> Int
on pos (x, y, w, z) = oneIfTrue (x == pos) + oneIfTrue (y == pos) + oneIfTrue (w == pos) + oneIfTrue (z == pos)
  where
    oneIfTrue a = if a then 1 else 0

playerPieces :: Player -> Board -> Positions
playerPieces p board
  | p == Red = red board
  | p == Green = green board
  | p == Blue = blue board
  | otherwise = yellow board

home :: Player -> Board -> Int
home p board = on 0 (playerPieces p board)

candidates :: Int -> Positions -> [Movement]
candidates amount (x, y, w, z) = nub (mov x ++ mov y ++ mov w ++ mov z)
  where
    mov i = [Movement i (i + amount) | i /= 0]

moves :: Player -> Board -> Int -> Int -> [Movement]
moves p board dice1 dice2 = nub (steps dice1 ++ steps dice2 ++ steps (dice1 + dice2))
  where
    steps amount
      | amount == 5 && home p board >= 1 && on 1 (playerPieces p board) < 2 = [initMove]
      | otherwise = filter (valid board) (candidates amount (playerPieces p board))

valid :: Board -> Movement -> Bool
valid board mov
  | end mov == start mov = False
  | end mov > 72 = False
  | end mov > 64 = True
  | otherwise = all (\x -> piecesIn board x < 2) [(start mov + 1) .. (end mov)]

piecesIn :: Board -> Int -> Int
piecesIn board endPos = sum (map (\x -> on endPos (playerPieces x board)) players)

safeSquares :: [Int]
safeSquares = [1, 8, 13, 18, 25, 30, 35, 42, 47, 52, 59, 64]

move :: Board -> Player -> Int -> Int -> Board
move board p s e
  | p == Red =
      if e `notElem` safeSquares && e <= 64
        then board {red = replace (red board) s e, green = replace (green board) (transpose 1) 0, yellow = replace (yellow board) (transpose 2) 0, blue = replace (blue board) (transpose 3) 0}
        else board {red = replace (red board) s e}
  | p == Green =
      if e `notElem` safeSquares && e <= 64
        then board {green = replace (green board) s e, yellow = replace (yellow board) (transpose 1) 0, blue = replace (blue board) (transpose 2) 0, red = replace (red board) (transpose 3) 0}
        else board {green = replace (green board) s e}
  | p == Yellow =
      if e `notElem` safeSquares && e <= 64
        then board {yellow = replace (yellow board) s e, blue = replace (blue board) (transpose 1) 0, red = replace (red board) (transpose 2) 0, green = replace (green board) (transpose 3) 0}
        else board {yellow = replace (yellow board) s e}
  | otherwise =
      if e `notElem` safeSquares && e <= 64
        then board {blue = replace (blue board) s e, red = replace (red board) (transpose 1) 0, green = replace (green board) (transpose 2) 0, yellow = replace (yellow board) (transpose 3) 0}
        else board {blue = replace (blue board) s e}
  where
    replace (x, y, w, z) s e
      | x == s = (e, y, w, z)
      | y == s = (x, e, w, z)
      | w == s = (x, y, e, z)
      | z == s = (x, y, w, e)
      | otherwise = (x, y, w, z)
    transpose pos = if e < 13 + offset then e + 41 - offset else e - 17 - offset
      where
        offset = 17 * (pos - 1)

rollDice :: (Random a, RandomGen g, Num a) => g -> (a, g)
rollDice = randomR (1, 6)

randomChoice :: (RandomGen g, Foldable t) => t a -> g -> (Int, g)
randomChoice moveSet = randomR (0, length moveSet - 1)

makePlay :: Board -> Movement -> Player -> Board
makePlay board theMove p = move board p (start theMove) (end theMove)

extraMove :: Game -> Int -> Game
extraMove game@(Game [] _ _ _ _ _) _ = game
extraMove game@(Game bs@(originalBoard : _) initGen p _ _ m) amount
  | any (winner originalBoard m) players = game
  | null moveSet = game
  | end movement == 72 = extraMove game {gen = finalGen, lastMove = movement, boards = finalBoard : bs} 10
  | piecesIn originalBoard 0 < piecesIn finalBoard 0 = extraMove game {gen = finalGen, lastMove = movement, boards = finalBoard : bs} 20
  | otherwise = game {gen = finalGen, lastMove = movement, boards = finalBoard : bs}
  where
    moveSet = moves p originalBoard amount 0
    (moveChoice, moveGen) = randomChoice moveSet initGen
    movement = moveSet !! moveChoice
    (finalBoard, finalGen) = (makePlay originalBoard movement p, moveGen)

simulate :: Game -> Game
simulate game@(Game [] _ _ _ _ _) = simulate (game {boards = [initialBoard]})
simulate game@(Game bs@(originalBoard : _) initGen p d l m)
  | any (winner originalBoard m) players = game
  | d == 2 && isDouble =
      if end l <= 64 && end l > 0
        then simulate game {boards = move originalBoard p (end l) 0 : bs, player = Lib.next p, doubles = finalDoubles, numMoves = m + 1}
        else simulate game {player = Lib.next p, doubles = finalDoubles, numMoves = m + 1}
  | null moveSet = simulate game {player = finalPlayer, gen = rand2Gen, doubles = finalDoubles, numMoves = m + 1}
  | otherwise = simulate finalGame {player = finalPlayer, doubles = finalDoubles, numMoves = m + 1}
  where
    (rand1, rand1Gen) = rollDice initGen
    (rand2, rand2Gen) = rollDice rand1Gen
    isDouble = rand1 == rand2
    (finalDoubles, finalPlayer) = if isDouble then (d + 1, p) else (0, Lib.next p)
    moveSet = moves p originalBoard rand1 rand2
    nextGame = extraMove game {gen = rand1Gen} rand1
    movement = lastMove nextGame
    diff = end movement - start movement
    finalGame
      | diff == rand1 = extraMove nextGame rand2
      | diff == rand2 = extraMove nextGame rand1
      | otherwise = nextGame

winner :: Board -> Int -> Player -> Bool
winner board m p = on 72 (playerPieces p board) == 4
