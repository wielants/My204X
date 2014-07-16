module My204X where

import Data.List (transpose, intercalate, splitAt, (\\))
import System.IO (hSetEcho, stdin)
import System.Random (randomRIO)
import Prelude hiding (Left, Right)

--------
-- TODO
--
-- 1. customize random cell values as distribution e.g. [(2, 9), (4, 1)]
-- 2. user cursor keys
-- 3. better UI
-- 4. tests: QuickCheck? HSpec?
--------

-- parameters

boardSize = 4
luckyNumber = 2048
emptyCell = 0
compactFunc = (+)

--- derived parameters

cellWidth = length $ show luckyNumber

---

type Board = [[Int]]
data Direction = Left | Right | Up | Down deriving (Show)

emptyBoard :: Board
emptyBoard = replicate boardSize $ replicate boardSize emptyCell

testBoard :: Board
testBoard = [ [  0,  2,  0,  0]
            , [  0,  2,  4,  4]
            , [  0,  0,  8,  0]
            , [  2,  2,  0,  8]]

emptyCells :: Board -> [Pos]
emptyCells b = concatMap (dropValue . filterEmptyCells) makeCoordinates
     where dropValue = map (\(x, y, v) -> (x, y))
           filterEmptyCells = filter (\( _, _, x) -> x == emptyCell)
           makeCoordinates = map (\(y, xs) -> zipWith (\x v -> (x, y, v)) [1..] xs) $ zip [1..] b

-- values of all non-empty cells
values :: Board -> [Int]
values b = filter (/= emptyCell) $ concat b

setCell :: Pos -> Int -> Board -> Board
setCell (x, y) v b = map add $ zip [1..] b
        where add (y', xs) = if (y' == y) then
                               let (as, b:bs) = splitAt (x - 1) xs
                               in as ++ [v] ++ bs
                             else xs

showBoard :: Board -> [String]
showBoard =  map showRow 
   where showRow  = intercalate "|" . map showCell
         showCell = center cellWidth ' ' . showValue
         showValue x = if (x == emptyCell) then " " else show x
         center n c xs = let l  = length xs
                             l2 = l `div` 2
                             n2 = n `div` 2
                         in if l > n then 
                               xs 
                            else 
                               take n $ replicate (n2 - l2) c ++ xs ++ repeat c

compact :: [Int] -> [Int]
compact []  = []
compact [x] = [x]
compact (x:y:xs)
   | x == emptyCell = compact (y:xs)
   | y == emptyCell = compact (x:xs)
   | x == y = (x `compactFunc` y) : compact xs
   | x /= y = x:compact (y:xs)

shiftArray :: [Int] -> [Int]
shiftArray xs = take boardSize $ compact xs ++ repeat emptyCell

shiftBoard :: Direction -> Board -> Board
shiftBoard Left  = shiftLeft
shiftBoard Right = shiftRight
shiftBoard Up    = transpose . shiftLeft . transpose
shiftBoard Down  = transpose . shiftRight . transpose

shiftLeft  = map shiftArray
shiftRight = map (reverse . shiftArray . reverse) 

data KeyMapping = Move Direction | Exit | Unknown deriving (Show)

mapKey :: Char -> KeyMapping
mapKey 'g' = Move Left
mapKey 'z' = Move Up
mapKey 'h' = Move Right
mapKey 'b' = Move Down
mapKey 'x' = Exit
mapKey _   = Unknown

-- common IO

getCh :: IO Char
getCh = do
         hSetEcho stdin False
         c <- getChar
         hSetEcho stdin True
         return c

writeat :: Pos -> String -> IO ()
writeat (x, y) xs = do
      goto (x, y)
      putStr xs

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

cls :: IO ()
cls = putStr "\ESC[2J"

-- game IO

displayBoard :: Board -> IO ()
displayBoard b = cls >> sequence_ [writeat (1, y) xs | (y, xs) <- zip [1..] (showBoard b)]

addRandomCell :: Board -> IO Board
addRandomCell b = do
                     let availableCells = emptyCells b
                     newCell <- randomRIO (1, length availableCells) :: IO Int
                     r <- randomRIO (1, 10) :: IO Int
                     let newValue = if (r < 10) then 2 else 4
                     return $ setCell (availableCells !! (newCell - 1)) newValue b

data GameState = Won | Lost | NextMove deriving (Show)

gameState :: Board -> GameState
gameState b 
  | hasWon    = Won
  | finished  = Lost
  | otherwise = NextMove
  where finished = emptyCells b == []
        hasWon   = luckyNumber `elem` (values b)

displayStatus :: String -> IO ()
displayStatus xs = writeat (1, boardSize + 2) xs >> putStrLn ""

-- points for a round
getPoints :: Board -> Board -> Int
getPoints oldB newB = sum $ (values newB) \\ (values oldB)

gameStep :: Int -> Board -> IO ()
gameStep points b = do
    c <- getCh
    case  mapKey c of
      Exit -> displayStatus $ "Good bye! Points : " ++ show points 
      Unknown -> displayStatus "What? Try again!" >> gameStep points b
      Move d -> do
             let newB = shiftBoard d b
             if (newB /= b) then do
                let newPoints = points + (getPoints b newB)
                newB' <- addRandomCell newB
                displayBoard newB'
                case gameState newB' of
                   Won -> displayStatus $ "You won! Points : " ++ show newPoints
                   Lost -> displayStatus $ "You lost!  Points : " ++ show newPoints
                   NextMove -> do
                                 displayStatus $ "Points : " ++ show newPoints
                                 gameStep newPoints newB'
             else
                  displayStatus "Nothing to do.Try again!" >> gameStep points b -- ignore invalid move

main = do
      firstBoard <- (addRandomCell emptyBoard >>= addRandomCell)
      displayBoard firstBoard
      gameStep 0 firstBoard
