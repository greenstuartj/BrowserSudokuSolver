module Sudoku where

import System.Environment
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

type PossibleSet = S.Set Int
type Board = M.Map Int (M.Map Int PossibleSet)

showBoard :: Board -> String
showBoard board = finalBoard ++ "\n"
  where groupN _ [] = []
        groupN mult lst = (take mult lst) : groupN mult (drop mult lst)
        insertAtMult mult toAdd lst = mconcat $ intersperse [toAdd] $ groupN mult lst
        (Just b) = sequence $ map (\r -> getRow r board) [1..9]
        lb = map (\ls -> map (\s -> if S.size s == 1 then S.toList s else [0]) ls) b
        splitBoard = groupN 9 $ map show $ concat $ concat lb
        segs = div 9 3
        barSpaceBoard = map (intersperse " ") $ map (insertAtMult segs "|") splitBoard
        joinedLineBoard = map mconcat barSpaceBoard
        horSplit = take ((9-2)*segs) (cycle "-")
        finalBoard = mconcat $ intersperse "\n" $ insertAtMult segs horSplit joinedLineBoard

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

stringToNList :: String -> [PossibleSet]
stringToNList =
  map (\c -> if c == '.' then S.fromList [1..9] else S.fromList [(read [c])])

buildBoard :: [PossibleSet] -> Board
buildBoard list = buildBoard' list 1 M.empty
  where indexMap lst = M.fromList $ zip [1..9] lst
        buildBoard' [] _ acc = acc
        buildBoard' lst n acc = buildBoard' (drop 9 lst)
                                            (succ n)
                                            (M.insert n (indexMap $ take 9 lst) acc)

stringToBoard :: String -> Board
stringToBoard = buildBoard . stringToNList

boardToString :: Board -> String
boardToString board = map (\c -> if c == 0 then '.' else head $ show c) $ mconcat lb
  where (Just b) = sequence $ map (\r -> getRow r board) [1..9]
        lb = map (\ls -> map (\s -> if S.size s == 1 then (head $ S.toList s) else 0) ls) b

getCoord :: Int -> Int -> Board -> Maybe PossibleSet
getCoord row column board =
  return board >>= (M.lookup row) >>= (M.lookup column)

unionKnown :: [PossibleSet] -> PossibleSet
unionKnown [] = S.fromList []
unionKnown pl = unioned
  where knownSets = filter (\s -> (S.size s) == 1) pl
        unioned = if knownSets == [] then S.fromList [] else foldl S.union (head knownSets) (tail knownSets)

getRow :: Int -> Board -> Maybe [PossibleSet]
getRow row board =
  sequence $ map (\column -> getCoord row column board) [1..9]

getColumn :: Int -> Board -> Maybe [PossibleSet]
getColumn column board =
  sequence $ map (\row -> getCoord row column board) [1..9]

squareCoord :: Int -> [Int]
squareCoord n
  | n <= 3 = [1,2,3]
  | n >= 7 = [7,8,9]
  | otherwise = [4,5,6]

getSquare :: Int -> Int -> Board -> Maybe [PossibleSet]
getSquare row column board =
  sequence $ [getCoord r c board | r <- squareCoord row, c <- squareCoord column]

getPossible :: Int -> Int -> Board -> Maybe PossibleSet
getPossible row column board = do
  rowKnown    <- getRow row board
  columnKnown <- getColumn column board
  squareKnown <- getSquare row column board
  possible    <- getCoord row column board
  let allKnown = rowKnown ++ columnKnown ++ squareKnown
  return $ S.difference possible (unionKnown allKnown)

updateCoord :: Int -> Int -> Board -> Maybe (Either Board Board)
updateCoord row column board = do
  oldValue <- getCoord row column board
  if S.size oldValue == 1
    then return $ Left board
    else do
    newValue <- getPossible row column board
    if S.size newValue == 0
      then Nothing
      else do
      r <- M.lookup row board
      let newRow = M.insert column newValue r
      if oldValue == newValue
        then return $ Left board
        else return $ Right (M.insert row newRow board)

updateBoard' ::
  Int
  -> Int
  -> Maybe (Either Board Board)
  -> Bool
  -> Maybe (Either Board Board)
updateBoard' _ _ Nothing _ = Nothing
updateBoard' 10 _ board False = board
updateBoard' 10 _ board True = updateBoard' 1 1 board False
updateBoard' row 10 board edits = updateBoard' (succ row) 1 board edits
updateBoard' row column (Just (Right board)) _ =
  updateBoard' row (succ column) (updateCoord row column board) True
updateBoard' row column (Just (Left board)) edits =
  updateBoard' row (succ column) (updateCoord row column board) edits

updateBoard :: Board -> Maybe Board
updateBoard board = toBoard $ updateBoard' 1 1 (Just (Left board)) False
  where toBoard (Just (Right b)) = (Just b)
        toBoard (Just (Left b))  = (Just b)
        toBoard Nothing          = Nothing

getCoordOfFirstUnknown' :: Int -> Int -> Maybe Board -> Maybe (Int, Int)
getCoordOfFirstUnknown' _ _ Nothing = Nothing
getCoordOfFirstUnknown' 10 _ _ = Nothing
getCoordOfFirstUnknown' row 10 board = getCoordOfFirstUnknown' (succ row) 1 board
getCoordOfFirstUnknown' row column (Just board)
  | S.size pvs > 1 = Just (row, column)
  | otherwise      = getCoordOfFirstUnknown' row (succ column) (Just board)
  where (Just pvs) = getCoord row column board

getCoordOfFirstUnknown :: Maybe Board -> Maybe (Int, Int)
getCoordOfFirstUnknown board = getCoordOfFirstUnknown' 1 1 board

guessCoord :: Int -> Int -> Int -> Board -> Maybe Board
guessCoord row column guess board = do
  oldValue <- M.lookup row board
  let newValue = M.insert column (S.fromList [guess]) oldValue
  return $ M.insert row newValue board

guessUpdate :: Int -> Int -> [Int] -> Board -> Maybe Board
guessUpdate _ _ [] _ = Nothing
guessUpdate row column (p:possibles) board = do
  let result = (guessCoord row column p board) >>= updateBoard
  case result of
    Nothing -> guessUpdate row column possibles board
    Just uBoard -> do
      let coord = getCoordOfFirstUnknown (Just uBoard)
      case coord of
        Nothing -> (Just uBoard)
        Just (newRow, newColumn) -> do
          newPossibles <- getPossible newRow newColumn uBoard
          let deepResult = guessUpdate newRow newColumn (S.toList newPossibles) uBoard
          case deepResult of
            Nothing -> guessUpdate row column possibles board
            Just uBoardDeep -> Just uBoardDeep

guess :: Maybe Board -> Maybe Board
guess mboard = do
  let coord = getCoordOfFirstUnknown mboard
  case coord of
    Nothing -> mboard
    Just (row, column) -> do
      board <- mboard
      possibles <- getPossible row column board
      guessUpdate row column (S.toList possibles) board

solve :: String -> Either String String
solve s = getResult result
  where result = guess $ updateBoard $ stringToBoard s
        getResult Nothing = Left s
        getResult (Just res) = Right (boardToString res)

