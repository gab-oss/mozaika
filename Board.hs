module Board where

import Data.List
import Data.Array
import Field
import Control.Monad (forM_)

type Board = [[Field]]

getIndexies :: Board -> [(Int, Int)]
getIndexies board = [(row,column) | row <- [0..(countRows board)-1], column <- [0..(countColumns board)-1]]

countRows :: Board -> Int
countRows board = length board

countColumns :: Board -> Int
countColumns board = length (head board)

getField :: Board -> Int -> Int -> Field
getField table x y = table !! x !! y

parseToBoard :: [String] -> Board
parseToBoard = map (map toDefaultField) --https://stackoverflow.com/questions/8735072/double-map-in-haskell

printBoard :: Board -> IO()
printBoard = mapM_ (print . map getFieldPrint)

printBoardIndexies :: Board -> IO() -- funkcja ktra wypisuje do konsoli indexy wszystkich elementów (dla testów)
printBoardIndexies board = mapM_ (putStrLn . showTup) (getIndexies board)

checkIfAllFieldGetStateNotNull :: Board -> [(Int, Int)] -> IO() -- funkcja która wypisuje do konsoli elementy z boarda po kolei ( ta funkcja jest dla testu iteracji po macierzy)
checkIfAllFieldGetStateNotNull _ [] = print ":("
checkIfAllFieldGetStateNotNull board ((x,y):xs)
    | ( getState (getField board x y)) == Null = checkIfAllFieldGetStateNotNull board xs  -- warunek w celach zapętlenia przejścia przez calą listę, ponieważ wsyztsko jest ustawione na NULL
    | otherwise = print (getNotNullValue (getField board x y)) -- przejdzie przez calę listę jak ona się zrobi pusta to wypiszę smutną buźkę ;(

showTup :: (Show a, Show b) => (a,b) -> String
showTup (a,b) = (show a) ++ "," ++ (show b)

showValue :: (Show a) => a -> String
showValue (a) = show a