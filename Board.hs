module Board where

import Data.List
import Data.Array
import Data.Maybe
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

changeState :: Board -> (Int,Int) -> State -> Board -- wspolrzedne pola jako (wiersz, kolumna)
changeState [] _ _ = []
changeState (row : mosaic) (y,x) state | y > 0 = [row] ++ changeState mosaic (y - 1, x) state 
                                       | y == 0 = [checkCell row x state] ++ mosaic

checkCell :: [Field] -> Int -> State -> [Field]
checkCell [] _ _ = [] -- zamalowuje x-ta komorke w wierszu
checkCell (cell : row) 0 state = [(Field state (getValue cell) True)] ++ row
checkCell (cell : row) x state = [cell] ++ checkCell row (x - 1) state

cutCellGroup :: Board -> (Int, Int) -> Board -- zwraca fragment planszy wokol podpowiedzi (od 4x4 do 9x9 zaleznie od polozenia odpowiedzi)
cutCellGroup mosaic (y,x) = map (take (x2 - x1 + 1)) (map (drop x1) (take (y2 - y1 + 1) (drop y1 mosaic))) 
                                    where y1 = if (y - 1 < 0) then y else (y - 1)
                                          y2 = if (y + 1 >= (length $ mosaic)) then y else (y + 1)
                                          x1 = if (x - 1 < 0) then x else (x - 1)
                                          x2 = if (x + 1 >= (length $ head(mosaic))) then x else (x + 1)

countStateForClue :: Board -> (Int, Int) -> State -> Int -- zwraca liczbe pol o danym stanie wokol pola o zadanych wspolrzednych
countStateForClue mosaic (y,x) state = sum (map length (map (filter (\field -> (getState field) == state)) (cutCellGroup mosaic (y,x))))


collectCluesAroundCell :: Board -> (Int, Int) -> [((Int,Int), Field)] -- zbierz podpowiedzi dotykajace pola w jedna liste - jako element listy krotka: wspolrzedne pola z podpowiedzia + samo pole
collectCluesAroundCell mosaic (y,x) = collectClues (cutCellGroup mosaic (y,x)) (y - 1, x - 1)
                            where collectClues [] (_,_) = []
                                  collectClues mosaic (-1, x) = collectClues mosaic (0, x)
                                  collectClues mosaic (y, -1) = collectClues mosaic (y, 0)
                                  collectClues (row : mosaic) (i,j) = collectCluesFromRow row (i,j) ++ collectClues mosaic (i + 1, j)
                                      where collectCluesFromRow [] (_,_) = []
                                            collectCluesFromRow (cell : row) (k,l) = [((k,l), cell)] ++ collectCluesFromRow row (k, l + 1)

checkIfEqual :: Board -> (Int, Int) -> Field -> Bool -- sprawdz czy suma zamalowanych pol wokol pola z podpowiedzia jest rowna podpowiedzi
checkIfEqual mosaic (y,x) field = (countStateForClue mosaic (y,x) Filled) == (fromMaybe 0 (getValue field))

checkIfNotMore :: Board -> (Int, Int) -> Field -> Bool -- sprawdz czy suma zamalowanych pol wokol pola z podpowiedzia jest nie wieksza niz podpowiedz
checkIfNotMore mosaic (y,x) field = (countStateForClue mosaic (y,x) Filled) <= (fromMaybe 0 (getValue field))

checkValidityAroundPosition :: Board -> (Int,Int) -> Bool -- sprawdz czy dla podpowiedzi wokol pola wypelnienie pol jest prawidlowe
checkValidityAroundPosition mosaic (y,x) = checkCluesValidity mosaic (collectCluesAroundCell mosaic (y,x)) (y,x)
                            where checkCluesValidity mosaic [] (_,_) = True
                                  checkCluesValidity mosaic (((i,j), field) : clues) (y,x) | (i == y - 1 && j == x - 1) =  if equal -- podpowiedz u gory z lewej przerabianego pola
                                                                                                                           then equal && checkCluesValidity mosaic clues (y,x)
                                                                                                                           else False
                                                                                           | (i == y - 1 && x == lastColNum) =  if equal -- przerabiamy pole przy prawej krawedzi, podpowiedz nad polem
                                                                                                                                then equal && checkCluesValidity mosaic clues (y,x)
                                                                                                                                else False
                                                                                           | (j == x - 1 && y == lastRowNum) =  if equal -- przerabiamy pole przy dolnej krawedzi, podpowiedz na lewo od pola
                                                                                                                                then equal && checkCluesValidity mosaic clues (y,x)
                                                                                                                                else False
                                                                                           | (i == lastRowNum && j == lastColNum) =  if equal -- przerabiamy ostatnie pole (w prawym dolnym rogu)
                                                                                                                                     then True
                                                                                                                                     else False
                                                                                           | otherwise = if notMore 
                                                                                                         then notMore && checkCluesValidity mosaic clues (y,x)
                                                                                                         else False
                                                                                            where equal = checkIfEqual mosaic (i,j) field 
                                                                                                  notMore = checkIfNotMore mosaic (i,j) field
                                                                                                  lastColNum = length (head mosaic) - 1
                                                                                                  lastRowNum = length mosaic - 1


