module Board where

import Data.List
import Data.Array
import Data.Maybe
import Field
import Control.Monad (forM_)

type Board = [[Field]]

getIndexies :: Board -> [(Int, Int)]
getIndexies board = [(column, row) | column <- [0..(countColumns board)-1], row <- [0..(countRows board)-1]]

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

showTup :: (Show a, Show b) => (a,b) -> String
showTup (a,b) = (show a) ++ "," ++ (show b)

showValue :: (Show a) => a -> String
showValue (a) = show a

getFieldNeighbour :: Board -> (Int,Int) -> (Int,Int)
getFieldNeighbour board (y,x) | (y == (countColumns board) - 1) && (x == (countRows board) - 1) = (0,0)
                              | x == (countRows board) -1 = (y+1,0)
                              | otherwise = (y, x+1) 

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
collectCluesAroundCell mosaic (y,x) = collectClues (cutCellGroup mosaic (y,x)) (y - 1, x - 1) -- wytnij fragment planszy wokol pola i zbierz liste podpowiedzi zaczynajac od lewego gornego rogu fragmentu
                            where collectClues [] (_,_) = []
                                  collectClues mosaic (-1, x) = collectClues mosaic (0, x) -- jesli wyrzucilo nas poza plansze, wracamy do (0,0)
                                  collectClues mosaic (y, -1) = collectClues mosaic (y, 0)
                                  collectClues (row : mosaic) (i,j) = collectCluesFromRow row (i,j) ++ collectClues mosaic (i + 1, j) -- iteracja po wierszach fragmentu
                                      where collectCluesFromRow [] (_,_) = []
                                            collectCluesFromRow (cell : row) (k,l) = if (isNothing (getValue cell)) -- iteracja po polach w wierszu
                                                                                     then [] ++ collectCluesFromRow row (k, l + 1) 
                                                                                     else [((k,l), cell)] ++ collectCluesFromRow row (k, l + 1) 
                                                

checkIfEqual :: Board -> (Int, Int) -> Bool -- sprawdz czy suma zamalowanych pol wokol pola z podpowiedzia jest rowna podpowiedzi
checkIfEqual mosaic (y,x) = (countStateForClue mosaic (y,x) Filled) == (fromMaybe 0 (getValue (getField mosaic y x)))

checkIfNotMore :: Board -> (Int, Int) -> Bool -- sprawdz czy suma zamalowanych pol wokol pola z podpowiedzia jest nie wieksza niz podpowiedz
checkIfNotMore mosaic (y,x) = (countStateForClue mosaic (y,x) Filled) <= (fromMaybe 0 (getValue (getField mosaic y x)))

checkValidityAroundPosition :: Board -> (Int,Int) -> Bool -- sprawdz czy dla podpowiedzi wokol pola wypelnienie pol jest prawidlowe
checkValidityAroundPosition mosaic (y,x) = checkCluesValidity mosaic (collectCluesAroundCell mosaic (y,x)) (y,x)
                            where checkCluesValidity mosaic [] (_,_) = True
                                  checkCluesValidity mosaic (((i,j), field) : clues) (y,x) | (i == y - 1 && j == x - 1) -- podpowiedz u gory z lewej przerabianego pola
                                                                                                || (i == y - 1 && x == lastColNum) -- przerabiamy pole przy prawej krawedzi, podpowiedz nad polem
                                                                                                    || (j == x - 1 && y == lastRowNum)-- przerabiamy pole przy dolnej krawedzi, podpowiedz na lewo od pola
                                                                                                        || (i == lastRowNum && j == lastColNum) =  if equal -- przerabiamy ostatnie pole (w prawym dolnym rogu)
                                                                                                                                                   then equal && checkCluesValidity mosaic clues (y,x)
                                                                                                                                                   else False
                                                                                           | otherwise = if notMore 
                                                                                                         then notMore && checkCluesValidity mosaic clues (y,x)
                                                                                                         else False
                                                                                            where equal = checkIfEqual mosaic (i,j)
                                                                                                  notMore = checkIfNotMore mosaic (i,j)
                                                                                                  lastColNum = length (head mosaic) - 1
                                                                                                  lastRowNum = length mosaic - 1

solve :: Board -> [(Int, Int)] -> IO()
solve _ [] = print "Empty"
solve board ((y,x):xs) 
    | ((countRows board) - 1 == x) && ((countColumns board) - 1 == y) && (checkValidityAroundPosition (changeState board (y,x) Empty) (y,x))  = printBoard (changeState board (y,x) Empty)
    | ((countRows board) - 1 == x) && ((countColumns board) - 1 == y) && (checkValidityAroundPosition (changeState board (y,x) Filled) (y,x))  = printBoard (changeState board (y,x) Filled)
    | otherwise = if validEmpty && validFilled
                  then do
                        solve (changeState board (y,x) Empty) xs 
                        solve (changeState board (y,x) Filled) xs
                  else 
                        if validEmpty 
                        then 
                            solve (changeState board (y,x) Empty) xs
                        else 
                            if validFilled 
                            then
                                solve (changeState board (y,x) Filled) xs
                            else return()
    where validEmpty = checkValidityAroundPosition (changeState board (y,x) Empty) (y,x)
          validFilled = checkValidityAroundPosition (changeState board (y,x) Filled) (y,x)
