import System.IO (readFile)
-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename -- odczytaj całą zawartość pliku
    let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
    return puzzle

emptyMosaic :: [String] -> [String] -- generuje puste rozwiazanie (same '.')
emptyMosaic puzzle = replicate (length puzzle) $ (replicate (length (head puzzle)) '.')

checkNext :: [String] -> (Int,Int) -> [String]
checkNext [] _ = []
checkNext (row : mosaic) (y,x) | y > 0 = [row] ++ checkNext mosaic (y - 1,x) -- zamaluj kolejna komorke (o wspolrzednych y,x - wiersz, kolumna)
                               | y == 0 = [checkCell row x] ++ mosaic

checkCell :: [Char] -> Int -> [Char]
checkCell [] _ = [] -- zamalowuje x-ta komorke w wierszu
checkCell (cell : row) 0 = ['#'] ++ row
checkCell (cell : row) x = [cell] ++ checkCell row (x - 1)  

cutCellGroup mosaic (y,x) = map (take (x2 - x1 + 1)) (map (drop x1) (take (y2 - y1 + 1) (drop y1 mosaic))) 
                                    where y1 = if (y - 1 < 0) then y else (y - 1)
                                          y2 = if (y + 1 >= (length $ mosaic)) then y else (y + 1)
                                          x1 = if (x - 1 < 0) then x else (x - 1)
                                          x2 = if (x + 1 >= (length $ head(mosaic))) then x else (x + 1)

countCheckedForClue mosaic (y,x) = sum (map length (map (filter (\n -> n == '#')) (cutCellGroup mosaic (y,x))))

-- solve puzzle = print ( checkCell (head (emptyMosaic puzzle)) 3 )
-- solve puzzle = print (checkNext (emptyMosaic puzzle) (1,3))
solve puzzle = print (countCheckedForClue (checkNext (emptyMosaic puzzle) (1,3)) (1,3))

main = do puzzle <- readPuzzle "puzzle"
          putStrLn (show $ length $ puzzle) -- wyświetl liczbę wierszy łamigłówki
          solve puzzle

-- do zrobienia - wersja minimum:
-- rozwiazanie jako lista stringow zlozonych z . i # (# = zamalowana komorka)
-- generowanie pustego rozwiazania +
-- zamalowywanie komorek +
-- drukowanie rozwiazania
-- funkcja sprawdzajaca poprawnosc rozwiazania dla konkretnej podpowiedzi 
-- funkcja sprawdzajaca poprawnosc (na razie przechodzimy za kazdym razem przez wszystkie komorki mozaiki)
-- funkcja 'rozwiaz'
-- funkcja wywowlywana rekrusywnie w 'rozwiaz'
