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

-- solve puzzle = print ( checkCell (head (emptyMosaic puzzle)) 3 )
solve puzzle = print (checkNext (emptyMosaic puzzle) (1,3))

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
