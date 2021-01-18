import Data.List
import Data.Char
import System.IO (readFile)
import Field
import Board


-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename -- odczytaj całą zawartość pliku
    let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
    return puzzle

main = do
    puzzle <- readPuzzle "tests/puzzle"
    print puzzle
    let board = parseToBoard puzzle
    let indexies = getIndexies board
    solve board indexies
