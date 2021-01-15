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
    puzzle <- readPuzzle "puzzle"
    print puzzle
    let test = parseToBoard puzzle
    let indexies = getIndexies test
    checkIfAllFieldGetStateNotNull test indexies
    -- printBoardIndexies test
