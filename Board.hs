module Board where

import Data.List
import Field

type Board = [[Field]]

countRows :: Board -> Int
countRows board = length board

countColumns :: Board -> Int
countColumns board = length (head board)

parseToBoard :: [String] -> Board
parseToBoard = map (map toDefaultField) --https://stackoverflow.com/questions/8735072/double-map-in-haskell

printBoard :: Board -> IO()
printBoard = mapM_ (print . map getFieldPrint)