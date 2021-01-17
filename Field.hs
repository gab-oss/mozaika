module Field where 
import Data.Maybe
import Data.Char

type FieldValue = Maybe Int
data State = Filled | Empty | Null deriving(Eq,Show)

{-
    State : Stan pola
    FieldValue : wartość pola
    Bool : czy zostało już sprawdzone --TODO: nie wiem czy jest potrzebne ponniewż można ustawić state na NUll np   
-}
data Field = Field State FieldValue Bool deriving Show 

getState :: Field -> State
getState(Field s _ _) = s 

getValue :: Field -> FieldValue
getValue(Field _ v _) = v 

hasValue :: FieldValue -> Bool
hasValue value
    | value == Nothing = False
    | otherwise = True


getFieldProcessedStatus :: Field -> Bool
getFieldProcessedStatus(Field _ _ b) = b 

toDefaultField :: Char -> Field -- TODO: parse z pliczku uwzględniając format
toDefaultField '.' = Field Null Nothing False 
toDefaultField c = Field Null (Just (digitToInt c)) False 

getFieldPrint :: Field -> Char
getFieldPrint field 
    | ((getState field) == Filled) = '#'
    | ((getState field) == Empty) = '.'
    | ((getState field) == Null) = '?'
    | otherwise = '!'

checkCompleted :: Field -> Bool
checkCompleted field
    | ((getState field) == Null) = False
    | otherwise = True
    