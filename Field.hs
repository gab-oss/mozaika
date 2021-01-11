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

isNotNothingValue :: FieldValue -> Bool
isNotNothingValue value
    | value == Nothing = False
    | otherwise = True

isNothingValue :: FieldValue -> Bool
isNothingValue value
    | value == Nothing = True
    | otherwise = False

getNotNullValue :: Field -> Int
getNotNullValue field = fromMaybe 11 (getValue field) -- 11 nie poprawna wartość pola

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