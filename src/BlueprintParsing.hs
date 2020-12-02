{-# LANGUAGE LambdaCase #-}

module BlueprintParsing where

import Blueprint
import Data.Maybe
import Geometry
import Machine
import Operator
import Wire

-- this code is messy and hacky and should be rewritten for production
-- it mostly aids in writing complex test cases and visualizing things for debugging
-- it includes partial functions. use at your peril. 

parseChar :: Char -> Maybe Machine
parseChar ' ' = Nothing
parseChar '[' = Just $ Wire NE
parseChar ']' = Just $ Wire NW
parseChar '{' = Just $ Wire SE
parseChar '}' = Just $ Wire SW
parseChar '|' = Just $ Wire Vertical
parseChar '_' = Just $ Wire Horizontal
parseChar '.' = Just $ Wire Overlap
-- initially parsing ops to Source 0 is a bad hack, but it works
parseChar '+' = Just $ Source 0
parseChar '-' = Just $ Source 0
parseChar '*' = Just $ Source 0
parseChar '/' = Just $ Source 0
parseChar c =
  case charToValue c of
    Just n -> Just $ Source n
    Nothing -> Nothing

parseCharToGoal :: Char -> Maybe Machine
parseCharToGoal c = Sink <$> charToValue c

parseCharToOperator :: Char -> Maybe Machine
parseCharToOperator '+' = Just $ Op additionOperator
parseCharToOperator '-' = Just $ Op subtractionOperator
parseCharToOperator '*' = Just $ Op multiplicationOperator
parseCharToOperator '/' = Just $ Op divisionOperator
parseCharToOperator 'F' = Just $ Op factoringOperator
parseCharToOperator 'D' = Just $ Op divisionOperator
parseCharToOperator x = error $ "cannot parse " ++ [x] ++ " to operator"

parseCharToOccupied :: Char -> Maybe Machine
parseCharToOccupied 'V' = Just Occupied
parseCharToOccupied _ = Nothing

stringToBlueprint :: String -> Maybe Blueprint
stringToBlueprint s =
  case lines s of
    [] -> Nothing
    [_] -> Just $ blankBlueprint 0 0
    (header : body@(first : _)) ->
      Just $ stbFoldBody body (stbParseHeader header) (length body - 1) $ blankBlueprint (length first - 1) (length body)
  
stbFoldBody :: [String] -> [Char -> Maybe Machine] -> Int -> Blueprint -> Blueprint
stbFoldBody [] _ _ b = b
stbFoldBody (l : ls) parsers height b =
    let (newParsers, f) = stbProcessLine l parsers 0 height
    in stbFoldBody ls newParsers (height - 1) $ f b

stbProcessLine :: String -> [Char -> Maybe Machine] -> Int -> Int -> ([Char -> Maybe Machine], Blueprint -> Blueprint)
stbProcessLine ('@' : _) parsers _ _ = (parsers, id)
stbProcessLine (c : cs) parsers width height =
    case (parseChar c, parsers) of
    (Just (Source _), p : ps) ->
        let (a, b) = stbProcessLine cs ps (width + 1) height
            m = fromJust $ p c
        in (a, b . placeMachineAt (Point width height) m)
    (Just m, _) ->
        let (a, b) = stbProcessLine cs parsers (width + 1) height
        in (a, b . placeMachineAt (Point width height) m)
    (_, _) -> stbProcessLine cs parsers (width + 1) height

stbParseHeader :: String -> [Char -> Maybe Machine]
stbParseHeader =
    mapMaybe
    ( \case
        'S' -> Just parseChar
        'G' -> Just parseCharToGoal
        'O' -> Just parseCharToOperator
        'V' -> Just parseCharToOccupied
        ' ' -> Nothing
        '.' -> Nothing
    )

blueprintToString :: Blueprint -> String
blueprintToString b = btsMakeHeader b ++ "\n" ++ btsMakeBody b

btsMakeHeaderSegment :: Int -> Blueprint -> String
btsMakeHeaderSegment height b = 
    let temp = aux 0 height b in
        if temp == "" then "." else temp
    where
        aux :: Int -> Int -> Blueprint -> String
        aux w _ b | w >= width b = ""
        aux w h b = case getMachineAt (Point w h) b of 
            Just (Source _) -> 'S' : aux (w + 1) h b
            Just (Sink _) -> 'G' : aux (w + 1) h b
            Just (Op _) -> 'O' : aux (w + 1) h b
            Just Occupied -> 'V' : aux (w + 1) h b
            _ -> aux (w + 1) h b

btsMakeHeader :: Blueprint -> String
btsMakeHeader b = unwords $ map (`btsMakeHeaderSegment` b) [height b - 1, height b - 2 .. 0]

btsMakeBody :: Blueprint -> String
btsMakeBody b = aux 0 (height b - 1) b
    where
    aux :: Int -> Int -> Blueprint -> String
    aux w (-1) b = ""
    aux w h b | w >= width b = '@' : '\n' : aux 0 (h - 1) b
    aux w h b =
        case getMachineAt (Point w h) b of
        Just (Op op) -> opToChar op : aux (w + 1) h b
        Just (Source n) -> valueToChar n : aux (w + 1) h b
        Just (Sink n) -> valueToChar n : aux (w + 1) h b
        Just Occupied -> 'V' : aux (w + 1) h b
        Just (Wire wt) -> bpWireChar wt : aux (w + 1) h b
        Nothing -> ' ' : aux (w + 1) h b
    bpWireChar Horizontal = '_'
    bpWireChar Vertical = '|'
    bpWireChar Overlap = '.'
    bpWireChar NE = '['
    bpWireChar NW = ']'
    bpWireChar SE = '{'
    bpWireChar SW = '}'