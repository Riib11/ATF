module Utilities
( beheaded_by
, join
, join_container
, join_sep
, is_empty_string
) where

beheaded_by :: String -> String -> Maybe String
string `beheaded_by` target = case (string, target) of
    (s, "")      -> Just s
    ("", x:xs)   -> Nothing
    (s:ss, x:xs) -> if s == x
        then ss `beheaded_by` xs
        else Nothing

join :: [String] -> String
join = foldr (++) ""

join_sep :: String -> [String] -> String
join_sep div []        = []
join_sep div (x:[])    = x
join_sep div (x:y:xs) = x ++ div ++ (join_sep div $ y:xs)

join_container :: String -> String -> ([String] -> String)
join_container begin end xs = begin ++ (join xs) ++ end

join_sep_container :: String -> String -> String -> ([String] -> String)
join_sep_container begin end div xs = begin ++ (join_sep div xs) ++ end

is_empty_string :: String -> Bool
is_empty_string s = case s of
    "" -> True
    (x:xs) -> if x `elem` [' ', '\n']
        then is_empty_string xs
        else False
