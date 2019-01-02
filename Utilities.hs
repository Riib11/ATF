module Utilities
( beheaded_by
, join
, join_container
, join_sep
, is_empty_string
, string_to_bool
, string_to_int
, begins_with
, splitted_with
, at
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

string_to_bool :: String -> Bool
string_to_bool s = case s of
    "true" -> True
    "false" -> False
    _ -> error $ "couldn't interpret '" ++ s ++ "' as a Bool"


string_to_int :: String -> Int
string_to_int = read

begins_with :: String -> String -> Bool
s `begins_with` x = case (s, x) of
    (_   , ""   ) -> True
    (""  , _    ) -> False
    (s:ss, x:xs) -> if s == x
        then ss `begins_with` xs
        else False

-- if any string in the list begins that given string,
-- then Just that string
-- else, Nothing
filter_beginner :: [String] -> String -> Maybe (String, String)
filter_beginner [] _ = Nothing
filter_beginner (x:xs) s = case s `beheaded_by` x of
    Nothing -> filter_beginner xs s
    Just rest -> Just (x, rest)

-- breaks string into list of string,
-- where a new entry is started whenever a substring
-- matches one of the targets
splitted_with :: String -> [String] -> [String]
string `splitted_with` targets =
    let helper :: String -> String -> [String]
        helper "" work = case work of
            "" -> []
            _ -> [work]
        helper ('\\':s:ss) work = helper ss (work ++ [s])
        helper (s:ss) work = case filter_beginner targets (s:ss) of
            -- no matches, so add char to work and continue
            Nothing -> helper ss (work ++ [s])
            -- matched `match`, so add work and match, and continue on rest
            Just (match, rest) -> work : match : helper rest ""
    in helper string ""

at :: [a] -> Int -> a
at list index =
    let helper [] _ = error
            $ "couldn't access element #" ++ (show index)
            ++ " in list of length " ++ (show $ length list)
        helper (x:xs) 0 = x
        helper (x:xs) i = xs `at` (i - 1)
    in helper list index

