import Data.Char
decode a b | null a        = a
           | isAlphaUpper (head a) || isAlphaLower (head a)
                           = [add (head a) (head b)] ++  decode (tail a) (rotateString b)
           | otherwise     = [head a] ++ decode (tail a) b

encode a b | null a        = a
           | isAlphaUpper (head a) || isAlphaLower (head a)
                           = [sub (head a) (head b)] ++  encode (tail a) (rotateString b)
           | otherwise     = [head a] ++ encode (tail a) b


add:: Char -> Char -> Char
add a b |  isAlphaUpper b || isAlphaUpper a = add (toAlphaLower a) (toAlphaLower b)
        |  otherwise =  rollLetter (chr (ord a + (ord b - ord 'a')))      --
sub:: Char -> Char -> Char
sub a b |  isAlphaUpper b || isAlphaUpper a = sub (toAlphaLower a) (toAlphaLower b)
        |  otherwise =  rollLetter (chr (ord a - (ord b - ord 'a')))      --

rollLetter a = chr (roll (ord  a) (ord 'z') (ord 'a'))
-- This code should probably be reused at some point in time. Just saying.
roll a large small | large < small = roll a small large
                   | a> large = roll (a - (large - small + 1)) large small
                   | a< small = roll (a + (large - small + 1)) large small
                   | otherwise = a

-- Somewhere, the recursion is going to infinity. Need to figure out why.
-- Solution found. Inequality was required to be non-strict. -- Very silly mistake.It kept on toggling between 'z' and 'a' in some caases.
--Usual alphabet case manipulation. Don't want to use pre-built functions.
--Besides, what's the fun in doing that?
isAlphaLower:: Char -> Bool
isAlphaLower a = elem a ['a'..'z']
isAlphaUpper:: Char -> Bool
isAlphaUpper a = elem a ['A'..'Z']
isAlphaVowel:: Char -> Bool
isAlphaVowel a = elem (toLower a) ['a','e','i','o','u']
toAlphaLower:: Char -> Char
toAlphaLower a = if isUpper a then chr (ord a + (ord 'a' - ord 'A')) else a
toAlphaUpper:: Char -> Char
toAlphaUpper a = if isLower a then chr (ord a - (ord 'a' - ord 'A')) else a
rotateString:: String -> String
rotateString (x:xs) = xs ++ [x]
