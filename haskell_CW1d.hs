import Data.Char (intToDigit, digitToInt, isDigit)

-- 1. chop --

chomp :: String -> String

-- Base case. If the input is an empty string, return an empty string. Prevents the recursion looping indefinitely. 'while(NULL == NULL)'.
chomp [] = []

-- Recursive case. Splits input string into head and tail. Then constructs a new string with x as the head
-- and adds each character that is equal to x (inside xs) to the tail, until a character that is not equal to x is reached. 
chomp (x:xs) = x : takeWhile (== x) xs

-- 2. munch --

munch :: String -> String

-- Peform chomp on input string, then return the first 9 characters of the chomp result.
munch = take 9 . chomp

-- 3. runs --

runs :: String -> [String]

-- Base case.
runs [] = []

-- Performs 'runs' on list 'xs'. Splits 'xs' into head and tail 'currentRun : remaining'.
-- 'runs remaining' recursively calls 'runs' to perform the operation on 'remaining'. 
-- On the first iteration of the program, 'runs remaining' is encountered and is then scheduled as the next iteration, because Haskell uses lazy evaluation.  
runs xs = currentRun : runs remaining
    where
        -- Performs the 'munch' function on 'xs' and assigns to currentRun. 
        currentRun = munch xs
        -- Removes the characters in currentRun from xs and assigns to remaining.
        remaining = drop (length currentRun) xs

-- 4. encode --

encode :: String -> [(Char, Int)]

-- On the right side list comprehension is being used to build tuples for each run of characters. 'runs str' performs the runs function on the input string.
-- For each run evaluated by 'runs str' create a tuple where the left side is the letter of the run (head) and the right side is the length of the run.
encode str = [(head run, length run) | run <- runs str]

-- 5. flatten --

flatten :: [(Char, Int)] -> String

-- concatMap applies the function to each tuple in the list. The function converts each tuple into a string,
-- the left side being a character and the right side being an integer converted to a string.
-- It then concatenates the separate strings into a single string.
flatten = concatMap (\(c, n) -> c : show n)

-- 6. compress --

-- Uses encode to get a list of tuples then uses flatten to compress the tuples into a single string. 
--It combines both functions and means we only need to input a single string.
compress :: String -> String
compress = flatten . encode

-- 7. decode --

-- concatMap applies the function to each tuple in the list. For each tuple 'replicate n c' creates a string where the character 'c' is repeated 'n' times.
-- It then concatenates all the repeated characters from each tuple into a single string.
decode :: [(Char, Int)] -> String
decode = concatMap (\(c, n) -> replicate n c)

-- 8. expand --

-- Recursively processes the string, takes each character and how many times they repeat.
-- 'span' separates the numbers from the rest of the string.
-- 'read' then converts the string of numbers into an integer.
-- then expands the string into a list of tuples where the number is and integer.
expand [] = []
expand (c:rest) = (c, read numStr) : expand remaining
  where
    (numStr, remaining) = span isDigit rest

-- 9. decompress --

-- expands the string using the expand function to create a list of tuples.
-- Turns the list of tuples into a string using the decode function.
decompress :: String -> String
decompress = decode . expand
