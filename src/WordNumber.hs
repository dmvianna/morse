
module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = ["zero",
                 "one",
                 "two",
                 "three",
                 "four",
                 "five",
                 "six",
                 "seven",
                 "eight",
                 "nine"] !! n
                                                          

digits :: Int -> [Int]
digits n = go n []
    where go x xs
              | div x 10 == 0 = mod x 10 : xs
              | otherwise = go (div x 10) (mod x 10 : xs)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
