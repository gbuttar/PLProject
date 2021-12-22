module OCR (convert) where
import Data.List (find, transpose, intercalate)
import Data.Bifunctor (first)
groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs =
    let (ys, zs) = splitAt n xs
    in  ys : groups n zs
zero :: [String]
zero =
  [ " _ ",
    "| |",
    "|_|",
    "   " ]
one :: [String]
one =
  [ "   ",
    "  |",
    "  |",
    "   " ]
two :: [String]
two =[ " _ "
     , " _|"
     , "|_ "
     , "   " ]
three :: [String]
three = [ " _ "
        , " _|"
        , " _|"
        , "   " ]
four :: [String]
four = [ "   "
       , "|_|"
       , "  |"
       , "   " ]
five :: [String]
five = [ " _ "
       , "|_ "
       , " _|"
       , "   " ]
six :: [String]
six = [ " _ "
      , "|_ "
      , "|_|"
      , "   " ]
seven :: [String]
seven = [ " _ "
        , "  |"
        , "  |"
        , "   " ]
eight :: [String]
eight = [ " _ "
        , "|_|"
        , "|_|"
        , "   " ]
nine :: [String]
nine = [ " _ "
       , "|_|"
       , " _|"
       , "   " ]
pattern :: [([String], Char)]
pattern = map (first transpose) [(zero, '0'), (one, '1'), (two, '2'), (three, '3'), (four, '4'), (five, '5'), (six, '6'), (seven, '7'), (eight, '8'), (nine, '9')]
parseDigit :: [String] -> Char
parseDigit strs = maybe '?' snd . find (\(patt, _) -> patt == strs) $ pattern
convertLine :: [String] -> String
convertLine = map parseDigit . groups 3 . transpose
convert :: String -> String
convert = intercalate "," . map convertLine . groups 4 . lines