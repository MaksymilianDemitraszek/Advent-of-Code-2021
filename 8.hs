import System.IO
import Data.Map (Map, empty, insert, member, elems, fromList, findWithDefault, (!))
import Data.List
import Data.Maybe (mapMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try, parse)
import Text.Parsec.Char (oneOf, char, digit, satisfy, spaces, letter)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

word:: Parser [Char]
word = do
    d <- many letter
    char ' '
    return d

wordWithPreSpace = do
    char ' '
    many letter

line:: Parser ([[Char]], [[Char]])
line = do
    z <- many word
    char '|'
    z2 <- many wordWithPreSpace
    char '\n'
    return (z, z2)

doc = do
    many line

loadData:: String -> Either ParseError [([[Char]], [[Char]])]
loadData = parse doc ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s

mapTuple f (a,b) = (f a, f b)

isSimple = (`elem` [2,4,3,7])

commonElem a b = head (a `intersect` b)

getL n = (head . filter ((==n).length))

isSubset a b = all (`elem` b) a

buildTranslator:: [[Char]] -> [([Char], Int)]
buildTranslator def =  [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
                    where one = getL 2 def
                          four = getL 4 def
                          seven = getL 3 def
                          eight = getL 7 def
                          nine = (head.filter (/= four) . filter (isSubset four). filter (/= eight)) def
                          rem = filter (not.(`elem` [nine, eight, seven, four, one])) def
                          zero = (head.filter ((==6).length). filter (isSubset one) ) rem
                          three = (head.filter ((==5).length). filter (isSubset one) ) rem
                          rem2 = filter (not.(`elem` [ three, zero, nine, eight, seven, four, one])) def
                          five = (head.filter (`isSubset` nine)) rem2
                          two = (head.filter ((==5).length). filter (/= five)) rem2
                          six = (head.filter ((==6).length)) rem2


transInt:: Map [Char] Int -> [[Char]] -> [Int]
transInt trans [] = []
transInt trans (l:ls) = (trans ! l):(transInt trans ls)

fromDigits:: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

buildVal:: [([Char], Int)] -> [[Char]] -> Int
buildVal trans ws = (fromDigits .  transInt (fromList trans) ) ws

helper :: [([[Char]], [[Char]])] -> IO ()
helper zs = do
    let unz = map snd zs
    let unz2 = map fst zs
    let result = (length.concatMap ( filter (isSimple.length))) unz
    print "PART 1"
    print result

    print "PART 2"
    let definitions = map (map sort) unz2
    let outs = map (map sort) unz

    let translators = map buildTranslator definitions
    let sumD = sum $ zipWith buildVal translators outs
    print sumD

