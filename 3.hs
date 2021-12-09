import System.IO
import Data.Map (Map, empty, insert, member, elems)
import Data.List
import Data.Maybe (mapMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try, parse)
import Text.Parsec.Char (oneOf, char, digit, satisfy, spaces, letter)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit, chr, digitToInt)
import Data.Set (fromList, size, split, Set, elems, insert, splitMember)
import Data.Bits

int:: Parser Int
int = do
    d <- digit
    if d == '0' then
        return 0
    else
        return 1

d = do
    many (do
        m <- many int
        char '\n'
        return m)

loadData = parse d ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s


toInt:: [Int] -> Int
toInt = toDec . concatMap  show

toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

getNext:: Int -> Int -> (Int, Int) -> (Int, Int)
getNext len a (p, val)  = (p+1, val .|. ( a `shift` (len - (p + 1)) ) )

splitUntilSingleton:: Int -> (Int -> Int -> Bool) -> Set Int -> (Int, Int) -> Set Int
splitUntilSingleton len comp set (p,val) | Data.Set.size set == 1 = set
                                         | Data.Set.size sm `comp` Data.Set.size gr = splitUntilSingleton len comp sm (getNext len 0 (p,val))
                                         | otherwise = splitUntilSingleton len comp gr (getNext len 1 (p,val))
                                            where (o, co) = getNext len 1 (p,val)
                                                  (a, memb, b) = splitMember co set
                                                  (sm, gr) = if memb then (a, Data.Set.insert co b) else (a,b)

helper :: [[Int]] -> IO ()
helper l = do
    let len = length (head l)
    let valuesSet = (fromList . map toInt) l
    let solA = splitUntilSingleton len (>) valuesSet (0,0)
    let solB = splitUntilSingleton len (<=) valuesSet (0,0)
    let a = head $ Data.Set.elems solA
    let b = head $ Data.Set.elems solB
    print (a*b)
