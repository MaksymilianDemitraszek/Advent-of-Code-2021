import System.IO
import qualified Data.Map as M
import Data.List
import Data.Maybe (mapMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try, parse)
import Text.Parsec.Char (oneOf, char, digit, satisfy, spaces)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

int:: Parser Int
int = do
    char ','
    d <- many digit
    return (read d :: Int)

numbers = do
    d <- many digit
    ds <- many int
    char '\n'
    return ((read d :: Int):ds)

loadData:: String -> Either ParseError [Int]
loadData = parse numbers ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s

costSingle a b = abs (a - b)

costSingle2 a b = ((1 + steps)*steps) `div` 2
                where steps = abs (a - b)

cost:: [Int] -> Int -> Int
cost a c = (sum . map (costSingle c)) a

cost2:: [Int] -> Int -> Int
cost2 a c = (sum . map (costSingle2 c)) a


helper :: [Int] -> IO ()
helper zs = do
    let minCost = (minimum . map (cost zs)) [(minimum zs)..(maximum zs)]
    print "PART 1"
    print minCost
    let minCost2 = (minimum . map (cost2 zs)) [(minimum zs)..(maximum zs)]
    print "PART 2"
    print minCost2





