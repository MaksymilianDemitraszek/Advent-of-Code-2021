import System.IO
import Data.Map (Map, empty, insert, member, elems)
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
    d <- many digit
    char '\n'
    return (read d :: Int)

numbers = do
    many int

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


-- First 
-- fnc e (n, l) = if l > e then (n+1,e) else (n,e)

-- helper :: [Int] -> IO ()
-- helper ns = do
--     let fst = head ns
--     let sol = foldr fnc (0, fst) (tail ns)
--     print sol

buildWindows:: [Int] -> [[Int]]
buildWindows (a:b:c:ns) = [a,b,c]:buildWindows (b:c:ns)
buildWindows (a:b:ns) = []


fnc e (n, l) = if l > e then (n+1,e) else (n,e)

-- Second
helper :: [Int] -> IO ()
helper zs = do
    let ns = (map sum.buildWindows) zs
    let fst = head ns
    let sol = foldr fnc (0, fst) (tail ns)
    print sol


