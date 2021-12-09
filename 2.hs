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
import Data.Char (isLetter, isDigit)

int:: Parser Int
int = do
    d <- many digit
    char '\n'
    return (read d :: Int)

d = do
    many (do
        direction <- many letter
        spaces
        m <- many digit
        char '\n'
        return (direction, read m :: Int))

loadData = parse d ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s



-- first 
-- helper :: [(String, Int)] -> IO ()
-- helper zs = do
--     let up = (sum . map snd . filter (\(e, _) -> e == "up")) zs
--     let down = (sum . map snd . filter (\(e, _) -> e =="down")) zs
--     let forward = (sum . map snd . filter (\(e, _) -> e =="forward")) zs
--     print ((down - up)*forward)

-- second 
iter (a, d, h) ("forward", n) = (a, d+(a*n), h+n)
iter (a, d, h) ("down", n) = (a+n, d, h)
iter (a, d, h) ("up", n) = (a-n, d, h)

helper :: [(String, Int)] -> IO ()
helper zs = do
    let (a,d,h) = foldl iter (0, 0, 0) zs
    print (d*h)

