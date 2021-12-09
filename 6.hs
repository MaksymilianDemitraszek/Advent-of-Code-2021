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

data Fish = Fish Int Int
    deriving (Show)

comparePair (a,b) (c,d) = case compare a c of
                                EQ -> compare b d
                                other -> other

isIn a = any ((==a).fst)

addTo v n = map (\(a,b) -> if a == n then (a, b+v) else (a,b))

addUp v n fsh = if isIn n fsh then addTo v n fsh else sortBy comparePair ((n,v):fsh)

nextIter:: [(Int, Int)] -> [(Int,Int)]
nextIter fsh = result
            where nxt = map (\(a,b) -> (a-1, b)) fsh
                  (f, fc) = head nxt
                  ref = if f == -1 then addUp fc 8 $ addUp fc 6 (tail nxt) else nxt
                  result = sortBy comparePair ref

howMany = sum . map snd


helper :: [Int] -> IO ()
helper zs = do
    -- let groups = foldr (\(e:es) a -> M.insert e (length (e:es)) a ) M.empty $ (group.sort) zs
    let groups = (map (\(e:es) -> (e, length (e:es))).group.sort) zs
    let end = foldl (\a _ -> nextIter a) groups [1..80]
    print "PART 1"
    print (howMany end)
    let end2 = foldl (\a _ -> nextIter a) groups [1..256]
    print "PART 2"
    print (howMany end2)

