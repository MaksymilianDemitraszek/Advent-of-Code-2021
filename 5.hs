import System.IO
import Data.Map (Map, empty, insert, member, elems)
import Data.List
import Data.Maybe (mapMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try, parse)
import Text.Parsec.Char (string, oneOf, char, digit, satisfy, spaces)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import qualified Data.Set as S

data Point = Point Int Int
    deriving (Eq, Show)

instance Ord Point where
    compare = comparePointByX

data Vector = Vector Point Point
    deriving (Eq, Show)


int:: Parser Int
int = do
    d <- many digit
    return (read d :: Int)

line = do
    a <- int
    char ','
    b <- int
    spaces
    string "->"
    spaces
    x <- int
    char ','
    y <- int
    char '\n'
    let pointA = (Point a b)
    let pointB = (Point x y)
    return (Vector (minBy comparePointByX pointA pointB) (maxBy comparePointByX pointA pointB))

loadData:: String -> Either ParseError [Vector]
loadData = parse (many line) ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s


isHorizontal (Vector (Point a b) (Point x y)) = b == y
isVertical (Vector (Point a b) (Point x y)) = a == x


minBy o a b = minimumBy o [a,b]
maxBy o a b = maximumBy o [a,b]

comparePointByX (Point a b) (Point c d) = case compare a c of
                                            EQ -> compare b d
                                            r -> r

comparePointByY (Point a b) (Point c d) = case compare b d of
                                            EQ -> compare a c
                                            r -> r

compareByX (Vector a b)(Vector c d) = case comparePointByX a c of
                                            EQ -> comparePointByX b d
                                            r -> r

compareByY (Vector a b)(Vector c d) = case comparePointByY b d of
                                            EQ -> comparePointByY a c
                                            r -> r

eqByY (Vector (Point a b) k)(Vector (Point c d) f) = b == d

toPoints:: Vector -> [Point]
toPoints (Vector (Point a b) (Point c d)) | a == c && b == d = [Point a b]
                                          | a == c = (Point a b):toPoints (Vector (Point a (b + 1)) (Point c d) )
                                          | b == d = (Point a b):toPoints (Vector (Point (a + 1) b) (Point c d) )
                                          | a < c && b < d = (Point a b):toPoints (Vector (Point (a + 1)(b+1)) (Point c d) )
                                          | a < c && b > d = (Point a b):toPoints (Vector (Point (a + 1)(b - 1)) (Point c d) )
                                          | a > c && b < d = (Point a b):toPoints (Vector (Point (a + 1)(b-1)) (Point c d) )
                                          | otherwise = (Point a b):toPoints (Vector (Point (a - 1)(b-1)) (Point c d) )
helper :: [Vector] -> IO ()
helper zs = do
    let hsPoints = sortBy comparePointByY $ concatMap toPoints zs
    -- let inter = S.toList $ S.intersection (S.fromList hsPoints) (S.fromList vsPoints)
    -- let hs = (filter ((>1).length)) $ group $ (sortBy comparePointByY) (hsPoints ++ (concat [[a,a] | a <- inter]))
    let hs = (filter ((>1).length)) $ group hsPoints
    -- print (hs)
    print (length hs) 

