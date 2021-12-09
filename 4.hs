import System.IO
import Data.List (maximumBy, minimumBy)
import Data.Map (Map, empty, insert, member, elems, lookup)
import Data.Maybe (mapMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec (try, parse)
import Text.Parsec.Char (oneOf, char, digit, satisfy, spaces, string)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

type Board = [[Int]]

int:: Parser Int
int = do
    d <- many digit
    return (read d :: Int)

nextInt = do
    n <- int
    char ','
    return n

numbers = do
    numbs <- many ( try nextInt )
    last <- int
    char '\n'
    return (numbs ++ [last])

remainingCells = do
    (char ' ' >> spaces >> cells) <|> return []

cells = do
    spaces
    f <- int
    fs <- remainingCells
    return (f:fs)

row:: Parser [Int]
row = do
    c <- cells
    char '\n'
    return c


board:: Parser [[Int]]
board = do
    a <- row
    b <- row
    c <- row
    d <- row
    e <- row
    return [a,b,c,d,e]

remainingBoards = do
    (char '\n' >> boards) <|> return []

boards = do
    b <- board
    bs <- remainingBoards
    return (b:bs)


parseDoc = do
    numbs <- numbers
    char '\n'
    b <- boards
    return (numbs, b)


loadData:: String -> Either ParseError ([Int], [Board])
loadData = parse parseDoc ""

main :: IO ()
main = do
    let list = []
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    case loadData contents of
        (Right ns) -> helper ns
        (Left s) -> print s


data Translator = Translator (Data.Map.Map Int Int) (Data.Map.Map Int Int)

wrap (Translator l r) n = Data.Map.lookup n l
unwrap (Translator l r) n  = Data.Map.lookup n r

buildTranslator l = fst $ foldl (\(Translator l r, i) e -> (Translator (Data.Map.insert e i l) (Data.Map.insert i e r), i+1)  ) (Translator empty empty, 0) l

toCols ([]:d) = []
toCols b = map head b:toCols (map tail b)

calcBoardValue b = minimum (map maximum b ++ map maximum (toCols b))


compareBoardVals:: ([[Int]], Int) -> ([[Int]], Int) -> Ordering
compareBoardVals (ab, avl) (bb, bvl) | avl < bvl = LT
                                     | avl == bvl = EQ
                                     | otherwise = GT


calcScore:: Translator -> (Board, Int) -> Int
calcScore tr (ab, z) = (sum . mapMaybe (unwrap tr) . filter (>z) . concat) ab * f
                    where Just f = unwrap tr z

-- Second
helper :: ([Int], [Board]) -> IO ()
helper (ord, boards) = do
    let trans = buildTranslator ord
    let trBoards = map (map (mapMaybe (wrap trans))) boards
    let calcedVals = map (\e -> (e, calcBoardValue e)) trBoards
    let best = minimumBy compareBoardVals calcedVals
    print "PART 1"
    print (calcScore trans best)
    print "PART 2"
    let best2 = maximumBy compareBoardVals calcedVals
    print (map (mapMaybe (unwrap trans)) (fst best2))
    print (calcScore trans best2)
