import System.IO
import System.Environment

import Data.List
import Data.Char
import Data.Maybe

import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B

import TridiagonalList

-- | Считывает Double из строки
readD :: B.ByteString -> Double
readD = fst . fromJust . B.readDouble

-- | Считывает из строки список вещественных чисел, разделенных пробелами
readDoubleList :: B.ByteString -> [Double]
readDoubleList = map readD . B.words

-- | По идентификатору файла handle извлекает следующий вектор вещественных
-- чисел
nextVector :: Handle -> IO [Double]
nextVector handle = liftM readDoubleList (B.hGetLine handle)

-- | Извлекает n векторов вещественных чисел из файла с именем f
fileReadDoubleLists :: Int -> String -> IO [[Double]]
fileReadDoubleLists n f = withFile f ReadMode $ ((replicateM n) . nextVector)

-- | Извлекает 5 первых векторов из файла (a, b, c, d, x)
fileArgsAns :: String -> IO [[Double]]
fileArgsAns = fileReadDoubleLists 5

-- | Извлекает 4 первых вектора из файла (a, b, c, d)
fileArgs :: String -> IO [[Double]]
fileArgs = fileReadDoubleLists 5

-- | Считает норму разности между векторами a и b
dist :: (Real a) => [a] -> [a] -> a
dist a b = foldl1' max $ map abs $ zipWith (-) a b

main :: IO ()
main = do
    [f] <- getArgs
    [a,b,c,d,x] <- fileArgsAns f
    let eqd = eqData a b c d

    print $ liftM2 dist (return x) (getSolution eqd)
