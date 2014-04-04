import System.IO
import System.Environment

import Data.List
import Data.Maybe

import Control.Monad
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B

import Equation
import TDMA
import OptionsParser

-- | Считывает Double из строки
readD :: B.ByteString -> Double
readD = fst . fromJust . B.readDouble

-- | Считывает из строки список вещественных чисел, разделенных пробелами
readDoubleList :: B.ByteString -> [Double]
readDoubleList = map readD . B.words

-- | Извлекает следующий вектор вещественных
-- чисел из stdin
nextVector :: IO [Double]
nextVector = liftM readDoubleList B.getLine

getEqData :: IO (EquationData Double)
getEqData = do [a, b, c, d] <- replicateM 4 nextVector
               return $ EquationData a b c d

-- | Считает норму разности между векторами a и b
dist :: (Real a) => [a] -> [a] -> a
dist a b = foldl1' max $ map abs $ zipWith (-) a b

main :: IO ()
main = do
    opts <- getArgs >>= programOpts
    eqd  <- getEqData
    x    <- nextVector
    let norm = liftM2 dist (return x) (getSolution eqd)
    case norm of
        Nothing -> putStrLn "Не удалось найти решение"
        Just x -> printf "Полученное решение отличается от точного на %f\n" x

