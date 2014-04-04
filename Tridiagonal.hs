import System.IO
import System.Environment
import System.Exit

import Data.List
import Data.Maybe

import Control.Monad
import Text.Printf

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B

import Equation
import qualified TDMA
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

-- | Извлекает коэффициенты уравнения из stdin
getEqDataStdIn :: IO (EquationData Double)
getEqDataStdIn = do [a, b, c, d] <- replicateM 4 nextVector
                    return $ EquationData a b c d

-- | Генерирует случайный набор значений коэффициентов
getEqDataRandom :: Int -> IO (EquationData Double)
getEqDataRandom = undefined

-- | Аргумент соответствует optRandom
getEqData :: Maybe Int -> IO (EquationData Double)
getEqData Nothing   = getEqDataStdIn
getEqData (Just n)  = getEqDataRandom n

-- | Показать сообщение об использовании программы и выйти, если аргумент True
maybeShowUsage :: Bool -> IO ()
maybeShowUsage True = putStr usageMessage >> exitSuccess
maybeShowUsage _    = return ()

printListLn :: (Show a) => [a] -> IO ()
printListLn xs = mapM_ (\x -> putStr (show x ++ " ")) xs >> putStr "\n"

testingMode :: Bool -> Maybe (EquationSolution Double) -> IO ()
testingMode True (Just sol) = do x <- nextVector
                                 printListLn x
                                 print $ dist x sol
testingMode _ _             = return ()

printSolution :: Maybe (EquationSolution Double) -> IO ()
printSolution (Just sol) = printListLn sol
printSolution _          = noSolutionExit

noSolutionExit :: IO ()
noSolutionExit = hPutStrLn stderr "Не удалось найти решение" >> exitFailure

solveEquation :: Method -> EquationData Double -> Maybe (EquationSolution Double)
solveEquation MethodTDMA     = TDMA.getSolution
solveEquation MethodUnstable = undefined

-- | Считает норму разности между векторами a и b
dist :: (Real a) => [a] -> [a] -> a
dist a b = foldl1' max $ map abs $ zipWith (-) a b

main :: IO ()
main = do opts <- getArgs >>= programOpts
          maybeShowUsage (optHelp opts)
          eqd <- getEqData (optRandom opts) 
          let sol = solveEquation (optMethod opts) eqd
          printSolution sol
          testingMode (optTesting opts) sol
