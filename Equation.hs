module Equation
 ( EquationData(..)
 , EquationSolution
 , Method(..)
 ) where

-- | Входные данные уравнения: коэффициенты a, b, c и d соответственно.
data EquationData a = EquationData [a] [a] [a] [a]
    deriving (Show)

-- | Решение уравнения
type EquationSolution a = [a]

-- | Метод решения уравнения
data Method = MethodTDMA | MethodUnstable 
    deriving Show
