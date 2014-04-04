-- Версия на основе Array

module Tridiagonal (
      eqData
    , getSolution
    ) where

import Data.List (zip5, mapAccumL, mapAccumR)
import Data.Array (Array, Ix, array, (!), elems, listArray)

-- | Входные данные уравнения: коэффициенты a, b, c и d соответственно.
data EqData a = EqData (Array Int a) (Array Int a) (Array Int a) (Array Int a)

-- | Принимает четыре непустых массива коэффициентов a, b, c и d. 
-- Длины списков должны быть одинаковы (все элементы, лежащие за границами
-- списка минимальной длины, игнорируются). 
eqData :: [a] -> [a] -> [a] -> [a] -> EqData a
eqData a b c d = EqData a' b' c' d'
    where
        n = minimum $ map length [a, b, c, d]
        [a', b', c', d'] = map (listArray (1, n)) [a, b, c, d]

-- | Возвращает длину массива
arraySize :: (Ix i) => Array i a -> Int
arraySize = length . elems

-- | Возвращает два массива: [L_i] и [M_i]
getLM :: (RealFloat a) => EqData a -> (Array Int a, Array Int a)
getLM (EqData a b c d) = (ls, ms)
    where
        n = arraySize a
        ls = array (1, n) (
               (1, 0) : 
               [(i+1, c!i / (b!i - a!i * ls!i)) | i <- [1..n-1]]
             )
        ms = array (1, n+1) (
               (1, 0) : 
               [(i+1, (d!i - a!i * ms!i) / (b!i - a!i * ls!i)) | i <- [1..n]]
             )

-- | Возвращает решение системы в виде списка, либо Nothing, если решение не
-- найдено
getSolution :: (RealFloat a) => EqData a -> Maybe [a]
getSolution eqdata = if hasSolution then Just (elems xs) else Nothing
    where
        (ls, ms) = getLM eqdata
        n = arraySize ls
        hasSolution = not (any (\x -> isNaN x || isInfinite x) $ elems ls)
        xs = array (1, n) (
               (n, ms!(n+1)) : 
               [(i-1, ms!i - ls!i * xs!i) | i <- reverse [2..n]]
             )
