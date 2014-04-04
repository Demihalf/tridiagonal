{-# LANGUAGE BangPatterns #-}

module TDMA 
 ( getSolution
 ) where

import Data.List

import Equation

-- Возвращает два списка: [L_i] и [M_i]
getLM :: RealFloat a => EquationData a -> ([a], [a])
getLM (EquationData a b c d) = (ls, ms)
    where
        ls = init $ scanl nextL 0 (zip3 a b c)
        ms = tail $ scanl nextM 0 (zip5 a b c d ls)
        nextL !li (ai, bi, ci)         = ci / (bi - ai * li)
        nextM !mi (ai, bi, ci, di, li) = (di - ai * mi) / (bi - ai * li)

-- Возвращает решение системы в виде вектора с коэффициентами
getSolution :: RealFloat a => EquationData a -> Maybe (EquationSolution a)
getSolution eqData = if hasSolution then Just xs else Nothing
    where
        (ls, ms)          = getLM eqData
        hasSolution       = not (any (\x -> isNaN x || isInfinite x) ls)
        prevX (li, mi) xi = mi - li * xi
        xs                = scanr prevX (last ms) (zip (tail ls) (init ms))
