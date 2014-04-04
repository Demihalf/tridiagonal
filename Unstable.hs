{-# LANGUAGE BangPatterns #-}

module Unstable 
 ( getSolution
 ) where

import Data.List

import Equation

getY :: RealFloat a => EquationData a -> [a]
getY (EquationData a b c d) = 0 : y2 : (snd $ mapAccumL nextY (0, y2) (init $ tail (zip4 a b c d)))
    where
        y2                                 = head d / head c
        nextY (!yii, !yi) (ai, bi, ci, di) = ((yi, yi1), yi1)
         where yi1 = (di - ai * yii - bi * yi) / ci

getZ :: RealFloat a => EquationData a -> [a]
getZ (EquationData a b c _) = 1 : z2 : (snd $ mapAccumL nextZ (1, z2) (init $ tail (zip3 a b c)))
    where
        z2                             = -head b / head c
        nextZ (!zii, !zi) (ai, bi, ci) = ((zi, zi1), zi1)
         where zi1 = -(ai * zii + bi * zi) / ci

-- Возвращает решение системы в виде вектора с коэффициентами
getSolution :: RealFloat a => EquationData a -> Maybe (EquationSolution a)
getSolution eqData@(EquationData a b c d) = if hasSolution then Just xs else Nothing
    where
        (ys, zs)             = (getY eqData, getZ eqData)
        hasSolution          = not (any (\x -> isNaN x || isInfinite x) (k:ys))
        [an, bn, dn, yn, zn] = map last [a, b, d, ys, zs]
        [yn1, zn1]           = map (last . init) [ys, zs]
        k                    = (dn - an * yn1 - bn * yn) / (an * zn1 + bn * zn)
        xs                   = zipWith (\yi zi -> yi + k * zi) ys zs
