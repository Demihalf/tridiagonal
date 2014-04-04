module OptionsParser
 ( Options(..)
 , programOpts
 , usageMessage
 ) where

import System.Console.GetOpt
import Data.Char

import Equation

data Options = Options 
 { optTesting :: Bool
 , optMethod  :: Method
 , optRandom  :: Maybe Int
 , optHelp    :: Bool
 } deriving Show

defaultOptions = Options
 { optTesting  = False
 , optMethod   = MethodTDMA
 , optRandom   = Nothing
 , optHelp     = False
 }

readMethod :: String -> Method
readMethod "tdma" = MethodTDMA
readMethod "unstable" = MethodUnstable
readMethod _ = MethodTDMA

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

options :: [OptDescr (Options -> Options)]
options = 
 [ Option ['t'] ["testing"]
     (NoArg (\opts -> opts {optTesting = True}))
     "режим тестирования"
 , Option ['m'] ["method"]
     (ReqArg (\m opts -> opts {optMethod = readMethod (map toLower m)}) "METHOD")
     "метод решения (TDMA (метод прогонки, по умолчанию) либо Unstable (неустойчивый метод))"
 , Option ['r'] ["random"]
     (ReqArg (\n opts -> opts {optRandom = readMaybe n :: Maybe Int}) "N")
     "генерировать ввод случайным образом (N -- размерность уравнения)"
 , Option ['h'] ["help"]
     (NoArg (\opts -> opts {optHelp = True}))
     "показать справочное сообщение"
 ]

programOpts :: [String] -> IO Options
programOpts argv = case getOpt Permute options argv of
                     (o, _, []  ) -> return $ foldl (flip id) defaultOptions o
                     (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

header = "Usage: tridiagonal [OPTION...]"

usageMessage :: String
usageMessage = usageInfo header options
