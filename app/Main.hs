module Main where

import Control.Lens
import Rec.Reflection


main :: IO ()
main = pure ()

data T = T1 { v :: Int, u :: String } | T2 Integer Bool Char
  deriving (Eq, Show, Ord)

_x :: (Int, Char, Bool, Bool, [Int])
_x = (1,'x',True,False,[2..10]) & rf @2 .~ 'y' & rf @5 %~ (1:)

mkRec ''T

data R = R { r1 :: Int, r2 :: String }
  deriving (Eq, Show, Ord)

mkRec ''R

_r :: R
_r = R 3 "test" & rf @"r1" %~ (+2)

_t1 :: T
_t1 = T1 7 "tset" & rcf @"T1" @"u" %~ reverse

_t2 :: T
_t2 = T2 1 True 'x' & rcf @"T1" @"u" %~ reverse

_t2' :: T
_t2' = T2 1 True 'x' & rcfn @"T2" @2 %~ not

{-
ghci> _r
R {r1 = 5, r2 = "test"}
ghci> _t1
T1 {v = 7, u = "test"}
ghci> _t2
T2 1 True 'x'
ghci> _t2'
T2 1 False 'x'

ghci> import Data.Proxy
ghci> import GHC.TypeLits
ghci> natVal (Proxy @(TFldNum "r1" R))
1

ghci> :kind! (TCons T)
(TCons T) :: [Symbol] = '["T1", "T2"]
ghci> :kind! (TCons R)
(TCons R) :: [Symbol] = TCons R -- no instance

ghci> :kind! (TFlds "T1" T)
(TFlds "T1" T) :: [Symbol] = '["v", "u"]
ghci> :kind! (TFlds "T2" T)
(TFlds "T2" T) :: [Symbol] = '["1", "2", "3"] -- fake field names

ghci> :kind! (TFlds "R" R)
(TFlds "R" R) :: [Symbol] = TFlds "R" R -- no instance

-}
