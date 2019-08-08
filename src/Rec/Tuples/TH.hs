module Rec.Tuples.TH where

import Language.Haskell.TH
import TH.LiftType
import Rec.Class
import Data.List


mkTupleRecFld :: Int -> DecsQ
mkTupleRecFld cnt
  | cnt < 2   = pure []
  | otherwise = concat <$> traverse mkInst [1..cnt]
  where
    vars = mkName . ('x':) . show <$> [1..cnt]
    tupT = foldl' appT (tupleT $ length vars) $ varT <$> vars
    mkInst ind =
      [d|
        instance CRecFld $(indQ) $(tupT) where
          type TRecFld $(indQ) $(tupT) = $(varT var)
          getFld $(tupPInd) = $(varE var)
          setFld $(tupPRest) $(varP var) = $(tupE $ varE <$> vars)
        |]
      where
        var = vars !! (ind - 1)
        indQ = liftType $ toInteger ind
        tupPInd = tupP
          $ replicate (ind-1) wildP ++ [varP var] ++ replicate (cnt-ind) wildP
        tupPRest = tupP
          $ (varP <$> take (ind-1) vars) ++ [wildP] ++ (varP <$> drop ind vars)
