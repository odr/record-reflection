module Rec.TH(mkRec) where

import Control.Lens hiding (cons)
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Rec.Class
import TH.LiftType
import Control.Monad
import Data.List as L
import Rec.Tuples.Inst()


appConE :: Name -> [Name] -> ExpQ
appConE c vars = foldl' appE (conE c) $ varE <$> vars

tupTypesT :: [TypeQ] -> TypeQ
tupTypesT tqs = foldl' appT (tupleT $ length tqs) tqs

conName
  :: (Semigroup (f Con), Applicative f) => (Name -> f Name) -> Con -> f Con
conName = _RecC . _1 <> _NormalC . _1

{- |
* for multi-constructor record generate instances for 'TCons' and 'CRecCon'
* for single-constructor record generate instances for 'TFldNum' and 'CRecFld'
-}
mkRec :: Name -> DecsQ
mkRec r = do
  TyConI (toListOf decCons -> cons) <- reify r
  case cons of
    []  -> pure []
    [c] -> mkOneCon c
    _   -> mkManyCons cons
  where
    decCons = _DataD . _5 . traverse <> _NewtypeD . _5
    conTypes = _RecC . _2 . traverse . _3 <> _NormalC . _2 . traverse . _2
    conFldNames c
      = c ^.. _RecC . _2 . traverse . _1 . to nameBase
      <> (show . (+1) . fst <$> c ^@.. _NormalC . _2 . itraversed)
    rq = liftType r
    mkManyCons :: [Con] -> DecsQ
    mkManyCons cons = do
      tcons <- [d| type instance TCons $(liftType r) = $(liftType conStrs) |]
      creccon <- concat <$> traverse instRecCon cons
      pure $ tcons ++ creccon
      where
        conStrs = cons ^.. traverse . conName . to nameBase
        instRecCon c = case mcn of
          Nothing -> pure [] -- impossible!
          Just cn -> do
            let cst = liftType $ nameBase cn
            vars <- traverse (newName . const "x") flds
            [d|
              instance CRecCon $(cst) $(rq) where
                type TRecCon $(cst) $(rq) = $(tupTypesT fldTypes)
                type TFlds $(cst) $(rq) = $(liftType flds)
                fromCon $(tupP $ varP <$> vars) = $(appConE cn vars)
                toCon $(conP cn $ varP <$> vars) = Just $(tupE $ varE <$> vars)
                toCon _ = Nothing
              |]
          where
            flds = conFldNames c
            fldTypes = pure <$> c ^.. conTypes
            mcn = c ^? conName

    mkOneCon c = do
      fldNum <- concat <$> zipWithM instFldNum flds [(1::Integer)..]
      vars <- traverse (newName . const "x") flds
      inst <- concat <$> zipWithM (\n _ -> instRecFld vars n) [1..] flds
      pure $ fldNum ++ inst
      where
        flds = conFldNames c
        fldTypes = pure <$> c ^.. conTypes
        mcn = c ^? conName
        cntFlds = length flds
        instFldNum (liftType -> fq) (liftType -> num) =
          [d| type instance TFldNum $(fq) $(rq) = $(num)|]
        instRecFld vars num = case mcn of
          Nothing -> pure [] -- impossible!
          Just cn -> [d|
            instance CRecFld $(nq) $(rq) where
              type TRecFld $(nq) $(rq) = $(fldType)
              getFld $(conPInd cn) = $(varE var)
              setFld $(conPRest cn) $(varP var) = $(appConE cn vars)
            |]
          where
            fldType = fldTypes !! (num - 1)
            var = vars !! (num - 1)
            nq = liftType $ toInteger num
            conPInd cn = conP cn
              $ replicate (num-1) wildP ++ [varP var]
                ++ replicate (cntFlds-num) wildP
            conPRest cn = conP cn
              $ (varP <$> take (num-1) vars) ++ [wildP]
                ++ (varP <$> drop num vars)

      {-
      class CRecFld fld r where
        type TRecFld fld r
        getFld :: r -> TRecFld fld r
        setFld :: r -> TRecFld fld r -> r
      -}
