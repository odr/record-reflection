{-# LANGUAGE UndecidableInstances #-}
module TH.LiftType where

import Control.Monad
import Data.List as L
import Language.Haskell.TH


class LiftType a where
  liftType :: a -> TypeQ

instance LiftType Integer where
  liftType = pure . integerToSym
    where
      integerToSym = LitT . NumTyLit

instance LiftType Name where
  liftType = conT

instance LiftType a => LiftType (Maybe a) where
  liftType = traverse liftType >=> maybeQ
    where
      maybeQ Nothing  = [t|'Nothing|]
      maybeQ (Just t) = appT [t|'Just|] (pure t)

instance LiftType Bool where
  liftType True  = [t|'True|]
  liftType False = [t|'False|]

type family IsChar a :: Bool where
  IsChar Char = 'True
  IsChar x = 'False

instance LiftTypeB (IsChar a) [a] => LiftType [a] where
  liftType = liftTypeB @(IsChar a)

class LiftTypeB (b::Bool) a where
  liftTypeB :: a -> TypeQ

instance LiftType a => LiftTypeB 'False [a] where
  liftTypeB = fmap toPromotedList . traverse liftType

instance LiftTypeB 'True String where
  liftTypeB = pure . strToSym
    where
      strToSym = LitT . StrTyLit

toPromotedList :: [Type] -> Type
toPromotedList =
  L.foldr (\x xs -> AppT (AppT PromotedConsT x) xs) PromotedNilT

instance (LiftType a, LiftType b) => LiftType (a,b) where
  liftType (a,b) = [t| '( $(liftType a), $(liftType b) ) |]

instance (LiftType a, LiftType b) => LiftType (Either a b) where
  liftType (Left a)  = [t|'Left $(liftType a)|]
  liftType (Right b) = [t|'Right $(liftType b)|]
