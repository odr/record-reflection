{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rec.Tuples.Inst where

import Rec.Tuples.TH


-- Remark: 62 is a maximum tuple size in GHC
concat <$> traverse mkTupleRecFld [2..32]
