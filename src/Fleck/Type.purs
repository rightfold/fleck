module Fleck.Type
( Type(..)
, Path(..)
, TypeDecl(..)
, ValueDecl(..)
, isConcrete
) where

import Prelude

data Type
  = TopType
  | BottomType
  | SelectType Path String
  | RefineType Type (Array TypeDecl) (Array ValueDecl)
  | IntersectType Type Type
  | UnionType Type Type

data Path = VariablePath Int | SelectPath Path String

data TypeDecl = TypeDecl String Type Type

data ValueDecl = ValueDecl String (Array (Array Type)) Type

isConcrete :: Type -> Boolean
isConcrete TopType = true
isConcrete (RefineType t _ _) = isConcrete t
isConcrete (IntersectType a b) = isConcrete a && isConcrete b
isConcrete _ = false
