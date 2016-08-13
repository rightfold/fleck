module Fleck.AST
( ValueExpr(..)
, Init(..)
, TypeExpr(..)
, PathExpr(..)
, TypeDeclExpr(..)
, ValueDeclExpr(..)
) where

data ValueExpr v t
  = VariableValueExpr v String
  | NewValueExpr (TypeExpr v t) (Array (Init v t))
  | SelectValueExpr (ValueExpr v t) String
  | InvokeValueExpr (ValueExpr v t) String (Array (Array (ValueExpr v t)))

data Init v t = Init String (Array (Array String)) (ValueExpr v t)

data TypeExpr v t
  = VariableTypeExpr t String
  | SelectTypeExpr (PathExpr v) String
  | RefineTypeExpr (TypeExpr v t) (Array (TypeDeclExpr v t)) (Array (ValueDeclExpr v t))
  | IntersectTypeExpr (TypeExpr v t) (TypeExpr v t)
  | UnionTypeExpr (TypeExpr v t) (TypeExpr v t)

data PathExpr v = VariablePathExpr v String | SelectPathExpr (PathExpr v) String

data TypeDeclExpr v t = TypeDeclExpr String (TypeExpr v t) (TypeExpr v t)

data ValueDeclExpr v t = ValueDeclExpr String (Array (Array (TypeExpr v t))) (TypeExpr v t)
