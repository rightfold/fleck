module Fleck.Check
( Error(..)

, checkValueExpr
) where

import Prelude
import Fleck.Env as Env
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (zip)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Fleck.AST (Init(..), TypeExpr(..), ValueExpr(..))
import Fleck.Env (Env)
import Fleck.Type (isConcrete, Type(..))

data Error
  = ValueIsNotInScope String
  | NewIsUsedWithNonConcreteType Type
  | TypeIsNotInScope String

checkValueExpr :: forall m v t
                . (MonadError Error m)
               => Env
               -> ValueExpr v t
               -> m (ValueExpr Type Type)
checkValueExpr env (VariableValueExpr _ name) =
  case Env.lookupValue name env of
    Just type_ -> pure (VariableValueExpr type_ name)
    Nothing -> throwError (ValueIsNotInScope name)
checkValueExpr env (NewValueExpr type_ inits)
  | isConcrete type_ = do
      type_' <- checkTypeExpr env type_
      inits' <- traverse (checkInit env) inits
      -- TODO: check that all inits conform to decls in type_'
      pure (NewValueExpr type_' inits')
  | otherwise = throwError (NewIsUsedWithNonConcreteType type_)
checkValueExpr env (SelectValueExpr value label) = do
  value' <- checkValueExpr env value
  -- TODO: check that label exists in the type of value'
  pure (SelectValueExpr value' label)
checkValueExpr env (InvokeValueExpr value label args) = do
  value' <- checkValueExpr env value
  args' <- traverse (traverse (checkValueExpr env)) args
  pure (InvokeValueExpr value' label args')

checkInit :: forall m v t
           . (MonadError Error m)
          => Env
          -> Array (Array Type)
          -> Init v t
          -> m (Init Type Type)
checkInit env paramTypes (Init name params body) =
  let env' = foldl insert env (join paramTypes `zip` join params)
      insert e (Tuple type_ name) = Env.insertValue name type_ e
   in Init name params <$> checkValueExpr env' body

checkTypeExpr :: forall m v t
               . (MonadError Error m)
              => Env
              -> TypeExpr v t
              -> m (TypeExpr Type Type)
checkTypeExpr env (VariableTypeExpr _ name) =
  case Env.lookupType name env of
    Just type_ -> pure (VariableTypeExpr type_ name)
    Nothing -> throwError (TypeIsNotInScope name)
