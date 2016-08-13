module Fleck.Env
( Env

, lookupValue
, insertValue

, lookupType

, fundamental
) where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe)
import Fleck.Type (Type(..))

data Env = Env (Map String Type)
               (Map String Type)

lookupValue :: String -> Env -> Maybe Type
lookupValue name (Env vs _) = Map.lookup name vs

insertValue :: String -> Type -> Env -> Env
insertValue name type_ (Env vs ts) = Env (Map.insert name type_ vs) ts

lookupType :: String -> Env -> Maybe Type
lookupType name (Env _ ts) = Map.lookup name ts

fundamental :: Env
fundamental = Env ts vs
  where ts = Map.empty
             # Map.insert "Top" TopType
             # Map.insert "Bottom" BottomType
        vs = Map.empty
