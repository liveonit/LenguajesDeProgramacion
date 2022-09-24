import Data.Map.Strict (Map, fromList)
import Data.Map ((!))

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  | SchemyBool Bool
  | SchemyEq SchemyExp SchemyExp
  | SchemyLessThan SchemyExp SchemyExp
  | SchemyAnd SchemyExp SchemyExp
  | SchemyNot SchemyExp
  deriving (Eq, Show)

eval :: (Map String SchemyExp) -> SchemyExp -> SchemyExp
eval env (SchemyNumber a) = SchemyNumber a
eval env (SchemyBool a) = SchemyBool a
eval env (SchemyAdd (SchemyNumber a) (SchemyNumber b)) = SchemyNumber (a + b)
eval env (SchemyMult (SchemyNumber a) (SchemyNumber b)) = SchemyNumber (a + b)
eval env (SchemyEq (SchemyNumber a) (SchemyNumber b)) = SchemyBool (a == b)
eval env (SchemyEq (SchemyBool a) (SchemyBool b)) = SchemyBool (a && b)
eval env (SchemyLessThan (SchemyNumber a) (SchemyNumber b)) = SchemyBool (a <= b)
eval env (SchemyAnd (SchemyBool a) (SchemyBool b)) = SchemyBool (a <= b)
eval env (SchemyNot (SchemyBool a)) = SchemyBool (not a)
eval env (SchemySymbol s) = env ! s
eval env _ = error "Operation not allowed"
