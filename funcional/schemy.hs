import Data.Map.Strict (Map, fromList)
import Data.Map ((!))

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  | SchemyBool Bool
  deriving (Eq, Show)

eval :: (Map String SchemyExp) -> SchemyExp -> SchemyExp
eval map (SchemyNumber n) = SchemyNumber n
eval map (SchemyAdd a b) = SchemyNumber (evalSchemyDouble (eval map a) + evalSchemyDouble (eval map b))
eval map (SchemyMult a b) = SchemyNumber (evalSchemyDouble (eval map a) * evalSchemyDouble (eval map b))
eval map (SchemySymbol s) = map ! s

evalSchemyDouble :: SchemyExp -> Double
evalSchemyDouble (SchemyNumber a) = a

--let env = fromList[("pi",pi)]