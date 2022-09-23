import Data.Map.Strict (Map, fromList)
import Data.Map ((!))

data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  | SchemySymbol String
  deriving (Eq, Show)

eval :: (Map String Double) -> SchemyExp -> Double
eval map (SchemyNumber n) = n
eval map (SchemyAdd a b) = eval map a + eval map b
eval map (SchemyMult a b) = eval map a * eval map b
eval map (SchemySymbol s) = map ! s