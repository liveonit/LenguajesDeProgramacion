data SchemyExp = SchemyNumber Double
  | SchemyAdd SchemyExp SchemyExp
  | SchemyMult SchemyExp SchemyExp
  deriving (Eq, Show)

eval :: SchemyExp -> Double
eval (SchemyNumber a) = a
eval (SchemyAdd a b) = eval a + eval b
eval (SchemyMult a b) = eval a * eval b
