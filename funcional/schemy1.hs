import Data.Map.Strict (Map, fromList)
import Data.Map ((!))

basicEnv = fromList [("+", SchemyProcedure addProc),("-", SchemyProcedure subProc),("/", SchemyProcedure divProc),("<", SchemyProcedure lessProc),("||", SchemyProcedure orProc)]

type SchemyEnv = Map String SchemyExp
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

data SchemyExp = SchemyNumber Double
  | SchemyBool Bool
  | SchemySymbol String
  | SchemyProcedure Procedure
  | SchemyForm SchemyExp [SchemyExp]

eval :: SchemyEnv -> SchemyExp -> SchemyExp
eval env (SchemyNumber a) = (SchemyNumber a)
eval env (SchemyBool a) = (SchemyBool a)
eval env (SchemyForm (SchemySymbol s) arrExp) = handleForm env proc arrExp
  where proc = (env ! s)

handleForm:: SchemyEnv -> SchemyExp -> [SchemyExp] -> SchemyExp
handleForm env (SchemyProcedure procedure) xs = procedure env xs

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm exps xs) = "(SchemyForm "++ (show exps) ++ show xs ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"

-- Procedures

addProc :: Procedure
addProc env [(SchemyNumber a), (SchemyNumber b)] = SchemyNumber (a + b)
addProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = addProc env ([SchemyNumber (x + y)] ++ xs)
addProc _  _= error "!"

subProc :: Procedure
subProc env [(SchemyNumber a), (SchemyNumber b)] = SchemyNumber (a - b)
subProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = subProc env ([SchemyNumber (x - y)] ++ xs)
subProc _  _= error "!"

divProc :: Procedure
divProc env [(SchemyNumber a), (SchemyNumber b)] = if b==0 then error "!" else SchemyNumber (a / b)
divProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = divProc env ([SchemyNumber (x / y)] ++ xs)
divProc _  _= error "!"

lessProc :: Procedure
lessProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = if (x<y) then SchemyBool True else SchemyBool False
lessProc _  _= error "!"

orProc :: Procedure
orProc env ((SchemyBool x) : (SchemyBool y) : xs) = SchemyBool (x || y)
orProc _  _= error "!"