import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.Map.Strict (Map, fromList, (!))
import Text.Read (readMaybe)

basicEnv = fromList [ ("pi", SchemyNumber pi),
                      ("+", SchemyProcedure addProc),
                      ("*", SchemyProcedure multProc),
                      ("-", SchemyProcedure subProc),
                      ("/", SchemyProcedure divProc),
                      ("<", SchemyProcedure lessProc),
                      ("||", SchemyProcedure orProc),
                      ("&&", SchemyProcedure andProc),
                      ("==", SchemyProcedure equalProc),
                      ("<=", SchemyProcedure lessOrEqualProc),
                      ("not", SchemyProcedure notProc)]

type SchemyEnv = Map String SchemyExp
type Procedure = SchemyEnv -> [SchemyExp] -> SchemyExp

data SchemyExp = SchemyNumber Double
  | SchemyBool Bool
  | SchemySymbol String
  | SchemyProcedure Procedure
  | SchemyForm SchemyExp [SchemyExp]

eval :: SchemyEnv -> SchemyExp -> SchemyExp
eval env (SchemyNumber a) = (SchemyNumber a)
eval env (SchemySymbol a) = (env ! a)
eval env (SchemyBool a) = (SchemyBool a)
eval env (SchemyForm (SchemySymbol s) arrExp) = handleForm env proc arrExp
  where proc = (env ! s)
eval _ _ = error "!"

handleForm:: SchemyEnv -> SchemyExp -> [SchemyExp] -> SchemyExp
handleForm env (SchemyProcedure procedure) xs = (aor procedure) env xs

aor :: Procedure -> Procedure
aor p env args = p env (map (eval env) args)

instance Show SchemyExp where
  show (SchemyBool b) = "(SchemyBool "++ (show b) ++")"
  show (SchemyNumber d) = "(SchemyNumber "++ (show d) ++")"
  show (SchemySymbol s) = "(SchemySymbol "++ (show s) ++")"
  show (SchemyForm exps xs) = "(SchemyForm "++ (show exps) ++ show xs ++")"
  show (SchemyProcedure p) = "(SchemyProcedure ?)"

-- Procedures

addProc :: Procedure
addProc env [(SchemyNumber a), (SchemyNumber b)] = SchemyNumber (a + b)
addProc env ((SchemyNumber x) : (SchemyNumber y) : xs) =  addProc env ([SchemyNumber (x + y)] ++ xs)
addProc _  _= error "!"

multProc :: Procedure
multProc env [(SchemyNumber a), (SchemyNumber b)] = SchemyNumber (a * b)
multProc env ((SchemyNumber x) : (SchemyNumber y) : xs) =  multProc env ([SchemyNumber (x * y)] ++ xs)
multProc _  _= error "!"

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

lessOrEqualProc :: Procedure
lessOrEqualProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = if (x <= y) then SchemyBool True else SchemyBool False
lessOrEqualProc _ _ = error "!"

orProc :: Procedure
orProc env [(SchemyBool a), (SchemyBool b)] = SchemyBool (a || b)
orProc env ((SchemyBool x) : (SchemyBool y) : xs) = orProc env ([SchemyBool (x || y)] ++ xs)
orProc _  _= error "!"

andProc :: Procedure
andProc env [(SchemyBool a), (SchemyBool b)] = SchemyBool (a && b)
andProc env ((SchemyBool x) : (SchemyBool y) : xs) = andProc env ([SchemyBool (x && y)] ++ xs)
andProc _  _= error "!"

equalProc :: Procedure
equalProc env ((SchemyBool x) : (SchemyBool y) : xs) = SchemyBool (x == y)
equalProc env ((SchemyNumber x) : (SchemyNumber y) : xs) = SchemyBool (x == y)
equalProc _ _ = error "!"

notProc :: Procedure
notProc env [(SchemyBool x)] = SchemyBool (not x)
notProc _ _ = error "!"

-- Syntax ----------------------------------------------------------------------
unparse :: SchemyExp -> String
unparse (SchemyBool b) = if b then "true" else "false"
unparse (SchemyNumber d) = show d
unparse (SchemySymbol s) = s
unparse (SchemyForm f args) = "("++ (intercalate " " (map unparse (f:args))) ++")"
unparse (SchemyProcedure _) = error "Cannot unparse procedures!"
mayParse :: String -> Maybe (SchemyExp, String)
mayParse input
  | input == "" = Nothing
  | isSpace (head input) = mayParse (tail input)
  | (head input) == '(' = mayParseForm [] (tail input)
  | otherwise = if (token == "true") then Just (SchemyBool True, rest)
    else if (token == "false") then Just (SchemyBool False, rest)
    else case readMaybe token of
      Just n -> Just (SchemyNumber n, rest)
      _ -> Just (SchemySymbol token, rest)
  where token = takeWhile (\s -> notElem s " ()\f\n\r\t") input
        rest = drop (length token) input

mayParseForm :: [SchemyExp] -> String -> Maybe (SchemyExp, String)
mayParseForm list input
  | input == "" = Nothing
  | isSpace (head input) = mayParseForm list (tail input)
  | (head input) == ')'  = Just (SchemyForm (head list) (tail list), (tail input))
  | otherwise = case mayParse input of
    Just (exp, rest) -> mayParseForm (list ++ [exp]) rest
    _ -> Nothing

parse :: String -> SchemyExp
parse input = case mayParse input of
  Just (exp, rest) | all isSpace rest -> exp
  _ -> error "Parse error!"

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Welcome to Schemy REPL. Input an blank line to exit."
  repl

repl :: IO ()
repl = do
  line <- getLine
  if not (all isSpace line) then do
    putStrLn (show (parse line)) -- Show the parse result
    -- putStrLn (unparse (parse line)) -- Echo the code
    putStrLn (unparse (eval basicEnv (parse line))) -- Print the evaluation
    repl
  else
    return ()
