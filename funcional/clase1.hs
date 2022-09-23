import Data.List

allEqual :: (Eq a) => [a] -> Bool
allEqual xs
  | length(xs) == 0 = error "Lista vacia"
  | length(xs) == 1 = error "Lista con 1 elemento"
  | length(xs) == 2 = xs !! 0 == xs !! 1
  | otherwise = (xs !! 0 == xs !! 1) && (allEqual (tail xs))

data JSON = ValueNull | ValueString String | ValueBool Bool | ValueDouble Double | Array [JSON] | Object [(String, JSON)]
  deriving (Eq, Show)

-- (Object [("menu", (Object [("id", ValueString "file"), ("value", ValueString "File"), ("popup", Object [("menuitem", Array [(Object [("value", ValueString "New"),("onclick", ValueString "CreateNewDoc()")]), (Object [("value", ValueString "Open"),("onclick", ValueString "OpenDoc()")]), (Object [("value", ValueString "Close"),("onclick", ValueString "CloseDoc()")])])])]))])

stringify :: JSON -> IO()
stringify json = putStrLn (stringifyAux json)

stringifyAux :: JSON -> String
stringifyAux ValueNull = "null"
stringifyAux (ValueString s) = "\"" ++ s ++ "\""
stringifyAux (ValueBool b) = if b then "true" else "false"
stringifyAux (ValueDouble d) = show d
stringifyAux (Array vs) = "[" ++ intercalate "," [stringifyAux v | v <- vs] ++ "]"
stringifyAux (Object ps) = "{" ++ intercalate "," ["\"" ++ x ++ "\"" ++ ":" ++ stringifyAux y | (x,y) <- ps] ++ "}"
