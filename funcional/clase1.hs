allEqual :: (Eq a) => [a] -> Bool
allEqual xs
  | length(xs) == 0 = error "Lista vacia"
  | length(xs) == 1 = error "Lista con 1 elemento"
  | length(xs) == 2 = xs !! 0 == xs !! 1
  | otherwise = (xs !! 0 == xs !! 1) && (allEqual (tail xs))

data JSON = ValueNull | ValueString String | ValueBool Bool | ValueDouble Double | Array [JSON] | Object [(String, JSON)]
  deriving (Eq, Show)

-- (Object [("menu", (Object [("id", ValueString "file"), ("value", ValueString "File"), ("popup", Object [("menuitem", Array [(Object [("value", ValueString "New"),("onclick", ValueString "CreateNewDoc()")]), (Object [("value", ValueString "Open"),("onclick", ValueString "OpenDoc()")]), (Object [("value", ValueString "Close"),("onclick", ValueString "CloseDoc()")])])])]))])

stringify :: JSON -> String
stringify ValueNull = "null"
stringify (ValueString s) = s
stringify (ValueBool b) = show b
stringify (ValueDouble d) = show d
stringify (Array vs) = "[" ++ aux [stringify v | v <- vs] ++ "]"
stringify (Object ps) = "{" ++ aux [x ++ ":" ++ stringify y | (x,y) <- ps] ++ "}"

aux :: [String] -> String
aux (x:xs) = x ++ "," ++ (aux xs)
aux _ = ""