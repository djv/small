import Control.Arrow
import System( getArgs )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char

data Expr = T String | Arr Expr Expr deriving (Show, Eq)

flatten :: Expr -> String
flatten (T s) = s
flatten e@(Arr _ _) = init . tail $ flatten' e where
  flatten' (T s) = s
  flatten' (Arr l r) = "(" ++ (flatten' l) ++ " -> " ++ (flatten' r) ++ ")"

expr = buildExpressionParser table factor <?> "expression"

table = [[op "->" Arr AssocRight]] where
    op s f assoc = Infix (do {space; string s; space; return f }) assoc

parens p = between (string "(") (string ")") p

factor = parens expr
        <|> t

typeName = many1 ( alphaNum <|> oneOf "._'`[]>|" )
t = do
      x <- typeName `sepBy1l` space
      return (T $ unwords x)

sepBy1l p sep        = do{ x <- p
                        ; xs <- many (try (sep >> p))
                        ; return (x:xs)
                        }

attach_type s t = "(" ++ s ++ " : " ++ t ++ ")"

ziptl t [] = ([], flatten t)
ziptl (Arr l r) (x:xs) = ((x `attach_type` (flatten l)) : t, f) where
  (t,f) = ziptl r xs

main = do
  args <- getArgs
  let fun_header = unwords args
  inp <- getLine
  let (Right types) = parse expr "" inp
  let ((fun:args), rest) = first tail . break (== "=") . words $ fun_header
  let (targs, ftype) = ziptl types args
  let result = unwords $ targs ++ [":"] ++ [ftype] ++ rest
  putStr $ "let " ++ fun ++ " " ++ result
