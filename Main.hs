module Main where

import System.IO

-- File to be compiled
inputFile = "input.pow"

-- Minimal compiler.
-- For expressions with positive integers, addition and multiplication.
-- Reads input expression from file input.pow
-- Compiles to C, places into file output.c
-- Result of evaluating expression is the C program's return code.
main :: IO ()
main = do
  putStrLn "Hello!"
  inh <- openFile inputFile ReadMode
  outh <- openFile "output.c" WriteMode
  f <- hGetContents inh
  let lexed = lex' f
  --hPutStrLn outh (show lexed)
  let parsed = parsestack [] lexed
  --hPutStrLn outh (show parsed)
  let ccoded = cboilerplatebeg ++ (ccodegen parsed) ++ cboilerplateend
  hPutStrLn outh ccoded
  hClose inh
  hClose outh

data Token = IntLiteral Int
    | Plus
    | Times
    | WS
    -- | OpenPar
    -- | ClosePar
    deriving Show

lex' :: String -> [Token]
lex' [] = []
lex' (' ':xs) = WS : lex' xs
lex' ('\n':xs) = WS : lex' xs
lex' ('\t':xs) = WS : lex' xs
lex' ('1':xs) = IntLiteral 1 : lex' xs
lex' ('2':xs) = IntLiteral 2 : lex' xs
lex' ('3':xs) = IntLiteral 3 : lex' xs
lex' ('+':xs) = Plus : lex' xs
lex' ('*':xs) = Times : lex' xs
--lex' ('(':xs) = OpenPar : lex' xs
--lex' (')':xs) = ClosePar : lex' xs

data ParseTree = Int' Int
    | Add ParseTree ParseTree
    | Mult ParseTree ParseTree
    | ParseError String
    deriving Show

-- Args: 1. List of ParseTrees, used as a stack for subtrees
-- 2. List of remaining tokens
-- Returns the whole file's final parse tree
parsestack :: [ParseTree] -> [Token] -> ParseTree
parsestack ([result]) ([]) = result
parsestack ([]) ([]) = ParseError "Warning: Empty Parse Tree"
parsestack (x:xs) ([]) = ParseError "Error: Wrong number of operands"
parsestack stack (WS : xs) = parsestack stack xs
parsestack stack (IntLiteral x : xs) = parsestack (Int' x : stack) xs
parsestack (x:ss) (Times : ts) = Mult x (parsestack ss ts)
parsestack (x:ss) (Plus : ts) = Add x (parsestack ss ts)
parsestack _ (Times:ts) = ParseError "Error: not enough operands to *" 
parsestack _ (Plus:ts) = ParseError "Error: not enough operands to +"
--parsestack _ _ = ParseError "Error: Unknown Error"

cboilerplatebeg = "int main() {return ("
cboilerplateend = ");}"

ccodegen :: ParseTree -> String
ccodegen (Int' i) = show i
ccodegen (Mult x y) = "(" ++ (ccodegen x) ++ ") * (" ++ (ccodegen y) ++ ")"
ccodegen (Add x y) = "(" ++ (ccodegen x) ++ ") + (" ++ (ccodegen y) ++ ")"
ccodegen (ParseError str) = "/* Fatal: " ++ str ++ " */ -1"
