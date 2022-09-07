-- simple untyped Lambda Calculus

module Main where

import System.IO
import Data.Char

greeting = "Hello! enter lambda-terms, type '#help' for help!"
prompt = ">> "

main :: IO ()
main = do
    putStrLn greeting
    repl
--    read <- getLine
--    putStrLn read
--    let lexed = lex' Nost read
--    putStrLn (show lexed)

repl = do
    putStr prompt
    hFlush stdout
    read <- getLine
    --putStrLn read
    let lexed = lex' Nost read
    putStrLn ("Lexed: " ++ show lexed)
    let parsed = parse lexed
    putStrLn ("Parsed: " ++ show parsed)
    repl

data Tok = Lambda
    | Ident String
    | Dot
    | Lpar
    | Rpar
    | WS
    | LexError String
    | Direct DirCmd
    
    | Lcb -- {
    | Rcb -- }
    | Gt -- >
    
    deriving Show
    
data DirCmd = Help
    | Exit
    | Unknown String
    deriving Show

data LexState = Identst String
    | Directst String
    | Nost
    deriving Show

lex' :: LexState -> [Char] -> [Tok]
lex' Nost [] = []
lex' (Identst st) [] = (lexIdent st):[]  -- End string with an ident
lex' (Directst ds) [] = (lexDirective ds):[]  -- End w directive
lex' Nost ('.':xs) = Dot:(lex' Nost xs)
lex' Nost ('(':xs) = Lpar:(lex' Nost xs)
lex' Nost (')':xs) = Rpar:(lex' Nost xs)
lex' Nost ('\\':xs) = Lambda:(lex' Nost xs)

-- Temporarily adding 'easy mode' chars
lex' Nost ('{':xs) = Lcb:(lex' Nost xs) -- Begin scope of abstraction, like \x.{x}
lex' Nost ('}':xs) = Rcb:(lex' Nost xs) -- End scope of abstraction
lex' Nost ('>':xs) = Gt:(lex' Nost xs) -- Infix application, like (a > b)

lex' Nost (x:xs)
    | isSpace x = lex' Nost xs
-- There is a syntactic place for directives, as another kind of expr at toplevel only; done in parser
lex' Nost ('#':xs) = lex' (Directst []) xs -- Begin directive
lex' (Directst ds) (x:xs) = if (or [isAlpha x, (x==' ')]) -- Either letter or space continues directive
                            then lex' (Directst (ds ++ (x:[]))) xs
                            else (lexDirective ds):(lex' Nost (x:xs)) -- End directive
lex' Nost (x:xs) = if isAlpha x 
                    then lex' (Identst (x:[])) xs  -- Begin an identifier
                    else [LexError "Invalid character."]
lex' (Identst st) (x:xs) = if isAlpha x
                            then lex' (Identst (st ++ (x:[]))) xs -- Continue ident
                            else (lexIdent st):(lex' Nost (x:xs)) -- End ident
lex' st rem = [LexError ("Unknown error. State: "
                        ++(show st)
                        ++", Remainder: "
                        ++(show rem))]

lexIdent :: [Char] -> Tok
lexIdent st = case st of
                "lambda" -> Lambda
                st -> Ident st

lexDirective :: [Char] -> Tok
lexDirective ds = case ds of
                    "help" -> Direct Help
                    "exit" -> Direct Exit
                    ds -> Direct (Unknown ds)



data Var = MkVar String
    deriving Show

data Term = JustVar Var
    | Abs Var Term
    | App Term Term
    | ParseError String
    deriving Show

data ParseState = ParseNost
    | ParseLparst
    deriving Show

parse :: [Tok] -> Term
parse l = parseh [] [ParseNost] l

parseh :: [Term] -- Stack for intermediate parse trees, "stack"
            -> [ParseState] -- Stack of parse states, "st"
            -> [Tok] -- Input token stream, "rem"
            -> Term -- Resulting tree, "result"

parseh [] [ParseNost] [] = ParseError "Empty input."
parseh [result] [ParseNost] [] = result

--Parens
parseh stack st (Lpar : xs) = parseh (parseh [] (ParseLparst:st) xs): stack st [] -- Begin parenthesized expr
parseh stack (ParseLparst : st) (Rpar : xs) = parseh stack st xs -- End scope of paren
parseh stack (ParseLparst : st) (Rpar : xs)

--App
parseh (x:y:ys) st rem = parseh (App y x : ys) st rem -- Pop 2, push app

--Var
parseh stack st (Ident x : xs) = parseh (JustVar (MkVar x) : stack) st xs -- Push var

--Abs
parseh [] st (Lambda : Ident x : Dot : xs) 
    = Abs (MkVar x) (parseh [] st xs)  -- Abstraction is right associative low prec
    -- That means its scope extends as far to the right as the current scope goes.
    
parseh stack state rem = ParseError ("Unknown error. "
                                        ++"Stack: "++(show stack)
                                        ++", State: "++(show state)
                                        ++", Remainder: "++(show rem))


