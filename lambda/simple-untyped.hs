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
    putStrLn (show lexed)
    repl

data Tok = Lambda
    | Ident String
    | Dot
    | Lpar
    | Rpar
    | WS
    | LexError String
    | Direct DirCmd
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
    

--lexit :: [Char] -> LexResult


data Var = MkVar String
    deriving Show

data Term = Var
    | Abs Var Term
    | App Term Term
    deriving Show
    
--data Name = String
--    deriving Show

--parse :: [Term] -> [Tok] -> Term

    


