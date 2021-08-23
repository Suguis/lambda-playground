module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))
import System.IO

data Term = Var String
          | Abs String Term
          | App Term Term

instance Show (Term) where
  show (Var v) = v
  show (Abs v b) = "(λ" ++ v ++ "." ++ show b ++ ")"
  show (App t1 t2@(App _ _)) = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2
main :: IO ()
main = loop
  where loop = do putStr "λ> "
                  hFlush stdout
                  s <- getLine
                  case (parse term "error" s) of
                    Left err -> putStrLn $ show err
                    Right t  -> evalAndPrint t
                  loop

{-
Term ::= App | Var | Abs | "(" Term ")"
Var  ::= /[a-z]+/
Abs  ::= "\" /[a-z]+/ "." Term
App  ::= NoApp Term
NoApp  ::= Var | Abs | "(" Term ")"
-}

term :: Parser Term
term = spaces >> (try termApp <|> termVar <|> termAbs <|> termParen)

termParen :: Parser Term
termParen = do _ <- char '('
               t <- term
               _ <- char ')'
               return t

termApp :: Parser Term
termApp = chainl1 termNoApp chainer
  where chainer = do spaces
                     return App

-- Used to avoid left-recursion on termApp
termNoApp :: Parser Term
termNoApp = do t <- termVar <|> termAbs <|> termParen
               spaces
               return t

termAbs :: Parser Term
termAbs = do _ <- char '\\'
             v <- many1 letter
             _ <- char '.'
             t <- term
             return $ Abs v t

termVar :: Parser Term
termVar = do v <- many1 letter
             return $ Var v

------------------------------------------------------------

eval :: Term -> Term
eval t = maybe t eval (evalStep t)

evalStep :: Term -> Maybe Term
evalStep (App (Abs v1 b1) t2@(Abs _ _)) = Just $ substitute v1 t2 b1
evalStep (App t1@(Abs _ _) t2) = liftA2 App (pure t1) (evalStep t2)
evalStep (App t1 t2) = liftA2 App (evalStep t1) (pure t2)
evalStep _ = Nothing

substitute :: String -> Term -> Term -> Term
substitute s t (Var n) = if s == n then t else (Var n)
substitute s t (Abs n b) = Abs n (substitute s t b)
substitute s t (App t1 t2) = App (substitute s t t1) (substitute s t t2)

evalAndPrint :: Term -> IO ()
evalAndPrint t = mapM_ (putStrLn . show) (evalAndPrint' (Just t))
  where evalAndPrint' Nothing  = []
        evalAndPrint' (Just t') = t':evalAndPrint' (evalStep t')
