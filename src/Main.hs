module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))
import Data.Maybe
import Data.Set (Set, notMember, union, singleton, empty, (\\))
import System.IO

data Term = Var String
          | Abs String Term
          | App Term Term

data Statement = Assignment String Term
               | Evaluation Term

type Env = [(String, Term)]

instance Show Term where
  show (Abs v b) = "λ" ++ v ++ "." ++ show b
  show t = show' t
    where show' (Var v) = v
          show' (Abs v b) = "(λ" ++ v ++ "." ++ show b ++ ")"
          show' (App t1 t2@(App _ _)) = show' t1 ++ " (" ++ show' t2 ++ ")"
          show' (App t1 t2) = show' t1 ++ " " ++ show' t2

------------------------------------------------------------

main :: IO ()
main = loop [ ("id"    , Abs "x" (Var "x"))
            , ("true"  , Abs "t" (Abs "f" (Var "t")))
            , ("false" , Abs "t" (Abs "f" (Var "f")))
            , ("zero"  , Abs "s" (Abs "z" (Var "z")))
            , ("one"   , Abs "s" (Abs "z" (App (Var "s") (Var "z"))))
            , ("two"   , Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z")))))
            , ("succ"  , Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))
            ]
  where loop :: Env -> IO ()
        loop env = do putStr "λ> "
                      hFlush stdout
                      s <- getLine
                      case parse statement "error" s of
                        Left err -> print err
                        Right st -> case st of
                          (Assignment v t) -> loop ((v,t):env)
                          (Evaluation t)   -> evalAndPrint (replaceEnv env t) >> loop env

{-

Informal grammar:

Statement ::= Assignment | Evaluation
Assignment ::= String "=" Term
Evaluation ::= Term
Term ::= App | Var | Abs | "(" Term ")"
Var  ::= /[a-z]+/
Abs  ::= "\" /[a-z]+/ "." Term
App  ::= NoApp Term
NoApp  ::= Var | Abs | "(" Term ")"
-}


statement :: Parser Statement
statement = do st <- spaces >> (try assignment <|> evaluation)
               eof
               return st


assignment :: Parser Statement
assignment = do v <- many1 letter
                spaces
                _ <- char '='
                t <- term
                return $ Assignment v t

evaluation :: Parser Statement
evaluation = Evaluation <$> term

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

replaceEnv :: Env -> Term -> Term
replaceEnv [] t = t
replaceEnv env t@(Var x) = fromMaybe t (x `lookup` env)
replaceEnv env (App t1 t2) = App (replaceEnv env t1) (replaceEnv env t2)
replaceEnv env (Abs n b) = Abs n (replaceEnv envWithoutBinded b)
  where envWithoutBinded = filter ((/= n) . fst) env

eval :: Term -> Term
eval t = maybe t eval (evalStep t)

{-

The interpreter follows a strict evaluation strategy,
based on the following rules:

   t2 -> t2'
---------------
t1 t2 -> t1 t2'

   t1 -> t1'
---------------
t1 t2 -> t1' t2

-------------------------
(\v.tb) t2 -> [v -> t2]tb

        tb -> tb'
-------------------------
   (\v.tb) -> (\v.tb')

-}

isNormal :: Term -> Bool
isNormal (Var _) = True
isNormal (Abs _ tb) = isNormal tb
isNormal (App (Abs _ _) _) = False
isNormal (App t1 t2) = isNormal t1 && isNormal t2

evalStep :: Term -> Maybe Term
evalStep (Abs n tb) = fmap (Abs n) (evalStep tb)
evalStep (App (Abs n tb) t2) = Just $ substitute n t2 tb
evalStep (App t1 t2)
  | not $ isNormal t2 = fmap (App t1) (evalStep t2)
  | not $ isNormal t1 = liftA2 App (evalStep t1) (pure t2)
  | otherwise         = Nothing
evalStep (Var _) = Nothing

-- substitute s t r == r[s := t]
substitute :: String -> Term -> Term -> Term
substitute s t (Var n) = if s == n then t else Var n
substitute s t (App t1 t2) = App (substitute s t t1) (substitute s t t2)
substitute s t a@(Abs n b) = if n `notMember` freeVars t
  then Abs n (substitute s t b)
  else substitute s t (rename a)

-- This function renames the binding variable of an abstraction.
-- This is used when there are problems in the substitution process
-- related to variables with the same name
rename :: Term -> Term
rename t@(Abs n b) = if n' `notMember` freeVars t && n' `notMember` bindingVars t
  then Abs n' b
  else rename $ Abs n' b
  where n' = n ++ "'"
rename t = t

freeVars :: Term -> Set String
freeVars (Var x) = singleton x
freeVars (App t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Abs x t) = freeVars t \\ singleton x

bindingVars :: Term -> Set String
bindingVars (Var _) = Data.Set.empty
bindingVars (App t1 t2) = bindingVars t1 `union` bindingVars t2
bindingVars (Abs x t) = singleton x `union` bindingVars t

evalAndPrint :: Term -> IO ()
evalAndPrint t = mapM_ print (evalAndPrint' (Just t))
  where evalAndPrint' Nothing  = []
        evalAndPrint' (Just t') = t':evalAndPrint' (evalStep t')
