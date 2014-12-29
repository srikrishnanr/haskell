module Transformers where
	import Control.Monad.Identity
	import Control.Monad.Error
	import Control.Monad.Reader
	import Control.Monad.State
	import Control.Monad.Writer
	import Data.Maybe
	import qualified Data.Map as Map
	
	-- variable names
	type Name = String
	
	-- expressions
	data Exp = Lit Integer
			 | Var Name
			 | Plus Exp Exp
			 | Abs Name Exp
			 | App Exp Exp
			 deriving (Show)

	-- values
	data Value = IntVal Integer
	           | FunVal Env Name Exp
			   -- Above is a function declaration
			   -- 'Name' is parameter name
			   -- Exp is function body
			   deriving (Show)
			 
	-- environment - mapping from name to values
	type Env   = Map.Map Name Value
	
	-- reference implementation
	eval0 :: Env -> Exp -> Value
	eval0 env (Lit i) = IntVal i
	eval0 env (Var name) = fromJust (Map.lookup name env)
	eval0 env (Plus exp1 exp2) = let  IntVal v1  = (eval0 env exp1)
	                                  IntVal v2  = (eval0 env exp2)
	                             in (IntVal (v1 + v2))
	eval0 env (Abs name exp1) = FunVal env name exp1
	-- Below: App stands for function application
	-- v1 is the function declaration which references a parameter whose value is given by v2
	eval0 env (App exp1 exp2)  = let v1 = eval0 env exp1
	                                 v2 = eval0 env exp2
								  in case v1 of
									  FunVal env' n body -> eval0 (Map.insert n v2 env') body

-- Running this example
	-- 12 + ( ( \x -> x )(4 + 2) )
	ex :: Exp
	ex = Lit 12 `Plus` ( App ( Abs "x" (Var "x") ) (Plus (Lit 4) (Lit 2)))

	ex0 = eval0 (Map.empty) ex

-- Convert the reference implementation to use Identity monad
	-- First make the result type (Value) as a Identity Monad
	type Eval1 a = Identity a
	runEval1 :: Eval1 a -> a
	runEval1 e = runIdentity e
	
	-- implement eval0 to return an Identity monad
	eval1 :: Env -> Exp -> Eval1 Value
	eval1 env (Abs name exp1) = return $ FunVal env name exp1
	eval1 env (Lit i) = return $ IntVal i
	eval1 env (Var name) = return $ fromJust $ Map.lookup name env
	eval1 env (Plus exp1 exp2) = do 
	                                  IntVal v1 <- eval1 env exp1
	                                  IntVal v2 <- eval1 env exp2
	                                  return (IntVal (v1 + v2))
	eval1 env (App exp1 exp2) = do
	                                  v1 <- eval1 env exp1
	                                  v2 <- eval1 env exp2
	                                  case v1 of
	                                     FunVal env' n body -> eval1 (Map.insert n v2 env')  body
	ex1 = runEval1 $ eval1 (Map.empty) ex

	-- Because Eval1 doesn't depend on Identity monad as it only refers to 'do' and 'return', we can define Eval1 using a generic monad
	eval1' :: (Monad m) => Env -> Exp -> m Value
	eval1' env (Abs name exp1) = return $ FunVal env name exp1
	eval1' env (Lit i) = return $ IntVal i
	eval1' env (Var name) = return $ fromJust $ Map.lookup name env
	eval1' env (Plus exp1 exp2) = do 
	                                  IntVal v1 <- eval1' env exp1
	                                  IntVal v2 <- eval1' env exp2
	                                  return (IntVal (v1 + v2))
	eval1' env (App exp1 exp2) = do
	                                  v1 <- eval1' env exp1
	                                  v2 <- eval1' env exp2
	                                  case v1 of
	                                     FunVal env' n body -> eval1' (Map.insert n v2 env')  body

	-- We can run eval1' in GHCI because it would use the IO monad by default
	ex1' :: IO Value
	ex1' = eval1' (Map.empty) ex
	
	-- eval2 introduces error handling.
	-- Note the (Var name) case definition below does not have a do expression because the discriminator itself is a Maybe type. 
	-- return on ErrorT monad type class builds a Right value. Any exception builds a Left value (as in throwError)
	
	eval2 :: Env -> Exp -> (ErrorT String Identity Value)
	eval2 env (Abs name exp1) = return $ FunVal env name exp1
	eval2 env (Lit i) = return $ IntVal i
	eval2 env (Var name) = case Map.lookup name env of
	                         Nothing -> throwError ("Unknown variable " ++ show name)
	                         Just x  -> return x
	eval2 env (Plus exp1 exp2) = do 
	                                  v1 <- eval2 env exp1
	                                  v2 <- eval2 env exp2
	                                  case (v1, v2) of 
	                                    (IntVal i1, IntVal i2) -> return $ (IntVal (i1 + i2))
	                                    _                      -> throwError "Expected integer types for addition"
	eval2 env (App exp1 exp2) = do
	                                  v1 <- eval2 env exp1
	                                  v2 <- eval2 env exp2
	                                  case v1 of
	                                     FunVal env' n body -> eval2 (Map.insert n v2 env')  body
	                                     _                  -> throwError ("Expected first arg (" ++ show exp1 ++ ") to evaluate to a FunVal type")
	
	
	
