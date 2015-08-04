{-# LANGUAGE GADTs #-}

-- |
-- Module     : Data.Skel.Interp
-- Maintainer : Michael Vollmer <vollmerm@indiana.edu>
--
-- This is a quick mock-up of an AST and interpreter
-- see: https://github.com/vollmerm/lift-sketch/wiki/Language
--

module Data.Skel.Interp where

-- The AST.
-- I'm using lists again.
data Expr a where
  
  -- Boring stuff.
  Lift      :: a -> Expr a
  Tuple     :: Expr a -> Expr b -> Expr (a, b)
  Lambda    :: (Expr a -> Expr b) -> Expr (a -> b)
  Apply     :: Expr (a -> b) -> Expr a -> Expr b

  -- Array/List operations (high-level)
  Map       :: Expr (a -> b) -> Expr [a] -> Expr [b]
  Zip       :: Expr [a] -> Expr [b] -> Expr [(a, b)]
  Reduce    :: Expr (a -> a -> a) -> Expr a -> Expr [a] -> Expr a
  Split     :: Expr Int -> Expr [a] -> Expr [[a]]
  Join      :: Expr [[a]] -> Expr [a]
  Iterate   :: Expr Int -> Expr ([a] -> [a]) -> Expr [a] -> Expr [a]
  Reorder   :: Expr [a] -> Expr [a]
  Transpose :: Expr [[a]] -> Expr [[a]]

  -- Array/List operations (low-level)
  MapWrkgp  :: Expr (a -> b) -> Expr [a] -> Expr [b]
  MapLocal  :: Expr (a -> b) -> Expr [a] -> Expr [b]
  MapGlobal :: Expr (a -> b) -> Expr [a] -> Expr [b]
  MapSeq    :: Expr (a -> b) -> Expr [a] -> Expr [b]
  MapVec    :: Expr (a -> b) -> Expr [a] -> Expr [b]
  ReduceSeq :: Expr (a -> a -> a) -> Expr a -> Expr [a] -> Expr a
  ReducePrt :: Expr (a -> a -> a) -> Expr a -> Expr [a] -> Expr a

  -- Simple scalar expressions
  Plus      :: (Num a) => Expr a -> Expr a -> Expr a
  Minus     :: (Num a) => Expr a -> Expr a -> Expr a
  Mult      :: (Num a) => Expr a -> Expr a -> Expr a

-- Simple interpreter.
eval :: Expr a -> a
eval (Lift v)           = v
eval (Tuple e1 e2)      = (eval e1, eval e2)
eval (Lambda f)         = eval . f . Lift
eval (Apply e1 e2)      = (eval e1) (eval e2)
eval (Map e1 e2)        = map (eval e1) (eval e2)
eval (Zip e1 e2)        = zip (eval e1) (eval e2)
eval (Reduce e1 e2 e3)  = foldr (eval e1) (eval e2) (eval e3)
eval (Split e1 e2)      =
  if length xs `mod` n == 0
  then split (length xs `div` n) xs
  else error "Length mismatch"
  where xs = eval e2
        n  = eval e1
        split 1 xs' = [take n xs']
        split i xs' = take n xs' : (split (i-1) (drop n xs'))
eval (Join e1)          = concat (eval e1)
eval (Iterate e1 e2 e3) =
  if n >= 0 then compute n a
  else error "Invalid iteration count"
  where n = eval e1
        f = eval e2
        a = eval e3
        compute 0 a' = a'
        compute n' a' = compute (n' - 1) (f a')
eval (Reorder e)    = eval e
eval (Transpose e)  = eval e
eval (MapWrkgp e1 e2)  = map (eval e1) (eval e2)
eval (MapLocal e1 e2)  = map (eval e1) (eval e2)
eval (MapGlobal e1 e2) = map (eval e1) (eval e2)
eval (MapSeq e1 e2)    = map (eval e1) (eval e2)
eval (MapVec e1 e2)    = map (eval e1) (eval e2)
eval (ReduceSeq e1 e2 e3)  = foldr (eval e1) (eval e2) (eval e3)
eval (ReducePrt e1 e2 e3)  = foldr (eval e1) (eval e2) (eval e3)

eval (Plus e1 e2)   = (eval e1) + (eval e2)
eval (Minus e1 e2)  = (eval e1) - (eval e2)
eval (Mult e1 e2)   = (eval e1) * (eval e2)


-- Example use.
data1 :: Expr [Int]
data1 = Lift [1,2,3,4]
fun1 :: Expr (Int -> Int)
fun1 = Lambda (\x -> Mult x x)
fun2 :: Expr ([Int] -> [Int])
fun2 = Lambda (\xs -> Map fun1 xs)
prog1 :: Expr [Int]
prog1 = Map fun1 data1
prog2 :: Expr [[Int]]
prog2 = Split (Lift 2) prog1
prog3 :: Expr [Int]
prog3 = Join (Map fun2 prog2)
prog1_result :: [Int]
prog1_result = eval prog1 -- => [1,4,9,16]
prog2_result :: [[Int]]
prog2_result = eval prog2 -- => [[1,4],[9,16]]
prog3_result :: [Int]
prog3_result = eval prog3 -- => [1,16,81,256]
