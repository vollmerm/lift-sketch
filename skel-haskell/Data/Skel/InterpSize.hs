{-# LANGUAGE GADTs #-}

-- |
-- Module     : Data.Skel.InterpSize
-- Maintainer : Michael Vollmer <vollmerm@indiana.edu>
--
-- This is a quick mock-up of an AST and interpreter
-- see: https://github.com/vollmerm/lift-sketch/wiki/Language
--
-- Here I also try to track changes in array size.

module Data.Skel.InterpSize where

data Array a = Nil | Arr [a] Int
               deriving (Show)

-- The AST.

data Expr a where
  
  -- Boring stuff.
  Lift      :: a -> Expr a
  Tuple     :: Expr a -> Expr b -> Expr (a, b)
  Lambda    :: (Expr a -> Expr b) -> Expr (a -> b)
  Apply     :: Expr (a -> b) -> Expr a -> Expr b

  -- Array/List operations (high-level)
  Map       :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  Zip       :: Expr (Array a) -> Expr (Array b) -> Expr (Array (a, b))
  Reduce    :: Expr (a -> a -> a) -> Expr a -> Expr (Array a) -> Expr a
  Split     :: Expr Int -> Expr (Array a) -> Expr (Array (Array a))
  Join      :: Expr (Array (Array a)) -> Expr (Array a)
  Iterate   :: Expr Int -> Expr ((Array a) -> (Array a)) -> Expr (Array a) -> Expr (Array a)
  Reorder   :: Expr (Array a) -> Expr (Array a)
  Transpose :: Expr (Array (Array a)) -> Expr (Array (Array a))

  -- Array/List operations (low-level)
  MapWrkgp  :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  MapLocal  :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  MapGlobal :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  MapSeq    :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  MapVec    :: Expr (a -> b) -> Expr (Array a) -> Expr (Array b)
  ReduceSeq :: Expr (a -> a -> a) -> Expr a -> Expr (Array a) -> Expr a
  ReducePrt :: Expr (a -> a -> a) -> Expr a -> Expr (Array a) -> Expr a

  -- Simple scalar expressions
  Plus      :: (Num a) => Expr a -> Expr a -> Expr a
  Minus     :: (Num a) => Expr a -> Expr a -> Expr a
  Mult      :: (Num a) => Expr a -> Expr a -> Expr a

-- Simple interpreter.
eval :: Expr a -> a
eval (Lift v) = v
eval (Tuple e1 e2) = (eval e1, eval e2)
eval (Lambda f) = eval . f . Lift
eval (Apply e1 e2) = (eval e1) (eval e2)
eval (Map e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (Zip e1 e2)
  | n1 == n2 = Arr (zip xs1 xs2) n1
  | otherwise = error "Length mismatch on Zip"
  where (Arr xs1 n1) = eval e1
        (Arr xs2 n2) = eval e2
eval (Reduce e1 e2 e3)  = foldr (eval e1) (eval e2) xs
  where (Arr xs _n) = eval e3
eval (Split e1 e2) =
  if len `mod` n == 0
  then Arr (map (\x -> Arr x inner) (split inner xs)) n
  else error "Length mismatch"
  where (Arr xs len) = eval e2
        n = eval e1
        inner = len `div` n
        split 1 xs' = [take n xs']
        split i xs' = take n xs' : (split (i-1) (drop n xs'))
eval (Join e1) = Arr (concat xs') n'
  where (Arr xs _n) = eval e1
        xs' = map (\(Arr xs'' _n) -> xs'') xs
        n' = length xs'
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
eval (MapWrkgp e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (MapLocal e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (MapGlobal e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (MapSeq e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (MapVec e1 e2) = Arr (map f xs) n
  where f = eval e1
        (Arr xs n) = eval e2
eval (ReduceSeq e1 e2 e3) = foldr (eval e1) (eval e2) xs
  where (Arr xs _n) = eval e3
eval (ReducePrt e1 e2 e3) = foldr (eval e1) (eval e2) xs
  where (Arr xs _n) = eval e3

eval (Plus e1 e2)   = (eval e1) + (eval e2)
eval (Minus e1 e2)  = (eval e1) - (eval e2)
eval (Mult e1 e2)   = (eval e1) * (eval e2)


-- Example use.
data1 :: Expr (Array Int)
data1 = Lift (Arr [1,2,3,4] 4)
fun1 :: Expr (Int -> Int)
fun1 = Lambda (\x -> Mult x x)
fun2 :: Expr (Array Int -> Array Int)
fun2 = Lambda (\xs -> Map fun1 xs)
prog1 :: Expr (Array Int)
prog1 = Map fun1 data1
prog2 :: Expr (Array (Array Int))
prog2 = Split (Lift 2) prog1
prog3 :: Expr (Array Int)
prog3 = Join (Map fun2 prog2)
prog1_result :: Array Int
prog1_result = eval prog1 -- => Arr [1,4,9,16] 4
prog2_result :: Array (Array Int)
prog2_result = eval prog2 -- => Arr [Arr [1,4] 2,Arr [9,16] 2] 2
prog3_result :: Array Int
prog3_result = eval prog3 -- => Arr [1,16,81,256] 2
