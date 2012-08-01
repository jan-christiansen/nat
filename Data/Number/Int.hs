-- | The implementations of all functions except for rem, quot, div, mod are 
--   supposed to be as non-strict as possible.
module Data.Number.Int ( Int(..), pos ) where


import Prelude hiding ( Int )
import Data.Number.Nat1
import Data.Number.Nat
import Data.Ratio ( (%) )


-- | Integers
data Int 
  -- | A negative natural number
  = Neg Nat1 
  -- | A positive natural number or zero
  | Pos0 Nat
  deriving Eq


pos :: Nat1 -> Int
pos = Pos0 . Pos

zero :: Int
zero = Pos0 Zero


instance Show Int where
  show (Neg n)  = "-" ++ show n
  show (Pos0 n) = show n


instance Read Int where
  readsPrec n = map (\(x,str) -> (toEnum x,str)) . readsPrec n  


instance Ord Int where
  compare (Neg _)  (Pos0 _) = LT
  compare (Pos0 _) (Neg _)  = GT
  compare (Pos0 m) (Pos0 n) = compare m n
  compare (Neg m)  (Neg n)  = compare n m

  x <  y = cmpIntLT y x == GT
  x >  y = cmpIntLT x y == GT
  x <= y = cmpIntLT x y == LT
  x >= y = cmpIntLT y x == LT

cmpIntLT :: Int -> Int -> Ordering
cmpIntLT (Neg _)  (Pos0 _) = LT
cmpIntLT (Pos0 _) (Neg _)  = GT
cmpIntLT (Pos0 m) (Pos0 n) = cmpNatLT m n
cmpIntLT (Neg m)  (Neg n)  = invOrd (cmpNat1LT m n)


instance Enum Int where
  succ (Pos0 n)  = Pos0 (succ n)
  succ (Neg IHi) = zero
  succ (Neg n)   = Neg (succ n)

  pred (Neg n)     = Neg (pred n)
  pred (Pos0 Zero) = Neg IHi
  pred (Pos0 n)    = Pos0 (pred n)

  toEnum = toInt

  fromEnum = fromInt


instance Num Int where
  n            + Pos0 Zero    = n
  Neg m        + Pos0 (Pos n) = n -^ m
  Pos0 Zero    + n            = n
  Pos0 (Pos m) + Neg n        = m -^ n
  Pos0 m       + Pos0 n       = Pos0 (m+n)
  Neg m        + Neg n        = Neg (m+n)

  m - Neg n        = m + pos n
  m - Pos0 Zero    = m
  m - Pos0 (Pos n) = m + Neg n

  _            * Pos0 Zero    = zero
  Pos0 Zero    * _            = zero
  Neg m        * Pos0 (Pos n) = Neg (m*n)
  Pos0 (Pos m) * Neg n        = Neg (m*n)
  Neg m        * Neg n        = pos (m*n)
  Pos0 m       * Pos0 n       = Pos0 (m*n) 

  negate   (Neg n)        = pos n
  negate n@(Pos0 Zero)    = n
  negate   (Pos0 (Pos n)) = Neg n

  signum   (Neg _)    = Neg IHi
  signum n@(Pos0 Zero) = n
  signum   (Pos0 _)    = pos IHi

  abs (Neg n) = pos n
  abs n       = n

  fromInteger = toInt

-- this implementation is least strict
-- for example: I _|_ -^ O IHi =  (Nat _|_)
-- while the standard implementation yields _|_ in this case
-- another solution would be to use another datatype ???
-- data Int = Neg NatO | Pos Nat
(-^) :: Nat1 -> Nat1 -> Int
x -^ y = 
  case minus x y of
    Pos0 n  -> Pos0 n
    Neg IHi -> zero
    Neg n   -> Neg (pred n)

-- minus x y yields x - y - 1 if x - y <= 0
--                  x - y     otherwise
minus :: Nat1 -> Nat1 -> Int
minus IHi     y     = Neg y
minus x@(O _) IHi   = pos (pred x)
minus (I x)   IHi   = pos (O x)
minus (O x)   (O y) = incNeg (o (minus x y))
minus (I x)   (I y) = incNeg (o (minus x y))
minus (O x)   (I y) = decNeg (o (minus x y))
minus (I x)   (O y) = decNeg (o (negate (minus y x)))

o :: Int -> Int
o (Neg n)  = Neg (O n)
o (Pos0 n) = 
  Pos0 (case n of
             Zero  -> Zero
             Pos m -> Pos (O m))

incNeg :: Int -> Int
incNeg (Neg n)  = Neg (pred n)
incNeg (Pos0 n) = Pos0 n

decNeg :: Int -> Int
decNeg (Neg n)  = Neg n
decNeg (Pos0 n) = Pos0 (pred n)


instance Integral Int where
  quotRem (Neg m) (Neg n) = (Pos0 q,neg r)
   where
    (q,r) = quotRem (Pos m) (Pos n)
  quotRem (Neg m) (Pos0 n) = (neg q,neg r)
   where
    (q,r) = quotRem (Pos m) n
  quotRem (Pos0 m) (Neg n) = (neg q,Pos0 r)
   where
    (q,r) = quotRem m (Pos n)
  quotRem (Pos0 m) (Pos0 n) = (Pos0 q,Pos0 r)
   where
    (q,r) = quotRem m n

  toInteger = fromInt

neg :: Nat -> Int 
neg Zero    = zero
neg (Pos n) = Neg n


fromInt :: Num n => Int -> n
fromInt (Neg n)  = -fromNat1 n
fromInt (Pos0 n) = fromNat n

toInt :: (Integral n,Num n) => n -> Int
toInt n 
  | n<0       = Neg (toNat1 (-n))
  | otherwise = Pos0 (toNat n)


instance Real Int where
  toRational n = toInteger n % 1
