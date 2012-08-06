-- | The implementations of all functions except for rem, quot, div, mod are 
--   supposed to be as non-strict as possible.
module Data.Number.Nat
  ( 
    -- * Datatype
    Nat(..), 
    -- * Helper Functions
    cmpNatLT, fromNat, toNat,

    -- * Example Implementations using Nat
    length, take, drop, replicate,
    -- * Example Implementations using Num
    lengthNum, takeNum, dropNum, replicateNum, 

    divmodNat1
    
  ) where


import Prelude hiding ( length, take, drop, replicate )
import Data.Number.Nat1
import Data.Ratio ( (%) )


-- | Natural numbers and zero
data Nat
  -- | Constructor representing zero
  = Zero  
  -- | A natural number 
  | Pos Nat1
  deriving Eq


instance Show Nat where
  show Zero = "0"
  show (Pos n) = show n


instance Read Nat where
  readsPrec n = map (\(x,str) -> (toEnum x,str)) . readsPrec n  


instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero _    = LT
  compare _    Zero = GT
  compare (Pos m) (Pos n) = compare m n

  -- these instances are lazier than the standard implementation
  -- for example Zero <= _|_ = True
  -- while the standard implementation yields _|_
  x < y  = cmpNatLT y x == GT
  x >  y = cmpNatLT x y == GT
  x <= y = cmpNatLT x y == LT
  x >= y = cmpNatLT y x == LT

-- | This function is used to implement lazy instances of compare and (\<), 
--   (\<=), (\>), (\>=). It is used to transfer information to more significant
--   bits. Instead of yielding EQ it yields LT if the numbers are equal.
cmpNatLT :: Nat -> Nat -> Ordering
cmpNatLT Zero  _    = LT
cmpNatLT _     Zero = GT
cmpNatLT (Pos m) (Pos n) = cmpNat1LT m n


instance Enum Nat where
  succ = Pos . succ'
   where
    succ' Zero    = IHi
    succ' (Pos n) = succ n

  pred Zero      = error "predecessor of Zero"
  pred (Pos IHi) = Zero
  pred (Pos n)   = Pos (pred n)

  toEnum = toNat

  fromEnum = fromNat


instance Num Nat where
  Zero + n = n
  n + Zero = n
  Pos m + Pos n = Pos (m+n) 

  Zero  - Pos _ = error "negative result in (-)"
  n     - Zero  = n
  Pos m - Pos n = pred (Pos (minusNat1 m n))

  Zero  * _     = Zero
  _     * Zero  = Zero
  Pos m * Pos n = Pos (m*n)

  negate Zero = Zero
  negate _    = error "negative result in negate"
  
  signum Zero    = Zero
  signum (Pos _) = Pos IHi

  abs = id

  fromInteger = toNat


instance Integral Nat where
  div m n = fst $ divmodNat m n

  mod m n = snd $ divmodNat m n

  quotRem = divmodNat

  toInteger = fromNat


divmodNat :: Nat -> Nat -> (Nat,Nat)
divmodNat _ Zero = error "divide by zero"
divmodNat Zero _ = (Zero,Zero)
divmodNat (Pos x) (Pos y) = divmodNat1 x y


divmodNat1 :: Nat1 -> Nat1 -> (Nat,Nat)
divmodNat1 x     IHi   = (Pos x,Zero)
divmodNat1 (O x) (O y) = (q,o r) 
 where
  (q,r) = divmodNat1 x y
divmodNat1 (I x) (O y) = (q,i r) 
 where
  (q,r) = divmodNat1 x y
divmodNat1 x y
  | x<y       = (Zero,Pos x)
  | otherwise = (succ q,r)
 where
  (q,r) = divmodNat (Pos x-Pos y) (Pos y)

o :: Nat -> Nat
o Zero    = Zero
o (Pos n) = Pos (O n)

i :: Nat -> Nat
i Zero    = Pos IHi
i (Pos n) = Pos (I n)


-- | This is used for the implementation of toInteger and fromEnum.
fromNat :: Num n => Nat -> n
fromNat Zero    = 0
fromNat (Pos n) = fromNat1 n

-- | This is used for the implementation of fromInteger and toEnum.
toNat :: (Integral n,Num n) => n -> Nat
toNat n 
  | n<0       = error "fromInteger/toEnum of negative number"
  | n==0      = Zero
  | otherwise = Pos (toNat1 n)


instance Real Nat where
  toRational n = toInteger n % 1


-- example functions

length :: [a] -> Nat
length []     = Zero
length (_:xs) = succ (length xs)

take :: Nat -> [a] -> [a]
take Zero _      = []
take _    []     = []
take n    (x:xs) = x:take (pred n) xs

drop :: Nat -> [a] -> [a]
drop Zero xs     = xs
drop _    []     = []
drop n    (_:xs) = drop (pred n) xs

replicate :: Nat -> a -> [a]
replicate n = take n . repeat


lengthNum :: (Enum n,Num n) => [a] -> n
lengthNum []     = 0
lengthNum (_:xs) = succ (lengthNum xs)

takeNum :: (Eq n,Enum n,Num n) => n -> [a] -> [a]
takeNum n l
  | n==0      = []
  | otherwise = takeNum' l 
 where 
  takeNum' []     = []
  takeNum' (x:xs) = x:takeNum (pred n) xs

dropNum :: (Eq n,Enum n,Num n) => n -> [a] -> [a]
dropNum n l
  | n==0      = l
  | otherwise = dropNum' l
 where
  dropNum' []     = []
  dropNum' (_:xs) = dropNum (pred n) xs

replicateNum :: (Eq n,Enum n,Num n) => n -> a -> [a]
replicateNum n = takeNum n . repeat


-- examples

-- lazy :: String
-- lazy =
--   if length [1::Int ..]>0 then "long"
--                          else "short"

