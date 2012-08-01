-- | The implementations of all functions are 
--   supposed to be as non-strict as possible. This is non-trivial as for
--   example a naive implementation of (*) yields O _|_ for the application
--   I _|_ * O IHi while a least-strict version yields O (I _|_). Also the
--   naive / standard implementations of (\-), compare, (\<), (\<=), (\>), 
--   (\>=) are more strict than necessary.
module Data.Number.Nat1 
  ( 
    -- * Datatype
    Nat1(..), 
    -- * Helper Functions
    cmpNat1LT, invOrd, minusNat1, fromNat1, toNat1  
  ) where


import Prelude hiding ( Int )
import Data.Ratio ( (%) )


-- | A binary representation of natural numbers which starts with the least
--   significant bit.
data Nat1
  -- | This constructor represents the most significant bit. There are no 
  --   leading zero bits. 
  = IHi 
  -- | A zero bit
  | O Nat1 
  -- | A one bit
  | I Nat1
  deriving Eq


instance Show Nat1 where
  show = show . fromEnum 


instance Read Nat1 where
  readsPrec n = map (\(x,str) -> (toEnum x,str)) . readsPrec n  


instance Ord Nat1 where
  compare IHi IHi   = EQ
  compare IHi (O _) = LT
  compare IHi (I _) = LT
  compare (O _) IHi = GT
  compare (I _) IHi = GT
  compare (O x) (O y) = compare x y
  compare (I x) (I y) = compare x y
  compare (O x) (I y) = cmpNat1LT x y
  compare (I x) (O y) = invOrd (cmpNat1LT y x)

  -- these instances are lazier than the standard implementation
  -- for example IHi <= _|_ = True
  -- while the standard implementation yields _|_
  x < y  = cmpNat1LT y x == GT
  x > y  = cmpNat1LT x y == GT
  x <= y = cmpNat1LT x y == LT
  x >= y = cmpNat1LT y x == LT

-- | This function is used to implement lazy instances of compare and (\<), 
--   (\<=), (\>), (\>=). It is used to transfer information to more significant
--   bits. Instead of yielding EQ it yields LT if the numbers are equal.
cmpNat1LT :: Nat1 -> Nat1 -> Ordering
cmpNat1LT IHi _     = LT
cmpNat1LT (O _) IHi = GT
cmpNat1LT (I _) IHi = GT
cmpNat1LT (O x) (O y) = cmpNat1LT x y
cmpNat1LT (I x) (I y) = cmpNat1LT x y
cmpNat1LT (O x) (I y) = cmpNat1LT x y
cmpNat1LT (I x) (O y) = invOrd (cmpNat1LT y x)

-- | Maps LT to GT and GT to LT. It is used instead of defining a function
--   cmpNat1GT.
invOrd :: Ordering -> Ordering
invOrd EQ = EQ
invOrd LT = GT
invOrd GT = LT


instance Enum Nat1 where
  succ (O bs) = I bs
  succ (I bs) = O (succ bs)
  succ IHi    = O IHi

  pred IHi         = error "predecessor of 1"
  pred (O IHi)     = IHi
  pred (O x@(O _)) = I (pred x)
  pred (O (I x))   = I (O x) 
  pred (I x)       = O x
 
  fromEnum = fromNat1

  toEnum = toNat1


instance Num Nat1 where
  O x + O y = O (x + y)
  O x + I y = I (x + y)
  O x + IHi = I x
  I x + O y = I (x + y)
  I x + I y = O (succ x + y)
  I x + IHi = O (succ x)
  IHi + y   = succ y

  x - y =
    case minusNat1 x y of
         IHi -> error "result zero in (-)"
         n   -> pred n

  IHi * y = y
  I x * y = O (y * x) + y
  O x * y = O (x * y)

  negate = error "no non-positive numbers in Nat1"

  abs = id

  signum = const IHi

  fromInteger = toNat1

-- | minusNat1 x y yields x - y + 1. This is used to implement (-) for natural
--   numbers.
minusNat1 :: Nat1 -> Nat1 -> Nat1
minusNat1 x   IHi = x
minusNat1 IHi (O _) = error "negative result in (-)"
minusNat1 IHi (I _) = error "negative result in (-)"
minusNat1 (O x) (O y) = pred (O $! minusNat1 x y)
minusNat1 (O x) (I y) = O $! pred (minusNat1 x y)
minusNat1 (I x) (O y) = O $! minusNat1 x y
minusNat1 (I x) (I y) = pred (O $! minusNat1 x y)  


instance Real Nat1 where
  toRational n = fromNat1 n % 1 


-- | This is used for the implementation of toInteger and fromEnum.
fromNat1 :: Num n => Nat1 -> n
fromNat1 IHi   = 1
fromNat1 (O n) = 2 * fromNat1 n
fromNat1 (I n) = 2 * fromNat1 n + 1

-- | This is used for the implementation of fromInteger and toEnum.
toNat1 :: (Integral n,Num n) => n -> Nat1
toNat1 n
  | n<0       = error "fromInteger/toEnum of negative number"
  | n==0      = error "fromInteger/toEnum of zero"
  | n==1      = IHi
  | even n    = O (toNat1 (n `div` 2))
  | otherwise = I (toNat1 (n `div` 2))

