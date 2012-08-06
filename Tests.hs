import Test.QuickCheck (quickCheck, (==>), Property)
import Text.Printf (printf)
import Data.Number.Nat1
import Data.Number.Nat
import qualified Data.Number.Int as Int


main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_binOpNat1 :: (Nat1 -> Nat1 -> Nat1) -> (Int -> Int -> Int) 
               -> (Int -> Int -> Bool) -> Int -> Int -> Property
prop_binOpNat1 op1 op2 p m n = 
  p m n ==> 
    fromEnum (op1 (toEnum m :: Nat1) (toEnum n :: Nat1)) == (op2 m n :: Int)

prop_binOpNat :: (Nat -> Nat -> Nat) -> (Int -> Int -> Int) 
               -> (Int -> Int -> Bool) -> Int -> Int -> Property
prop_binOpNat op1 op2 p m n =
  p m n ==> 
    fromEnum (op1 (toEnum m :: Nat) (toEnum n :: Nat)) == (op2 m n :: Int)

prop_binOpInt :: (Int.Int -> Int.Int -> Int.Int) -> (Int -> Int -> Int) -> Int 
              -> Int -> Bool
prop_binOpInt op1 op2 m n =
  fromEnum (op1 (toEnum m :: Int.Int) (toEnum n :: Int.Int)) == (op2 m n :: Int)

isNat1 :: Int -> Bool
isNat1 = (>0)

isNat :: Int -> Bool
isNat = (>=0)

areNat1s :: Int -> Int -> Bool
areNat1s m n = isNat1 m && isNat1 n

areNats :: Int -> Int -> Bool
areNats m n = isNat m && isNat n

true :: Int -> Int -> Bool
true _ _ = True

posNat1 :: Int -> Int -> Bool
posNat1 m n = areNat1s m n && m>n

posNat :: Int -> Int -> Bool
posNat m n = areNats m n && m>=n

prop_divNat :: Int -> Int -> Property
prop_divNat m n = 
  areNats m n && n /= 0 ==> 
    fromEnum (div (toEnum m :: Nat) (toEnum n :: Nat)) == (div m n :: Int)

prop_divInt :: Int -> Int -> Property
prop_divInt m n = 
  n /= 0 ==> 
    fromEnum (div (toEnum m :: Int.Int) (toEnum n :: Int.Int)) == 
        (div m n :: Int)


tests = 
  [("mult :: Nat1",quickCheck (prop_binOpNat1 (*) (*) areNat1s))
  ,("mult :: Nat",quickCheck (prop_binOpNat (*) (*) areNats))
  ,("mult :: Int",quickCheck (prop_binOpInt (*) (*)))
  ,("add :: Nat1",quickCheck (prop_binOpNat1 (+) (+) areNat1s))
  ,("add :: Nat",quickCheck (prop_binOpNat (+) (+) areNats))
  ,("add :: Int",quickCheck (prop_binOpInt (+) (+)))
  ,("minus :: Nat1",quickCheck (prop_binOpNat1 (-) (-) posNat1))
  ,("minus :: Nat",quickCheck (prop_binOpNat (-) (-) posNat))
  ,("minus :: Int",quickCheck (prop_binOpInt (-) (-)))
--   ,("div :: Nat1",quickCheck (prop_binOpNat1 div div posNat1))
  ,("div :: Nat",quickCheck prop_divNat)
  ,("div :: Int",quickCheck prop_divInt)
  ]

