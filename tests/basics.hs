{-# LANGUAGE ScopedTypeVariables #-}
import Data.HList hiding (tuple)
import Data.HList.Tuple
import Test.HUnit

instance Eq HTrue where
  _ == _ = True

instance Eq HFalse where
  _ == _ = True

hlist = (5::Int) .*. "string" .*. () .*. (Just 'a') .*. HNil
tup = (5::Int,"string",(),(Just 'a'))

testTuple = TestCase $ assertEqual "tuple" tup (tuple hlist)
testUntuple = TestCase $ assertEqual "untuple" hlist (untuple tup)


testIsTupleFalse = TestCase $ assertEqual "isTuple :: () -> HFalse" hFalse (isTuple ())
testIsTupleTrue = TestCase $ assertEqual "isTuple :: ((),()) -> HTrue" hTrue (isTuple ((),()))

tests = TestList [testTuple
                 ,testUntuple
                 ,testIsTupleFalse
                 ,testIsTupleTrue
                 ]

main = runTestTT tests

