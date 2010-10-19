{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies
           #-}
import Data.HList
import Data.HList.Tuple
import Control.Applicative
import Test.HUnit
import Data.Char

-- HFoldl is missing from HList library
class HFoldl f v l r | f v l -> r where
  hFoldl :: f -> v -> l -> r
  
instance HFoldl f v HNil v where  
  hFoldl _ v _ = v
  
instance (Apply f (v,e) v'
         ,HFoldl f v' l r
         ) => HFoldl f v (HCons e l) r where
  hFoldl f v (HCons e l) = hFoldl f (apply f (v,e)) l
  
-- Type-level function for <*>
data ApplicativeF = ApplicativeF

instance (a ~ a'
         ,b ~ b'
         ,f ~ f'
         ,f ~ f''
         ,Applicative f
         ) => Apply ApplicativeF (f (a -> b) , f' a') (f'' b') where
  apply _ (f,e) = f <*> e

doApplicative f t = hFoldl ApplicativeF f l
  where l = untuple t
        
tuplerT = pure . tupler . hLength . untuple

applicativeIn = (Just 'a', Just (), Just 5)
applicativeOut = Just ('a', (), 5)
applicativeOut' = doApplicative (tuplerT applicativeIn) applicativeIn

test1 = TestCase $ assertEqual "(<*>)" applicativeOut applicativeOut'

-- Type-level function for (.)
data Dot = Dot
instance (a ~ a'
         ,b ~ b'
         ,c ~ c'
         ) => Apply Dot (b' -> c, a -> b) (a' -> c') where
  apply _ (f,g) = f . g

doDot = hFoldl Dot id . untuple

dotIn = "hello, world"
dotOut = "World"
dotOut' = doDot (\(c:cs) -> toUpper c : cs
                , tail
                , drop 3
                , drop 3
                ) dotIn

test2 = TestCase $ assertEqual "(.)" dotOut dotOut'

tests = TestList [test1
                 ,test2
                 ]
        
main = runTestTT tests        