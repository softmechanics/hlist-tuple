hlist-tuple provides a tuple-based interface to HList

### creating an HList:

    (e1) .*. (e2) .*. (e3) .*. HNil 

becomes

    untuple (e1,e2,e3)

### pattern matching on an HList:

    let (HCons e1 (HCons e2 (HCons e3 HNil))) = l 

becomes

    let (e1,e2,e3) = tuple l

***

HList-based libraries can add support for the tuple-based interface using the IsTuple type level predicate:

    class SomeClass a b | a -> b where
      someF :: a -> b
    instance SomeClass HNil ...
    instance (SomeClass rest ...) => SomeClass (HCons e rest) ... 

becomes

    instance (IsTuple l tup
             ,SomeClass' tup l ...           
             ) => SomeClass l ... where
      someF = someF' (undefined::tup)

    instance (Untuple t l
             ,SomeClass l ...
             ) => SomeClass' HTrue t ... where 
      someF' _ t = someF $ untuple t

    instance SomeClass' HFalse HNil ...
    instance (SomeClass' HFalse rest ...
             ) => SomeClass' HFalse (HCons e rest) ... where
      someF' hFalse (HCons e rest) = ...

Where the (SomeClass' HFalse ...) instances are analagous to the original (SomeClass ...) ones.
