&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
&nbsp;     
  
Categories great and small
==========================

 - empty category
 - make a category from directed graph (free category)

Orders
------

- preorder (transitive relation)
- partial order (preorder + antisymetric) (no cycles, topo sort)
- total order (comparability: a<=b or b<=a) (can sort)

hom-set C(a,b) or Hom(a,b) is the set of morphisms from a top b

- preorder: C(a,b) is singleton or empty
- total: |C(a,b)| + |C(b,a)| = 1 ??

Monoid
------

empty element, and associative binary operator

think 0 and +, * and 1, "" and concat 
laws

    x + (y + z) = (x + y) + z
    0 + x = x = x + 0

Haskell
    
    class Monoid where
        mempty: m
        mappend: m -> m -> m

Currying ok?
mappend x returns a function that "mappends" x to the argument

    mappend x y = (mappend x) y

Monoid Category
---------------

- Morpisms are unary. Mappend is binary
- (+ 5) is a unary function that maps e.g. any number x to x+5
- (+ 5) . (+ 6) = (+ 11)
- (+ 0) is identity

We get

- single object categoty
- for each element x in our set, a morphism (mappend x)
- identity (mappend mempty)
- composition 

We can get back to monoid = set, mempty and mappend

- The set is all mphisms (+ x)
- mempty is identity morphism
- mappend is composition


