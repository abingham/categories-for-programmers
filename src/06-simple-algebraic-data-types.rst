.. raw:: html

   <div id="rap">

.. raw:: html

   <div id="header">

-  `Home <https://bartoszmilewski.com>`__
-  `About <https://bartoszmilewski.com/about/>`__

.. raw:: html

   <div id="headimg">

.. rubric:: `  Bartosz Milewski's Programming
   Cafe <https://bartoszmilewski.com>`__
   :name: bartosz-milewskis-programming-cafe

.. raw:: html

   <div id="desc">

Concurrency, C++, Haskell, Category Theory

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="main">

.. raw:: html

   <div id="content">

.. raw:: html

   <div
   class="post-3823 post type-post status-publish format-standard hentry category-c category-category-theory category-haskell">

January 13, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Simple Algebraic Data Types
   :name: simple-algebraic-datatypes
   :class: post-title

Posted by Bartosz Milewski under
`C++ <https://bartoszmilewski.com/category/c/>`__, `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__
`[15]
Comments <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_3823" class="pd-rating">

.. raw:: html

   </div>

    Categories for Programmers. Previously `Products and
    Coproducts <https://bartoszmilewski.com/2015/01/07/products-and-coproducts/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

We’ve seen two basic ways of combining types: using a product and a
coproduct. It turns out that a lot of data structures in everyday
programming can be built using just these two mechanisms. This fact has
important practical consequences. Many properties of data structures are
composable. For instance, if you know how to compare values of basic
types for equality, and you know how to generalize these comparisons to
product and coproduct types, you can automate the derivation of equality
operators for composite types. In Haskell you can automatically derive
equality, comparison, conversion to and from string, and more, for a
large subset of composite types.

Let’s have a closer look at product and sum types as they appear in
programming.

.. rubric:: Product Types
   :name: product-types

The canonical implementation of a product of two types in a programming
language is a pair. In Haskell, a pair is a primitive type constructor;
in C++ it’s a relatively complex template defined in the Standard
Library.

|Pair|

Pairs are not strictly commutative: a pair ``(Int, Bool)`` cannot be
substituted for a pair ``(Bool, Int)``, even though they carry the same
information. They are, however, commutative up to isomorphism — the
isomorphism being given by the ``swap`` function (which is its own
inverse):

::

    swap :: (a, b) -> (b, a)
    swap (x, y) = (y, x)

You can think of the two pairs as simply using a different format for
storing the same data. It’s just like big endian vs. little endian.

You can combine an arbitrary number of types into a product by nesting
pairs inside pairs, but there is an easier way: nested pairs are
equivalent to tuples. It’s the consequence of the fact that different
ways of nesting pairs are isomorphic. If you want to combine three types
in a product, ``a``, ``b``, and ``c``, in this order, you can do it in
two ways:

::

    ((a, b), c)

or

::

    (a, (b, c))

These types are different — you can’t pass one to a function that
expects the other — but their elements are in one-to-one correspondence.
There is a function that maps one to another:

::

    alpha :: ((a, b), c) -> (a, (b, c))
    alpha ((x, y), z) = (x, (y, z))

and this function is invertible:

::

    alpha_inv :: (a, (b, c)) -> ((a, b), c)
    alpha_inv  (x, (y, z)) = ((x, y), z)

so it’s an isomorphism. These are just different ways of repackaging the
same data.

You can interpret the creation of a product type as a binary operation
on types. From that perspective, the above isomorphism looks very much
like the associativity law we’ve seen in monoids:

::

    (a * b) * c = a * (b * c)

Except that, in the monoid case, the two ways of composing products were
equal, whereas here they are only equal “up to isomorphism.”

If we can live with isomorphisms, and don’t insist on strict equality,
we can go even further and show that the unit type, ``()``, is the unit
of the product the same way 1 is the unit of multiplication. Indeed, the
pairing of a value of some type ``a`` with a unit doesn’t add any
information. The type:

::

    (a, ())

is isomorphic to ``a``. Here’s the isomorphism:

::

    rho :: (a, ()) -> a
    rho (x, ()) = x

::

    rho_inv :: a -> (a, ())
    rho_inv x = (x, ())

These observations can be formalized by saying that **Set** (the
category of sets) is a *monoidal category*. It’s a category that’s also
a monoid, in the sense that you can multiply objects (here, take their
cartesian product). I’ll talk more about monoidal categories, and give
the full definition in the future.

There is a more general way of defining product types in Haskell —
especially, as we’ll see soon, when they are combined with sum types. It
uses named constructors with multiple arguments. A pair, for instance,
can be defined alternatively as:

::

    data Pair a b = P a b

Here, ``Pair a b`` is the name of the type paremeterized by two other
types, ``a`` and ``b``; and ``P`` is the name of the data constructor.
You define a pair type by passing two types to the ``Pair`` type
constructor. You construct a pair value by passing two values of
appropriate types to the constructor ``P``. For instance, let’s define a
value ``stmt`` as a pair of ``String`` and ``Bool``:

::

    stmt :: Pair String Bool
    stmt = P "This statements is" False

The first line is the type declaration. It uses the type constructor
``Pair``, with ``String`` and ``Bool`` replacing ``a`` and the ``b`` in
the generic definition of ``Pair``. The second line defines the actual
value by passing a concrete string and a concrete Boolean to the data
constructor ``P``. Type constructors are used to construct types; data
constructors, to construct values.

Since the name spaces for type and data constructors are separate in
Haskell, you will often see the same name used for both, as in:

::

    data Pair a b = Pair a b

And if you squint hard enough, you may even view the built-in pair type
as a variation on this kind of declaration, where the name ``Pair`` is
replaced with the binary operator ``(,)``. In fact you can use ``(,)``
just like any other named constructor and create pairs using prefix
notation:

::

    stmt = (,) "This statement is" False

Similarly, you can use ``(,,)`` to create triples, and so on.

Instead of using generic pairs or tuples, you can also define specific
named product types, as in:

::

    data Stmt = Stmt String Bool

which is just a product of ``String`` and ``Bool``, but it’s given its
own name and constructor. The advantage of this style of declaration is
that you may define many types that have the same content but different
meaning and functionality, and which cannot be substituted for each
other.

Programming with tuples and multi-argument constructors can get messy
and error prone — keeping track of which component represents what. It’s
often preferable to give names to components. A product type with named
fields is called a record in Haskell, and a ``struct`` in C.

.. rubric:: Records
   :name: records

Let’s have a look at a simple example. We want to describe chemical
elements by combining two strings, name and symbol; and an integer, the
atomic number; into one data structure. We can use a tuple
``(String, String, Int)`` and remember which component represents what.
We would extract components by pattern matching, as in this function
that checks if the symbol of the element is the prefix of its name (as
in **He** being the prefix of **Helium**):

::

    startsWithSymbol :: (String, String, Int) -> Bool
    startsWithSymbol (name, symbol, _) = isPrefixOf symbol name

This code is error prone, and is hard to read and maintain. It’s much
better to define a record:

::

    data Element = Element { name         :: String
                           , symbol       :: String
                           , atomicNumber :: Int }

The two representations are isomorphic, as witnessed by these two
conversion functions, which are the inverse of each other:

::

    tupleToElem :: (String, String, Int) -> Element
    tupleToElem (n, s, a) = Element { name = n
                                    , symbol = s
                                    , atomicNumber = a }

::

    elemToTuple :: Element -> (String, String, Int)
    elemToTuple e = (name e, symbol e, atomicNumber e)

Notice that the names of record fields also serve as functions to access
these fields. For instance, ``atomicNumber e`` retrieves the
``atomicNumber`` field from ``e``. We use ``atomicNumber`` as a function
of the type:

::

    atomicNumber :: Element -> Int

With the record syntax for ``Element``, our function
``startsWithSymbol`` becomes more readable:

::

    startsWithSymbol :: Element -> Bool
    startsWithSymbol e = isPrefixOf (symbol e) (name e)

We could even use the Haskell trick of turning the function
``isPrefixOf`` into an infix operator by surrounding it with backquotes,
and make it read almost like a sentence:

::

    startsWithSymbol e = symbol e `isPrefixOf` name e

The parentheses could be omitted in this case, because an infix operator
has lower precedence than a function call.

.. rubric:: Sum Types
   :name: sum-types

Just as the product in the category of sets gives rise to product types,
the coproduct gives rise to sum types. The canonical implementation of a
sum type in Haskell is:

::

    data Either a b = Left a | Right b

And like pairs, ``Either``\ s are commutative (up to isomorphism), can
be nested, and the nesting order is irrelevant (up to isomorphism). So
we can, for instance, define a sum equivalent of a triple:

::

    data OneOfThree a b c = Sinistral a | Medial b | Dextral c

and so on.

It turns out that **Set** is also a (symmetric) monoidal category with
respect to coproduct. The role of the binary operation is played by the
disjoint sum, and the role of the unit element is played by the initial
object. In terms of types, we have ``Either`` as the monoidal operator
and ``Void``, the uninhabited type, as its neutral element. You can
think of ``Either`` as plus, and ``Void`` as zero. Indeed, adding
``Void`` to a sum type doesn’t change its content. For instance:

::

    Either a Void

is isomorphic to ``a``. That’s because there is no way to construct a
``Right`` version of this type — there isn’t a value of type ``Void``.
The only inhabitants of ``Either a Void`` are constructed using the
``Left`` constructors and they simply encapsulate a value of type ``a``.
So, symbolically, ``a + 0 = a``.

Sum types are pretty common in Haskell, but their C++ equivalents,
unions or variants, are much less common. There are several reasons for
that.

First of all, the simplest sum types are just enumerations and are
implemented using ``enum`` in C++. The equivalent of the Haskell sum
type:

::

    data Color = Red | Green | Blue

is the C++:

::

    enum { Red, Green, Blue };

An even simpler sum type:

::

    data Bool = True | False

is the primitive ``bool`` in C++.

Simple sum types that encode the presence or absence of a value are
variously implemented in C++ using special tricks and “impossible”
values, like empty strings, negative numbers, null pointers, etc. This
kind of optionality, if deliberate, is expressed in Haskell using the
``Maybe`` type:

::

    data Maybe a = Nothing | Just a

The ``Maybe`` type is a sum of two types. You can see this if you
separate the two constructors into individual types. The first one would
look like this:

::

    data NothingType = Nothing

It’s an enumeration with one value called ``Nothing``. In other words,
it’s a singleton, which is equivalent to the unit type ``()``. The
second part:

::

    data JustType a = Just a

is just an encapsulation of the type ``a``. We could have encoded
``Maybe`` as:

::

    data Maybe a = Either () a

More complex sum types are often faked in C++ using pointers. A pointer
can be either null, or point to a value of specific type. For instance,
a Haskell list type, which can be defined as a (recursive) sum type:

::

    List a = Nil | Cons a (List a)

can be translated to C++ using the null pointer trick to implement the
empty list:

::

    template<class A> 
    class List {
        Node<A> * _head;
    public:
        List() : _head(nullptr) {}  // Nil
        List(A a, List<A> l)        // Cons
          : _head(new Node<A>(a, l))
        {}
    };

Notice that the two Haskell constructors ``Nil`` and ``Cons`` are
translated into two overloaded ``List`` constructors with analogous
arguments (none, for ``Nil``; and a value and a list for ``Cons``). The
``List`` class doesn’t need a tag to distinguish between the two
components of the sum type. Instead it uses the special ``nullptr``
value for ``_head`` to encode ``Nil``.

The main difference, though, between Haskell and C++ types is that
Haskell data structures are immutable. If you create an object using one
particular constructor, the object will forever remember which
constructor was used and what arguments were passed to it. So a
``Maybe`` object that was created as ``Just "energy"`` will never turn
into ``Nothing``. Similarly, an empty list will forever be empty, and a
list of three elements will always have the same three elements.

It’s this immutability that makes construction reversible. Given an
object, you can always disassemble it down to parts that were used in
its construction. This deconstruction is done with pattern matching and
it reuses constructors as patterns. Constructor arguments, if any, are
replaced with variables (or other patterns).

The ``List`` data type has two constructors, so the deconstruction of an
arbitrary ``List`` uses two patterns corresponding to those
constructors. One matches the empty ``Nil`` list, and the other a
``Cons``-constructed list. For instance, here’s the definition of a
simple function on ``List``\ s:

::

    maybeTail :: List a -> Maybe (List a)
    maybeTail Nil = Nothing
    maybeTail (Cons _ t) = Just t

The first part of the definition of ``maybeTail`` uses the ``Nil``
constructor as pattern and returns ``Nothing``. The second part uses the
``Cons`` constructor as pattern. It replaces the first constructor
argument with a wildcard, because we are not interested in it. The
second argument to ``Cons`` is bound to the variable ``t`` (I will call
these things variables even though, strictly speaking, they never vary:
once bound to an expression, a variable never changes). The return value
is ``Just t``. Now, depending on how your ``List`` was created, it will
match one of the clauses. If it was created using ``Cons``, the two
arguments that were passed to it will be retrieved (and the first
discarded).

Even more elaborate sum types are implemented in C++ using polymorphic
class hierarchies. A family of classes with a common ancestor may be
understood as one variant type, in which the vtable serves as a hidden
tag. What in Haskell would be done by pattern matching on the
constructor, and by calling specialized code, in C++ is accomplished by
dispatching a call to a virtual function based on the vtable pointer.

You will rarely see ``union`` used as a sum type in C++ because of
severe limitations on what can go into a union. You can’t even put a
``std::string`` into a union because it has a copy constructor.

.. rubric:: Algebra of Types
   :name: algebra-of-types

Taken separately, product and sum types can be used to define a variety
of useful data structures, but the real strength comes from combining
the two. Once again we are invoking the power of composition.

Let’s summarize what we’ve discovered so far. We’ve seen two commutative
monoidal structures underlying the type system: We have the sum types
with ``Void`` as the neutral element, and the product types with the
unit type, ``()``, as the neutral element. We’d like to think of them as
analogous to addition and multiplication. In this analogy, ``Void``
would correspond to zero, and unit, ``()``, to one.

Let’s see how far we can stretch this analogy. For instance, does
multiplication by zero give zero? In other words, is a product type with
one component being ``Void`` isomorphic to ``Void``? For example, is it
possible to create a pair of, say ``Int`` and ``Void``?

To create a pair you need two values. Although you can easily come up
with an integer, there is no value of type ``Void``. Therefore, for any
type ``a``, the type ``(a, Void)`` is uninhabited — has no values — and
is therefore equivalent to ``Void``. In other words, ``a*0 = 0``.

Another thing that links addition and multiplication is the distributive
property:

::

    a * (b + c) = a * b + a * c

Does it also hold for product and sum types? Yes, it does — up to
isomorphisms, as usual. The left hand side corresponds to the type:

::

    (a, Either b c)

and the right hand side corresponds to the type:

::

    Either (a, b) (a, c)

Here’s the function that converts them one way:

::

    prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
    prodToSum (x, e) = 
        case e of
          Left  y -> Left  (x, y)
          Right z -> Right (x, z)

and here’s one that goes the other way:

::

    sumToProd :: Either (a, b) (a, c) -> (a, Either b c)
    sumToProd e = 
        case e of
          Left  (x, y) -> (x, Left  y)
          Right (x, z) -> (x, Right z)

The ``case of`` statement is used for pattern matching inside functions.
Each pattern is followed by an arrow and the expression to be evaluated
when the pattern matches. For instance, if you call ``prodToSum`` with
the value:

::

    prod1 :: (Int, Either String Float)
    prod1 = (2, Left "Hi!")

the ``e`` in ``case e of`` will be equal to ``Left "Hi!"``. It will
match the pattern ``Left  y``, substituting ``"Hi!"`` for ``y``. Since
the ``x`` has already been matched to ``2``, the result of the
``case of`` clause, and the whole function, will be ``Left (2, "Hi!")``,
as expected.

I’m not going to prove that these two functions are the inverse of each
other, but if you think about it, they must be! They are just trivially
re-packing the contents of the two data structures. It’s the same data,
only different format.

Mathematicians have a name for such two intertwined monoids: it’s called
a *semiring*. It’s not a full *ring*, because we can’t define
subtraction of types. That’s why a semiring is sometimes called a *rig*,
which is a pun on “ring without an *n*\ ” (negative). But barring that,
we can get a lot of mileage from translating statements about, say,
natural numbers, which form a ring, to statements about types. Here’s a
translation table with some entries of interest:

+-------------+-------------------------------------------+
| Numbers     | Types                                     |
+=============+===========================================+
| 0           | ``Void``                                  |
+-------------+-------------------------------------------+
| 1           | ``()``                                    |
+-------------+-------------------------------------------+
| a + b       | ``Either a b = Left a | Right b``         |
+-------------+-------------------------------------------+
| a \* b      | ``(a, b) `` or `` Pair a b = Pair a b``   |
+-------------+-------------------------------------------+
| 2 = 1 + 1   | ``data Bool = True | False``              |
+-------------+-------------------------------------------+
| 1 + a       | ``data Maybe = Nothing | Just a``         |
+-------------+-------------------------------------------+

The list type is quite interesting, because it’s defined as a solution
to an equation. The type we are defining appears on both sides of the
equation:

::

    List a = Nil | Cons a (List a)

If we do our usual substitutions, and also replace ``List a`` with
``x``, we get the equation:

::

    x = 1 + a * x

We can’t solve it using traditional algebraic methods because we can’t
subtract or divide types. But we can try a series of substitutions,
where we keep replacing ``x`` on the right hand side with ``(1 + a*x)``,
and use the distributive property. This leads to the following series:

::

    x = 1 + a*x
    x = 1 + a*(1 + a*x) = 1 + a + a*a*x
    x = 1 + a + a*a*(1 + a*x) = 1 + a + a*a + a*a*a*x
    ...
    x = 1 + a + a*a + a*a*a + a*a*a*a...

We end up with an infinite sum of products (tuples), which can be
interpreted as: A list is either empty, ``1``; or a singleton, ``a``; or
a pair, ``a*a``; or a triple, ``a*a*a``; etc… Well, that’s exactly what
a list is — a string of ``a``\ s!

There’s much more to lists than that, and we’ll come back to them and
other recursive data structures after we learn about functors and fixed
points.

Solving equations with symbolic variables — that’s algebra! It’s what
gives these types their name: algebraic data types.

Finally, I should mention one very important interpretation of the
algebra of types. Notice that a product of two types ``a`` and ``b``
must contain both a value of type ``a`` *and* a value of type ``b``,
which means both types must be inhabited. A sum of two types, on the
other hand, contains either a value of type ``a`` *or* a value of type
``b``, so it’s enough if one of them is inhabited. Logical *and* and
*or* also form a semiring, and it too can be mapped into type theory:

+------------+-------------------------------------+
| Logic      | Types                               |
+============+=====================================+
| false      | ``Void``                            |
+------------+-------------------------------------+
| true       | ``()``                              |
+------------+-------------------------------------+
| a \|\| b   | ``Either a b = Left a | Right b``   |
+------------+-------------------------------------+
| a && b     | ``(a, b)``                          |
+------------+-------------------------------------+

This analogy goes deeper, and is the basis of the Curry-Howard
isomorphism between logic and type theory. We’ll come back to it when we
talk about function types.

.. rubric:: Challenges
   :name: challenges

#. Show the isomorphism between ``Maybe a`` and ``Either () a``.
#. Here’s a sum type defined in Haskell:

   ::

       data Shape = Circle Float
                  | Rect Float Float

   When we want to define a function like ``area`` that acts on a
   ``Shape``, we do it by pattern matching on the two constructors:

   ::

       area :: Shape -> Float
       area (Circle r) = pi * r * r
       area (Rect d h) = d * h

   Implement ``Shape`` in C++ or Java as an interface and create two
   classes: ``Circle`` and ``Rect``. Implement ``area`` as a virtual
   function.

#. Continuing with the previous example: We can easily add a new
   function ``circ`` that calculates the circumference of a ``Shape``.
   We can do it without touching the definition of ``Shape``:

   ::

       circ :: Shape -> Float
       circ (Circle r) = 2.0 * pi * r
       circ (Rect d h) = 2.0 * (d + h)

   Add ``circ`` to your C++ or Java implementation. What parts of the
   original code did you have to touch?

#. Continuing further: Add a new shape, ``Square``, to ``Shape`` and
   make all the necessary updates. What code did you have to touch in
   Haskell vs. C++ or Java? (Even if you’re not a Haskell programmer,
   the modifications should be pretty obvious.)
#. Show that ``a + a = 2 * a`` holds for types (up to isomorphism).
   Remember that ``2`` corresponds to ``Bool``, according to our
   translation table.

`Follow @BartoszMilewski <https://twitter.com/BartoszMilewski>`__
Next: `Functors <https://bartoszmilewski.com/2015/01/20/functors/>`__.

.. rubric:: Acknowledments
   :name: acknowledments

Thanks go to Gershom Bazerman for reviewing this post and helpful
comments.

.. raw:: html

   <div class="wpcnt">

.. raw:: html

   <div class="wpa wpmrec wpmrec2x">

Advertisements

.. raw:: html

   <div class="u">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1678033623" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-910170488" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="jp-post-flair"
   class="sharedaddy sd-rating-enabled sd-like-enabled sd-sharing-enabled">

.. raw:: html

   <div class="sharedaddy sd-sharing-enabled">

.. raw:: html

   <div
   class="robots-nocontent sd-block sd-social sd-social-icon-text sd-sharing">

.. rubric:: Share this:
   :name: share-this
   :class: sd-title

.. raw:: html

   <div class="sd-content">

-  `Reddit <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/?share=email>`__
-  
-  

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="like-post-wrapper-3549518-3823-59ae3bab74a0d"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=3823&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-3823-59ae3bab74a0d"
   data-name="like-post-frame-3549518-3823-59ae3bab74a0d">

.. rubric:: Like this:
   :name: like-this
   :class: sd-title

.. raw:: html

   <div class="likes-widget-placeholder post-likes-widget-placeholder"
   style="height: 55px;">

Like Loading...

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="jp-relatedposts" class="jp-relatedposts">

.. rubric:: *Related*
   :name: related
   :class: jp-relatedposts-headline

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="post-info">

.. raw:: html

   </div>

.. raw:: html

   <div class="post-footer">

 

.. raw:: html

   </div>

.. raw:: html

   </div>

.. rubric:: 15 Responses to “Simple Algebraic Data Types”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-38781">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-38781">

   .. raw:: html

      <div class="comment-author vcard">

   `Products and Coproducts \|   Bartosz Milewski's Programming
   Cafe <https://bartoszmilewski.com/2015/01/07/products-and-coproducts/>`__
   Says:

   .. raw:: html

      </div>

   `January 13, 2015 at 5:57
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-38781>`__
   […] Next: Simple Algebraic Data Types. […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-38817">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-38817">

   .. raw:: html

      <div class="comment-author vcard">

   |image1| `david karapetyan <http://www.scriptcrafty.com>`__ Says:

   .. raw:: html

      </div>

   `January 14, 2015 at 1:41
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-38817>`__
   Really great post. Looking forward to more like this.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-39296">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-39296">

   .. raw:: html

      <div class="comment-author vcard">

   |image2| Peter Jankuliak Says:

   .. raw:: html

      </div>

   `January 18, 2015 at 1:47
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-39296>`__
   Very interesting stuff. One nit pick if I may, shouldn’t the the
   correspondence of the Either type in logic be the logical XOR as
   opposed to the logical OR? As far as I know, Either a b can never be
   both, a and b.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-39300">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-39300">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 18, 2015 at 3:07
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-39300>`__
   @Peter: I guess I have oversimplified it. Let me rephrase it: The
   type ``Either a b`` is *inhabited* if any of the two types a or b are
   inhabited. Which is also true if both are inhabited. The only
   uninhabited case is ``Either Void Void``, which corresponds to False
   \|\| False. I’ll edit the post accordingly.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-44412">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-44412">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| `Ollie Charles <http://ocharles.org.uk>`__ Says:

   .. raw:: html

      </div>

   `April 7, 2015 at 9:36
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-44412>`__
   This post mentions that coproducts in Set show that set is a
   symmetric monoidal category, but there is no more mention of what it
   means to be symmetric. I’d suggest that should either be: left out,
   explained, cross referenced to prior writing that I missed 🙂

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-45619">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45619">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| Hi-Angel Says:

   .. raw:: html

      </div>

   `May 3, 2015 at 9:43
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-45619>`__
   Does anybody have a solution to the idea to make an ADT in C++? The
   best I come with is having a base class that plays a role of type
   constructor, i.e. it have a constructor that calls a constructor of
   «derived» classes, and adds the created class to an internal union
   with pointers. The method «area()» in the exercise would then check
   which one member active in the union, and call their «area()».

   One can’t do area() to be a pure virtual, because then you couldn’t
   use a constructor of the «base» class. And no need for inheritance
   here, actually.

   And it is a horrible solution, at least because one could just use a
   «derived» class separately. Also, if data constructors signature the
   same *(i.e. it’s two constructors that takes exactly one integer)*,
   you probably need to add one more argument to find which one ought to
   construct the «type constructor».

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-51225">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-51225">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| R Schubert Says:

   .. raw:: html

      </div>

   `August 6, 2015 at 5:29
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-51225>`__
   Since C++11, std::string and other types with non-trivial special
   member functions are allowed in unions.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66278">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66278">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `Constantine
   Kharlamov <https://www.facebook.com/app_scoped_user_id/100002376773652/>`__
   Says:

   .. raw:: html

      </div>

   `July 29, 2016 at 8:09
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-66278>`__
   I’m struggle with one little thing. Suppose, I have a coproduct
   «Either a b». Thus I have two arrows, a “a → Either” and “b →
   Either”.

   So far so good, but the problem is that by definition of coproduct we
   should have at least one “NotACoproduct” type, with arrows a →
   NotACoproduct, b → NotACoproduct, and Either → NotACoproduct.

   What is this type NotACoproduct, and most importantly, why don’t you
   mention it? Could it be easily omitted?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66280">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66280">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 29, 2016 at 10:32
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-66280>`__
   Take, for instance, ``Either Int Bool``. Almost every type is
   not-a-coproduct of ``Int`` and ``Bool``. Try ``Bool`` for instance as
   a candidate with these two injections:

   ::

       i :: Int -> Bool
       i x = (x == 0)
       j :: Bool -> Bool
       j b = b

   You can check that the following is the unique refactoring:

   ::

       m :: Either Int Bool -> Bool
       m (Left x) = (x == 0)
       m (Right b) = b

   You see the pattern? You branch on the two possibilities and apply
   the corresponding injections.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66283">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66283">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| `Constantine
   Kharlamov <https://www.facebook.com/app_scoped_user_id/100002376773652/>`__
   Says:

   .. raw:: html

      </div>

   `July 29, 2016 at 2:22
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-66283>`__
   Ah, I see. Thank you.

   I’m wondering though: why is the requirement to have that second
   not-a-coproduct type? Does it have an actual reason, or it’s just
   happened to be like this? To me it seems like we could remove that
   requirement, and Either Int Bool still would have worked.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66284">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66284">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 29, 2016 at 2:37
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-66284>`__
   The requirement is not that a not-a-coproduct type must exist. The
   requirement is that for every such type (if it exists) there is a
   unique factoring through the actual coproduct. That’s the essence of
   every universal construction.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66327">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66327">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| `Josh
   Freckleton <https://www.facebook.com/app_scoped_user_id/1025422647495310/>`__
   Says:

   .. raw:: html

      </div>

   `August 3, 2016 at 9:29
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-66327>`__
   Thank you! Category Theory is pretty inaccessible for me as a
   software engineer without a solid math background, and your posts are
   incredibly indispensable to me! I’m working my way through them
   sequentially and I am excited about working through them all!

   Thank you!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70216">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70216">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| `Harmen <http://stoppels.blog>`__ Says:

   .. raw:: html

      </div>

   `April 24, 2017 at 4:03
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-70216>`__
   Currently C++17 offers a type-safe union / sum type with
   std::variant. For instance

   | variant<string, int> v(“abc”);
   | v = 12;

   will work just fine with the compiler validating whether the types
   are correct. And yes, variants are mutable if not const.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73952">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73952">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| Kevin Grandemange Says:

   .. raw:: html

      </div>

   `August 23, 2017 at 11:10
   am <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-73952>`__
   A family of classes with a common ancestor may be understood as one
   variant type, in which the vtable serves as a hidden tag. What in
   Haskell would be done by pattern matching on the constructor, and by
   calling specialized code, in C++ is accomplished by dispatching a
   call to a virtual function based on the vtable pointer

   It seems to me that these are very different from each other because
   pattern matching needs the real type at the call site to work but
   vtable have as you say a hidden tag which means the caller doesn’t
   need to know the tag to use the object. The decision was taken at
   initialization.

   I think in haskell, existentials are the nearest concept to this kind
   of construction.

   pattern matching would be more of a switch on steroid.

   | Am I missing something ?
   | Thanks,
   | Kevin

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73955">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73955">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `August 23, 2017 at 1:15
   pm <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/#comment-73955>`__
   In Haskell, pattern matching needs the type, but the variant
   information is not encoded in the type. It’s encoded in the hidden
   tag. In C++ the information is originally encoded in the type, but
   once you erase the actual type and cast the pointer to the base type,
   the information is stored in the hidden tag — the v-pointer.

   The theories behind sum types and subtype polymorphism may be
   different, but they serve similar purpose.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

.. raw:: html

   <div class="navigation">

.. raw:: html

   <div class="alignleft">

.. raw:: html

   </div>

.. raw:: html

   <div class="alignright">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="respond" class="comment-respond">

.. rubric:: Leave a Reply `Cancel
   reply </2015/01/13/simple-algebraic-data-types/#respond>`__
   :name: reply-title
   :class: comment-reply-title

.. raw:: html

   <div class="comment-form-field comment-textarea">

Enter your comment here...

.. raw:: html

   <div id="comment-form-comment">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-identity">

.. raw:: html

   <div id="comment-form-nascar">

Fill in your details below or click an icon to log in:

-  ` <#comment-form-guest>`__
-  ` <#comment-form-load-service:WordPress.com>`__
-  ` <#comment-form-load-service:Twitter>`__
-  ` <#comment-form-load-service:Facebook>`__
-  

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-guest" class="comment-form-service selected">

.. raw:: html

   <div class="comment-form-padder">

.. raw:: html

   <div class="comment-form-avatar">

|Gravatar|

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-fields">

.. raw:: html

   <div class="comment-form-field comment-form-email">

Email (required) (Address never made public)

.. raw:: html

   <div class="comment-form-input">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-field comment-form-author">

Name (required)

.. raw:: html

   <div class="comment-form-input">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-field comment-form-url">

Website

.. raw:: html

   <div class="comment-form-input">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-wordpress" class="comment-form-service">

.. raw:: html

   <div class="comment-form-padder">

.. raw:: html

   <div class="comment-form-avatar">

|WordPress.com Logo|

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-fields">

**** You are commenting using your WordPress.com account.
( `Log Out <javascript:HighlanderComments.doExternalLogout(%20'wordpress'%20);>`__ / `Change <#>`__ )

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-twitter" class="comment-form-service">

.. raw:: html

   <div class="comment-form-padder">

.. raw:: html

   <div class="comment-form-avatar">

|Twitter picture|

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-fields">

**** You are commenting using your Twitter account.
( `Log Out <javascript:HighlanderComments.doExternalLogout(%20'twitter'%20);>`__ / `Change <#>`__ )

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-facebook" class="comment-form-service">

.. raw:: html

   <div class="comment-form-padder">

.. raw:: html

   <div class="comment-form-avatar">

|Facebook photo|

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-fields">

**** You are commenting using your Facebook account.
( `Log Out <javascript:HighlanderComments.doExternalLogout(%20'facebook'%20);>`__ / `Change <#>`__ )

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-googleplus" class="comment-form-service">

.. raw:: html

   <div class="comment-form-padder">

.. raw:: html

   <div class="comment-form-avatar">

|Google+ photo|

.. raw:: html

   </div>

.. raw:: html

   <div class="comment-form-fields">

**** You are commenting using your Google+ account.
( `Log Out <javascript:HighlanderComments.doExternalLogout(%20'googleplus'%20);>`__ / `Change <#>`__ )

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-load-service" class="comment-form-service">

.. raw:: html

   <div class="comment-form-posting-as-cancel">

`Cancel <javascript:HighlanderComments.cancelExternalWindow();>`__

.. raw:: html

   </div>

Connecting to %s

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="comment-form-subscribe">

Notify me of new comments via email.

Notify me of new posts via email.

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div style="clear: both">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="sidebar">

.. rubric:: Archived Entry
   :name: archived-entry

-  **Post Date :**
-  January 13, 2015 at 5:48 pm
-  **Category :**
-  `C++ <https://bartoszmilewski.com/category/c/>`__, `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-5adacc327dd1070b25220480845e8f6d">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-3c6efeb2be5b40ff776d4ebe8306b0b0">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-0d4a7e6c8edd88e835508df8397592e5">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-39cdbe47fde550c8a47f365079f1d673">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-7be843a16f814f0d882f43eb7fffd402">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-e381b63eb80c65e825f702b9eb73361d">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-62288ac65c238d13d7273143d4442fcd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-bcbe533d714cbe1bba8cc0799b532a5c">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="carousel-reblog-box">

Post to

.. raw:: html

   <div class="submit">

`Cancel <#>`__

.. raw:: html

   </div>

.. raw:: html

   <div class="arrow">

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="sharing_email" style="display: none;">

Send to Email Address Your Name Your Email Address

.. raw:: html

   <div id="sharing_recaptcha" class="recaptcha">

.. raw:: html

   </div>

|loading| `Cancel <#cancel>`__

.. raw:: html

   <div class="errors errors-1" style="display: none;">

Post was not sent - check your email addresses!

.. raw:: html

   </div>

.. raw:: html

   <div class="errors errors-2" style="display: none;">

Email check failed, please try again

.. raw:: html

   </div>

.. raw:: html

   <div class="errors errors-3" style="display: none;">

Sorry, your blog cannot share posts by email.

.. raw:: html

   </div>

.. raw:: html

   </div>

.. raw:: html

   <div id="likes-other-gravatars">

.. raw:: html

   <div class="likes-text">

%d bloggers like this:

.. raw:: html

   </div>

.. raw:: html

   </div>

|image21|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Pair| image:: https://bartoszmilewski.files.wordpress.com/2015/01/pair.jpg?w=150&h=102
   :class: aligncenter size-thumbnail wp-image-3942
   :width: 150px
   :height: 102px
   :target: https://bartoszmilewski.files.wordpress.com/2015/01/pair.jpg
.. |image1| image:: https://2.gravatar.com/avatar/5adacc327dd1070b25220480845e8f6d?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image2| image:: https://0.gravatar.com/avatar/3c6efeb2be5b40ff776d4ebe8306b0b0?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image3| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://0.gravatar.com/avatar/0d4a7e6c8edd88e835508df8397592e5?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://0.gravatar.com/avatar/39cdbe47fde550c8a47f365079f1d673?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://1.gravatar.com/avatar/7be843a16f814f0d882f43eb7fffd402?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://i2.wp.com/graph.facebook.com/v2.2/100002376773652/picture?q=type%3Dlarge&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://i2.wp.com/graph.facebook.com/v2.2/100002376773652/picture?q=type%3Dlarge%26_md5%3Db542bec5f86cc8bbb91264a68f20b37c&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://i2.wp.com/graph.facebook.com/v2.2/1025422647495310/picture?q=type%3Dlarge%26_md5%3Db6cb0889e2a43794a96e44d5b8c8990d&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://0.gravatar.com/avatar/62288ac65c238d13d7273143d4442fcd?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://2.gravatar.com/avatar/bcbe533d714cbe1bba8cc0799b532a5c?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |Gravatar| image:: https://1.gravatar.com/avatar/ad516503a11cd5ca435acc9bb6523536?s=25
   :class: no-grav
   :width: 25px
   :target: https://gravatar.com/site/signup/
.. |WordPress.com Logo| image:: https://1.gravatar.com/avatar/ad516503a11cd5ca435acc9bb6523536?s=25
   :class: no-grav
   :width: 25px
.. |Twitter picture| image:: https://1.gravatar.com/avatar/ad516503a11cd5ca435acc9bb6523536?s=25
   :class: no-grav
   :width: 25px
.. |Facebook photo| image:: https://1.gravatar.com/avatar/ad516503a11cd5ca435acc9bb6523536?s=25
   :class: no-grav
   :width: 25px
.. |Google+ photo| image:: https://1.gravatar.com/avatar/ad516503a11cd5ca435acc9bb6523536?s=25
   :class: no-grav
   :width: 25px
.. |loading| image:: https://s2.wp.com/wp-content/mu-plugins/post-flair/sharing/images/loading.gif
   :class: loading
   :width: 16px
   :height: 16px
.. |image21| image:: https://pixel.wp.com/b.gif?v=noscript

