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
   class="post-3889 post type-post status-publish format-standard hentry category-category-theory category-haskell">

January 20, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Functors
   :name: functors
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__
`[26]
Comments <https://bartoszmilewski.com/2015/01/20/functors/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_3889" class="pd-rating">

.. raw:: html

   </div>

    This is part of Categories for Programmers. Previously: `Simple
    Algebraic Data
    Types <https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

At the risk of sounding like a broken record, I will say this about
functors: A functor is a very simple but powerful idea. Category theory
is just full of those simple but powerful ideas. A functor is a mapping
between categories. Given two categories, C and D, a functor F maps
objects in C to objects in D — it’s a function on objects. If *a* is an
object in C, we’ll write its image in D as *F a* (no parentheses). But a
category is not just objects — it’s objects and morphisms that connect
them. A functor also maps morphisms — it’s a function on morphisms. But
it doesn’t map morphisms willy-nilly — it preserves connections. So if a
morphism *f* in C connects object *a* to object *b*,

::

    f :: a -> b

the image of *f* in D, *F f*, will connect the image of *a* to the image
of *b*:

::

    F f :: F a -> F b

(This is a mixture of mathematical and Haskell notation that hopefully
makes sense by now. I won’t use parentheses when applying functors to
objects or morphisms.) |Functor| As you can see, a functor preserves the
structure of a category: what’s connected in one category will be
connected in the other category. But there’s something more to the
structure of a category: there’s also the composition of morphisms. If
*h* is a composition of *f* and *g*:

::

    h = g . f

we want its image under F to be a composition of the images of *f* and
*g*:

::

    F h = F g . F f

|FunctorCompos| Finally, we want all identity morphisms in C to be
mapped to identity morphisms in D:

::

    F ida = idF a

Here, *id\ :sub:`a`* is the identity at the object *a*, and *id\ :sub:`F
a`* the identity at *F a*. |FunctorId| Note that these conditions make
functors much more restrictive than regular functions. Functors must
preserve the structure of a category. If you picture a category as a
collection of objects held together by a network of morphisms, a functor
is not allowed to introduce any tears into this fabric. It may smash
objects together, it may glue multiple morphisms into one, but it may
never break things apart. This no-tearing constraint is similar to the
continuity condition you might know from calculus. In this sense
functors are “continuous” (although there exists an even more
restrictive notion of continuity for functors). Just like functions,
functors may do both collapsing and embedding. The embedding aspect is
more prominent when the source category is much smaller than the target
category. In the extreme, the source can be the trivial singleton
category — a category with one object and one morphism (the identity). A
functor from the singleton category to any other category simply selects
an object in that category. This is fully analogous to the property of
morphisms from singleton sets selecting elements in target sets. The
maximally collapsing functor is called the constant functor Δ\ :sub:`c`.
It maps every object in the source category to one selected object *c*
in the target category. It also maps every morphism in the source
category to the identity morphism *id\ :sub:`c`*. It acts like a black
hole, compacting everything into one singularity. We’ll see more of this
functor when we discuss limits and colimits.

.. rubric:: Functors in Programming
   :name: functors-in-programming

Let’s get down to earth and talk about programming. We have our category
of types and functions. We can talk about functors that map this
category into itself — such functors are called endofunctors. So what’s
an endofunctor in the category of types? First of all, it maps types to
types. We’ve seen examples of such mappings, maybe without realizing
that they were just that. I’m talking about definitions of types that
were parameterized by other types. Let’s see a few examples.

.. rubric:: The Maybe Functor
   :name: the-maybe-functor

The definition of ``Maybe`` is a mapping from type ``a`` to type
``Maybe a``:

::

    data Maybe a = Nothing | Just a

Here’s an important subtlety: ``Maybe`` itself is not a type, it’s a
*type constructor*. You have to give it a type argument, like ``Int`` or
``Bool``, in order to turn it into a type. ``Maybe`` without any
argument represents a function on types. But can we turn ``Maybe`` into
a functor? (From now on, when I speak of functors in the context of
programming, I will almost always mean endofunctors.) A functor is not
only a mapping of objects (here, types) but also a mapping of morphisms
(here, functions). For any function from ``a`` to ``b``:

::

    f :: a -> b

we would like to produce a function from ``Maybe a`` to ``Maybe b``. To
define such a function, we’ll have two cases to consider, corresponding
to the two constructors of ``Maybe``. The ``Nothing`` case is simple:
we’ll just return ``Nothing`` back. And if the argument is ``Just``,
we’ll apply the function ``f`` to its contents. So the image of ``f``
under ``Maybe`` is the function:

::

    f’ :: Maybe a -> Maybe b
    f’ Nothing = Nothing
    f’ (Just x) = Just (f x)

(By the way, in Haskell you can use apostrophes in variables names,
which is very handy in cases like these.) In Haskell, we implement the
morphism-mapping part of a functor as a higher order function called
``fmap``. In the case of ``Maybe``, it has the following signature:

::

    fmap :: (a -> b) -> (Maybe a -> Maybe b)

|FunctorMaybe| We often say that ``fmap`` *lifts* a function. The lifted
function acts on ``Maybe`` values. As usual, because of currying, this
signature may be interpreted in two ways: as a function of one argument
— which itself is a function ``(a->b)`` — returning a function
``(Maybe a -> Maybe b)``; or as a function of two arguments returning
``Maybe b``:

::

    fmap :: (a -> b) -> Maybe a -> Maybe b

Based on our previous discussion, this is how we implement ``fmap`` for
``Maybe``:

::

    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

To show that the type constructor ``Maybe`` together with the function
``fmap`` form a functor, we have to prove that ``fmap`` preserves
identity and composition. These are called “the functor laws,” but they
simply ensure the preservation of the structure of the category.

.. rubric:: Equational Reasoning
   :name: equational-reasoning

To prove the functor laws, I will use *equational reasoning*, which is a
common proof technique in Haskell. It takes advantage of the fact that
Haskell functions are defined as equalities: the left hand side equals
the right hand side. You can always substitute one for another, possibly
renaming variables to avoid name conflicts. Think of this as either
inlining a function, or the other way around, refactoring an expression
into a function. Let’s take the identity function as an example:

::

    id x = x

If you see, for instance, ``id y`` in some expression, you can replace
it with ``y`` (inlining). Further, if you see ``id`` applied to an
expression, say ``id (y + 2)``, you can replace it with the expression
itself ``(y + 2)``. And this substitution works both ways: you can
replace any expression ``e`` with ``id e`` (refactoring). If a function
is defined by pattern matching, you can use each sub-definition
independently. For instance, given the above definition of ``fmap`` you
can replace ``fmap f Nothing`` with ``Nothing``, or the other way
around. Let’s see how this works in practice. Let’s start with the
preservation of identity:

::

    fmap id = id

There are two cases to consider: ``Nothing`` and ``Just``. Here’s the
first case (I’m using Haskell pseudo-code to transform the left hand
side to the right hand side):

::

      fmap id Nothing 
    = { definition of fmap }
      Nothing 
    = { definition of id }
      id Nothing

Notice that in the last step I used the definition of ``id`` backwards.
I replaced the expression ``Nothing`` with ``id Nothing``. In practice,
you carry out such proofs by “burning the candle at both ends,” until
you hit the same expression in the middle — here it was ``Nothing``. The
second case is also easy:

::

      fmap id (Just x) 
    = { definition of fmap }
      Just (id x) 
    = { definition of id }
      Just x
    = { definition of id }
      id (Just x)

Now, lets show that ``fmap`` preserves composition:

::

    fmap (g . f) = fmap g . fmap f

First the ``Nothing`` case:

::

      fmap (g . f) Nothing 
    = { definition of fmap }
      Nothing 
    = { definition of fmap }
      fmap g Nothing
    = { definition of fmap }
      fmap g (fmap f Nothing)

And then the ``Just`` case:

::

      fmap (g . f) (Just x)
    = { definition of fmap }
      Just ((g . f) x)
    = { definition of composition }
      Just (g (f x))
    = { definition of fmap }
      fmap g (Just (f x))
    = { definition of fmap }
      fmap g (fmap f (Just x))
    = { definition of composition }
      (fmap g . fmap f) (Just x)

It’s worth stressing that equational reasoning doesn’t work for C++
style “functions” with side effects. Consider this code:

::

    int square(int x) {
        return x * x;
    }

    int counter() {
        static int c = 0;
        return c++;
    }

    double y = square(counter());

Using equational reasoning, you would be able to inline ``square`` to
get:

::

    double y = counter() * counter();

This is definitely not a valid transformation, and it will not produce
the same result. Despite that, the C++ compiler will try to use
equational reasoning if you implement ``square`` as a macro, with
disastrous results.

.. rubric:: Optional
   :name: optional

Functors are easily expressed in Haskell, but they can be defined in any
language that supports generic programming and higher-order functions.
Let’s consider the C++ analog of ``Maybe``, the template type
``optional``. Here’s a sketch of the implementation (the actual
implementation is much more complex, dealing with various ways the
argument may be passed, with copy semantics, and with the resource
management issues characteristic of C++):

::

    template<class T>
    class optional {
        bool _isValid; // the tag
        T    _v;
    public:
        optional()    : _isValid(false) {}         // Nothing
        optional(T x) : _isValid(true) , _v(x) {}  // Just
        bool isValid() const { return _isValid; }
        T val() const { return _v; }
    };

This template provides one part of the definition of a functor: the
mapping of types. It maps any type ``T`` to a new type ``optional<T>``.
Let’s define its action on functions:

::

    template<class A, class B>
    std::function<optional<B>(optional<A>)> 
    fmap(std::function<B(A)> f) 
    {
        return [f](optional<A> opt) {
            if (!opt.isValid())
                return optional<B>{};
            else
                return optional<B>{ f(opt.val()) };
        };
    }

This is a higher order function, taking a function as an argument and
returning a function. Here’s the uncurried version of it:

::

    template<class A, class B>
    optional<B> fmap(std::function<B(A)> f, optional<A> opt) {
        if (!opt.isValid())
            return optional<B>{};
        else
            return optional<B>{ f(opt.val()) };
    }

There is also an option of making ``fmap`` a template method of
``optional``. This embarrassment of choices makes abstracting the
functor pattern in C++ a problem. Should functor be an interface to
inherit from (unfortunately, you can’t have template virtual functions)?
Should it be a curried or an uncurried free template function? Can the
C++ compiler correctly infer the missing types, or should they be
specified explicitly? Consider a situation where the input function
``f`` takes an ``int`` to a ``bool``. How will the compiler figure out
the type of ``g``:

::

    auto g = fmap(f);

especially if, in the future, there are multiple functors overloading
``fmap``? (We’ll see more functors soon.)

.. rubric:: Typeclasses
   :name: typeclasses

So how does Haskell deal with abstracting the functor? It uses the
typeclass mechanism. A typeclass defines a family of types that support
a common interface. For instance, the class of objects that support
equality is defined as follows:

::

    class Eq a where
        (==) :: a -> a -> Bool

This definition states that type ``a`` is of the class ``Eq`` if it
supports the operator ``(==)`` that takes two arguments of type ``a``
and returns a ``Bool``. If you want to tell Haskell that a particular
type is ``Eq``, you have to declare it an *instance* of this class and
provide the implementation of ``(==)``. For example, given the
definition of a 2D ``Point`` (a product type of two ``Float``\ s):

::

    data Point = Pt Float Float

you can define the equality of points:

::

    instance Eq Point where
        (Pt x y) == (Pt x' y') = x == x' && y == y'

Here I used the operator ``(==)`` (the one I’m defining) in the infix
position between the two patterns ``(Pt x y)`` and ``(Pt x' y')``. The
body of the function follows the single equal sign. Once ``Point`` is
declared an instance of ``Eq``, you can directly compare points for
equality. Notice that, unlike in C++ or Java, you don’t have to specify
the ``Eq`` class (or interface) when defining ``Point`` — you can do it
later in client code. Typeclasses are also Haskell’s only mechanism for
overloading functions (and operators). We will need that for overloading
``fmap`` for different functors. There is one complication, though: a
functor is not defined as a type but as a mapping of types, a type
constructor. We need a typeclass that’s not a family of types, as was
the case with ``Eq``, but a family of type constructors. Fortunately a
Haskell typeclass works with type constructors as well as with types. So
here’s the definition of the ``Functor`` class:

::

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

It stipulates that ``f`` is a ``Functor`` if there exists a function
``fmap`` with the specified type signature. The lowercase ``f`` is a
type variable, similar to type variables ``a`` and ``b``. The compiler,
however, is able to deduce that it represents a type constructor rather
than a type by looking at its usage: acting on other types, as in
``f a`` and ``f b``. Accordingly, when declaring an instance of
``Functor``, you have to give it a type constructor, as is the case with
``Maybe``:

::

    instance Functor Maybe where
        fmap _ Nothing = Nothing
        fmap f (Just x) = Just (f x)

By the way, the ``Functor`` class, as well as its instance definitions
for a lot of simple data types, including ``Maybe``, are part of the
standard Prelude library.

.. rubric:: Functor in C++
   :name: functor-in-c

Can we try the same approach in C++? A type constructor corresponds to a
template class, like ``optional``, so by analogy, we would parameterize
``fmap`` with a *template template parameter* ``F``. This is the syntax
for it:

::

    template<template<class> F, class A, class B>
    F<B> fmap(std::function<B(A)>, F<A>);

We would like to be able to specialize this template for different
functors. Unfortunately, there is a prohibition against partial
specialization of template functions in C++. You can’t write:

::

    template<class A, class B>
    optional<B> fmap<optional>(std::function<B(A)> f, optional<A> opt)

Instead, we have to fall back on function overloading, which brings us
back to the original definition of the uncurried ``fmap``:

::

    template<class A, class B>
    optional<B> fmap(std::function<B(A)> f, optional<A> opt) 
    {
        if (!opt.isValid())
            return optional<B>{};
        else
            return optional<B>{ f(opt.val()) };
    }

This definition works, but only because the second argument of ``fmap``
selects the overload. It totally ignores the more generic definition of
``fmap``.

.. rubric:: The List Functor
   :name: the-list-functor

To get some intuition as to the role of functors in programming, we need
to look at more examples. Any type that is parameterized by another type
is a candidate for a functor. Generic containers are parameterized by
the type of the elements they store, so let’s look at a very simple
container, the list:

::

    data List a = Nil | Cons a (List a)

We have the type constructor ``List``, which is a mapping from any type
``a`` to the type ``List a``. To show that ``List`` is a functor we have
to define the lifting of functions: Given a function ``a->b`` define a
function ``List a -> List b``:

::

    fmap :: (a -> b) -> (List a -> List b)

A function acting on ``List a`` must consider two cases corresponding to
the two list constructors. The ``Nil`` case is trivial — just return
``Nil`` — there isn’t much you can do with an empty list. The ``Cons``
case is a bit tricky, because it involves recursion. So let’s step back
for a moment and consider what we are trying to do. We have a list of
``a``, a function ``f`` that turns ``a`` to ``b``, and we want to
generate a list of ``b``. The obvious thing is to use ``f`` to turn each
element of the list from ``a`` to ``b``. How do we do this in practice,
given that a (non-empty) list is defined as the ``Cons`` of a head and a
tail? We apply ``f`` to the head and apply the lifted (``fmap``\ ped)
``f`` to the tail. This is a recursive definition, because we are
defining lifted ``f`` in terms of lifted ``f``:

::

    fmap f (Cons x t) = Cons (f x) (fmap f t)

Notice that, on the right hand side, ``fmap f`` is applied to a list
that’s shorter than the list for which we are defining it — it’s applied
to its tail. We recurse towards shorter and shorter lists, so we are
bound to eventually reach the empty list, or ``Nil``. But as we’ve
decided earlier, ``fmap f`` acting on ``Nil`` returns ``Nil``, thus
terminating the recursion. To get the final result, we combine the new
head ``(f x)`` with the new tail ``(fmap f t)`` using the ``Cons``
constructor. Putting it all together, here’s the instance declaration
for the list functor:

::

    instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons x t) = Cons (f x) (fmap f t)

If you are more comfortable with C++, consider the case of a
``std::vector``, which could be considered the most generic C++
container. The implementation of ``fmap`` for ``std::vector`` is just a
thin encapsulation of ``std::transform``:

::

    template<class A, class B>
    std::vector<B> fmap(std::function<B(A)> f, std::vector<A> v)
    {
        std::vector<B> w;
        std::transform( std::begin(v)
                      , std::end(v)
                      , std::back_inserter(w)
                      , f);
        return w;
    }

We can use it, for instance, to square the elements of a sequence of
numbers:

::

    std::vector<int> v{ 1, 2, 3, 4 };
    auto w = fmap([](int i) { return i*i; }, v);
    std::copy( std::begin(w)
             , std::end(w)
             , std::ostream_iterator(std::cout, ", "));

Most C++ containers are functors by virtue of implementing iterators
that can be passed to ``std::transform``, which is the more primitive
cousin of ``fmap``. Unfortunately, the simplicity of a functor is lost
under the usual clutter of iterators and temporaries (see the
implementation of ``fmap`` above). I’m happy to say that the new
proposed C++ range library makes the functorial nature of ranges much
more pronounced.

.. rubric:: The Reader Functor
   :name: the-reader-functor

Now that you might have developed some intuitions — for instance,
functors being some kind of containers — let me show you an example
which at first sight looks very different. Consider a mapping of type
``a`` to the type of a function returning ``a``. We haven’t really
talked about function types in depth — the full categorical treatment is
coming — but we have some understanding of those as programmers. In
Haskell, a function type is constructed using the arrow type constructor
``(->)`` which takes two types: the argument type and the result type.
You’ve already seen it in infix form, ``a->b``, but it can equally well
be used in prefix form, when parenthesized:

::

    (->) a b

Just like with regular functions, type functions of more than one
argument can be partially applied. So when we provide just one type
argument to the arrow, it still expects another one. That’s why:

::

    (->) a

is a type constructor. It needs one more type ``b`` to produce a
complete type ``a->b``. As it stands, it defines a whole family of type
constructors parameterized by ``a``. Let’s see if this is also a family
of functors. Dealing with two type parameters can get a bit confusing,
so let’s do some renaming. Let’s call the argument type ``r`` and the
result type ``a``, in line with our previous functor definitions. So our
type constructor takes any type ``a`` and maps it into the type
``r->a``. To show that it’s a functor, we want to lift a function
``a->b`` to a function that takes ``r->a`` and returns ``r->b``. These
are the types that are formed using the type constructor ``(->) r``
acting on, respectively, ``a`` and ``b``. Here’s the type signature of
``fmap`` applied to this case:

::

    fmap :: (a -> b) -> (r -> a) -> (r -> b)

We have to solve the following puzzle: given a function ``f::a->b`` and
a function ``g::r->a``, create a function ``r->b``. There is only one
way we can compose the two functions, and the result is exactly what we
need. So here’s the implementation of our ``fmap``:

::

    instance Functor ((->) r) where
        fmap f g = f . g

It just works! If you like terse notation, this definition can be
reduced further by noticing that composition can be rewritten in prefix
form:

::

    fmap f g = (.) f g

and the arguments can be omitted to yield a direct equality of two
functions:

::

    fmap = (.)

This combination of the type constructor ``(->) r`` with the above
implementation of ``fmap`` is called the reader functor.

.. rubric:: Functors as Containers
   :name: functors-as-containers

We’ve seen some examples of functors in programming languages that
define general-purpose containers, or at least objects that contain some
value of the type they are parameterized over. The reader functor seems
to be an outlier, because we don’t think of functions as data. But we’ve
seen that pure functions can be memoized, and function execution can be
turned into table lookup. Tables are data. Conversely, because of
Haskell’s laziness, a traditional container, like a list, may actually
be implemented as a function. Consider, for instance, an infinite list
of natural numbers, which can be compactly defined as:

::

    nats :: [Integer]
    nats = [1..]

In the first line, a pair of square brackets is the Haskell’s built-in
type constructor for lists. In the second line, square brackets are used
to create a list literal. Obviously, an infinite list like this cannot
be stored in memory. The compiler implements it as a function that
generates ``Integer``\ s on demand. Haskell effectively blurs the
distinction between data and code. A list could be considered a
function, and a function could be considered a table that maps arguments
to results. The latter can even be practical if the domain of the
function is finite and not too large. It would not be practical,
however, to implement ``strlen`` as table lookup, because there are
infinitely many different strings. As programmers, we don’t like
infinities, but in category theory you learn to eat infinities for
breakfast. Whether it’s a set of all strings or a collection of all
possible states of the Universe, past, present, and future — we can deal
with it! So I like to think of the functor object (an object of the type
generated by an endofunctor) as containing a value or values of the type
over which it is parameterized, even if these values are not physically
present there. One example of a functor is a C++ ``std::future``, which
may at some point contain a value, but it’s not guaranteed it will; and
if you want to access it, you may block waiting for another thread to
finish execution. Another example is a Haskell ``IO`` object, which may
contain user input, or the future versions of our Universe with “Hello
World!” displayed on the monitor. According to this interpretation, a
functor object is something that may contain a value or values of the
type it’s parameterized upon. Or it may contain a recipe for generating
those values. We are not at all concerned about being able to access the
values — that’s totally optional, and outside of the scope of the
functor. All we are interested in is to be able to manipulate those
values using functions. If the values can be accessed, then we should be
able to see the results of this manipulation. If they can’t, then all we
care about is that the manipulations compose correctly and that the
manipulation with an identity function doesn’t change anything. Just to
show you how much we don’t care about being able to access the values
inside a functor object, here’s a type constructor that ignores
completely its argument ``a``:

::

    data Const c a = Const c

The ``Const`` type constructor takes two types, ``c`` and ``a``. Just
like we did with the arrow constructor, we are going to partially apply
it to create a functor. The data constructor (also called ``Const``)
takes just one value of type ``c``. It has no dependence on ``a``. The
type of ``fmap`` for this type constructor is:

::

    fmap :: (a -> b) -> Const c a -> Const c b

Because the functor ignores its type argument, the implementation of
``fmap`` is free to ignore its function argument — the function has
nothing to act upon:

::

    instance Functor (Const c) where
        fmap _ (Const v) = Const v

This might be a little clearer in C++ (I never thought I would utter
those words!), where there is a stronger distinction between type
arguments — which are compile-time — and values, which are run-time:

::

    template<class C, class A>
    struct Const {
        Const(C v) : _v(v) {}
        C _v;
    };

The C++ implementation of ``fmap`` also ignores the function argument
and essentially re-casts the ``Const`` argument without changing its
value:

::

    template<class C, class A, class B>
    Const<C, B> fmap(std::function<B(A)> f, Const<C, A> c) {
        return Const<C, B>{c._v};
    }

Despite its weirdness, the ``Const`` functor plays an important role in
many constructions. In category theory, it’s a special case of the
Δ\ :sub:`c` functor I mentioned earlier — the endo-functor case of a
black hole. We’ll be seeing more of it it in the future.

.. rubric:: Functor Composition
   :name: functor-composition

It’s not hard to convince yourself that functors between categories
compose, just like functions between sets compose. A composition of two
functors, when acting on objects, is just the composition of their
respective object mappings; and similarly when acting on morphisms.
After jumping through two functors, identity morphisms end up as
identity morphisms, and compositions of morphisms finish up as
compositions of morphisms. There’s really nothing much to it. In
particular, it’s easy to compose endofunctors. Remember the function
``maybeTail``? I’ll rewrite it using the Haskell’s built in
implementation of lists:

::

    maybeTail :: [a] -> Maybe [a]
    maybeTail [] = Nothing
    maybeTail (x:xs) = Just xs

(The empty list constructor that we used to call ``Nil`` is replaced
with the empty pair of square brackets ``[]``. The ``Cons`` constructor
is replaced with the infix operator ``:`` (colon).) The result of
``maybeTail`` is of a type that’s a composition of two functors,
``Maybe`` and ``[]``, acting on ``a``. Each of these functors is
equipped with its own version of ``fmap``, but what if we want to apply
some function ``f`` to the contents of the composite: a ``Maybe`` list?
We have to break through two layers of functors. We can use ``fmap`` to
break through the outer ``Maybe``. But we can’t just send ``f`` inside
``Maybe`` because ``f`` doesn’t work on lists. We have to send
``(fmap f)`` to operate on the inner list. For instance, let’s see how
we can square the elements of a ``Maybe`` list of integers:

::

    square x = x * x

    mis :: Maybe [Int]
    mis = Just [1, 2, 3]

    mis2 = fmap (fmap square) mis

The compiler, after analyzing the types, will figure out that, for the
outer ``fmap``, it should use the implementation from the ``Maybe``
instance, and for the inner one, the list functor implementation. It may
not be immediately obvious that the above code may be rewritten as:

::

    mis2 = (fmap . fmap) square mis

But remember that ``fmap`` may be considered a function of just one
argument:

::

    fmap :: (a -> b) -> (f a -> f b)

In our case, the second ``fmap`` in ``(fmap . fmap)`` takes as its
argument:

::

    square :: Int -> Int

and returns a function of the type:

::

    [Int] -> [Int]

The first ``fmap`` then takes that function and returns a function:

::

    Maybe [Int] -> Maybe [Int]

Finally, that function is applied to ``mis``. So the composition of two
functors is a functor whose ``fmap`` is the composition of the
corresponding ``fmap``\ s. Going back to category theory: It’s pretty
obvious that functor composition is associative (the mapping of objects
is associative, and the mapping of morphisms is associative). And there
is also a trivial identity functor in every category: it maps every
object to itself, and every morphism to itself. So functors have all the
same properties as morphisms in some category. But what category would
that be? It would have to be a category in which objects are categories
and morphisms are functors. It’s a category of categories. But a
category of *all* categories would have to include itself, and we would
get into the same kinds of paradoxes that made the set of all sets
impossible. There is, however, a category of all *small* categories
called **Cat** (which is big, so it can’t be a member of itself). A
small category is one in which objects form a set, as opposed to
something larger than a set. Mind you, in category theory, even an
infinite uncountable set is considered “small.” I thought I’d mention
these things because I find it pretty amazing that we can recognize the
same structures repeating themselves at many levels of abstraction.
We’ll see later that functors form categories as well.

.. rubric:: Challenges
   :name: challenges

#. Can we turn the ``Maybe`` type constructor into a functor by
   defining:

   ::

       fmap _ _ = Nothing

   which ignores both of its arguments? (Hint: Check the functor laws.)

#. Prove functor laws for the reader functor. Hint: it’s really simple.
#. Implement the reader functor in your second favorite language (the
   first being Haskell, of course).
#. Prove the functor laws for the list functor. Assume that the laws are
   true for the tail part of the list you’re applying it to (in other
   words, use *induction*).

.. rubric:: Acknowledgments
   :name: acknowledgments

Gershom Bazerman is kind enough to keep reviewing these posts. I’m
grateful for his patience and insight.

*Next:
`Functoriality <https://bartoszmilewski.com/2015/02/03/functoriality/>`__*

`Follow @BartoszMilewski <https://twitter.com/BartoszMilewski>`__

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

   <div id="crt-1260507762" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-2117006534" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/01/20/functors/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/01/20/functors/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/01/20/functors/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/01/20/functors/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/01/20/functors/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/01/20/functors/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/01/20/functors/?share=email>`__
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

   <div id="like-post-wrapper-3549518-3889-59ae3bc12bea1"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=3889&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-3889-59ae3bc12bea1"
   data-name="like-post-frame-3549518-3889-59ae3bc12bea1">

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

.. rubric:: 26 Responses to “Functors”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-39710">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-39710">

   .. raw:: html

      <div class="comment-author vcard">

   `Непросмотренные ссылки – 11 \| Откомпилируй
   Это <http://compileit.ru/?p=348>`__ Says:

   .. raw:: html

      </div>

   `January 22, 2015 at 8:16
   am <https://bartoszmilewski.com/2015/01/20/functors/#comment-39710>`__
   […] Functors […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40035">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40035">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| Philip Craig Says:

   .. raw:: html

      </div>

   `January 25, 2015 at 12:21
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40035>`__
   Thanks so much for all these posts

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40425">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40425">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| Nikolay Says:

   .. raw:: html

      </div>

   `January 30, 2015 at 6:33
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40425>`__
   | How cool is it! I was a bit confused though in the beginning with
     “Given two categories, C and D, a functor F maps objects … A
     functor also maps morphisms — it’s a function on morphisms.”
   | To me, what functor does sounds roughly like “it defines a category
     on objects of D with morphisms from C”. Or, otherwise, D is assumed
     to be isomorphic to C.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40428">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40428">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 30, 2015 at 7:05
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40428>`__
   @Nikolai: An isomorphism is invertible, a general functor isn’t. It
   can map multiple objects to one object and multiple morphisms to one
   morphism. You can’t invert such a mapping. A collapsing or an
   embedding mapping is not invertible.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40430">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40430">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| Nikolay Says:

   .. raw:: html

      </div>

   `January 30, 2015 at 7:54
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40430>`__
   Yes, you’re right of course. Isomorphism is a too strict statement
   here. What I was trying to say is that not any two categories C and D
   can be mapped with a functor, for example, F C can contain morphisms
   that do not exist in D, right?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40434">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40434">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 30, 2015 at 9:12
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40434>`__
   Actually, any two non-empty categories can be mapped using the
   ``Const`` functor.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40627">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40627">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| Nikolay Says:

   .. raw:: html

      </div>

   `February 1, 2015 at 7:59
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40627>`__
   So in the sense that for any C, D there exists F such that F C is a
   “subset” of D? I I understand that. I guess, I was missing a bit more
   strictness in the beginning. I misinterpreted the mapping of
   morphisms as a necessary condition of F being a functor: “for
   categories C, D and function on objects F: ob(C) -> ob(D) => F is a
   functor (=maps morphisms)”, whereas it’s a a part of functor
   definition

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-40932">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-40932">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `February 2, 2015 at 10:29
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-40932>`__
   I’m not sure what the distinction between “necessary condition” and
   “part of definition.” A functor is a mapping of objects *and*
   morphisms. Every object and every morphism from C must be mapped. But
   it doesn’t matter whether all objects and morphism in D are covered
   or not. It’s the same situation as with functions: they don’t have to
   be “onto” (surjective) or injective (have a look at the last section
   of Products and Coproducts).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-41175">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-41175">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| Nikolay Says:

   .. raw:: html

      </div>

   `February 4, 2015 at 5:35
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-41175>`__
   | After I’ve finished reading it’s pretty clear from the text what a
     functor is, so it probably makes no sense to explain now what I was
     confused with after the first paragraph.
   | Maybe I can formulate it as “a more formal definition sometimes
     would be nice to have, in my opinion”. It helps to get a feeling
     what you’re talking about in what follows.
   | Anyway, your posts are just enjoyable. Thanks for that.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-42056">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-42056">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| `Legogris (@Legogris) <http://twitter.com/Legogris>`__
   Says:

   .. raw:: html

      </div>

   `February 16, 2015 at 3:57
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-42056>`__
   I am taking a course in category theory and if I have any regrets
   right now it would be not finding your blog earlier. If I am able to
   pass the final report it will be hugely thanks to these and your
   older posts – the Haskell connection is making everything so much
   easier to understand. Have you considered attaching a Bitcoin address
   or something for people like me who want to donate as a thank you for
   the work so far?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-43429">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-43429">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| Randall Says:

   .. raw:: html

      </div>

   `March 18, 2015 at 10:41
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-43429>`__
   On the remark that the C++ version of the Const functor is perhaps
   simpler: would it maybe become more complicated if \_v was declared
   const, and c was passed to fmap by const reference, rather than by
   copy (and perhaps other stuff like that that one has to worry about
   that starts to clutter declarations)?

   I ended up with

   ::

       template<class C, class A>
       struct Const {
         Const(const C &v) : _v(v) {}
         const C _v;
       };

       template<class A, class B, class C>
       Const<C, B> fmap(const std::function<B(A)>&, const Const<C, A> &c) {
         return Const<C, B>{c._v};
       }

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-45602">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45602">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| Robin Trew Says:

   .. raw:: html

      </div>

   `May 2, 2015 at 5:46
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-45602>`__
   Excellent work – thank you.

   ( FWIW There seems to be a slight style bleed from the code style
   into the discursive text (reducing the legibility) from “This is a
   higher order function” onwards. )

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-45611">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45611">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| Leno Says:

   .. raw:: html

      </div>

   `May 3, 2015 at 12:59
   am <https://bartoszmilewski.com/2015/01/20/functors/#comment-45611>`__
   I’ve chuckled a bit on this one: “Let’s call the argument type r and
   the result type a”. Wouldn’t it be cleared to the reader to name the
   argument type “a”, and the result type “r”? You know, just use the
   initial letter of each rather than using the initial letter of the
   *other one* :).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50274">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50274">

   .. raw:: html

      <div class="comment-author vcard">

   |image16| jjj Says:

   .. raw:: html

      </div>

   `July 18, 2015 at 5:48
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-50274>`__
   First, Bartosz, let me extend a great thanks for your truly excellent
   tutorials. I have read a good number of articles and texts, but your
   series draws connections between category theory and computer science
   better than any other.

   There is a proof above showing the preservation of identity.

   ::

       fmap id = id

   The ``just x`` case seems to end prematurely. Forgive me if I am
   picking a nit, but this is often where I get stuck. Specifically, on
   what constitutes an adequate proof (honestly I often have difficulty
   identifying what we are even trying to prove when it comes to
   category theory).

   ::

       fmap id (Just x)
       = { def fmap}
       Just (id x)
       = { def id }
       Just x
       = { def id } // I think this last step is needed
       id (Just x)

   I hate to think of the last step (the one I added) as ‘obvious’ since
   many steps are seemingly obvious and it is the correct sequence of
   seemingly obvious steps that makes it a proof. On the other hand, the
   step I added may not be needed, but then I am confused about how the
   result (without the extra step) actually proves that
   ``fmap id = id``.

   Thank you again for the excellent tutorials.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50276">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50276">

   .. raw:: html

      <div class="comment-author vcard">

   |image17| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 18, 2015 at 7:03
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-50276>`__
   @jjj: You’re right. Since I did this last step explicitly in the
   ``Nothing`` case, there is no good justification to skip it in the
   ``Just`` case. I fixed it. Thanks for paying attention.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-57374">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-57374">

   .. raw:: html

      <div class="comment-author vcard">

   `Containers (not ioc) \| cannibal
   coder <https://cannibalcoder.wordpress.com/2015/11/18/containers-not-ioc/>`__
   Says:

   .. raw:: html

      </div>

   `November 18, 2015 at 1:23
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-57374>`__
   […] object ( here a value wrapped in a function ) that can be mapped
   to another object of the same type paraphrased from here.  it is
   typically shown […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60121">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60121">

   .. raw:: html

      <div class="comment-author vcard">

   |image18| `gregnwosu <http://gregnwosu.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `January 8, 2016 at 8:48
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-60121>`__
   | not sure whether my inductive reasoning is correct , can you check?
   | fmap (g.f) Nothing = Nothing
   | (fmap g) . (fmap f) $ Nothing =Nothing

   | fmap (g.f) (x:xs) = (g.f) x: fmap (g.f) xs
   | (fmap g) . (fmap f) = fmap g (fmap f x:xs) = fmap g (f x: fmap xs)
     = g ( f x) : (fmap g(fmap f xs)) = (g.f)x : tail which has been
     proved by induction?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60248">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60248">

   .. raw:: html

      <div class="comment-author vcard">

   |image19| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 10, 2016 at 3:37
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-60248>`__
   The relevant observation is that, when you’re checking composition
   for ``x:xs``, you can take as granted the inductive hypothesis for
   the tail:

   ::

       fmap (g . f) xs = (fmap g . fmap f) xs

   You also have to prove that composition works for an empty list.

   It also makes sense to use the equational reasoning style, where each
   step is explicitly justified by one of your assumptions.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-65949">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-65949">

   .. raw:: html

      <div class="comment-author vcard">

   |image20| Alexey Birukov Says:

   .. raw:: html

      </div>

   `July 7, 2016 at 12:42
   am <https://bartoszmilewski.com/2015/01/20/functors/#comment-65949>`__
   Have made russian translation to this part:
   https://habrahabr.ru/post/305018/

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66984">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66984">

   .. raw:: html

      <div class="comment-author vcard">

   |image21| `Kevin
   Zeidler <https://www.facebook.com/app_scoped_user_id/10105738640530183/>`__
   Says:

   .. raw:: html

      </div>

   `September 24, 2016 at 3:18
   am <https://bartoszmilewski.com/2015/01/20/functors/#comment-66984>`__
   Outstanding stuff, Bartosz. I was just talking with someone the other
   day, opining about the lack of a truly accessible introduction to
   category theory (a la Linear Algebra Done Right, or Spivak’s
   Calculus). Yours is the best I’ve found so far. Thanks!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67548">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67548">

   .. raw:: html

      <div class="comment-author vcard">

   `Improving Java Optional – Think green, think
   functional <https://viniciusluisr.wordpress.com/2016/11/09/improving-java-optional/>`__
   Says:

   .. raw:: html

      </div>

   `November 8, 2016 at 6:58
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-67548>`__
   […] is the monadic context wrapped object type” that unfortunately
   are terminal operations “functors can solve this problem, and I will
   make a post about it […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67567">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67567">

   .. raw:: html

      <div class="comment-author vcard">

   |image22| `octaviantuchila14 <http://octavianex.wordpress.com>`__
   Says:

   .. raw:: html

      </div>

   `November 12, 2016 at 5:13
   am <https://bartoszmilewski.com/2015/01/20/functors/#comment-67567>`__
   Hi!

   | Great post!
   | Are there any answers to the exercises posted somewhere?

   It’s really difficult too see if my solutions are correct.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68169">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68169">

   .. raw:: html

      <div class="comment-author vcard">

   `Purity in an impure language with the free monad in a CQRS
   app <http://blog.leifbattermann.de/2016/12/25/purity-in-an-impure-language-free-monad-tic-tac-toe-cqrs-event-souring/>`__
   Says:

   .. raw:: html

      </div>

   `December 25, 2016 at 2:05
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-68169>`__
   […] Also each DSL has to have a definition of map. (It has to be a
   Functor.) […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70237">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70237">

   .. raw:: html

      <div class="comment-author vcard">

   |image23| `stabbles <http://stoppels.blog>`__ Says:

   .. raw:: html

      </div>

   `April 25, 2017 at 2:28
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-70237>`__
   Very nice!

   Small remark on

       Notice that, unlike in C++ or Java, you don’t have to specify the
       Eq class (or interface) when defining Point — you can do it later
       in client code

   You *can* do this in C++ because operator== is a free function. Here
   is an example:

   ::

       struct Point {
           int const x;
           int const y;
       };

       constexpr bool operator==(Point const &lhs, Point const &rhs)
       {
           return lhs.x == rhs.x && lhs.y == rhs.y;
       }

       int main()
       {
           static_assert(Point{1, 2} == Point{1,2}, "Should be equal");
       }

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73636">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73636">

   .. raw:: html

      <div class="comment-author vcard">

   |image24| `HenryChern <http://henrychern.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `August 12, 2017 at 11:55
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-73636>`__
   “type functions” are “function types” (under “The Reader Functor”).
   Is that so?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73693">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73693">

   .. raw:: html

      <div class="comment-author vcard">

   |image25| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `August 14, 2017 at 3:55
   pm <https://bartoszmilewski.com/2015/01/20/functors/#comment-73693>`__
   (->) is a type function: a function that acts on types and produces a
   type. When you apply it to two types, a and b, you get a function
   type, a->b. In other words, you produce the type of a function that
   takes a value of type a as an argument and returns a value of type b.

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
   reply </2015/01/20/functors/#respond>`__
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
-  January 20, 2015 at 5:44 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/01/20/functors/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Blog at WordPress.com. <https://wordpress.com/?ref=footer_blog>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-65f16e220ad21f38035c67ba6ae67047">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ee9ee7d0fc302fcfc3678e0c67442fd5">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ad6350f5199dc0b7baac49841da261f2">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-fad78be0d39573f5b05e459624ac10bf">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-463a92005cd9f50479a691903e883ae4">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-5fd83faa8136695f7d6d1f3fe7bfe919">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-a0430fb2f4acd7237ad585d734c4269e">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-1eb733404e284f54709375bea86226a4">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-d2bec3faf8ad66ce11c4592d9e7e7612">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-01aedf82ca3035479d03695235607dcd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-7c26dc91767d4916e6b4efbaccc4d75b">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-62288ac65c238d13d7273143d4442fcd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-d42dd46c69476ea0478111fa098ef4a4">

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

|image32|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Functor| image:: https://bartoszmilewski.files.wordpress.com/2015/01/functor.jpg?w=300&h=263
   :class: aligncenter wp-image-3944 size-medium
   :width: 300px
   :height: 263px
   :target: https://bartoszmilewski.files.wordpress.com/2015/01/functor.jpg
.. |FunctorCompos| image:: https://bartoszmilewski.files.wordpress.com/2015/01/functorcompos.jpg?w=276&h=300
   :class: aligncenter size-medium wp-image-3947
   :width: 276px
   :height: 300px
   :target: https://bartoszmilewski.files.wordpress.com/2015/01/functorcompos.jpg
.. |FunctorId| image:: https://bartoszmilewski.files.wordpress.com/2015/01/functorid.jpg?w=225&h=300
   :class: aligncenter size-medium wp-image-3949
   :width: 225px
   :height: 300px
   :target: https://bartoszmilewski.files.wordpress.com/2015/01/functorid.jpg
.. |FunctorMaybe| image:: https://bartoszmilewski.files.wordpress.com/2015/01/functormaybe.jpg?w=300&h=219
   :class: aligncenter size-medium wp-image-3950
   :width: 300px
   :height: 219px
   :target: https://bartoszmilewski.files.wordpress.com/2015/01/functormaybe.jpg
.. |image4| image:: https://0.gravatar.com/avatar/65f16e220ad21f38035c67ba6ae67047?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://2.gravatar.com/avatar/ee9ee7d0fc302fcfc3678e0c67442fd5?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://2.gravatar.com/avatar/ee9ee7d0fc302fcfc3678e0c67442fd5?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://2.gravatar.com/avatar/ee9ee7d0fc302fcfc3678e0c67442fd5?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://2.gravatar.com/avatar/ee9ee7d0fc302fcfc3678e0c67442fd5?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://i1.wp.com/pbs.twimg.com/profile_images/92686163/Untitled_normal.png?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://0.gravatar.com/avatar/fad78be0d39573f5b05e459624ac10bf?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://1.gravatar.com/avatar/463a92005cd9f50479a691903e883ae4?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://2.gravatar.com/avatar/5fd83faa8136695f7d6d1f3fe7bfe919?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image16| image:: https://1.gravatar.com/avatar/a0430fb2f4acd7237ad585d734c4269e?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image17| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image18| image:: https://1.gravatar.com/avatar/1eb733404e284f54709375bea86226a4?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image19| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image20| image:: https://1.gravatar.com/avatar/d2bec3faf8ad66ce11c4592d9e7e7612?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image21| image:: https://i0.wp.com/graph.facebook.com/v2.2/10105738640530183/picture?q=type%3Dlarge%26_md5%3D09b4560f1d56adfd0aa9dde5a15cf26d&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image22| image:: https://1.gravatar.com/avatar/7c26dc91767d4916e6b4efbaccc4d75b?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image23| image:: https://0.gravatar.com/avatar/62288ac65c238d13d7273143d4442fcd?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image24| image:: https://1.gravatar.com/avatar/d42dd46c69476ea0478111fa098ef4a4?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image25| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image32| image:: https://pixel.wp.com/b.gif?v=noscript

