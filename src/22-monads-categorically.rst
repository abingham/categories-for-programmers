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
   class="post-7805 post type-post status-publish format-standard hentry category-category-theory category-haskell category-monads category-programming">

December 27, 2016

.. raw:: html

   <div class="post-info">

.. rubric:: Monads Categorically
   :name: monads-categorically
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Monads <https://bartoszmilewski.com/category/monads/>`__,
`Programming <https://bartoszmilewski.com/category/programming/>`__
`[9]
Comments <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_7805" class="pd-rating">

.. raw:: html

   </div>

    This is part 22 of Categories for Programmers. Previously: `Monads
    and
    Effects <https://bartoszmilewski.com/2016/11/30/monads-and-effects/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

If you mention monads to a programmer, you’ll probably end up talking
about effects. To a mathematician, monads are about algebras. We’ll talk
about algebras later — they play an important role in programming — but
first I’d like to give you a little intuition about their relation to
monads. For now, it’s a bit of a hand-waving argument, but bear with me.

Algebra is about creating, manipulating, and evaluating expressions.
Expressions are built using operators. Consider this simple expression:

::

    x2 + 2 x + 1

This expression is formed using variables like ``x``, and constants like
1 or 2, bound together with operators like plus or times. As
programmers, we often think of expressions as trees.

|exptree|

Trees are containers so, more generally, an expression is a container
for storing variables. In category theory, we represent containers as
endofunctors. If we assign the type ``a`` to the variable ``x``, our
expression will have the type ``m a``, where ``m`` is an endofunctor
that builds expression trees. (Nontrivial branching expressions are
usually created using recursively defined endofunctors.)

What’s the most common operation that can be performed on an expression?
It’s substitution: replacing variables with expressions. For instance,
in our example, we could replace ``x`` with ``y - 1`` to get:

::

    (y - 1)2 + 2 (y - 1) + 1

Here’s what happened: We took an expression of type ``m a`` and applied
a transformation of type ``a -> m b`` (``b`` represents the type of
``y``). The result is an expression of type ``m b``. Let me spell it
out:

::

    m a -> (a -> m b) -> m b

Yes, that’s the signature of monadic bind.

That was a bit of motivation. Now let’s get to the math of the monad.
Mathematicians use different notation than programmers. They prefer to
use the letter ``T`` for the endofunctor, and Greek letters: μ for
``join`` and η for ``return``. Both ``join`` and ``return`` are
polymorphic functions, so we can guess that they correspond to natural
transformations.

Therefore, in category theory, a monad is defined as an endofunctor
``T`` equipped with a pair of natural transformations μ and η.

μ is a natural transformation from the square of the functor ``T2`` back
to ``T``. The square is simply the functor composed with itself,
``T ∘ T`` (we can only do this kind of squaring for endofunctors).

::

    μ :: T2 -> T

The component of this natural transformation at an object ``a`` is the
morphism:

::

    μa :: T (T a) -> T a

which, in *Hask*, translates directly to our definition of ``join``.

η is a natural transformation between the identity functor ``I`` and
``T``:

::

    η :: I -> T

Considering that the action of ``I`` on the object ``a`` is just ``a``,
the component of η is given by the morphism:

::

    ηa :: a -> T a

which translates directly to our definition of ``return``.

These natural transformations must satisfy some additional laws. One way
of looking at it is that these laws let us define a Kleisli category for
the endofunctor ``T``. Remember that a Kleisli arrow between ``a`` and
``b`` is defined as a morphism ``a -> T b``. The composition of two such
arrows (I’ll write it as a circle with the subscript ``T``) can be
implemented using μ:

::

    g ∘T f = μc ∘ (T g) ∘ f

where

::

    f :: a -> T b
    g :: b -> T c

Here ``T``, being a functor, can be applied to the morphism ``g``. It
might be easier to recognize this formula in Haskell notation:

::

    f >=> g = join . fmap g . f

or, in components:

::

    (f >=> g) a = join (fmap g (f a))

In terms of the algebraic interpretation, we are just composing two
successive substitutions.

For Kleisli arrows to form a category we want their composition to be
associative, and η\ :sub:`a` to be the identity Kleisli arrow at ``a``.
This requirement can be translated to monadic laws for μ and η. But
there is another way of deriving these laws that makes them look more
like monoid laws. In fact ``μ`` is often called multiplication, and
``η`` unit.

Roughly speaking, the associativity law states that the two ways of
reducing the cube of ``T``, ``T3``, down to ``T`` must give the same
result. Two unit laws (left and right) state that when ``η`` is applied
to ``T`` and then reduced by ``μ``, we get back ``T``.

Things are a little tricky because we are composing natural
transformations and functors. So a little refresher on horizontal
composition is in order. For instance, ``T3`` can be seen as a
composition of ``T`` after ``T2``. We can apply to it the horizontal
composition of two natural transformations:

::

    IT ∘ μ

|assoc1|

and get ``T∘T``; which can be further reduced to ``T`` by applying
``μ``. ``IT`` is the identity natural transformation from ``T`` to
``T``. You will often see the notation for this type of horizontal
composition ``IT ∘ μ`` shortened to ``T∘μ``. This notation is
unambiguous because it makes no sense to compose a functor with a
natural transformation, therefore ``T`` must mean ``IT`` in this
context.

We can also draw the diagram in the (endo-) functor category ``[C, C]``:

|assoc2|

Alternatively, we can treat ``T3`` as the composition of ``T2∘T`` and
apply ``μ∘T`` to it. The result is also ``T∘T`` which, again, can be
reduced to ``T`` using μ. We require that the two paths produce the same
result.

|assoc|

Similarly, we can apply the horizontal composition ``η∘T`` to the
composition of the identity functor ``I`` after ``T`` to obtain ``T2``,
which can then be reduced using ``μ``. The result should be the same as
if we applied the identity natural transformation directly to ``T``.
And, by analogy, the same should be true for ``T∘η``.

|unitlawcomp-1|

You can convince yourself that these laws guarantee that the composition
of Kleisli arrows indeed satisfies the laws of a category.

The similarities between a monad and a monoid are striking. We have
multiplication μ, unit η, associativity, and unit laws. But our
definition of a monoid is too narrow to describe a monad as a monoid. So
let’s generalize the notion of a monoid.

.. rubric:: Monoidal Categories
   :name: monoidal-categories

Let’s go back to the conventional definition of a monoid. It’s a set
with a binary operation and a special element called unit. In Haskell,
this can be expressed as a typeclass:

::

    class Monoid m where
        mappend :: m -> m -> m
        mempty  :: m

The binary operation ``mappend`` must be associative and unital (i.e.,
multiplication by the unit ``mempty`` is a no-op).

Notice that, in Haskell, the definition of ``mappend`` is curried. It
can be interpreted as mapping every element of ``m`` to a function:

::

    mappend :: m -> (m -> m)

It’s this interpretation that gives rise to the definition of a monoid
as a single-object category where endomorphisms ``(m -> m)`` represent
the elements of the monoid. But because currying is built into Haskell,
we could as well have started with a different definition of
multiplication:

::

    mu :: (m, m) -> m

Here, the cartesian product ``(m, m)`` becomes the source of pairs to be
multiplied.

This definition suggests a different path to generalization: replacing
the cartesian product with categorical product. We could start with a
category where products are globally defined, pick an object ``m``
there, and define multiplication as a morphism:

::

    μ :: m × m -> m

We have one problem though: In an arbitrary category we can’t peek
inside an object, so how do we pick the unit element? There is a trick
to it. Remember how element selection is equivalent to a function from
the singleton set? In Haskell, we could replace the definition of
``mempty`` with a function:

::

    eta :: () -> m

The singleton is the terminal object in **Set**, so it’s natural to
generalize this definition to any category that has a terminal object
``t``:

::

    η :: t -> m

This lets us pick the unit “element” without having to talk about
elements.

Unlike in our previous definition of a monoid as a single-object
category, monoidal laws here are not automatically satisfied — we have
to impose them. But in order to formulate them we have to establish the
monoidal structure of the underlying categorical product itself. Let’s
recall how monoidal structure works in Haskell first.

We start with associativity. In Haskell, the corresponding equational
law is:

::

    mu x (mu y z) = mu (mu x y) z

Before we can generalize it to other categories, we have to rewrite it
as an equality of functions (morphisms). We have to abstract it away
from its action on individual variables — in other words, we have to use
point-free notation. Knowning that the cartesian product is a bifunctor,
we can write the left hand side as:

::

    (mu . bimap id mu)(x, (y, z))

and the right hand side as:

::

    (mu . bimap mu id)((x, y), z)

This is almost what we want. Unfortunately, the cartesian product is not
strictly associative — ``(x, (y, z))`` is not the same as
``((x, y), z)`` — so we can’t just write point-free:

::

    mu . bimap id mu = mu . bimap mu id

On the other hand, the two nestings of pairs are isomorphic. There is an
invertible function called the associator that converts between them:

::

    alpha :: ((a, b), c) -> (a, (b, c))
    alpha ((x, y), z) = (x, (y, z))

With the help of the associator, we can write the point-free
associativity law for ``mu``:

::

    mu . bimap id mu . alpha = mu . bimap mu id

We can apply a similar trick to unit laws which, in the new notation,
take the form:

::

    mu (eta (), x) = x
    mu (x, eta ()) = x

They can be rewritten as:

::

    (mu . bimap eta id) ((), x) = lambda ((), x)
    (mu . bimap id eta) (x, ()) = rho (x, ())

The isomorphisms ``lambda`` and ``rho`` are called the left and right
unitor, respectively. They witness the fact that the unit ``()`` is the
identity of the cartesian product up to isomorphism:

::

    lambda :: ((), a) -> a
    lambda ((), x) = x

::

    rho :: (a, ()) -> a
    rho (x, ()) = x

The point-free versions of the unit laws are therefore:

::

    mu . bimap id eta = lambda
    mu . bimap eta id = rho

We have formulated point-free monoidal laws for ``mu`` and ``eta`` using
the fact that the underlying cartesian product itself acts like a
monoidal multiplication in the category of types. Keep in mind though
that the associativity and unit laws for the cartesian product are valid
only up to isomorphism.

It turns out that these laws can be generalized to any category with
products and a terminal object. Categorical products are indeed
associative up to isomorphism and the terminal object is the unit, also
up to isomorphism. The associator and the two unitors are natural
isomorphisms. The laws can be represented by commuting diagrams.

|assocmon|

Notice that, because the product is a bifunctor, it can lift a pair of
morphisms — in Haskell this was done using ``bimap``.

We could stop here and say that we can define a monoid on top of any
category with categorical products and a terminal object. As long as we
can pick an object ``m`` and two morphisms μ and η that satisfy monoidal
laws, we have a monoid. But we can do better than that. We don’t need a
full-blown categorical product to formulate the laws for μ and η. Recall
that a product is defined through a universal construction that uses
projections. We haven’t used any projections in our formulation of
monoidal laws.

A bifunctor that behaves like a product without being a product is
called a tensor product, often denoted by the infix operator ⊗. A
definition of a tensor product in general is a bit tricky, but we won’t
worry about it. We’ll just list its properties — the most important
being associativity up to isomorphism.

Similarly, we don’t need the object ``t`` to be terminal. We never used
its terminal property — namely, the existence of a unique morphism from
any object to it. What we require is that it works well in concert with
the tensor product. Which means that we want it to be the unit of the
tensor product, again, up to isomorphism. Let’s put it all together:

A monoidal category is a category *C* equipped with a bifunctor called
the tensor product:

::

    ⊗ :: C × C -> C

and a distinct object ``i`` called the unit object, together with three
natural isomorphisms called, respectively, the associator and the left
and right unitors:

::

    αa b c :: (a ⊗ b) ⊗ c -> a ⊗ (b ⊗ c)
    λa :: i ⊗ a -> a
    ρa :: a ⊗ i -> a

(There is also a coherence condition for simplifying a quadruple tensor
product.)

What’s important is that a tensor product describes many familiar
bifunctors. In particular, it works for a product, a coproduct and, as
we’ll see shortly, for the composition of endofunctors (and also for
some more esoteric products like Day convolution). Monoidal categories
will play an essential role in the formulation of enriched categories.

.. rubric:: Monoid in a Monoidal Category
   :name: monoid-in-a-monoidal-category

We are now ready to define a monoid in a more general setting of a
monoidal category. We start by picking an object ``m``. Using the tensor
product we can form powers of ``m``. The square of ``m`` is ``m ⊗ m``.
There are two ways of forming the cube of ``m``, but they are isomorphic
through the associator. Similarly for higher powers of ``m`` (that’s
where we need the coherence conditions). To form a monoid we need to
pick two morphisms:

::

    μ :: m ⊗ m -> m
    η :: i -> m

where ``i`` is the unit object for our tensor product.

|monoid-1|

These morphisms have to satisfy associativity and unit laws, which can
be expressed in terms of the following commuting diagrams:

|assoctensor|

|unitmon|

Notice that it’s essential that the tensor product be a bifunctor
because we need to lift pairs of morphisms to form products such as
``μ ⊗ id`` or ``η ⊗ id``. These diagrams are just a straightforward
generalization of our previous results for categorical products.

.. rubric:: Monads as Monoids
   :name: monads-as-monoids

Monoidal structures pop up in unexpected places. One such place is the
functor category. If you squint a little, you might be able to see
functor composition as a form of multiplication. The problem is that not
any two functors can be composed — the target category of one has to be
the source category of the other. That’s just the usual rule of
composition of morphisms — and, as we know, functors are indeed
morphisms in the category **Cat**. But just like endomorphisms
(morphisms that loop back to the same object) are always composable, so
are endofunctors. For any given category *C*, endofunctors from *C* to
*C* form the functor category ``[C, C]``. Its objects are endofunctors,
and morphisms are natural transformations between them. We can take any
two objects from this category, say endofunctors ``F`` and ``G``, and
produce a third object ``F ∘ G`` — an endofunctor that’s their
composition.

Is endofunctor composition a good candidate for a tensor product? First,
we have to establish that it’s a bifunctor. Can it be used to lift a
pair of morphisms — here, natural transformations? The signature of the
analog of ``bimap`` for the tensor product would look something like
this:

::

    bimap :: (a -> b) -> (c -> d) -> (a ⊗ c -> b ⊗ d)

If you replace objects by endofunctors, arrows by natural
transformations, and tensor products by composition, you get:

::

    (F -> F') -> (G -> G') -> (F ∘ G -> F' ∘ G')

which you may recognize as the special case of horizontal composition.

|horizcomp|

We also have at our disposal the identity endofunctor ``I``, which can
serve as the identity for endofunctor composition — our new tensor
product. Moreover, functor composition is associative. In fact
associativity and unit laws are strict — there’s no need for the
associator or the two unitors. So endofunctors form a strict monoidal
category with functor composition as tensor product.

What’s a monoid in this category? It’s an object — that is an
endofunctor ``T``; and two morphisms — that is natural transformations:

::

    μ :: T ∘ T -> T
    η :: I -> T

Not only that, here are the monoid laws:

|assoc|

|unitlawcomp|

They are exactly the monad laws we’ve seen before. Now you understand
the famous quote from Saunders Mac Lane:

All told, monad is just a monoid in the category of endofunctors.

You might have seen it emblazoned on some t-shirts at functional
programming conferences.

.. rubric:: Monads from Adjunctions
   :name: monads-from-adjunctions

An `adjunction <https://bartoszmilewski.com/2016/04/18/adjunctions/>`__,
``L ⊣ R``, is a pair of functors going back and forth between two
categories *C* and *D*. There are two ways of composing them giving rise
to two endofunctors, ``R ∘ L`` and ``L ∘ R``. As per an adjunction,
these endofunctors are related to identity functors through two natural
transformations called unit and counit:

::

    η :: ID -> R ∘ L
    ε :: L ∘ R -> IC

Immediately we see that the unit of an adjunction looks just like the
unit of a monad. It turns out that the endofunctor ``R ∘ L`` is indeed a
monad. All we need is to define the appropriate μ to go with the η.
That’s a natural transformation between the square of our endofunctor
and the endofunctor itself or, in terms of the adjoint functors:

::

    R ∘ L ∘ R ∘ L -> R ∘ L

And, indeed, we can use the counit to collapse the ``L ∘ R`` in the
middle. The exact formula for μ is given by the horizontal composition:

::

    μ = R ∘ ε ∘ L

Monadic laws follow from the identities satisfied by the unit and counit
of the adjunction and the interchange law.

We don’t see a lot of monads derived from adjunctions in Haskell,
because an adjunction usually involves two categories. However, the
definitions of an exponential, or a function object, is an exception.
Here are the two endofunctors that form this adjunction:

::

    L z = z × s
    R b = s ⇒ b

You may recognize their composition as the familiar state monad:

::

    R (L z) = s ⇒ (z × s)

We’ve seen this monad before in Haskell:

::

    newtype State s a = State (s -> (a, s))

Let’s also translate the adjunction to Haskell. The left functor is the
product functor:

::

    newtype Prod s a = Prod (a, s)

and the right functor is the reader functor:

::

    newtype Reader s a = Reader (s -> a)

They form the adjunction:

::

    instance Adjunction (Prod s) (Reader s) where
      counit (Prod (Reader f, s)) = f s
      unit a = Reader (\s -> Prod (a, s))

You can easily convince yourself that the composition of the reader
functor after the product functor is indeed equivalent to the state
functor:

::

    newtype State s a = State (s -> (a, s))

As expected, the ``unit`` of the adjunction is equivalent to the
``return`` function of the state monad. The ``counit`` acts by
evaluating a function acting on its argument. This is recognizable as
the uncurried version of the function ``runState``:

::

    runState :: State s a -> s -> (a, s)
    runState (State f) s = f s

(uncurried, because in ``counit`` it acts on a pair).

We can now define ``join`` for the state monad as a component of the
natural transformation μ. For that we need a horizontal composition of
three natural transformations:

::

    μ = R ∘ ε ∘ L

In other words, we need to sneak the counit ε across one level of the
reader functor. We can’t just call ``fmap`` directly, because the
compiler would pick the one for the ``State`` functor, rather than the
``Reader`` functor. But recall that ``fmap`` for the reader functor is
just left function composition. So we’ll use function composition
directly.

We have to first peel off the data constructor ``State`` to expose the
function inside the ``State`` functor. This is done using ``runState``:

::

    ssa :: State s (State s a)
    runState ssa :: s -> (State s a, s)

Then we left-compose it with the counit, which is defined by
``uncurry runState``. Finally, we clothe it back in the ``State`` data
constructor:

::

    join :: State s (State s a) -> State s a
    join ssa = State (uncurry runState . runState ssa)

This is indeed the implementation of ``join`` for the ``State`` monad.

It turns out that not only every adjunction gives rise to a monad, but
the converse is also true: every monad can be factorized into a
composition of two adjoint functors. Such factorization is not unique
though.

We’ll talk about the other endofunctor ``L ∘ R`` in the next section.

Next: `Comonads <https://bartoszmilewski.com/2017/01/02/comonads/>`__.

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

   <div id="crt-863814257" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1609512800" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2016/12/27/monads-categorically/?share=email>`__
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

   <div id="like-post-wrapper-3549518-7805-59ae3cdc43faf"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=7805&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-7805-59ae3cdc43faf"
   data-name="like-post-frame-3549518-7805-59ae3cdc43faf">

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

.. rubric:: 9 Responses to “Monads Categorically”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-68197">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68197">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| `Existential Type <http://existentialtype.wordpress.com>`__
   Says:

   .. raw:: html

      </div>

   `December 27, 2016 at 6:53
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68197>`__
   I assume by “Hask” you mean the category of cppo’s and continuous
   functions? If so, this has nothing much to do with Haskell, because
   it is not fully abstract with respect to the operational meaning of
   Haskell programs. It should be called CPPO, like everyone else does,
   and not the pretentious “Hask”, which serves only to mislead. In
   fact, Haskell has no semantics, which is pretty embarrassing for a
   supposedly mathematical language.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68209">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68209">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| allan j cooper Says:

   .. raw:: html

      </div>

   `December 28, 2016 at 4:20
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68209>`__
   The mapping a -> m b deserves a picture! I can see in my mind’s eye
   that you’ve taken the 3 node expression tree +(y (-1)) and spliced it
   over each of the three occurrences of x to obtain the new tree of
   type m b. All that’s missing is a color y to represent what happens
   to the pretty pink of x, and you’ve reached in and drawn the result
   of this monadic bind!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68215">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68215">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `December 29, 2016 at 3:53
   am <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68215>`__
   In the commuting diagram about unit laws in section “Monoid in a
   Monoidal Category”, the rightmost object should be the tensor product
   between m and i (not between m and id as it is shown).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68220">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68220">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `December 29, 2016 at 11:18
   am <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68220>`__
   @Juan Manuel: You’re right. I can’t even call it a “typo” since it’s
   hand-written. I’ll fix it at some point.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68285">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68285">

   .. raw:: html

      <div class="comment-author vcard">

   |image16| `jd823592 <http://gravatar.com/jd823592>`__ Says:

   .. raw:: html

      </div>

   `January 3, 2017 at 4:12
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68285>`__
   I actually thought the “typo” was in having “id x m” rather than “id
   x mu”

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68287">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68287">

   .. raw:: html

      <div class="comment-author vcard">

   |image17| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 3, 2017 at 6:00
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-68287>`__
   @jd823592: oops!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69683">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69683">

   .. raw:: html

      <div class="comment-author vcard">

   |image18| `BM Category Theory 10: Monad & Monoid \| Math Online Tom
   Circle <https://tomcircle.wordpress.com/2017/03/21/bm-category-theory-10-1-monads/>`__
   Says:

   .. raw:: html

      </div>

   `March 21, 2017 at 10:41
   am <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-69683>`__
   […] 10. 2 Monoidal Categories (read text) […]

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69925">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69925">

   .. raw:: html

      <div class="comment-author vcard">

   |image19| `Robert
   Peszek <https://www.facebook.com/app_scoped_user_id/100004670593545/>`__
   Says:

   .. raw:: html

      </div>

   `April 8, 2017 at 2:16
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-69925>`__
   | Thank you for writing this series! It is by far the most intuitive
     and programmer friendly into to category theory out there.
   | I only wish I found it earlier. I very much hope you find energy
     and time to continue writing.

   | I have a question about the definition of mu/join natural
     transformation for R.L adjunction (mu = R . epsilon . L).
   | To me, a more straightforward definition is simply one that uses:
   | R (epsilon)
   | with epsilon viewed as a family of morphism and R fmaps them to act
     on D. This approach is actually what you have ended up using in
     your State monad example.
   | Interestingly, this approach does not use L. Is this somehow
     equivalent to the horizontal composition R . epsilon . L you are
     using in your definition?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69938">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69938">

   .. raw:: html

      <div class="comment-author vcard">

   |image20| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `April 9, 2017 at 1:31
   pm <https://bartoszmilewski.com/2016/12/27/monads-categorically/#comment-69938>`__
   The L on the right is necessary to “shift” epsilon — to pick the
   right morphism from the family of morphisms. This is invisible in
   Haskell code because of type inference. The compiler figures out what
   component to pick by analyzing types. I tried to explain horizontal
   composition in more detail in this video:
   https://www.youtube.com/watch?v=zkDVCQiveEo .

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
   reply </2016/12/27/monads-categorically/#respond>`__
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
-  December 27, 2016 at 10:49 am
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Monads <https://bartoszmilewski.com/category/monads/>`__,
   `Programming <https://bartoszmilewski.com/category/programming/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2016/12/27/monads-categorically/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Blog at WordPress.com. <https://wordpress.com/?ref=footer_blog>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-9c67c24c8638f53306aac5322f0fc3f7">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-0e18ef5d3859fcb7537ac36ce691a9c9">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-b4a7426cee3700d21354b77b4a29fddd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-8e53773d91e4e81f93ef8679df58aa00">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-3eb51fc4b2b8c2abfa8210387c478092">

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

|image27|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |exptree| image:: https://bartoszmilewski.files.wordpress.com/2016/12/exptree.png?w=175&h=180
   :class: alignnone wp-image-8000
   :width: 175px
   :height: 180px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/exptree.png
.. |assoc1| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assoc1.png?w=248&h=151
   :class: alignnone wp-image-7996
   :width: 248px
   :height: 151px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assoc1.png
.. |assoc2| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assoc2.png?w=167&h=149
   :class: alignnone wp-image-7997
   :width: 167px
   :height: 149px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assoc2.png
.. |assoc| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assoc.png?w=208&h=165
   :class: alignnone wp-image-7995
   :width: 208px
   :height: 165px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assoc.png
.. |unitlawcomp-1| image:: https://bartoszmilewski.files.wordpress.com/2016/12/unitlawcomp-1.png?w=300&h=124
   :class: alignnone size-medium wp-image-8002
   :width: 300px
   :height: 124px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/unitlawcomp-1.png
.. |assocmon| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assocmon.png?w=300&h=149
   :class: alignnone size-medium wp-image-7998
   :width: 300px
   :height: 149px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assocmon.png
.. |monoid-1| image:: https://bartoszmilewski.files.wordpress.com/2016/12/monoid-1.jpg?w=300&h=268
   :class: alignnone size-medium wp-image-7982
   :width: 300px
   :height: 268px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/monoid-1.jpg
.. |assoctensor| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assoctensor.jpg?w=300&h=145
   :class: alignnone size-medium wp-image-8065
   :width: 300px
   :height: 145px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assoctensor.jpg
.. |unitmon| image:: https://bartoszmilewski.files.wordpress.com/2016/12/unitmon.jpg?w=300&h=121
   :class: alignnone size-medium wp-image-8038
   :width: 300px
   :height: 121px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/unitmon.jpg
.. |horizcomp| image:: https://bartoszmilewski.files.wordpress.com/2016/12/horizcomp.png?w=255&h=124
   :class: alignnone wp-image-8001
   :width: 255px
   :height: 124px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/horizcomp.png
.. |assoc| image:: https://bartoszmilewski.files.wordpress.com/2016/12/assoc.png?w=183&h=145
   :class: alignnone wp-image-7995
   :width: 183px
   :height: 145px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/assoc.png
.. |unitlawcomp| image:: https://bartoszmilewski.files.wordpress.com/2016/12/unitlawcomp.png?w=275&h=121
   :class: wp-image-8003 alignnone
   :width: 275px
   :height: 121px
   :target: https://bartoszmilewski.files.wordpress.com/2016/12/unitlawcomp.png
.. |image12| image:: https://0.gravatar.com/avatar/9c67c24c8638f53306aac5322f0fc3f7?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://0.gravatar.com/avatar/0e18ef5d3859fcb7537ac36ce691a9c9?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://i1.wp.com/pbs.twimg.com/profile_images/452017421855907841/W65GNlUV_normal.jpeg?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image16| image:: https://2.gravatar.com/avatar/8e53773d91e4e81f93ef8679df58aa00?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image17| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image18| image:: https://secure.gravatar.com/blavatar/f1511c29baf2f48c9c1a093731d0317b?s=48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image19| image:: https://i1.wp.com/graph.facebook.com/v2.2/100004670593545/picture?q=type%3Dlarge%26_md5%3D0fea0480cf64b8dc570a9af26020cb85&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image20| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image27| image:: https://pixel.wp.com/b.gif?v=noscript

