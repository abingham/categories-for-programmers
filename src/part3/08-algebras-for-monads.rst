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
   class="post-8363 post type-post status-publish format-standard hentry category-category-theory category-monads">

March 14, 2017

.. raw:: html

   <div class="post-info">

.. rubric:: Algebras for Monads
   :name: algebras-for-monads
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Monads <https://bartoszmilewski.com/category/monads/>`__
`[4]
Comments <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_8363" class="pd-rating">

.. raw:: html

   </div>

    This is part 25 of Categories for Programmers. Previously:
    `F-Algebras <https://bartoszmilewski.com/2017/02/28/f-algebras/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

| If we interpret endofunctors as ways of defining expressions, algebras
  let us evaluate them and monads let us form and manipulate them. By
  combining algebras with monads we not only gain a lot of functionality
  but we can also answer a few interesting questions. One such question
  concerns the relation between monads and adjunctions. As we’ve seen,
  every adjunction `defines a
  monad <https://bartoszmilewski.com/2016/12/27/monads-categorically/>`__
  (and a comonad). The question is: Can every monad (comonad) be derived
  from an adjunction? The answer is positive. There is a whole family of
  adjunctions that generate a given monad. I’ll show you two such
  adjunction.
| |image0|
| Let’s review the definitions. A monad is an endofunctor ``m`` equipped
  with two natural transformations that satisfy some coherence
  conditions. The components of these transformations at ``a`` are:

::

    ηa :: a -> m a
    μa :: m (m a) -> m a

An algebra for the same endofunctor is a selection of a particular
object — the carrier ``a`` — together with the morphism:

::

    alg :: m a -> a

The first thing to notice is that the algebra goes in the opposite
direction to ``ηa``. The intuition is that ``ηa`` creates a trivial
expression from a value of type ``a``. The first coherence condition
that makes the algebra compatible with the monad ensures that evaluating
this expression using the algebra whose carrier is ``a`` gives us back
the original value:

::

    alg ∘ ηa = ida

The second condition arises from the fact that there are two ways of
evaluating the doubly nested expression ``m (m a)``. We can first apply
``μa`` to flatten the expression, and then use the evaluator of the
algebra; or we can apply the lifted evaluator to evaluate the inner
expressions, and then apply the evaluator to the result. We’d like the
two strategies to be equivalent:

::

    alg ∘ μa = alg ∘ m alg

Here, ``m alg`` is the morphism resulting from lifting ``alg`` using the
functor ``m``. The following commuting diagrams describe the two
conditions (I replaced ``m`` with ``T`` in anticipation of what
follows):

|image1|

|image2|

We can also express these condition in Haskell:

::

    alg . return = id
    alg . join = alg . fmap alg

Let’s look at a small example. An algebra for a list endofunctor
consists of some type ``a`` and a function that produces an ``a`` from a
list of ``a``. We can express this function using ``foldr`` by choosing
both the element type and the accumulator type to be equal to ``a``:

::

    foldr :: (a -> a -> a) -> a -> [a] -> a

This particular algebra is specified by a two-argument function, let’s
call it ``f``, and a value ``z``. The list functor happens to also be a
monad, with ``return`` turning a value into a singleton list. The
composition of the algebra, here ``foldr f z``, after ``return`` takes
``x`` to:

::

    foldr f z [x] = x `f` z

where the action of ``f`` is written in the infix notation. The algebra
is compatible with the monad if the following coherence condition is
satisfied for every ``x``:

::

    x `f` z = x

If we look at ``f`` as a binary operator, this condition tells us that
``z`` is the right unit.

The second coherence condition operates on a list of lists. The action
of ``join`` concatenates the individual lists. We can then fold the
resulting list. On the other hand, we can first fold the individual
lists, and then fold the resulting list. Again, if we interpret ``f`` as
a binary operator, this condition tells us that this binary operation is
associative. These conditions are certainly fulfilled when ``(a, f, z)``
is a monoid.

.. rubric:: T-algebras
   :name: t-algebras

Since mathematicians prefer to call their monads ``T``, they call
algebras compatible with them T-algebras. T-algebras for a given monad T
in a category *C* form a category called the Eilenberg-Moore category,
often denoted by C\ :sup:`T`. Morphisms in that category are
homomorphisms of algebras. These are the same homomorphisms we’ve seen
defined for F-algebras.

A T-algebra is a pair consisting of a carrier object and an evaluator,
``(a, f)``. There is an obvious forgetful functor ``UT`` from
C\ :sup:`T` to C, which maps ``(a, f)`` to ``a``. It also maps a
homomorphism of T-algebras to a corresponding morphism between carrier
objects in C. You may remember from our discussion of adjunctions that
the left adjoint to a forgetful functor is called a free functor.

The left adjoint to ``UT`` is called ``FT``. It maps an object ``a`` in
C to a free algebra in C\ :sup:`T`. The carrier of this free algebra is
``T a``. Its evaluator is a morphism from ``T (T a)`` back to ``T a``.
Since ``T`` is a monad, we can use the monadic ``μa`` (Haskell ``join``)
as the evaluator.

We still have to show that this is a T-algebra. For that, two coherence
conditions must be satisified:

::

    alg ∘ ηTa = idTa

::

    alg ∘ μa = alg ∘ T alg

But these are just monadic laws, if you plug in ``μ`` for the algebra.

As you may recall, every adjunction defines a monad. It turns out that
the adjunction between F\ :sup:`T` and U\ :sup:`T` defines the very
monad ``T`` that was used in the construction of the Eilenberg-Moore
category. Since we can perform this construction for every monad, we
conclude that every monad can be generated from an adjunction. Later
I’ll show you that there is another adjunction that generates the same
monad.

Here’s the plan: First I’ll show you that ``FT`` is indeed the left
adjoint of ``UT``. I’ll do it by defining the unit and the counit of
this adjunction and proving that the corresponding triangular identities
are satisfied. Then I’ll show you that the monad generated by this
adjunction is indeed our original monad.

The unit of the adjunction is the natural transformation:

::

    η :: I -> UT ∘ FT

Let’s calculate the ``a`` component of this transformation. The identity
functor gives us ``a``. The free functor produces the free algebra
``(T a, μa)``, and the forgetful functor reduces it to ``T a``.
Altogether we get a mapping from ``a`` to ``T a``. We’ll simply use the
unit of the monad ``T`` as the unit of this adjunction.

Let’s look at the counit:

::

    ε :: FT ∘ UT -> I

Let’s calculate its component at some T-algebra ``(a, f)``. The
forgetful functor forgets the ``f``, and the free functor produces the
pair ``(T a, μa)``. So in order to define the component of the counit
``ε`` at ``(a, f)``, we need the right morphism in the Eilenberg-Moore
category, or a homomorphism of T-algebras:

::

    (T a, μa) -> (a, f)

Such homomorphism should map the carrier ``T a`` to ``a``. Let’s just
resurrect the forgotten evaluator ``f``. This time we’ll use it as a
homomorphism of T-algebras. Indeed, the same commuting diagram that
makes ``f`` a T-algebra may be re-interpreted to show that it’s a
homomorphism of T-algebras:

| |image3|
| We have thus defined the component of the counit natural
  transformation ``ε`` at ``(a, f)`` (an object in the category of
  T-algebras) to be ``f``.

To complete the adjunction we also need to show that the unit and the
counit satisfy triangular identites. These are:

|image4|

The first one holds because of the unit law for the monad ``T``. The
second is just the law of the T-algebra ``(a, f)``.

We have established that the two functors form an adjunction:

::

    FT ⊣ UT

Every adjunction gives rise to a monad. The round trip

::

    UT ∘ FT

is the endofunctor in C that gives rise to the corresponding monad.
Let’s see what its action on an object ``a`` is. The free algebra
created by ``FT`` is ``(T a, μa)``. The forgetful functor ``FT`` drops
the evaluator. So, indeed, we have:

::

    UT ∘ FT = T

As expected, the unit of the adjunction is the unit of the monad ``T``.

You may remember that the counint of the adjunction produces monadic
muliplication through the following formula:

::

    μ = R ∘ ε ∘ L

This is a horizontal composition of three natural transformations, two
of them being identity natural transformations mapping, respectively,
``L`` to ``L`` and ``R`` to ``R``. The one in the middle, the counit, is
a natural transformation whose component at an algebra ``(a, f)`` is
``f``.

Let’s calculate the component ``μa``. We first horizontally compose
``ε`` after ``FT``, which results in the component of ``ε`` at ``FTa``.
Since ``FT`` takes ``a`` to the algebra ``(T a, μa)``, and ``ε`` picks
the evaluator, we end up with ``μa``. Horizontal composition on the left
with ``UT`` doesn’t change anything, since the action of ``UT`` on
morphisms is trivial. So, indeed, the ``μ`` obtained from the adjunction
is the same as the ``μ`` of the original monad ``T``.

.. rubric:: The Kleisli Category
   :name: the-kleisli-category

We’ve seen the Kleisli category before. It’s a category constructed from
another category *C* and a monad ``T``. We’ll call this category
*C\ :sub:`T`*. The objects in the Kleisli category *C\ :sub:`T`* are the
objects of *C*, but the morphisms are different. A morphism ``fK`` from
``a`` to ``b`` in the Kleisli category corresponds to a morphism ``f``
from ``a`` to ``T b`` in the original category. We call this morphism a
Kleisli arrow from ``a`` to ``b``.

Composition of morphisms in the Kleisli category is defined in terms of
monadic composition of Kleisli arrows. For instance, let’s compose
``gK`` after ``fK``. In the Kleisli category we have:

::

    fK :: a -> b
    gK :: b -> c

which, in the category *C*, corresponds to:

::

    f :: a -> T b
    g :: b -> T c

We define the composition:

::

    hK = gK ∘ fK

as a Kleisli arrow in *C*

::

    h :: a -> T c
    h = μ ∘ (T g) ∘ f

In Haskell we would write it as:

::

    h = join . fmap g . f

There is a functor ``F`` from *C* to *C\ :sub:`T`* which acts trivially
on objects. On morphims, it maps ``f`` in *C* to a morphism in
*C\ :sub:`T`* by creating a Kleisli arrow that embellishes the return
value of ``f``. Given a morphism:

::

    f :: a -> b

it creates a morphism in *C\ :sub:`T`* with the corresponding Kleisli
arrow:

::

    η ∘ f

In Haskell we’d write it as:

::

    return . f

We can also define a functor ``G`` from *C\ :sub:`T`* back to *C*. It
takes an object ``a`` from the Kleisli category and maps it to an object
``T a`` in *C*. Its action on a morphism ``fK`` corresponding to a
Kleisli arrow:

::

    f :: a -> T b

is a morphism in *C*:

::

    T a -> T b

given by first lifting ``f`` and then applying ``μ``:

::

    μT b ∘ T f

In Haskell notation this would read:

::

    G fT = join . fmap f

You may recognize this as the definition of monadic bind in terms of
``join``.

It’s easy to see that the two functors form an adjunction:

::

    F ⊣ G

and their composition ``G ∘ F`` reproduces the original monad ``T``.

So this is the second adjunction that produces the same monad. In fact
there is a whole category of adjunctions ``Adj(C, T)`` that result in
the same monad ``T`` on *C*. The Kleisli adjunction we’ve just seen is
the initial object in this category, and the Eilenberg-Moore adjunction
is the terminal object.

.. rubric:: Coalgebras for Comonads
   :name: coalgebras-for-comonads

Analogous constructions can be done for any
`comonad <https://bartoszmilewski.com/2017/01/02/comonads/>`__ ``W``. We
can define a category of coalgebras that are compatible with a comonad.
They make the following diagrams commute:

|image5|

where ``coa`` is the coevaluation morphism of the coalgebra whose
carrier is ``a``:

::

    coa :: a -> W a

and ``ε`` and ``δ`` are the two natural transformations defining the
comonad (in Haskell, their components are called ``extract`` and
``duplicate``).

There is an obvious forgetful functor ``UW`` from the category of these
coalgebras to *C*. It just forgets the coevaluation. We’ll consider its
right adjoint ``FW``.

::

    UW ⊣ FW

The right adjoint to a forgetful functor is called a cofree functor.
``FW`` generates cofree coalgebras. It assigns, to an object ``a`` in
*C*, the coalgebra ``(W a, δa)``. The adjunction reproduces the original
comonad as the composite ``FW ∘ UW``.

Similarly, we can construct a co-Kleisli category with co-Kleisli arrows
and regenerate the comonad from the corresponding adjunction.

.. rubric:: Lenses
   :name: lenses

Let’s go back to our discussion of lenses. A lens can be written as a
coalgebra:

::

    coalgs :: a -> Store s a

for the functor ``Store s``:

::

    data Store s a = Store (s -> a) s

This coalgebra can be also expressed as a pair of functions:

::

    set :: a -> s -> a
    get :: a -> s

(Think of ``a`` as standing for “all,” and ``s`` as a “small” part of
it.) In terms of this pair, we have:

::

    coalgs a = Store (set a) (get a)

Here, ``a`` is a value of type ``a``. Notice that partially applied
``set`` is a function ``s->a``.

We also know that ``Store s`` is a comonad:

::

    instance Comonad (Store s) where
      extract (Store f s) = f s
      duplicate (Store f s) = Store (Store f) s

The question is: Under what conditions is a lens a coalgebra for this
comonad? The first coherence condition:

::

    εa ∘ coalg = ida

translates to:

::

    set a (get a) = a

This is the lens law that expresses the fact that if you set a field of
the structure ``a`` to its previous value, nothing changes.

The second condition:

::

    fmap coalg ∘ coalg = δa ∘ coalg

requires a little more work. First, recall the definition of ``fmap``
for the ``Store`` functor:

::

    fmap g (Store f s) = Store (g . f) s

Applying ``fmap coalg`` to the result of ``coalg`` gives us:

::

    Store (coalg . set a) (get a)

On the other hand, applying ``duplicate`` to the result of ``coalg``
produces:

::

    Store (Store (set a)) (get a)

For these two expressions to be equal, the two functions under ``Store``
must be equal when acting on an arbitrary ``s``:

::

    coalg (set a s) = Store (set a) s

Expanding ``coalg``, we get:

::

    Store (set (set a s)) (get (set a s)) = Store (set a) s

This is equivalent to two remaining lens laws. The first one:

::

    set (set a s) = set a

tells us that setting the value of a field twice is the same as setting
it once. The second law:

::

    get (set a s) = s

tells us that getting a value of a field that was set to ``s`` gives
``s`` back.

In other words, a well-behaved lens is indeed a comonad coalgebra for
the ``Store`` functor.

.. rubric:: Challenges
   :name: challenges

#. What is the action of the free functor ``F :: C -> CT`` on morphisms.
   Hint: use the naturality condition for monadic ``μ``.
#. Define the adjunction:

   ::

       UW ⊣ FW

#. Prove that the above adjunction reproduces the original comonad.

.. rubric:: Acknowledgment
   :name: acknowledgment

I’d like to thank Gershom Bazerman for helpful comments.

Next: `Ends and
Coends <https://bartoszmilewski.com/2017/03/29/ends-and-coends/>`__.

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

   <div id="crt-2116508041" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-207601446" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/?share=email>`__
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

   <div id="like-post-wrapper-3549518-8363-59ae3d008f090"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=8363&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-8363-59ae3d008f090"
   data-name="like-post-frame-3549518-8363-59ae3d008f090">

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

.. rubric:: 4 Responses to “Algebras for Monads”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-69641">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69641">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| Dan Says:

   .. raw:: html

      </div>

   `March 17, 2017 at 9:02
   pm <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/#comment-69641>`__
   With regards to the first of the triangle identities, don’t you need
   to show for all X in C that FX mapped to FGFX = FTX mapped to FX by
   (epsilon at FX) o (F.(eta at X)) is the same as the identity at FX?
   Isn’t the diagram given above looking instead at GFX =TX mapping to
   GFGFX = T.TX mapping to GFGX = TX (ie with G applied to each step)?
   Apologies if I’m missing something.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69649">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69649">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `March 18, 2017 at 12:08
   pm <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/#comment-69649>`__
   @Dan: I’m afraid I wasn’t very clear explaining triangular identities
   in the post about adjunctions. These are identities that involve
   horizontal composition of natural transformations and their
   translation to components is not immediately obvious. Fortunately,
   these are just blog posts so I can go back and update them. So check
   out the updated post on adjunctions. The relevant formulas are:

   ::

       ε L d ∘ L η d = id L d
       R ε c ∘ η R c = id R c

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70300">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70300">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `Robert
   Peszek <https://www.facebook.com/app_scoped_user_id/100004670593545/>`__
   Says:

   .. raw:: html

      </div>

   `April 30, 2017 at 5:22
   pm <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/#comment-70300>`__
   | It would be super cool to continue the algebra story because of
     this theorem:
   | “If C is a complete category, then the category of algebras of an
     endofunctor is equivalent
   | to the category of algebras over a monad of the free monad on F, if
     the latter exists.”
   | and because Free Monads lead to some amazing programming as in
     ‘Data types a la carte’ functional pearl.

   Love reading your book!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70345">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70345">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| `Henry
   Chern <https://www.facebook.com/app_scoped_user_id/1344392675639647/>`__
   Says:

   .. raw:: html

      </div>

   `May 4, 2017 at 3:56
   am <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/#comment-70345>`__
   In the expression (the second condition) “fmap colag ∘ coalg = δ\_a ∘
   coalg”, the word “colag” should be replaced by “coalg”. I was not
   mistake?

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
   reply </2017/03/14/algebras-for-monads/#respond>`__
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
-  March 14, 2017 at 2:06 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Monads <https://bartoszmilewski.com/category/monads/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2017/03/14/algebras-for-monads/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-ba92310172279063117a93ff469b56b3">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-3eb51fc4b2b8c2abfa8210387c478092">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-5c42ee0fb147266be2c21e05ac4bc58a">

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

|image16|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |image0| image:: https://bartoszmilewski.files.wordpress.com/2017/03/pigalg.png?w=122&h=133
   :class: alignnone wp-image-8438
   :width: 122px
   :height: 133px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/pigalg.png
.. |image1| image:: https://bartoszmilewski.files.wordpress.com/2017/03/talg1.png?w=174&h=128
   :class: alignnone wp-image-8430
   :width: 174px
   :height: 128px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/talg1.png
.. |image2| image:: https://bartoszmilewski.files.wordpress.com/2017/03/talg2.png?w=231&h=127
   :class: alignnone wp-image-8431
   :width: 231px
   :height: 127px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/talg2.png
.. |image3| image:: https://bartoszmilewski.files.wordpress.com/2017/03/talg31.png?w=247&h=153
   :class: alignnone wp-image-8441
   :width: 247px
   :height: 153px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/talg31.png
.. |image4| image:: https://bartoszmilewski.files.wordpress.com/2017/03/talg4.png?w=446&h=156
   :class: alignnone wp-image-8433
   :width: 446px
   :height: 156px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/talg4.png
.. |image5| image:: https://bartoszmilewski.files.wordpress.com/2017/03/talg5.png?w=449&h=130
   :class: alignnone wp-image-8434
   :width: 449px
   :height: 130px
   :target: https://bartoszmilewski.files.wordpress.com/2017/03/talg5.png
.. |image6| image:: https://2.gravatar.com/avatar/ba92310172279063117a93ff469b56b3?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/3eb51fc4b2b8c2abfa8210387c478092?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://i0.wp.com/graph.facebook.com/v2.2/1344392675639647/picture?q=type%3Dlarge%26_md5%3D2f5bf20a57a614960d5e6fe6554c5e9f&resize=48%2C48
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
.. |image16| image:: https://pixel.wp.com/b.gif?v=noscript

