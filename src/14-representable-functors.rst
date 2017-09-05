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
   class="post-4850 post type-post status-publish format-standard hentry category-category-theory category-functional-programming">

July 29, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Representable Functors
   :name: representable-functors
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Functional
Programming <https://bartoszmilewski.com/category/functional-programming/>`__
`[4]
Comments <https://bartoszmilewski.com/2015/07/29/representable-functors/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4850" class="pd-rating">

.. raw:: html

   </div>

    This is part 14 of Categories for Programmers. Previously: `Free
    Monoids <https://bartoszmilewski.com/2015/07/21/free-monoids/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

It’s about time we had a little talk about sets. Mathematicians have a
love/hate relationship with set theory. It’s the assembly language of
mathematics — at least it used to be. Category theory tries to step away
from set theory, to some extent. For instance, it’s a known fact that
the set of all sets doesn’t exist, but the category of all sets,
**Set**, does. So that’s good. On the other hand, we assume that
morphisms between any two objects in a category form a set. We even
called it a hom-set. To be fair, there is a branch of category theory
where morphisms don’t form sets. Instead they are objects in another
category. Those categories that use hom-objects rather than hom-sets,
are called *enriched* categories. In what follows, though, we’ll stick
to categories with good old-fashioned hom-sets.

A set is the closest thing to a featureless blob you can get outside of
categorical objects. A set has elements, but you can’t say much about
these elements. If you have a finite set, you can count the elements.
You can kind of count the elements of an inifinite set using cardinal
numbers. The set of natural numbers, for instance, is smaller than the
set of real numbers, even though both are infinite. But, maybe
surprisingly, a set of rational numbers is the same size as the set of
natural numbers.

Other than that, all the information about sets can be encoded in
functions between them — especially the invertible ones called
isomorphisms. For all intents and purposes isomorphic sets are
identical. Before I summon the wrath of foundational mathematicians, let
me explain that the distinction between equality and isomorphism is of
fundamental importance. In fact it is one of the main concerns of the
latest branch of mathematics, the Homotopy Type Theory (HoTT). I’m
mentioning HoTT because it’s a pure mathematical theory that takes
inspiration from computation, and one of its main proponents, Vladimir
Voevodsky, had a major epiphany while studying the Coq theorem prover.
The interaction between mathematics and programming goes both ways.

The important lesson about sets is that it’s okay to compare sets of
unlike elements. For instance, we can say that a given set of natural
transformations is isomorphic to some set of morphisms, because a set is
just a set. Isomorphism in this case just means that for every natural
transformation from one set there is a unique morphism from the other
set and vice versa. They can be paired against each other. You can’t
compare apples with oranges, if they are objects from different
categories, but you can compare sets of apples against sets of oranges.
Often transforming a categorical problem into a set-theoretical problem
gives us the necessary insight or even lets us prove valuable theorems.

.. rubric:: The Hom Functor
   :name: the-hom-functor

Every category comes equipped with a canonical family of mappings to
**Set**. Those mappings are in fact functors, so they preserve the
structure of the category. Let’s build one such mapping.

| Let’s fix one object ``a`` in *C* and pick another object ``x`` also
  in *C*. The hom-set ``C(a, x)`` is a set, an object in **Set**. When
  we vary ``x``, keeping ``a`` fixed, ``C(a, x)`` will also vary in
  **Set**. Thus we have a mapping from ``x`` to **Set**.
| |Hom-Set|

If we want to stress the fact that we are considering the hom-set as a
mapping in its second argument, we use the notation:

::

    C(a, -)

with the dash serving as the placeholder for the argument.

This mapping of objects is easily extended to the mapping of morphisms.
Let’s take a morphism ``f`` in *C* between two arbitrary objects ``x``
and ``y``. The object ``x`` is mapped to the set ``C(a, x)``, and the
object ``y`` is mapped to ``C(a, y)``, under the mapping we have just
defined. If this mapping is to be a functor, ``f`` must be mapped to a
function between the two sets:

::

    C(a, x) -> C(a, y)

Let’s define this function point-wise, that is for each argument
separately. For the argument we should pick an arbitrary element of
``C(a, x)`` — let’s call it ``h``. Morphisms are composable, if they
match end to end. It so happens that the target of ``h`` matches the
source of ``f``, so their composition:

::

    f ∘ h :: a -> y

is a morphism going from ``a`` to ``y``. It is therefore a member of
``C(a, y)``.

|Hom Functor|

We have just found our function from ``C(a, x)`` to ``C(a, y)``, which
can serve as the image of ``f``. If there is no danger of confusion,
we’ll write this lifted function as:

::

    C(a, f)

and its action on a morphism ``h`` as:

::

    C(a, f) h = f ∘ h

Since this construction works in any category, it must also work in the
category of Haskell types. In Haskell, the hom-functor is better known
as the ``Reader`` functor:

::

    type Reader a x = a -> x

::

    instance Functor (Reader a) where
        fmap f h = f . h

Now let’s consider what happens if, instead of fixing the source of the
hom-set, we fix the target. In other words, we’re asking the question if
the mapping

::

    C(-, a)

is also a functor. It is, but instead of being covariant, it’s
contravariant. That’s because the same kind of matching of morphisms end
to end results in postcomposition by ``f``; rather than precomposition,
as was the case with ``C(a, -)``.

We have already seen this contravariant functor in Haskell. We called it
``Op``:

::

    type Op a x = x -> a

::

    instance Contravariant (Op a) where
        contramap f h = h . f

Finally, if we let both objects vary, we get a profunctor ``C(-, =)``,
which is contravariant in the first argument and covariant in the second
(to underline the fact that the two arguments may vary independently, we
use a double dash as the second placeholder). We have seen this
profunctor before, when we talked about functoriality:

::

    instance Profunctor (->) where
      dimap ab cd bc = cd . bc . ab
      lmap = flip (.)
      rmap = (.)

The important lesson is that this observation holds in any category: the
mapping of objects to hom-sets is functorial. Since contravariance is
equivalent to a mapping from the opposite category, we can state this
fact succintly as:

::

    C(-, =) :: Cop × C -> Set

.. rubric:: Representable Functors
   :name: representable-functors-1

We’ve seen that, for every choice of an object ``a`` in *C*, we get a
functor from *C* to **Set**. This kind of structure-preserving mapping
to **Set** is often called a *representation*. We are representing
objects and morphisms of *C* as sets and functions in **Set**.

The functor ``C(a, -)`` itself is sometimes called representable. More
generally, any functor ``F`` that is naturally isomorphic to the
hom-functor, for some choice of ``a``, is called *representable*. Such
functor must necessarily be **Set**-valued, since ``C(a, -)`` is.

I said before that we often think of isomorphic sets as identical. More
generally, we think of isomorphic *objects* in a category as identical.
That’s because objects have no structure other than their relation to
other objects (and themselves) through morphisms.

For instance, we’ve previously talked about the category of monoids,
**Mon**, that was initially modeled with sets. But we were careful to
pick as morphisms only those functions that preserved the monoidal
structure of those sets. So if two objects in **Mon** are isomorphic,
meaning there is an invertible morphism between them, they have exactly
the same structure. If we peeked at the sets and functions that they
were based upon, we’d see that the unit element of one monoid was mapped
to the unit element of another, and that a product of two elements was
mapped to the product of their mappings.

The same reasoning can be applied to functors. Functors between two
categories form a category in which natural transformations play the
role of morphisms. So two functors are isomorphic, and can be thought of
as identical, if there is an invertible natural transformation between
them.

Let’s analyze the definition of the representable functor from this
perspective. For ``F`` to be representable we require that: There be an
object ``a`` in *C*; one natural transformation α from ``C(a, -)`` to
``F``; another natural transformation, β, in the opposite direction; and
that their composition be the identity natural transformation.

Let’s look at the component of α at some object ``x``. It’s a function
in **Set**:

::

    αx :: C(a, x) -> F x

The naturality condition for this transformation tells us that, for any
morphism ``f`` from ``x`` to ``y``, the following diagram commutes:

::

    F f ∘ αx = αy ∘ C(a, f)

In Haskell, we would replace natural transformations with polymorphic
functions:

::

    alpha :: forall x. (a -> x) -> F x

with the optional ``forall`` quantifier. The naturality condition

::

    fmap f . alpha = alpha . fmap f

is automatically satisfied due to parametricity (it’s one of those
theorems for free I mentioned earlier), with the understanding that
``fmap`` on the left is defined by the functor ``F``, whereas the one on
the right is defined by the reader functor. Since ``fmap`` for reader is
just function precomposition, we can be even more explicit. Acting on
``h``, an element of ``C(a, x)``, the naturality condition simplifies
to:

::

    fmap f (alpha h) = alpha (f . h)

The other transformation, ``beta``, goes the opposite way:

::

    beta :: forall x. F x -> (a -> x)

It must respect naturality conditions, and it must be the inverse of α:

::

    α ∘ β = id = β ∘ α

We will see later that a natural transformation from ``C(a, -)`` to any
**Set**-valued functor always exists (Yoneda’s lemma) but it is not
necessarily invertible.

Let me give you an example in Haskell with the list functor and ``Int``
as ``a``. Here’s a natural transformation that does the job:

::

    alpha :: forall x. (Int -> x) -> [x]
    alpha h = map h [12]

I have arbitrarily picked the number 12 and created a singleton list
with it. I can then ``fmap`` the function ``h`` over this list and get a
list of the type returned by ``h``. (There are actually as many such
transformations as there are list of integers.)

The naturality condition is equivalent to the composability of ``map``
(the list version of ``fmap``):

::

    map f (map h [12]) = map (f . h) [12]

But if we tried to find the inverse transformation, we would have to go
from a list of arbitrary type ``x`` to a function returning ``x``:

::

    beta :: forall x. [x] -> (Int -> x)

You might think of retrieving an ``x`` from the list, e.g., using
``head``, but that won’t work for an empty list. Notice that there is no
choice for the type ``a`` (in place of ``Int``) that would work here. So
the list functor is not representable.

Remember when we talked about Haskell (endo-) functors being a little
like containers? In the same vein we can think of representable functors
as containers for storing memoized results of function calls (the
members of hom-sets in Haskell are just functions). The representing
object, the type ``a`` in ``C(a, -)``, is thought of as the key type,
with which we can access the tabulated values of a function. The
transformation we called α is called ``tabulate``, and its inverse, β,
is called ``index``. Here’s a (slightly simplified) ``Representable``
class definition:

::

    class Representable f where
       type Rep f :: *
       tabulate :: (Rep f -> x) -> f x
       index    :: f x -> Rep f -> x

Notice that the representing type, our ``a``, which is called ``Rep f``
here, is part of the definition of ``Representable``. The star just
means that ``Rep f`` is a type (as opposed to a type constructor, or
other more exotic kinds).

Infinite lists, or streams, which cannot be empty, are representable.

::

    data Stream x = Cons x (Stream x)

You can think of them as memoized values of a function taking an
``Integer`` as an argument. (Strictly speaking, I should be using
non-negative natural numbers, but I didn’t want to complicate the code.)

To ``tabulate`` such a function, you create an infinite stream of
values. Of course, this is only possible because Haskell is lazy. The
values are evaluated on demand. You access the memoized values using
``index``:

::

    instance Representable Stream where
        type Rep Stream = Integer
        tabulate f = Cons (f 0) (tabulate (f . (+1)))
        index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

It’s interesting that you can implement a single memoization scheme to
cover a whole family of functions with arbitrary return types.

Representability for contravariant functors is similarly defined, except
that we keep the second argument of ``C(-, a)`` fixed. Or, equivalently,
we may consider functors from *C*\ :sup:`op` to **Set**, because
``Cop(a, -)`` is the same as ``C(-, a)``.

There is an interesting twist to representability. Remember that
hom-sets can internally be treated as exponential objects, in cartesian
closed categories. The hom-set ``C(a, x)`` is equivalent to ``xa``, and
for a representable functor ``F`` we can write:

::

    -a = F

Let’s take the logarithm of both sides, just for kicks:

::

    a = log F

Of course, this is a purely formal transformation, but if you know some
of the properties of logarithms, it is quite helpful. In particular, it
turns out that functors that are based on product types can be
represented with sum types, and that sum-type functors are not in
general representable (example: the list functor).

Finally, notice that a representable functor gives us two different
implementations of the same thing — one a function, one a data
structure. They have exactly the same content — the same values are
retrieved using the same keys. That’s the sense of “sameness” I was
talking about. Two naturally isomorphic functors are identical as far as
their contents are involved. On the other hand, the two representations
are often implemented differently and may have different performance
characteristics. Memoization is used as a performance enhancement and
may lead to substantially reduced run times. Being able to generate
different representations of the same underlying computation is very
valuable in practice. So, surprisingly, even though it’s not concerned
with performance at all, category theory provides ample opportunities to
explore alternative implementations that have practical value.

.. rubric:: Challenges
   :name: challenges

#. Show that the hom-functors map identity morphisms in *C* to
   corresponding identity functions in **Set**.
#. Show that ``Maybe`` is not representable.
#. Is the ``Reader`` functor representable?
#. Using ``Stream`` representation, memoize a function that squares its
   argument.
#. Show that ``tabulate`` and ``index`` for ``Stream`` are indeed the
   inverse of each other. (Hint: use induction.)
#. The functor:

   ::

       Pair a = Pair a a

   is representable. Can you guess the type that represents it?
   Implement ``tabulate`` and ``index``.

.. rubric:: Bibliography
   :name: bibliography

#. The Catsters video about `representable
   functors <https://www.youtube.com/watch?v=4QgjKUzyrhM>`__.

Next: `The Yoneda
Lemma <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/>`__.

.. rubric:: Acknowledgments
   :name: acknowledgments

| I’d like to thank Gershom Bazerman for checking my math and logic, and
  André van Meulebrouck, who has been volunteering his editing help
  throughout this series of posts.
| `Follow @BartoszMilewski <https://twitter.com/BartoszMilewski>`__

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

   <div id="crt-2117037124" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-953022710" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/07/29/representable-functors/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4850-59ae3c4a8caef"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4850&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4850-59ae3c4a8caef"
   data-name="like-post-frame-3549518-4850-59ae3c4a8caef">

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

.. rubric:: 4 Responses to “Representable Functors”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-50913">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50913">

   .. raw:: html

      <div class="comment-author vcard">

   |image2| Peter Says:

   .. raw:: html

      </div>

   `July 29, 2015 at 11:48
   pm <https://bartoszmilewski.com/2015/07/29/representable-functors/#comment-50913>`__
   Where you say “It requires that there be an object a in C; one
   natural transformation α from C(a, \_) to F;” F is any functor?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50948">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50948">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 30, 2015 at 10:16
   am <https://bartoszmilewski.com/2015/07/29/representable-functors/#comment-50948>`__
   F is the functor from the definition: “any functor that is naturally
   isomorphic to the hom-functor, for some choice of a, is called
   representable.” I’ll edit it to make it clrearer.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-53324">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-53324">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| `Leonid
   Romanov <https://www.facebook.com/app_scoped_user_id/1021414717890454/>`__
   Says:

   .. raw:: html

      </div>

   `September 13, 2015 at 6:37
   pm <https://bartoszmilewski.com/2015/07/29/representable-functors/#comment-53324>`__
   Hello! Suppose that for a fixed “a” in C(a,-) there are two objects,
   “x1” and “x2″, for which there are no arrow from ” a”. This means
   that Hom functor maps these object to empty sets. My question is: are
   these empty sets considered to be the same object in Set or are they
   distinct Set objects (albeit with the same internal structure)?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-53328">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-53328">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `September 13, 2015 at 7:35
   pm <https://bartoszmilewski.com/2015/07/29/representable-functors/#comment-53328>`__
   @Leonid: There is only one empty set in **Set**.

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
   reply </2015/07/29/representable-functors/#respond>`__
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
-  July 29, 2015 at 3:09 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Functional
   Programming <https://bartoszmilewski.com/category/functional-programming/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/07/29/representable-functors/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Blog at WordPress.com. <https://wordpress.com/?ref=footer_blog>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-a2c60200f7fd9ae7c1d21bbcb5e9f984">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-fd5954f07565707ec576e20ec82bfafb">

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

|image12|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Hom-Set| image:: https://bartoszmilewski.files.wordpress.com/2015/07/hom-set.jpg?w=300&h=181
   :class: alignnone size-medium wp-image-4780
   :width: 300px
   :height: 181px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/hom-set.jpg
.. |Hom Functor| image:: https://bartoszmilewski.files.wordpress.com/2015/07/hom-functor.jpg?w=300&h=189
   :class: alignnone size-medium wp-image-4781
   :width: 300px
   :height: 189px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/hom-functor.jpg
.. |image2| image:: https://1.gravatar.com/avatar/a2c60200f7fd9ae7c1d21bbcb5e9f984?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image3| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://i0.wp.com/graph.facebook.com/v2.2/1021414717890454/picture?q=type%3Dlarge%26_md5%3D412f22cf83e5755e16e8566e404f7dad&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image12| image:: https://pixel.wp.com/b.gif?v=noscript

