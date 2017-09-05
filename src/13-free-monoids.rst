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
   class="post-4377 post type-post status-publish format-standard hentry category-category-theory category-functional-programming category-haskell category-programming">

July 21, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Free Monoids
   :name: free-monoids
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Functional
Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Programming <https://bartoszmilewski.com/category/programming/>`__
`[14]
Comments <https://bartoszmilewski.com/2015/07/21/free-monoids/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4377" class="pd-rating">

.. raw:: html

   </div>

    This is part 13 of Categories for Programmers. Previously: `Limits
    and
    Colimits <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

Monoids are an important concept in both category theory and in
programming. Categories correspond to strongly typed languages, monoids
to untyped languages. That’s because in a monoid you can compose any two
arrows, just as in an untyped language you can compose any two functions
(of course, you may end up with a runtime error when you execute your
program).

We’ve seen that a monoid may be described as a category with a single
object, where all logic is encoded in the rules of morphism composition.
This categorical model is fully equivalent to the more traditional
set-theoretical definition of a monoid, where we “multiply” two elements
of a set to get a third element. This process of “multiplication” can be
further dissected into first forming a pair of elements and then
identifying this pair with an existing element — their “product.”

What happens when we forgo the second part of multiplication — the
identification of pairs with existing elements? We can, for instance,
start with an arbitrary set, form all possible pairs of elements, and
call them new elements. Then we’ll pair these new elements with all
possible elements, and so on. This is a chain reaction — we’ll keep
adding new elements forever. The result, an infinite set, will be
*almost* a monoid. But a monoid also needs a unit element and the law of
associativity. No problem, we can add a special unit element and
identify some of the pairs — just enough to support the unit and
associativity laws.

Let’s see how this works in a simple example. Let’s start with a set of
two elements, ``{a, b}``. We’ll call them the generators of the free
monoid. First, we’ll add a special element ``e`` to serve as the unit.
Next we’ll add all the pairs of elements and call them “products”. The
product of ``a`` and ``b`` will be the pair ``(a, b)``. The product of
``b`` and ``a`` will be the pair ``(b, a)``, the product of ``a`` with
``a`` will be ``(a, a)``, the product of ``b`` with ``b`` will be
``(b, b)``. We can also form pairs with ``e``, like ``(a, e)``,
``(e, b)``, etc., but we’ll identify them with ``a``, ``b``, etc. So in
this round we’ll only add ``(a, a)``, ``(a, b)`` and ``(b, a)`` and
``(b, b)``, and end up with the set
``{e, a, b, (a, a), (a, b), (b, a), (b, b)}``.

|Bunnies|

In the next round we’ll keep adding elements like: ``(a, (a, b))``,
``((a, b), a)``, etc. At this point we’ll have to make sure that
associativity holds, so we’ll identify ``(a, (b, a))`` with
``((a, b), a)``, etc. In other words, we won’t be needing internal
parentheses.

You can guess what the final result of this process will be: we’ll
create all possible lists of ``a``\ s and ``b``\ s. In fact, if we
represent ``e`` as an empty list, we can see that our “multiplication”
is nothing but list concatenation.

This kind of construction, in which you keep generating all possible
combinations of elements, and perform the minimum number of
identifications — just enough to uphold the laws — is called a free
construction. What we have just done is to construct a *free monoid*
from the set of generators ``{a, b}``.

.. rubric:: Free Monoid in Haskell
   :name: free-monoid-in-haskell

A two-element set in Haskell is equivalent to the type ``Bool``, and the
free monoid generated by this set is equivalent to the type ``[Bool]``
(list of ``Bool``). (I am deliberately ignoring problems with infinite
lists.)

A monoid in Haskell is defined by the type class:

::

    class Monoid m where
        mempty  :: m
        mappend :: m -> m -> m

This just says that every ``Monoid`` must have a neutral element, which
is called ``mempty``, and a binary function (multiplication) called
``mappend``. The unit and associativity laws cannot be expressed in
Haskell and must be verified by the programmer every time a monoid is
instantiated.

The fact that a list of any type forms a monoid is described by this
instance definition:

::

    instance Monoid [a] where
        mempty  = []
        mappend = (++)

It states that an empty list ``[]`` is the unit element, and list
concatenation ``(++)`` is the binary operation.

As we have seen, a list of type ``a`` corresponds to a free monoid with
the set ``a`` serving as generators. The set of natural numbers with
multiplication is not a free monoid, because we identify lots of
products. Compare for instance:

::

    2 * 3 = 6
    [2] ++ [3] = [2, 3] // not the same as [6]

That was easy, but the question is, can we perform this free
construction in category theory, where we are not allowed to look inside
objects? We’ll use our workhorse: the universal construction.

The second interesting question is, can any monoid be obtained from some
free monoid by identifying more than the minimum number of elements
required by the laws? I’ll show you that this follows directly from the
universal construction.

.. rubric:: Free Monoid Universal Construction
   :name: free-monoid-universal-construction

If you recall our previous experiences with universal constructions, you
might notice that it’s not so much about constructing something as about
selecting an object that best fits a given pattern. So if we want to use
the universal construction to “construct” a free monoid, we have to
consider a whole bunch of monoids from which to pick one. We need a
whole category of monoids to chose from. But do monoids form a category?

Let’s first look at monoids as sets equipped with additional structure
defined by unit and multiplication. We’ll pick as morphisms those
functions that preserve the monoidal structure. Such
structure-preserving functions are called *homomorphisms*. A monoid
homomorphism must map the product of two elements to the product of the
mapping of the two elements:

::

    h (a * b) = h a * h b

| and it must map unit to unit.
| For instance, consider a homomorphism from lists of integers to
  integers. If we map ``[2]`` to 2 and ``[3]`` to 3, we have to map
  ``[2, 3]`` to 6, because concatenation

::

    [2] ++ [3] = [2, 3]

becomes multiplication

::

    2 * 3 = 6

Now let’s forget about the internal structure of individual monoids, and
only look at them as objects with corresponding morphisms. You get a
category **Mon** of monoids.

Okay, maybe before we forget about internal structure, let us notice an
important property. Every object of **Mon** can be trivially mapped to a
set. It’s just the set of its elements. This set is called the
*underlying* set. In fact, not only can we map objects of **Mon** to
sets, but we can also map morphisms of **Mon** (homomorphisms) to
functions. Again, this seems sort of trivial, but it will become useful
soon. This mapping of objects and morphisms from **Mon** to **Set** is
in fact a functor. Since this functor “forgets” the monoidal structure —
once we are inside a plain set, we no longer distinguish the unit
element or care about multiplication — it’s called a *forgetful
functor*. Forgetful functors come up regularly in category theory.

We now have two different views of **Mon**. We can treat it just like
any other category with objects and morphisms. In that view, we don’t
see the internal structure of monoids. All we can say about a particular
object in **Mon** is that it connects to itself and to other objects
through morphisms. The “multiplication” table of morphisms — the
composition rules — are derived from the other view: monoids-as-sets. By
going to category theory we haven’t lost this view completely — we can
still access it through our forgetful functor.

To apply the universal construction, we need to define a special
property that would let us search through the category of monoids and
pick the best candidate for a free monoid. But a free monoid is defined
by its generators. Different choices of generators produce different
free monoids (a list of ``Bool`` is not the same as a list of ``Int``).
Our construction must start with a set of generators. So we’re back to
sets!

That’s where the forgetful functor comes into play. We can use it to
X-ray our monoids. We can identify the generators in the X-ray images of
those blobs. Here’s how it works:

We start with a set of generators, ``x``. That’s a set in **Set**.

The pattern we are going to match consists of a monoid ``m`` — an object
of **Mon** — and a function ``p`` in **Set**:

::

    p :: x -> U m

where ``U`` is our forgetful functor from **Mon** to **Set**. This is a
weird heterogeneous pattern — half in **Mon** and half in **Set**.

The idea is that the function ``p`` will identify the set of generators
inside the X-ray image of ``m``. It doesn’t matter that functions may be
lousy at identifying points inside sets (they may collapse them). It
will all be sorted out by the universal construction, which will pick
the best representative of this pattern.

|Monoid Pattern|

We also have to define the ranking among candidates. Suppose we have
another candidate: a monoid ``n`` and a function that identifies the
generators in its X-ray image:

::

    q :: x -> U n

We’ll say that ``m`` is better than ``n`` if there is a morphism of
monoids (that’s a structure-preserving homomorphism):

::

    h :: m -> n

whose image under ``U`` (remember, ``U`` is a functor, so it maps
morphisms to functions) factorizes through ``p``:

::

    q = U h . p

If you think of ``p`` as selecting the generators in ``m``; and ``q`` as
selecting “the same” generators in ``n``; then you can think of ``h`` as
mapping these generators between the two monoids. Remember that ``h``,
by definition, preserves the monoidal structure. It means that a product
of two generators in one monoid will be mapped to a product of the
corresponding two generators in the second monoid, and so on.

|Monoid Ranking|

This ranking may be used to find the best candidate — the free monoid.
Here’s the definition:

We’ll say that ``m`` (together with the function ``p``) is the **free
monoid** with the generators ``x`` if and only if there is a *unique*
morphism ``h`` from ``m`` to any other monoid ``n`` (together with the
function ``q``) that satisfies the above factorization property.

Incidentally, this answers our second question. The function ``U h`` is
the one that has the power to collapse multiple elements of ``U m`` to a
single element of ``U n``. This collapse corresponds to identifying some
elements of the free monoid. Therefore any monoid with generators ``x``
can be obtained from the free monoid based on ``x`` by identifying some
of the elements. The free monoid is the one where only the bare minimum
of identifications have been made.

We’ll come back to free monoids when we talk about adjunctions.

.. rubric:: Challenges
   :name: challenges

#. You might think (as I did, originally) that the requirement that a
   homomorphism of monoids preserve the unit is redundant. After all, we
   know that for all ``a``

   ::

       h a * h e = h (a * e) = h a

   So ``h e`` acts like a right unit (and, by analogy, as a left unit).
   The problem is that ``h a``, for all ``a`` might only cover a
   sub-monoid of the target monoid. There may be a “true” unit outside
   of the image of ``h``. Show that an isomorphism between monoids that
   preserves multiplication must automatically preserve unit.

#. Consider a monoid homomorphism from lists of integers with
   concatenation to integers with multiplication. What is the image of
   the empty list ``[]``? Assume that all singleton lists are mapped to
   the integers they contain, that is ``[3]`` is mapped to 3, etc.
   What’s the image of ``[1, 2, 3, 4]``? How many different lists map to
   the integer 12? Is there any other homomorphism between the two
   monoids?
#. What is the free monoid generated by a one-element set? Can you see
   what it’s isomorphic to?

Next: `Representable
Functors <https://bartoszmilewski.com/2015/07/29/representable-functors/>`__.

.. rubric:: Acknowledgments
   :name: acknowledgments

| I’d like to thank Gershom Bazerman for checking my math and logic, and
  André van Meulebrouck, who has been volunteering his editing help
  throughout this series of posts.
| `Follow @BartoszMilewski <https://twitter.com/BartoszMilewski>`__

.. raw:: html

   <div id="geo-post-4377" class="geo geo-post" style="display: none">

43.193044 11.286147

.. raw:: html

   </div>

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

   <div id="crt-230991503" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1268088963" style="width:300px;height:250px;">

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

   <div class="geolocation-chip">

Murlo, Province of Siena, Italy

.. raw:: html

   </div>

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

-  `Reddit <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/07/21/free-monoids/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4377-59ae3c3cafca1"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4377&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4377-59ae3c3cafca1"
   data-name="like-post-frame-3549518-4377-59ae3c3cafca1">

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

.. rubric:: 14 Responses to “Free Monoids”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-50509">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50509">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Валентин Тихомиров
   (@valtih1978) <http://twitter.com/valtih1978>`__ Says:

   .. raw:: html

      </div>

   `July 23, 2015 at 9:00
   am <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-50509>`__
   I do not see which freemon rule is broken in 2\*3=6, why should [2,3]
   equal [6]? Is it the problem that mapping is not one-to-one?

   It is left unclear which morphims are meant in
   ``let's look at individual monoids as objects with corresponding morphisms``.
   Are you talking about relations within objects or between objects?

   and

       The “multiplication” table of morphisms — the composition rules —
       are derived from the other view: monoids-as-sets. By going to
       category theory we haven’t lost this view completely — we can
       still access it through our forgetful functor.

   Do you mean that we can come up with composition rules that are
   Set-based rather than monoid-based, and, thus, do not care about
   preserving monoid structures? My initial impression was that
   morhpisms in Mon need to be homomprphic, preserving the structure of
   the source monoids and composability you are talking about must
   comply this. Whereas this seems difficult if forgetful functors
   cannot recover the structure.

   The intro explaining that Monoids stand for single untyped type in
   type system is brilliant.

   The catch in challenge seems to be that [] = [1], which fails the
   isomorphism.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50519">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50519">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 23, 2015 at 1:01
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-50519>`__
   @Valentin: I rewrote some paragraphs. Does it make it clearer?

   I’m not sure what catch you’re talking about.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52603">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52603">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `Lee Wei Yeong <http://www.facebook.com/503007554>`__ Says:

   .. raw:: html

      </div>

   `September 4, 2015 at 6:56
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-52603>`__
   This should be named “Part 13”, not “Part 12”.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52621">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52621">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `September 5, 2015 at 12:53
   am <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-52621>`__
   Fixed!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52927">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52927">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| kram1032 Says:

   .. raw:: html

      </div>

   `September 8, 2015 at 2:54
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-52927>`__
   There is no link to “Representable Functors” yet

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52984">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52984">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| Sam Cole Says:

   .. raw:: html

      </div>

   `September 9, 2015 at 6:28
   am <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-52984>`__
       I do not see which freemon rule is broken in 2\*3=6, why should
       [2,3] equal [6]? Is it the problem that mapping is not
       one-to-one?

   This confused me as well. The natural numbers do form a monoid under
   multiplication (right?). I think the point is it’s not a free monoid
   as you can’t choose a finite set of generators, though couldn’t you
   choose the primes as an infinite set of generators and under
   multiplication and end up with a free monoid of naturals that way?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-53331">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-53331">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `September 13, 2015 at 8:08
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-53331>`__
   @Sam: The simplest counterexample is that in the monoid of natural
   numbers with multiplication you identify 2\ *3 with 3*\ 2. This
   identification does not follow from monoid laws, so it’s not a free
   monoid.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54926">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54926">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 4:17
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-54926>`__
   I find the first challenge confusing: it’s too trivial! In this
   section you say that a monoid homomorphism “must map unit to unit”.
   In the challenge you define a monoid isomorphism as a certain type of
   monoid homomorphism. Therefore, it “must map unit to unit”. What is
   there to prove? Am I missing something?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54930">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54930">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 4:57
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-54930>`__
   Thinking about your last reply to Sam, it occurs to me that there are
   least two quite different ways in which “freedom” is lost in going
   from the monoid of lists of integers (LoI) to the monoid of integer
   multiplication (IM). The first one comes from forgetting the ordering
   of entries in the lists of integers (which, BTW, is not specific to
   integers at all), the second one comes from applying integer
   multiplication to the elements of the list. This suggests that the
   mapping from LoI to IM factors through a mapping from LoI to a third
   monoid X, for which I don’t have a good name. X is the monoid one
   gets from LoI by identifying all lists of integers that “hold the
   same elements” (i.e. disregarding order, but preserving repetitions;
   e.g. the lists [1, 2, 1] and [2, 1, 1] are treated as equivalent, but
   the lists [1, 2, 1] and [1, 2] are still considered distinct). The
   “multiplication” for this X monoid could still be ++, but it could be
   something more elaborate, like ++ followed by sorting.

   As I mentioned, this order-forgetting business is not specific to
   integers. In fact, one can envision a new type constructor, Multiset,
   that takes a type as an argument, and returns the type Multiset-of-a.
   (A multiset is like a list, except that equality of multisets
   disregards the ordering of elements.) With these definitions,
   Multiset is a functor analogous to the List functor, and the
   “forgetting-of-ordering” map described earlier is the integer
   component of a natural transformation from List to Multiset.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54931">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54931">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 5:00
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-54931>`__
   This sentence I wrote is confusing:

       | The “multiplication” for this X monoid could still be ++, but
         it could be
       | something more elaborate, like ++ followed by sorting.

   I meant to write

       | The “multiplication” for this X monoid could still be ++, but
         it could
       | ALSO be something more elaborate, like ++ followed by sorting.

   Sorry for this ineptitude.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54942">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54942">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 9:33
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-54942>`__
   Yes, challenge 1 was a result of editing the original one after an
   error was discovered and, as we all know, a quick bug fix usually
   results in a new bug. I hope the new version makes more sense.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54944">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54944">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 9:51
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-54944>`__
   @weekendwarrior: What you are suggesting is based on the distinction
   between a non-Abelian monoid of lists (with concatenation) and the
   Abelian monoid of integers (with multiplication).” Abelian” just
   means commutative. The process of identifying lists that only differ
   by the order of elements is called “taking a quotient.”

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-57700">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-57700">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| Ming Says:

   .. raw:: html

      </div>

   `November 27, 2015 at 8:01
   pm <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-57700>`__
   I’m learning category theory. Your explanation about the monoid is
   very intuitive and helpful. Thank you!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-62001">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-62001">

   .. raw:: html

      <div class="comment-author vcard">

   `Monoids and their efficiency in practice
   \| <http://myhaskelljournal.com/monoids-and-their-efficiency-in-practice/>`__
   Says:

   .. raw:: html

      </div>

   `February 6, 2016 at 1:33
   am <https://bartoszmilewski.com/2015/07/21/free-monoids/#comment-62001>`__
   […] You can read this very beautiful article of Bartosz
   Milewski: Free Monoids. […]

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
   reply </2015/07/21/free-monoids/#respond>`__
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
-  July 21, 2015 at 3:08 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Functional
   Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Programming <https://bartoszmilewski.com/category/programming/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/07/21/free-monoids/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-d727cca8faaedd04eb19dd840b9bbf16">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-79082c22d49ed5d836e3eae8da00dada">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-02b8f7ed2c25ec237e56603cd2669b4e">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-96a95dc766cb41438c9ce3bb6fa5ee49">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-4aa6db921795b84b986eb4aac8ffd569">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-9639b20361bf90d827ac66aa0bcbf2fc">

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

|image22|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Bunnies| image:: https://bartoszmilewski.files.wordpress.com/2015/07/bunnies.jpg?w=436&h=201
   :class: alignnone wp-image-4840
   :width: 436px
   :height: 201px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/bunnies.jpg
.. |Monoid Pattern| image:: https://bartoszmilewski.files.wordpress.com/2015/07/monoid-pattern.jpg?w=300&h=185
   :class: alignnone size-medium wp-image-4841
   :width: 300px
   :height: 185px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/monoid-pattern.jpg
.. |Monoid Ranking| image:: https://bartoszmilewski.files.wordpress.com/2015/07/monoid-ranking.jpg?w=300&h=221
   :class: alignnone size-medium wp-image-4842
   :width: 300px
   :height: 221px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/monoid-ranking.jpg
.. |image3| image:: https://i1.wp.com/pbs.twimg.com/profile_images/617673762893201410/I9g8EyCy_normal.png?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://i2.wp.com/graph.facebook.com/v2.2/503007554/picture?q=type%3Dlarge%26_md5%3D37ad4f51a7116168e6577ed60411af1f&resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://0.gravatar.com/avatar/02b8f7ed2c25ec237e56603cd2669b4e?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/96a95dc766cb41438c9ce3bb6fa5ee49?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://0.gravatar.com/avatar/9639b20361bf90d827ac66aa0bcbf2fc?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image22| image:: https://pixel.wp.com/b.gif?v=noscript

