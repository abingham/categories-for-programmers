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
   class="post-8703 post type-post status-publish format-standard hentry category-category-theory">

May 13, 2017

.. raw:: html

   <div class="post-info">

.. rubric:: Enriched Categories
   :name: enriched-categories
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__
`[9]
Comments <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_8703" class="pd-rating">

.. raw:: html

   </div>

    This is part 28 of Categories for Programmers. Previously: `Kan
    Extensions <https://bartoszmilewski.com/2017/04/17/kan-extensions/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

A category is *small* if its objects form a set. But we know that there
are things larger than sets. Famously, a set of all sets cannot be
formed within the standard set theory (the Zermelo-Fraenkel theory,
optionally augmented with the Axiom of Choice). So a category of all
sets must be large. There are mathematical tricks like Grothendieck
universes that can be used to define collections that go beyond sets.
These tricks let us talk about large categories.

A category is *locally small* if morphisms between any two objects form
a set. If they don’t form a set, we have to rethink a few definitions.
In particular, what does it mean to compose morphisms if we can’t even
pick them from a set? The solution is to bootstrap ourselves by
replacing hom-sets, which are objects in **Set**, with *objects* from
some other category *V*. The difference is that, in general, objects
don’t have elements, so we are no longer allowed to talk about
individual morphisms. We have to define all properties of an *enriched*
category in terms of operations that can be performed on hom-objects as
a whole. In order to do that, the category that provides hom-objects
must have additional structure — it must be a monoidal category. If we
call this monoidal category *V*, we can talk about a category *C*
enriched over *V*.

Beside size reasons, we might be interested in generalizing hom-sets to
something that has more structure than mere sets. For instance, a
traditional category doesn’t have the notion of a distance between
objects. Two objects are either connected by morphisms or not. All
objects that are connected to a given object are its neighbors. Unlike
in real life; in a category, a friend of a friend of a friend is as
close to me as my bosom buddy. In a suitably enriched category, we can
define distances between objects.

There is one more very practical reason to get some experience with
enriched categories, and that’s because a very useful online source of
categorical knowledge, the `nLab <https://ncatlab.org>`__, is written
mostly in terms of enriched categories.

.. rubric:: Why Monoidal Category?
   :name: why-monoidal-category

When constructing an enriched category we have to keep in mind that we
should be able to recover the usual definitions when we replace the
monoidal category with **Set** and hom-objects with hom-sets. The best
way to accomplish this is to start with the usual definitions and keep
reformulating them in a point-free manner — that is, without naming
elements of sets.

Let’s start with the definition of composition. Normally, it takes a
pair of morphisms, one from ``C(b, c)`` and one from ``C(a, b)`` and
maps it to a morphism from ``C(a, c)``. In other words it’s a mapping:

::

    C(b, c) × C(a, b) -> C(a, c)

This is a function between sets — one of them being the cartesian
product of two hom-sets. This formula can be easily generalized by
replacing cartesian product with something more general. A categorical
product would work, but we can go even further and use a completely
general tensor product.

Next come the identity morphisms. Instead of picking individual elements
from hom-sets, we can define them using functions from the singleton set
**1**:

::

    ja :: 1 -> C(a, a)

Again, we could replace the singleton set with the terminal object, but
we can go even further by replacing it with the unit ``i`` of the tensor
product.

As you can see, objects taken from some monoidal category *V* are good
candidates for hom-set replacement.

.. rubric:: Monoidal Category
   :name: monoidal-category

We’ve talked about monoidal categories before, but it’s worth restating
the definition. A monoidal category defines a tensor product that is a
bifunctor:

::

    ⊗ :: V × V -> V

We want the tensor product to be associative, but it’s enough to satisfy
associativity up to natural isomorphism. This isomorphism is called the
associator. Its components are:

::

    αa b c :: (a ⊗ b) ⊗ c -> a ⊗ (b ⊗ c)

It must be natural in all three arguments.

A monoidal category must also define a special unit object ``i`` that
serves as the unit of the tensor product; again, up to natural
isomorphism. The two isomorphisms are called, respectively, the left and
the right unitor, and their components are:

::

    λa :: i ⊗ a -> a
    ρa :: a ⊗ i -> a

The associator and the unitors must satisfy coherence conditions:

|image0|

|image1|

A monoidal category is called *symmetric* if there is a natural
isomorphism with components:

::

    γa b :: a ⊗ b -> b ⊗ a

whose “square is one”:

::

    γb a ∘ γa b = ida⊗b

and which is consistent with the monoidal structure.

An interesting thing about monoidal categories is that you may be able
to define the internal hom (the function object) as the right adjoint to
the tensor product. You may recall that the standard definition of the
function object, or the exponential, was through the right adjoint to
the categorical product. A category in which such an object existed for
any pair of objects was called cartesian closed. Here is the adjunction
that defines the internal hom in a monoidal category:

::

    V(a ⊗ b, c) ~ V(a, [b, c])

Following `G. M.
Kelly <http://www.tac.mta.ca/tac/reprints/articles/10/tr10.pdf>`__, I’m
using the notation ``[b, c]`` for the internal hom. The counit of this
adjunction is the natural transformation whose components are called
evaluation morphisms:

::

    εa b :: ([a, b] ⊗ a) -> b

Notice that, if the tensor product is not symmetric, we may define
another internal hom, denoted by ``[[a, c]]``, using the following
adjunction:

::

    V(a ⊗ b, c) ~ V(b, [[a, c]])

A monoidal category in which both are defined is called biclosed. An
example of a category that is not biclosed is the category of
endofunctors in **Set**, with functor composition serving as tensor
product. That’s the category we used to define monads.

.. rubric:: Enriched Category
   :name: enriched-category

A category *C* enriched over a monoidal category *V* replaces hom-sets
with hom-objects. To every pair of objects ``a`` and ``b`` in *C* we
associate an object ``C(a, b)`` in *V*. We use the same notation for
hom-objects as we used for hom-sets, with the understanding that they
don’t contain morphisms. On the other hand, *V* is a regular
(non-enriched) category with hom-sets and morphisms. So we are not
entirely rid of sets — we just swept them under the rug.

Since we cannot talk about individual morphisms in *C*, composition of
morphisms is replaced by a family of morphisms in *V*:

::

    ∘ :: C(b, c) ⊗ C(a, b) -> C(a, c)

| |image2|
| Similarly, identity morphisms are replaced by a family of morphisms in
  *V*:

::

    ja :: i -> C(a, a)

where ``i`` is the tensor unit in *V*.

|image3|

Associativity of composition is defined in terms of the associator in
*V*:

|image4|

Unit laws are likewise expressed in terms of unitors:

|image5|

|image6|

.. rubric:: Preorders
   :name: preorders

A preorder is defined as a thin category, one in which every hom-set is
either empty or a singleton. We interpret a non-empty set ``C(a, b)`` as
the proof that ``a`` is less than or equal to ``b``. Such a category can
be interpreted as enriched over a very simple monoidal category that
contains just two objects, 0 and 1 (sometimes called False and True).
Besides the mandatory identity morphisms, this category has a single
morphism going from 0 to 1, let’s call it ``0->1``. A simple monoidal
structure can be established in it, with the tensor product modeling the
simple arithmetic of 0 and 1 (i.e., the only non-zero product is
``1⊗1``). The identity object in this category is 1. This is a strict
monoidal category, that is, the associator and the unitors are identity
morphisms.

Since in a preorder the-hom set is either empty or a singleton, we can
easily replace it with a hom-object from our tiny category. The enriched
preorder *C* has a hom-object ``C(a, b)`` for any pair of objects ``a``
and ``b``. If ``a`` is less than or equal to ``b``, this object is 1;
otherwise it’s 0.

Let’s have a look at composition. The tensor product of any two objects
is 0, unless both of them are 1, in which case it’s 1. If it’s 0, then
we have two options for the composition morphism: it could be either
``id0`` or ``0->1``. But if it’s 1, then the only option is ``id1``.
Translating this back to relations, this says that if ``a <= b`` and
``b <= c`` then ``a <= c``, which is exactly the transitivity law we
need.

What about the identity? It’s a morphism from 1 to ``C(a, a)``. There is
only one morphism going from 1, and that’s the identity ``id1``, so
``C(a, a)`` must be 1. It means that ``a <= a``, which is the
reflexivity law for a preorder. So both transitivity and reflexivity are
automatically enforced, if we implement a preorder as an enriched
category.

.. rubric:: Metric Spaces
   :name: metric-spaces

An interesting example is due to `William
Lawvere <http://www.tac.mta.ca/tac/reprints/articles/1/tr1.pdf>`__. He
noticed that metric spaces can be defined using enriched categories. A
metric space defines a distance between any two objects. This distance
is a non-negative real number. It’s convenient to include inifinity as a
possible value. If the distance is infinite, there is no way of getting
from the starting object to the target object.

There are some obvious properties that have to be satisfied by
distances. One of them is that the distance from an object to itself
must be zero. The other is the triangle inequality: the direct distance
is no larger than the sum of distances with intermediate stops. We don’t
require the distance to be symmetric, which might seem weird at first
but, as Lawvere explained, you can imagine that in one direction you’re
walking uphill, while in the other you’re going downhill. In any case,
symmetry may be imposed later as an additional constraint.

So how can a metric space be cast into a categorical language? We have
to construct a category in which hom-objects are distances. Mind you,
distances are not morphisms but hom-objects. How can a hom-object be a
number? Only if we can construct a monoidal category *V* in which these
numbers are objects. Non-negative real numbers (plus infinity) form a
total order, so they can be treated as a thin category. A morphism
between two such numbers ``x`` and ``y`` exists if and only if
``x >= y`` (note: this is the opposite direction to the one
traditionally used in the definition of a preorder). The monoidal
structure is given by addition, with zero serving as the unit object. In
other words, the tensor product of two numbers is their sum.

A metric space is a category enriched over such monoidal category. A
hom-object ``C(a, b)`` from object ``a`` to ``b`` is a non-negative
(possibly infinite) number that we will call the distance from ``a`` to
``b``. Let’s see what we get for identity and composition in such a
category.

By our definitions, a morphism from the tensorial unit, which is the
number zero, to a hom-object ``C(a, a)`` is the relation:

::

    0 >= C(a, a)

Since ``C(a, a)`` is a non-negative number, this condition tells us that
the distance from ``a`` to ``a`` is always zero. Check!

Now let’s talk about composition. We start with the tensor product of
two abutting hom-objects, ``C(b, c)⊗C(a, b)``. We have defined the
tensor product as the sum of the two distances. Composition is a
morphism in *V* from this product to ``C(a, c)``. A morphism in *V* is
defined as the greater-or-equal relation. In other words, the sum of
distances from ``a`` to ``b`` and from ``b`` to ``c`` is greater than or
equal to the distance from ``a`` to ``c``. But that’s just the standard
triangle inequality. Check!

By re-casting the metric space in terms of an enriched category, we get
the triangle inequality and the zero self-distance “for free.”

.. rubric:: Enriched Functors
   :name: enriched-functors

The definition of a functor involves the mapping of morphisms. In the
enriched setting, we don’t have the notion of individual morphisms, so
we have to deal with hom-objects in bulk. Hom-objects are objects in a
monoidal category *V*, and we have morphisms between them at our
disposal. It therefore makes sense to define enriched functors between
categories when they are enriched over the same monoidal category *V*.
We can then use morphisms in *V* to map the hom-objects between two
enriched categories.

An *enriched functor* ``F`` between two categories *C* and *D*, besides
mapping objects to objects, also assigns, to every pair of objects in
*C*, a morphism in *V*:

::

    Fa b :: C(a, b) -> D(F a, F b)

A functor is a structure-preserving mapping. For regular functors it
meant preserving composition and identity. In the enriched setting, the
preservation of composition means that the following diagram commute:

|image7|

The preservation of identity is replaced by the preservation of the
morphisms in *V* that “select” the identity:

|image8|

.. rubric:: Self Enrichment
   :name: self-enrichment

A closed symmetric monoidal category may be self-enriched by replacing
hom-sets with internal homs (see the definition above). To make this
work, we have to define the composition law for internal homs. In other
words, we have to implement a morphism with the following signature:

::

    [b, c] ⊗ [a, b] -> [a, c]

This is not much different from any other programming task, except that,
in category theory, we usually use point free implementations. We start
by specifying the set whose element it’s supposed to be. In this case,
it’s a member of the hom-set:

::

    V([b, c] ⊗ [a, b], [a, c])

This hom-set is isomorphic to:

::

    V(([b, c] ⊗ [a, b]) ⊗ a, c)

I just used the adjunction that defined the internal hom ``[a, c]``. If
we can build a morphism in this new set, the adjunction will point us at
the morphism in the original set, which we can then use as composition.
We construct this morphism by composing several morphisms that are at
our disposal. To begin with, we can use the associator
``α[b, c] [a, b] a`` to reassociate the expression on the left:

::

    ([b, c] ⊗ [a, b]) ⊗ a -> [b, c] ⊗ ([a, b] ⊗ a)

We can follow it with the co-unit of the adjunction ``εa b``:

::

    [b, c] ⊗ ([a, b] ⊗ a) -> [b, c] ⊗ b

And use the counit ``εb c`` again to get to ``c``. We have thus
constructed a morphism:

::

    εb c . (id[b, c] ⊗ εa b) . α[b, c] [a, b] a

that is an element of the hom-set:

::

    V(([b, c] ⊗ [a, b]) ⊗ a, c)

The adjunction will give us the composition law we were looking for.

Similarly, the identity:

::

    ja :: i -> [a, a]

is a member of the following hom-set:

::

    V(i, [a, a])

which is isomorphic, through adjunction, to:

::

     V(i ⊗ a, a)

We know that this hom-set contains the left identity ``λa``. We can
define ``ja`` as its image under the adjunction.

A practical example of self-enrichment is the category **Set** that
serves as the prototype for types in programming languages. We’ve seen
before that it’s a closed monoidal category with respect to cartesian
product. In **Set**, the hom-set between any two sets is itself a set,
so it’s an object in **Set**. We know that it’s isomorphic to the
exponential set, so the external and the internal homs are equivalent.
Now we also know that, through self-enrichment, we can use the
exponential set as the hom-object and express composition in terms of
cartesian products of exponential objects.

.. rubric:: Relation to 2-Categories
   :name: relation-to-2-categories

I talked about 2-categories in the context of **Cat**, the category of
(small) categories. The morphisms between categories are functors, but
there is an additional structure: natural transformations between
functors. In a 2-category, the objects are often called zero-cells;
morphisms, 1-cells; and morphisms between morphisms, 2-cells. In **Cat**
the 0-cells are categories, 1-cells are functors, and 2-cells are
natural transformations.

But notice that functors between two categories form a category too; so,
in **Cat**, we really have a *hom-category* rather than a hom-set. It
turns out that, just like **Set** can be treated as a category enriched
over **Set**, **Cat** can be treated as a category enriched over
**Cat**. Even more generally, just like every category can be treated as
enriched over **Set**, every 2-category can be considered enriched over
**Cat**.

Next: `Topoi <https://bartoszmilewski.com/2017/07/22/topoi/>`__.

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

   <div id="crt-1869491585" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-55562728" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2017/05/13/enriched-categories/?share=email>`__
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

   <div id="like-post-wrapper-3549518-8703-59ae3d24094cc"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=8703&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-8703-59ae3d24094cc"
   data-name="like-post-frame-3549518-8703-59ae3d24094cc">

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

.. rubric:: 9 Responses to “Enriched Categories”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-70581">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70581">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| `John Armstrong <http://drmathochist.wordpress.com/>`__
   Says:

   .. raw:: html

      </div>

   `May 13, 2017 at 6:35
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-70581>`__
   A little tweak: the swap in a symmetric m.c. needs to satisfy a
   coherence condition: $s\_{BA}\\circ s\_{AB} = 1\_{A\\otimes B}$.
   Without that condition what you have is a “braided” m.c. which gives
   rise to representations of the braid category (and is particularly
   interesting in terms of anyons and topological QFTs).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70612">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70612">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `May 14, 2017 at 3:25
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-70612>`__
   You’re right. I tried to sweep it under the rug of “consistent with
   monoidal structure,” but braiding is also consistent.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-71400">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-71400">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `June 1, 2017 at 8:25
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-71400>`__
   “If they don’t form a set, we have to rethink a few definitions. In
   particular, what does it mean to compose morphisms if we can’t even
   pick them from a set?”

   What do you mean by ‘pick them’ in this context? Or, why can’t you
   pick them from things that are larger than sets?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-71403">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-71403">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `June 1, 2017 at 9:17
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-71403>`__
   A set is pretty much defined as something that has elements. Picking
   an element means defining an x to be an element of X. But if X is not
   a set, what does it mean to be an element of X? It all depends on
   what theory you use to define the entities that are larger than sets
   (classes, collections?). The beauty of category theory is that it
   doesn’t force you to commit to a particular foundation.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-71420">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-71420">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `June 1, 2017 at 11:09
   pm <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-71420>`__
   Ok, I see it. So instead of generalising via “picking elements for
   things greater than sets”, we generalise substituting sets by objects
   in another category and “redefining” the idea of picking an element.

   Thanks !!!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-71628">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-71628">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| Nikolaj Says:

   .. raw:: html

      </div>

   `June 8, 2017 at 5:27
   pm <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-71628>`__
   | Thanks for the series, Bartosz. The first paragraph is a little
     bumpy. Maybe the following comments help.
   | Given the setup and examples, you want speak of categories where
     you first define objects (the sets) in some way and then consider
     such sets as the type of objects for the category you look at. Now
     whether a set can be proven to exist depends entirely on the theory
     you choose to use, a collection of axioms written down in some
     logic. In Kripke–Platek set theory, you can’t proof that for every
     set X, the a set PX (“the power set of X”) exists. In ZFC you do
     can show existence of such sets. In ZFC you can’t show the
     existence of large cardinals or e.g. Grothendieck universes, but
     they are consistent with it, and so it’s consistent to add the
     stronger axioms to the theory, respectively. ZFC can be speaking of
     those notions of set universes too, if none of its axioms is in
     constradictions with them. So there are e.g. cardinally speaking
     bigger and smaller models for ZFC. Demanding existence of larger
     sets in the axioms means choosing stronger conditions and thus
     restricting what your theory talks about (just like talking about
     commutative groups only restricts your theory of groups, but makes
     more theorems possible). In that case your new theory of sets is
     one only talking about quite cardinally large objects. Passing from
     ZFC to a stronger theory where there provable exist sets called
     Grothendieck universes is not a trick to speak of objects “larger
     than sets”, it merely empowers you to prove the existence of
     different (or “more”) “sets” (taking the theory “of sets” to
     something which is further away from the most naive theory of
     sets). There are also (so called non-wellfounded) set theories with
     axioms that e.g. say the set with the property x={x} exists, some
     infinite loop of inheritance. ZFC actually disproves the existence
     of the latter fairly directly by one of it’s axioms, but my point
     here is that this is also an object that you may naively specify,
     but which is not a set, and not because it’s cardinally too large.
     Although mostly the sets that you can’t prove in ZFC to exist are
     really just “too large”, cardinally speaking. I.e. you can’t
     surjectively map the cardinally speaking smaller into the larger
     one.
   | Okay, I hope that wasn’t too long winded. You start “A category is
     small if its objects form a set” and later you use “large”, which I
     guess shall mean non-small here (not a priori of larger
     cardinality, even if that’s what it’s mostly about). You then go on
     with “But we know that there are things larger than sets.” Well,
     you may choose to be somewhat dogmatic about ZFC as is and say all
     the objects I can prove there are sets and when I can embed a model
     of them into a class of things with extra terms (that are out of
     reach of provability of ZFC) then those other things aren’t sets.
     In that case the sentence merely means that there are axioms that
     let you write down a theory of sets which goes far beyond taking
     powersets. But then you have just created another stronger theory
     of “sets”, and in turn the claim there’s something “larger than
     sets” (in either of the two ways of reading the word) lost it’s
     footing.
   | If you take a set theory with big Grothendieck universes and take
     such a universe as your type of objects, your cateogry is not
     ZFC-small, but it’s a set in another theory. The class “trick” is
     just to get an object that you can’t get inside the model, i.e. one
     that can’t be an element. The topoi abstract common features of set
     theories and thus end up sharing models with them.
   | All that being said, I don’t think wanting to be able to speak
     about extra large categories is a relevant motivator for your type
     theory centered needs anyway.
   | Keep up the good work.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-72374">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-72374">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| J. Jan Says:

   .. raw:: html

      </div>

   `July 2, 2017 at 2:19
   pm <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-72374>`__
   | Przepraszam, ze tu piszę, ale obejrzałem sobie twój filmik na
     Youtube gdzie mowisz o tkategorii dla programistów.
   | wydaje mi sie, ze warto bylo by powiedziec, ze strzałki moga być z
     zewnątrz (ze sceny) oraz zamiast id chyba lepiej mówić o
     niezmiennikach

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-72907">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-72907">

   .. raw:: html

      <div class="comment-author vcard">

   |image16| `HenryChern <http://henrychern.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `July 20, 2017 at 12:11
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-72907>`__
   Thank you for your work and the material presented. I translate the
   chapters of your book into Russian, and now I’m waiting for the next
   chapter about topoi. I posted the translation of all the chapters to
   WordPress at
   “\ `henrychern.wordpress.com <http://henrychern.wordpress.com>`__\ “.
   I dared to make the text coloring, trying to pick out separately
   mathematical expressions and separately program fragments. Is it
   interesting to see such a representation? (I can all lead to your
   text style.)

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-72913">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-72913">

   .. raw:: html

      <div class="comment-author vcard">

   |image17| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 20, 2017 at 2:43
   am <https://bartoszmilewski.com/2017/05/13/enriched-categories/#comment-72913>`__
   @HenryChern: Wow, your translation looks better than the original.
   Great job!

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
   reply </2017/05/13/enriched-categories/#respond>`__
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
-  May 13, 2017 at 5:15 am
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2017/05/13/enriched-categories/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-ed8df1b934fbb8259a5d1f369e168172">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-b4a7426cee3700d21354b77b4a29fddd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-cdd238c033f64cf484a76802ebc06522">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-812d1ddd63af9f53a7936c580487ed3d">

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

|image24|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |image0| image:: https://bartoszmilewski.files.wordpress.com/2017/05/assoc.jpg?w=510&h=240
   :class: alignnone wp-image-8745 size-large
   :width: 510px
   :height: 240px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/assoc.jpg
.. |image1| image:: https://bartoszmilewski.files.wordpress.com/2017/05/idcoherence.jpg?w=407&h=158
   :class: alignnone wp-image-8751
   :width: 407px
   :height: 158px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/idcoherence.jpg
.. |image2| image:: https://bartoszmilewski.files.wordpress.com/2017/05/composition.jpg?w=395&h=282
   :class: alignnone wp-image-8747
   :width: 395px
   :height: 282px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/composition.jpg
.. |image3| image:: https://bartoszmilewski.files.wordpress.com/2017/05/id.jpg?w=254&h=218
   :class: alignnone wp-image-8750
   :width: 254px
   :height: 218px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/id.jpg
.. |image4| image:: https://bartoszmilewski.files.wordpress.com/2017/05/compcoherence.jpg?w=514&h=122
   :class: alignnone wp-image-8746
   :width: 514px
   :height: 122px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/compcoherence.jpg
.. |image5| image:: https://bartoszmilewski.files.wordpress.com/2017/05/rightid.jpg?w=446&h=133
   :class: alignnone wp-image-8753
   :width: 446px
   :height: 133px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/rightid.jpg
.. |image6| image:: https://bartoszmilewski.files.wordpress.com/2017/05/leftid.jpg?w=447&h=136
   :class: alignnone wp-image-8752
   :width: 447px
   :height: 136px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/leftid.jpg
.. |image7| image:: https://bartoszmilewski.files.wordpress.com/2017/05/functorcomp.jpg?w=510&h=208
   :class: alignnone size-large wp-image-8748
   :width: 510px
   :height: 208px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/functorcomp.jpg
.. |image8| image:: https://bartoszmilewski.files.wordpress.com/2017/05/functorid.jpg?w=388&h=175
   :class: alignnone wp-image-8749
   :width: 388px
   :height: 175px
   :target: https://bartoszmilewski.files.wordpress.com/2017/05/functorid.jpg
.. |image9| image:: https://2.gravatar.com/avatar/ed8df1b934fbb8259a5d1f369e168172?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://2.gravatar.com/avatar/b4a7426cee3700d21354b77b4a29fddd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://2.gravatar.com/avatar/b4a7426cee3700d21354b77b4a29fddd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://0.gravatar.com/avatar/cdd238c033f64cf484a76802ebc06522?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://2.gravatar.com/avatar/812d1ddd63af9f53a7936c580487ed3d?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image16| image:: https://1.gravatar.com/avatar/d42dd46c69476ea0478111fa098ef4a4?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image17| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image24| image:: https://pixel.wp.com/b.gif?v=noscript

