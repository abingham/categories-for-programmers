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
   class="post-4967 post type-post status-publish format-standard hentry category-category-theory category-functional-programming category-haskell category-programming">

October 28, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Yoneda Embedding
   :name: yoneda-embedding
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Functional
Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Programming <https://bartoszmilewski.com/category/programming/>`__
`Leave a
Comment <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/#respond>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4967" class="pd-rating">

.. raw:: html

   </div>

    This is part 16 of Categories for Programmers. Previously: `The
    Yoneda
    Lemma <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

We’ve seen previously that, when we fix an object ``a`` in the category
*C*, the mapping ``C(a, -)`` is a (covariant) functor from *C* to
**Set**.

::

    x -> C(a, x)

(The codomain is **Set** because the hom-set C(a, x) is a *set*.) We
call this mapping a hom-functor — we have previously defined its action
on morphisms as well.

Now let’s vary ``a`` in this mapping. We get a new mapping that assigns
the hom-\ *functor* ``C(a, -)`` to any ``a``.

::

    a -> C(a, -)

It’s a mapping of objects from category *C* to functors, which are
*objects* in the functor category (see the section about functor
categories in `Natural
Transformations <https://bartoszmilewski.com/2015/04/07/natural-transformations/>`__).
Let’s use the notation ``[C, Set]`` for the functor category from *C* to
**Set**. You may also recall that hom-functors are the prototypical
`representable
functors <https://bartoszmilewski.com/2015/07/29/representable-functors/>`__.

Every time we have a mapping of objects between two categories, it’s
natural to ask if such a mapping is also a functor. In other words
whether we can lift a morphism from one category to a morphism in the
other category. A morphism in *C* is just an element of ``C(a, b)``, but
a morphism in the functor category ``[C, Set]`` is a natural
transformation. So we are looking for a mapping of morphisms to natural
transformations.

Let’s see if we can find a natural transformation corresponding to a
morphism ``f :: a->b``. First, lets see what ``a`` and ``b`` are mapped
to. They are mapped to two functors: ``C(a, -)`` and ``C(b, -)``. We
need a natural transformation between those two functors.

And here’s the trick: we use the Yoneda lemma:

::

    [C, Set](C(a, -), F) ≅ F a

and replace the generic ``F`` with the hom-functor ``C(b, -)``. We get:

::

    [C, Set](C(a, -), C(b, -)) ≅ C(b, a)

|Yoneda Embedding|

This is exactly the natural transformation between the two hom-functors
we were looking for, but with a little twist: We have a mapping between
a natural transformation and a morphism — an element of ``C(b, a)`` —
that goes in the “wrong” direction. But that’s okay; it only means that
the functor we are looking at is contravariant.

|Yoneda Embedding 2|

Actually, we’ve got even more than we bargained for. The mapping from
*C* to ``[C, Set]`` is not only a contravariant functor — it is a *fully
faithful* functor. Fullness and faithfulness are properties of functors
that describe how they map hom-sets.

A *faithful* functor is *injective* on hom-sets, meaning that it maps
distinct morphisms to distinct morphisms. In other words, it doesn’t
coalesce them.

A *full* functor is *surjective* on hom-sets, meaning that it maps one
hom-set *onto* the other hom-set, fully covering the latter.

A fully faithful functor ``F`` is a *bijection* on hom-sets — a one to
one matching of all elements of both sets. For every pair of objects
``a`` and ``b`` in the source category *C* there is a bijection between
``C(a, b)`` and ``D(F a, F b)``, where *D* is the target category of
``F`` (in our case, the functor category, ``[C, Set]``). Notice that
this doesn’t mean that ``F`` is a bijection on *objects*. There may be
objects in *D* that are not in the image of ``F``, and we can’t say
anything about hom-sets for those objects.

.. rubric:: The Embedding
   :name: the-embedding

The (contravariant) functor we have just described, the functor that
maps objects in *C* to functors in ``[C, Set]``:

::

    a -> C(a, -)

defines the *Yoneda embedding*. It *embeds* a category *C* (strictly
speaking, the category *C\ :sup:`op`*, because of contravariance) inside
the functor category ``[C, Set]``. It not only maps objects in *C* to
functors, but also faithfully preserves all connections between them.

This is a very useful result because mathematicians know a lot about the
category of functors, especially functors whose codomain is **Set**. We
can get a lot of insight about an arbitrary category *C* by embedding it
in the functor category.

Of course there is a dual version of the Yoneda embedding, sometimes
called the co-Yoneda embedding. Observe that we could have started by
fixing the target object (rather than the source object) of each
hom-set, ``C(-, a)``. That would give us a contravariant hom-functor.
Contravariant functors from *C* to **Set** are our familiar presheaves
(see, for instance, `Limits and
Colimits <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/>`__).
The co-Yoneda embedding defines the embedding of a category *C* in the
category of presheaves. Its action on morphisms is given by:

::

    [C, Set](C(-, a), C(-, b)) ≅ C(a, b)

Again, mathematicians know a lot about the category of presheaves, so
being able to embed an arbitrary category in it is a big win.

.. rubric:: Application to Haskell
   :name: application-to-haskell

In Haskell, the Yoneda embedding can be represented as the isomorphism
between natural transformations amongst reader functors on the one hand,
and functions (going in the opposite direction) on the other hand:

::

    forall x. (a -> x) -> (b -> x) ≅ b -> a

(Remember, the reader functor is equivalent to ``((->) a)``.)

The left hand side of this identity is a polymorphic function that,
given a function from ``a`` to ``x`` and a value of type ``b``, can
produce a value of type ``x`` (I’m uncurrying — dropping the parentheses
around — the function ``b -> x``). The only way this can be done for all
``x`` is if our function knows how to convert a ``b`` to an ``a``. It
has to secretly have access to a function ``b->a``.

Given such a converter, ``btoa``, one can define the left hand side,
call it\ ``fromY``, as:

::

    fromY :: (a -> x) -> b -> x
    fromY f b = f (btoa b)

Conversely, given a function ``fromY`` we can recover the converter by
calling ``fromY`` with the identity:

::

    fromY id :: b -> a

This establishes the bijection between functions of the type ``fromY``
and ``btoa``.

An alternative way of looking at this isomorphism is that it’s a CPS
encoding of a function from ``b`` to ``a``. The argument ``a->x`` is a
continuation (the handler). The result is a function from ``b`` to ``x``
which, when called with a value of type ``b``, will execute the
continuation precomposed with the function being encoded.

The Yoneda embedding also explains some of the alternative
representations of data structures in Haskell. In particular, it
provides a very useful `representation of
lenses <https://bartoszmilewski.com/2015/07/13/from-lenses-to-yoneda-embedding/>`__
from the ``Control.Lens`` library.

.. rubric:: Preorder Example
   :name: preorder-example

This example was suggested by Robert Harper. It’s the application of the
Yoneda embedding to a category defined by a preorder. A preorder is a
set with an ordering relation between its elements that’s traditionally
written as ``<=`` (less than or equal). The “pre” in preorder is there
because we’re only requiring the relation to be transitive and reflexive
but not necessarily antisymmetric (so it’s possible to have cycles).

A set with the preorder relation gives rise to a category. The objects
are the elements of this set. A morphism from object ``a`` to ``b``
either doesn’t exist, if the objects cannot be compared or if it’s not
true that ``a <= b``; or it exists if ``a <= b``, and it points from
``a`` to ``b``. There is never more than one morphism from one object to
another. Therefore any hom-set in such a category is either an empty set
or a one-element set. Such a category is called *thin*.

It’s easy to convince yourself that this construction is indeed a
category: The arrows are composable because, if ``a <= b`` and
``b <= c`` then ``a <= c``; and the composition is associative. We also
have the identity arrows because every element is (less than or) equal
to itself (reflexivity of the underlying relation).

We can now apply the co-Yoneda embedding to a preorder category. In
particular, we’re interested in its action on morphisms:

::

    [C, Set](C(-, a), C(-, b)) ≅ C(a, b)

The hom-set on the right hand side is non-empty if and only if
``a <= b`` — in which case it’s a one-element set. Consequently, if
``a <= b``, there exists a single natural transformation on the left.
Otherwise there is no natural transformation.

So what’s a natural transformation between hom-functors in a preorder?
It should be a family of functions between sets ``C(-, a)`` and
``C(-, b)``. In a preorder, each of these sets can either be empty or a
singleton. Let’s see what kind of functions are there at our disposal.

There is a function from an empty set to itself (the identity acting on
an empty set), a function ``absurd`` from an empty set to a singleton
set (it does nothing, since it only needs to be defined for elements of
an empty set, of which there are none), and a function from a singleton
to itself (the identity acting on a one-element set). The only
combination that is forbidden is the mapping from a singleton to an
empty set (what would the value of such a function be when acting on the
single element?).

So our natural transformation will never connect a singleton hom-set to
an empty hom-set. In other words, if ``x <= a`` (singleton hom-set
``C(x, a)``) then ``C(x, b)`` cannot be empty. A non-empty ``C(x, b)``
means that ``x`` is less or equal to ``b``. So the existence of the
natural transformation in question requires that, for every ``x``, if
``x <= a`` then ``x <= b``.

::

    for all x, x ≤ a ⇒ x ≤ b

On the other hand, co-Yoneda tells us that the existence of this natural
transformation is equivalent to ``C(a, b)`` being non-empty, or to
``a <= b``. Together, we get:

::

    a ≤ b if and only if for all x, x ≤ a ⇒ x ≤ b

We could have arrived at this result directly. The intuition is that, if
``a <= b`` then all elements that are below ``a`` must also be below
``b``. Conversely, when you substitute ``a`` for ``x`` on the right hand
side, it follows that ``a <= b``. But you must admit that arriving at
this result through the Yoneda embedding is much more exciting.

.. rubric:: Naturality
   :name: naturality

The Yoneda lemma establishes the isomorphism between the set of natural
transformations and an object in **Set**. Natural transformations are
morphisms in the functor category ``[C, Set]``. The set of natural
transformation between any two functors is a hom-set in that category.
The Yoneda lemma is the isomorphism:

::

    [C, Set](C(a, -), F) ≅ F a

This isomorphism turns out to be natural in both ``F`` and ``a``. In
other words, it’s natural in ``(F, a)``, a pair taken from the product
category ``[C, Set] × C``. Notice that we are now treating ``F`` as an
*object* in the functor category.

Let’s think for a moment what this means. A natural isomorphism is an
invertible *natural transformation* between two functors. And indeed,
the right hand side of our isomorphism is a functor. It’s a functor from
``[C, Set] × C`` to **Set**. Its action on a pair ``(F, a)`` is a set —
the result of evaluating the functor ``F`` at the object ``a``. This is
called the evaluation functor.

The left hand side is also a functor that takes ``(F, a)`` to a set of
natural transformations ``[C, Set](C(a, -), F)``.

To show that these are really functors, we should also define their
action on morphisms. But what’s a morphism between a pair ``(F, a)`` and
``(G, b)``? It’s a pair of morphisms, ``(Φ, f)``; the first being a
morphism between functors — a natural transformation — the second being
a regular morphism in *C*.

The evaluation functor takes this pair ``(Φ, f)`` and maps it to a
function between two sets, ``F a`` and ``G b``. We can easily construct
such a function from the component of ``Φ`` at ``a`` (which maps ``F a``
to ``G a``) and the morphism ``f`` lifted by ``G``:

::

    (G f) ∘ Φa

Notice that, because of naturality of ``Φ``, this is the same as:

::

    Φb ∘ (F f)

I’m not going to prove the naturality of the whole isomorphism — after
you’ve established what the functors are, the proof is pretty
mechanical. It follows from the fact that our isomorphism is built up
from functors and natural transformations. There is simply no way for it
to go wrong.

.. rubric:: Challenges
   :name: challenges

#. Express the co-Yoneda embedding in Haskell.
#. Show that the bijection we established between ``fromY`` and ``btoa``
   is an isomorphism (the two mappings are the inverse of each other).
#. Work out the Yoneda embedding for a monoid. What functor corresponds
   to the monoid’s single object? What natural transformations
   correspond to monoid morphisms?
#. What is the application of the *covariant* Yoneda embedding to
   preorders? (Question suggested by Gershom Bazerman.)
#. Yoneda embedding can be used to embed an arbitrary functor category
   ``[C, D]`` in the functor category ``[[C, D], Set]``. Figure out how
   it works on morphisms (which in this case are natural
   transformations).

Next: `It’s All About
Morphisms <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/>`__.

.. rubric:: Acknowledgments
   :name: acknowledgments

| I’d like to thank Gershom Bazerman for checking my math and logic.
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

   <div id="crt-1962911021" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-141040132" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4967-59ae3c7798b63"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4967&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4967-59ae3c7798b63"
   data-name="like-post-frame-3549518-4967-59ae3c7798b63">

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

.. raw:: html

   <div id="respond" class="comment-respond">

.. rubric:: Leave a Reply `Cancel
   reply </2015/10/28/yoneda-embedding/#respond>`__
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
-  October 28, 2015 at 11:57 am
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Functional
   Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Programming <https://bartoszmilewski.com/category/programming/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Blog at WordPress.com. <https://wordpress.com/?ref=footer_blog>`__

.. raw:: html

   <div style="display:none">

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

|image8|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Yoneda Embedding| image:: https://bartoszmilewski.files.wordpress.com/2015/07/yoneda-embedding.jpg?w=372&h=180
   :class: alignnone wp-image-4782
   :width: 372px
   :height: 180px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/yoneda-embedding.jpg
.. |Yoneda Embedding 2| image:: https://bartoszmilewski.files.wordpress.com/2015/07/yoneda-embedding-2.jpg?w=433&h=157
   :class: alignnone wp-image-4783
   :width: 433px
   :height: 157px
   :target: https://bartoszmilewski.files.wordpress.com/2015/07/yoneda-embedding-2.jpg
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
.. |image8| image:: https://pixel.wp.com/b.gif?v=noscript

