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
   class="post-5185 post type-post status-publish format-standard hentry category-category-theory category-functional-programming category-haskell">

November 17, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: It’s All About Morphisms
   :name: its-all-aboutmorphisms
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Functional
Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__
`[6]
Comments <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_5185" class="pd-rating">

.. raw:: html

   </div>

    This is part 17 of Categories for Programmers. Previously: `Yoneda
    Embedding <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

If I haven’t convinced you yet that category theory is all about
morphisms then I haven’t done my job properly. Since the next topic is
adjunctions, which are defined in terms of isomorphisms of hom-sets, it
makes sense to review our intuitions about the building blocks of
hom-sets. Also, you’ll see that adjunctions provide a more general
language to describe a lot of constructions we’ve studied before, so it
might help to review them too.

.. rubric:: Functors
   :name: functors

To begin with, you should really think of functors as mappings of
morphisms — the view that’s emphasized in the Haskell definition of the
``Functor`` typeclass, which revolves around ``fmap``. Of course,
functors also map objects — the endpoints of morphisms — otherwise we
wouldn’t be able to talk about preserving composition. Objects tell us
which pairs of morphisms are composable. The target of one morphism must
be equal to the source of the other — if they are to be composed. So if
we want the composition of morphisms to be mapped to the composition of
*lifted* morphisms, the mapping of their endpoints is pretty much
determined.

.. rubric:: Commuting Diagrams
   :name: commuting-diagrams

A lot of properties of morphisms are expressed in terms of commuting
diagrams. If a particular morphism can be described as a composition of
other morphisms in more than one way, then we have a commuting diagram.

In particular, commuting diagrams form the basis of almost all universal
constructions (with the notable exceptions of the initial and terminal
objects). We’ve seen this in the definitions of products, coproducts,
various other (co-)limits, exponential objects, free monoids, etc.

The product is a simple example of a universal construction. We pick two
objects ``a`` and ``b`` and see if there exists an object ``c``,
together with a pair of morphisms ``p`` and ``q``, that has the
universal property of being their product.

|ProductRanking|

A product is a special case of a limit. A limit is defined in terms of
cones. A general cone is built from commuting diagrams. Commutativity of
those diagrams may be replaced with a suitable naturality condition for
the mapping of functors. This way commutativity is reduced to the role
of the assembly language for the higher level language of natural
transformations.

.. rubric:: Natural Transformations
   :name: natural-transformations

In general, natural transformations are very convenient whenever we need
a mapping from morphisms to commuting squares. Two opposing sides of a
naturality square are the mappings of some morphism ``f`` under two
functors ``F`` and ``G``. The other sides are the components of the
natural transformation (which are also morphisms).

|3\_Naturality|

Naturality means that when you move to the “neighboring” component (by
neighboring I mean connected by a morphism), you’re not going against
the structure of either the category or the functors. It doesn’t matter
whether you first use a component of the natural transformation to
bridge the gap between objects, and then jump to its neighbor using the
functor; or the other way around. The two directions are orthogonal. A
natural transformation moves you left and right, and the functors move
you up and down or back and forth — so to speak. You can visualize the
*image* of a functor as a sheet in the target category. A natural
transformation maps one such sheet corresponding to F, to another,
corresponding to G.

|Sheets|

We’ve seen examples of this orthogonality in Haskell. There the action
of a functor modifies the content of a container without changing its
shape, while a natural transformation repackages the untouched contents
into a different container. The order of these operations doesn’t
matter.

We’ve seen the cones in the definition of a limit replaced by natural
transformations. Naturality ensures that the sides of every cone
commute. Still, a limit is defined in terms of mappings *between* cones.
These mappings must also satisfy commutativity conditions. (For
instance, the triangles in the definition of the product must commute.)

These conditions, too, may be replaced by naturality. You may recall
that the *universal* cone, or the limit, is defined as a natural
transformation between the (contravariant) hom-functor:

::

    F :: c -> C(c, Lim D)

and the (also contravariant) functor that maps objects in *C* to cones,
which themselves are natural transformations:

::

    G :: c -> Nat(Δc, D)

Here, ``Δc`` is the constant functor, and ``D`` is the functor that
defines the diagram in *C*. Both functors ``F`` and ``G`` have well
defined actions on morphisms in *C*. It so happens that this particular
natural transformation between ``F`` and ``G`` is an *isomorphism*.

.. rubric:: Natural Isomorphisms
   :name: natural-isomorphisms

A natural isomorphism — which is a natural transformation whose every
component is reversible — is category theory’s way of saying that “two
things are the same.” A component of such a transformation must be an
isomorphism between objects — a morphism that has the inverse. If you
visualize functor images as sheets, a natural isomorphism is a
one-to-one invertible mapping between those sheets.

.. rubric:: Hom-Sets
   :name: hom-sets

But what are morphisms? They do have more structure than objects: unlike
objects, morphisms have two ends. But if you fix the source and the
target objects, the morphisms between the two form a boring set (at
least for locally small categories). We can give elements of this set
names like ``f`` or ``g``, to distinguish one from another — but what is
it, really, that makes them different?

The essential difference between morphisms in a given hom-set lies in
the way they compose with other morphisms (from abutting hom-sets). If
there is a morphism ``h`` whose composition (either pre- or post-) with
``f`` is different than that with ``g``, for instance:

::

    h ∘ f ≠ h ∘ g

then we can directly “observe” the difference between ``f`` and ``g``.
But even if the difference is not directly observable, we might use
functors to zoom in on the hom-set. A functor ``F`` may map the two
morphisms to distinct morphisms:

::

    F f ≠ F g

in a richer category, where the abutting hom-sets provide more
resolution, e.g.,

::

    h' ∘ F f ≠ h' ∘ F g

where ``h'`` is not in the image of ``F``.

.. rubric:: Hom-Set Isomorphisms
   :name: hom-set-isomorphisms

A lot of categorical constructions rely on isomorphisms between
hom-sets. But since hom-sets are just sets, a plain isomorphism between
them doesn’t tell you much. For finite sets, an isomorphism just says
that they have the same number of elements. If the sets are infinite,
their cardinality must be the same. But any meaningful isomorphism of
hom-sets must take into account composition. And composition involves
more than one hom-set. We need to define isomorphisms that span whole
collections of hom-sets, and we need to impose some compatibility
conditions that interoperate with composition. And a *natural*
isomorphism fits the bill exactly.

But what’s a natural isomorphism of hom-sets? Naturality is a property
of mappings between functors, not sets. So we are really talking about a
natural isomorphism between hom-set-valued functors. These functors are
more than just set-valued functors. Their action on morphisms is induced
by the appropriate hom-functors. Morphisms are canonically mapped by
hom-functors using either pre- or post-composition (depending on the
covariance of the functor).

The Yoneda embedding is one example of such an isomorphism. It maps
hom-sets in *C* to hom-sets in the functor category; and it’s natural.
One functor in the Yoneda embedding is the hom-functor in *C* and the
other maps objects to sets of natural transformations between hom-sets.

The definition of a limit is also a natural isomorphism between hom-sets
(the second one, again, in the functor category):

::

    C(c, Lim D) ≃ Nat(Δc, D)

It turns out that our construction of an exponential object, or that of
a free monoid, can also be rewritten as a natural isomorphism between
hom-sets.

This is no coincidence — we’ll see next that these are just different
examples of adjunctions, which are defined as natural isomorphisms of
hom-sets.

.. rubric:: Asymmetry of Hom-Sets
   :name: asymmetry-of-hom-sets

There is one more observation that will help us understand adjunctions.
Hom-sets are, in general, not symmetric. A hom-set ``C(a, b)`` is often
very different from the hom-set ``C(b, a)``. The ultimate demonstration
of this asymmetry is a partial order viewed as a category. In a partial
order, a morphism from ``a`` to ``b`` exists if and only if ``a`` is
less than or equal to ``b``. If ``a`` and ``b`` are different, then
there can be no morphism going the other way, from ``b`` to ``a``. So if
the hom-set ``C(a, b)`` is non-empty, which in this case means it’s a
singleton set, then ``C(b, a)`` must be empty, unless ``a = b``. The
arrows in this category have a definite flow in one direction.

A preorder, which is based on a relation that’s not necessarily
antisymmetric, is also “mostly” directional, except for occasional
cycles. It’s convenient to think of an arbitrary category as a
generalization of a preoder.

A preorder is a thin category — all hom-sets are either singletons or
empty. We can visualize a general category as a “thick” preorder.

.. rubric:: Challenges
   :name: challenges

#. Consider some degenerate cases of a naturality condition and draw the
   appropriate diagrams. For instance, what happens if either functor
   ``F`` or ``G`` map both objects ``a`` and ``b`` (the ends of
   ``f :: a -> b``) to the same object, e.g., ``F a = F b`` or
   ``G a = G b``? (Notice that you get a cone or a co-cone this way.)
   Then consider cases where either ``F a = G a`` or ``F b = G b``.
   Finally, what if you start with a morphism that loops on itself —
   ``f :: a -> a``?

Next:
`Adjunctions <https://bartoszmilewski.com/2016/04/18/adjunctions/>`__.

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

   <div id="crt-933475472" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1888573100" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/?share=email>`__
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

   <div id="like-post-wrapper-3549518-5185-59ae3c8d67914"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=5185&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-5185-59ae3c8d67914"
   data-name="like-post-frame-3549518-5185-59ae3c8d67914">

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

.. rubric:: 6 Responses to “It’s All About Morphisms”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-58780">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-58780">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Jesus Prieto Colomina <http://github.com/jesuspc>`__ Says:

   .. raw:: html

      </div>

   `December 16, 2015 at 2:53
   pm <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-58780>`__
   Thanks a lot for the book! It has been my entrypoint to Haskell

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-61818">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-61818">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| Jonathan Says:

   .. raw:: html

      </div>

   `February 3, 2016 at 6:01
   am <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-61818>`__
   I really like the idea of bringing functional programming concepts
   closer to the average imperative programmer who’s normally running
   away as soon as she hears the word “functor”. Explaining the
   fundamentals in a way that they may be applied in every day life (or
   at least get you thinking) and keeping it simple.

   That said, I feel you’re drifting off more and more into the realm of
   theory and I struggle to keep the connection to my humble every day
   programming. The promised C++ examples got scarcer and scarcer and I
   had to more or less force myself through the last few chapters.

   Maybe my expectation were a bit too high on that, but I sincerely
   hope you can get back to where you started off: simple, more
   practical examples.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-62027">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-62027">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `February 6, 2016 at 1:09
   pm <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-62027>`__
   @Jonathan: Congratulations! You’ve been able to read through 17
   installments of this series, and even if you had to force yourself
   through the last few chapters, that’s a tremendous accomplishment.

   As the topics are getting more and more abstract, I’m having problems
   translating them to C++ and had to lean more on Haskell. One problem
   is that C++ doesn’t have enough abstracting power. The other is that
   the more abstract you go, the worse the C++ syntax becomes. I have
   already had to tap into template-template parameters and anonymous
   lambdas. C++ type inference is pitifully inadequate, which forces the
   client to specify template parameters explicitly. At this point, it
   really becomes worthwhile to learn some Haskell. In fact it was those
   problems with C++ template syntax that forced me to learn Haskell.

   A lot of higher order abstractions in Haskell translate into patterns
   in C++. It’s good to learn to recognize these patterns in C++, but if
   you want a compact description of such patterns, you express them in
   Haskell.

   The next installment of the series will be about adjunctions, which
   unify a lot of constructs that I presented before. This will probably
   be the most abstract part of the series. But adjunctions tie nicely
   to monads and comonads that will come next. And there will be a lot
   of examples of monads both in Haskell and C++ (where they are more of
   a pattern). Next are F-algebras, which have important `applications
   in C++ <http://ericniebler.com/2013/07/16/f-algebras-and-c/>`__. With
   Kan extensions we’ll again ratchet up the level of abstraction. Kan
   extensions can be expressed in Haskell, and they play an important
   role in constructing adjoint functors. Yoneda, adjunctions, and Kan
   extensions provide the abstract underpinning of the Haskell lens
   library.

   So the series will become incrementally more abstract and the
   readership will slowly shift from programmers to computer scientists,
   and maybe even to math students. I’ll do my best to present the
   material in the most approachable way possible, but it will get
   harder.

   If you were able to follow the series so far, you have learned some
   of the most advanced theory that’s been only available to
   professional mathematicians, some computer scientists and a few
   Haskell programmers.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-62200">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-62200">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| Jonathan Says:

   .. raw:: html

      </div>

   `February 8, 2016 at 10:46
   am <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-62200>`__
   | @Bartosz: Thanks for the clarification.
   | I know, this is indeed a really tough task and I’m grateful that
     you’re attempting to take it on!
   | It might be useful to have a few practical examples that relate to
     the theoretical concepts. Like, what different types can be
     described as a monad/functor and what are the implications of
     morphisms for those types (independent of languages)? Even though,
     of course, those examples couldn’t cover the breadth of abstraction
     the theory offers, they might give an idea of what the theory is
     aiming at.

   | I just can’t help but being a bit disappointed that the chapters
     seem to move away from your initial idea (according to the preface
     and the introduction) to write this book for programmers instead of
     scientists and the next installments seem to continue that trend.
   | Still, I think it’s a good step in the right direction.

   I’ll certainly keep reading and I can’t wait for the next
   installment.

   Thanks

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-62651">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-62651">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| Pyry Kontio Says:

   .. raw:: html

      </div>

   `February 14, 2016 at 3:06
   pm <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-62651>`__
   Thanks for this series! I had a lot of trouble following the
   construction of limits earlier, because I didn’t know what “commuting
   diagrams” meant! Did I mistakenly skip something? Also, it might be
   helpful to provide some connection or intuition about the notion of
   commutative property in the context that most of us know it: normal,
   boring algebra.

   Btw. I have to ask for clearing this definition up:

       A lot of properties of morphisms are expressed in terms of
       commuting diagrams. If a particular morphism can be described as
       a composition of other morphisms in more than one way, then we
       have a commuting diagram.

   Does the words “more than one way” mean that there has to be at least
   2 different ways BESIDES the morphism itself? Or is is one additional
   way + the morphism itself enough for commutitavity?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-62654">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-62654">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `February 14, 2016 at 4:03
   pm <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/#comment-62654>`__
   A commuting diagram is a diagram where there are two paths (following
   the arrows in the diagram) that lead from one object to another and
   we require that both paths be equivalent. For instance, in a
   triangular diagram — say the left triangle of the definition of the
   product — we have two paths: one is p’ (single arrow), and the other
   is p after m. Because we are in a category, p after m corresponds to
   a single arrow p∘m that is the composition of p and m. Commutativity
   is the requirement that these two morphism: p’ and p∘m are identical.

   In a square diagram you want to go from one corner object to another.
   For instance, in the naturality diagram above, you want to get from F
   a to G b. You can either do that by composing G f after α\ :sub:`a`
   or α\ :sub:`b` after F f. Commutativity means that both of these
   composite morphism are identical. These morphisms lay on the diagonal
   of the square.

   Notice that, in principle, these two composite morphisms could be
   different from each other.

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
   reply </2015/11/17/its-all-about-morphisms/#respond>`__
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
-  November 17, 2015 at 9:47 am
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Functional
   Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/11/17/its-all-about-morphisms/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Blog at WordPress.com. <https://wordpress.com/?ref=footer_blog>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-b6f0deafb2a0ce7548fd001c623935d5">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c46ec8d611e261969be968350833ffa1">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-60b23f3de35058180807f35b623fcf83">

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

|image15|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |ProductRanking| image:: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg?w=171&h=139
   :class: alignnone wp-image-3772
   :width: 171px
   :height: 139px
   :target: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg
.. |3\_Naturality| image:: https://bartoszmilewski.files.wordpress.com/2015/04/3_naturality.jpg?w=216&h=179
   :class: alignnone wp-image-4349
   :width: 216px
   :height: 179px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/3_naturality.jpg
.. |Sheets| image:: https://bartoszmilewski.files.wordpress.com/2015/11/sheets.png?w=510
   :class: alignnone size-full wp-image-5221
   :target: https://bartoszmilewski.files.wordpress.com/2015/11/sheets.png
.. |image3| image:: https://2.gravatar.com/avatar/b6f0deafb2a0ce7548fd001c623935d5?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://0.gravatar.com/avatar/c46ec8d611e261969be968350833ffa1?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/c46ec8d611e261969be968350833ffa1?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://0.gravatar.com/avatar/60b23f3de35058180807f35b623fcf83?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image15| image:: https://pixel.wp.com/b.gif?v=noscript

