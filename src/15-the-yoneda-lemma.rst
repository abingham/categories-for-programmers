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
   class="post-4914 post type-post status-publish format-standard hentry category-category-theory category-haskell category-programming">

September 1, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: The Yoneda Lemma
   :name: the-yoneda-lemma
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Programming <https://bartoszmilewski.com/category/programming/>`__
`[15]
Comments <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4914" class="pd-rating">

.. raw:: html

   </div>

    This is part 15 of Categories for Programmers. Previously:
    `Representable
    Functors <https://bartoszmilewski.com/2015/07/29/representable-functors/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

Most constructions in category theory are generalizations of results
from other more specific areas of mathematics. Things like products,
coproducts, monoids, exponentials, etc., have been known long before
category theory. They might have been known under different names in
different branches of mathematics. A cartesian product in set theory, a
meet in order theory, a conjunction in logic — they are all specific
examples of the abstract idea of a categorical product.

The Yoneda lemma stands out in this respect as a sweeping statement
about categories in general with little or no precedent in other
branches of mathematics. Some say that its closest analog is Cayley’s
theorem in group theory (every group is isomorphic to a permutation
group of some set).

The setting for the Yoneda lemma is an arbitrary category *C* together
with a functor ``F`` from *C* to **Set**. We’ve seen in the previous
section that some **Set**-valued functors are representable, that is
isomorphic to a hom-functor. The Yoneda lemma tells us that all
**Set**-valued functors can be obtained from hom-functors through
natural transformations, and it explicitly enumerates all such
transformations.

When I talked about natural transformations, I mentioned that the
naturality condition can be quite restrictive. When you define a
component of a natural transformation at one object, naturality may be
strong enough to “transport” this component to another object that is
connected to it through a morphism. The more arrows between objects in
the source and the target categories there are, the more constraints you
have for transporting the components of natural transformations. **Set**
happens to be a very arrow-rich category.

The Yoneda lemma tells us that a natural transformation between a
hom-functor and any other functor ``F`` is completely determined by
specifying the value of its single component at just one point! The rest
of the natural transformation just follows from naturality conditions.

So let’s review the naturality condition between the two functors
involved in the Yoneda lemma. The first functor is the hom-functor. It
maps any object ``x`` in *C* to the set of morphisms ``C(a, x)`` — for
``a`` a fixed object in *C*. We’ve also seen that it maps any morphism
``f`` from ``x`` to ``y`` to ``C(a, f)``.

The second functor is an arbitrary **Set**-valued functor ``F``.

Let’s call the natural transformation between these two functors ``α``.
Because we are operating in **Set**, the components of the natural
transformation, like ``αx`` or ``αy``, are just regular functions
between sets:

::

    αx :: C(a, x) -> F x
    αy :: C(a, y) -> F y

|Yoneda1|

And because these are just functions, we can look at their values at
specific points. But what’s a point in the set ``C(a, x)``? Here’s the
key observation: Every point in the set ``C(a, x)`` is also a morphism
``h`` from ``a`` to ``x``.

So the naturality square for ``α``:

::

    αy ∘ C(a, f) = F f ∘ αx

becomes, point-wise, when acting on ``h``:

::

    αy (C(a, f) h) = (F f) (αx h)

You might recall from the previous section that the action of the
hom-functor ``C(a,-)`` on a morphism ``f`` was defined as
precomposition:

::

    C(a, f) h = f ∘ h

which leads to:

::

    αy (f ∘ h) = (F f) (αx h)

Just how strong this condition is can be seen by specializing it to the
case of ``x`` equal to ``a``.

|Yoneda2|

In that case ``h`` becomes a morphism from ``a`` to ``a``. We know that
there is at least one such morphism, ``h = ida``. Let’s plug it in:

::

    αy f = (F f) (αa ida)

Notice what has just happened: The left hand side is the action of
``αy`` on an arbitrary element ``f`` of ``C(a, y)``. And it is totally
determined by the single value of ``αa`` at ``ida``. We can pick any
such value and it will generate a natural transformation. Since the
values of ``αa`` are in the set ``F a``, any point in ``F a`` will
define some ``α``.

Conversely, given any natural transformation ``α`` from ``C(a, -)`` to
``F``, you can evaluate it at ``ida`` to get a point in ``F a``.

We have just proven the Yoneda lemma:

There is a one-to-one correspondence between natural transformations
from ``C(a, -)`` to ``F`` and elements of ``F a``.

in other words,

::

    Nat(C(a, -), F) ≅ F a

Or, if we use the notation ``[C, Set]`` for the functor category between
*C* and **Set**, the set of natural transformation is just a hom-set in
that category, and we can write:

::

    [C, Set](C(a, -), F) ≅ F a

I’ll explain later how this correspondence is in fact a natural
isomorphism.

Now let’s try to get some intuition about this result. The most amazing
thing is that the whole natural transformation crystallizes from just
one nucleation site: the value we assign to it at ``ida``. It spreads
from that point following the naturality condition. It floods the image
of *C* in **Set**. So let’s first consider what the image of *C* is
under ``C(a, -)``.

Let’s start with the image of ``a`` itself. Under the hom-functor
``C(a, -)``, ``a`` is mapped to the set ``C(a, a)``. Under the functor
``F``, on the other hand, it is mapped to the set ``F a``. The component
of the natural transformation ``αa`` is some function from ``C(a, a)``
to ``F a``. Let’s focus on just one point in the set ``C(a, a)``, the
point corresponding to the morphism ``ida``. To emphasize the fact that
it’s just a point in a set, let’s call it ``p``. The component ``αa``
should map ``p`` to some point ``q`` in ``F a``. I’ll show you that any
choice of ``q`` leads to a unique natural transformation.

|Yoneda3|

The first claim is that the choice of one point ``q`` uniquely
determines the rest of the function ``αa``. Indeed, let’s pick any other
point, ``p'`` in ``C(a, a)``, corresponding to some morphism ``g`` from
``a`` to ``a``. And here’s where the magic of the Yoneda lemma happens:
``g`` can be viewed as a point ``p'`` in the set ``C(a, a)``. At the
same time, it selects two *functions* between sets. Indeed, under the
hom-functor, the morphism ``g`` is mapped to a function ``C(a, g)``; and
under ``F`` it’s mapped to ``F g``.

|Yoneda4|

Now let’s consider the action of ``C(a, g)`` on our original ``p``
which, as you remember, corresponds to ``ida``. It is defined as
precomposition, ``g∘ida``, which is equal to ``g``, which corresponds to
our point ``p'``. So the morphism ``g`` is mapped to a function that,
when acting on ``p`` produces ``p'``, which is ``g``. We have come full
circle!

Now consider the action of ``F g`` on ``q``. It is some ``q'``, a point
in ``F a``. To complete the naturality square, ``p'`` must be mapped to
``q'`` under ``αa``. We picked an arbitrary ``p'`` (an arbitrary ``g``)
and derived its mapping under ``αa``. The function ``αa`` is thus
completely determined.

The second claim is that ``αx`` is uniquely determined for any object
``x`` in *C* that is connected to ``a``. The reasoning is analogous,
except that now we have two more sets, ``C(a, x)`` and ``F x``, and the
morphism ``g`` from ``a`` to ``x`` is mapped, under the hom-functor, to:

::

    C(a, g) :: C(a, a) -> C(a, x)

and under ``F`` to:

::

    F g :: F a -> F x

Again, ``C(a, g)`` acting on our ``p`` is given by the precomposition:
``g ∘ ida``, which corresponds to a point ``p'`` in ``C(a, x)``.
Naturality determines the value of ``αx`` acting on ``p'`` to be:

::

    q' = (F g) q

Since ``p'`` was arbitrary, the whole function ``αx`` is thus
determined.

|Yoneda5|

What if there are objects in *C* that have no connection to ``a``? They
are all mapped under ``C(a, -)`` to a single set — the empty set. Recall
that the empty set is the initial object in the category of sets. It
means that there is a unique function from this set to any other set. We
called this function ``absurd``. So here, again, we have no choice for
the component of the natural transformation: it can only be ``absurd``.

One way of understanding the Yoneda lemma is to realize that natural
transformations between **Set**-valued functors are just families of
functions, and functions are in general lossy. A function may collapse
information and it may cover only parts of its codomain. The only
functions that are not lossy are the ones that are invertible — the
isomorphisms. It follows then that the best structure-preserving
**Set**-valued functors are the representable ones. They are either the
hom-functors or the functors that are naturally isomorphic to
hom-functors. Any other functor ``F`` is obtained from a hom-functor
through a lossy transformation. Such a transformation may not only lose
information, but it may also cover only a small part of the image of the
functor ``F`` in **Set**.

.. rubric:: Yoneda in Haskell
   :name: yoneda-in-haskell

We have already encountered the hom-functor in Haskell under the guise
of the reader functor:

::

    type Reader a x = a -> x

The reader maps morphisms (here, functions) by precomposition:

::

    instance Functor (Reader a) where
        fmap f h = f . h

The Yoneda lemma tells us that the reader functor can be naturally
mapped to any other functor.

A natural transformation is a polymorphic function. So given a functor
``F``, we have a mapping to it from the reader functor:

::

    alpha :: forall x . (a -> x) -> F x

As usual, ``forall`` is optional, but I like to write it explicitly to
emphasize parametric polymorphism of natural transformations.

The Yoneda lemma tells us that these natural transformations are in
one-to-one correspondence with the elements of ``F a``:

::

    forall x . (a -> x) -> F x ≅ F a

The right hand side of this identity is what we would normally consider
a data structure. Remember the interpretation of functors as generalized
containers? ``F a`` is a container of ``a``. But the left hand side is a
polymorphic function that takes a function as an argument. The Yoneda
lemma tells us that the two representations are equivalent — they
contain the same information.

Another way of saying this is: Give me a polymorphic function of the
type:

::

    alpha :: forall x . (a -> x) -> F x

and I’ll produce a container of ``a``. The trick is the one we used in
the proof of the Yoneda lemma: we call this function with ``id`` to get
an element of ``F a``:

::

    alpha id :: F a

The converse is also true: Given a value of the type ``F a``:

::

    fa :: F a

one can define a polymorphic function:

::

    alpha h = fmap h fa

of the correct type. You can easily go back and forth between the two
representations.

The advantage of having multiple representations is that one might be
easier to compose than the other, or that one might be more efficient in
some applications than the other.

The simplest illustration of this principle is the code transformation
that is often used in compiler construction: the continuation passing
style or CPS. It’s the simplest application of the Yoneda lemma to the
identity functor. Replacing ``F`` with identity produces:

::

    forall r . (a -> r) -> r ≅ a

The interpretation of this formula is that any type ``a`` can be
replaced by a function that takes a “handler” for ``a``. A handler is a
function accepting ``a`` and performing the rest of the computation —
the continuation. (The type ``r`` usually encapsulates some kind of
status code.)

This style of programming is very common in UIs, in asynchronous
systems, and in concurrent programming. The drawback of CPS is that it
involves inversion of control. The code is split between producers and
consumers (handlers), and is not easily composable. Anybody who’s done
any amount of nontrivial web programming is familiar with the nightmare
of spaghetti code from interacting stateful handlers. As we’ll see
later, judicious use of functors and monads can restore some
compositional properties of CPS.

.. rubric:: Co-Yoneda
   :name: co-yoneda

As usual, we get a bonus construction by inverting the direction of
arrows. The Yoneda lemma can be applied to the opposite category
*C*\ :sup:`op` to give us a mapping between contravariant functors.

Equivalently, we can derive the co-Yoneda lemma by fixing the target
object of our hom-functors instead of the source. We get the
contravariant hom-functor from *C* to **Set**: ``C(-, a)``. The
contravariant version of the Yoneda lemma establishes one-to-one
correspondence between natural transformations from this functor to any
other contravariant functor ``F`` and the elements of the set ``F a``:

::

    Nat(C(-, a), F) ≅ F a

Here’s the Haskell version of the co-Yoneda lemma:

::

    forall x . (x -> a) -> F x ≅ F a

Notice that in some literature it’s the contravariant version that’s
called the Yoneda lemma.

.. rubric:: Challenges
   :name: challenges

#. Show that the two functions ``phi`` and ``psi`` that form the Yoneda
   isomorphism in Haskell are inverses of each other.

   ::

       phi :: (forall x . (a -> x) -> F x) -> F a
       phi alpha = alpha id

   ::

       psi :: F a -> (forall x . (a -> x) -> F x)
       psi fa h = fmap h fa

#. A discrete category is one that has objects but no morphisms other
   than identity morphisms. How does the Yoneda lemma work for functors
   from such a category?
#. A list of units ``[()]`` contains no other information but its
   length. So, as a data type, it can be considered an encoding of
   integers. An empty list encodes zero, a singleton ``[()]`` (a value,
   not a type) encodes one, and so on. Construct another representation
   of this data type using the Yoneda lemma for the list functor.

.. rubric:: Bibliography
   :name: bibliography

#. `Catsters <https://www.youtube.com/watch?v=TLMxHB19khE>`__ video

Next: `Yoneda
Embedding <https://bartoszmilewski.com/2015/10/28/yoneda-embedding/>`__.

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

   <div id="crt-1129338883" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1100220514" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4914-59ae3c672e2ac"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4914&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4914-59ae3c672e2ac"
   data-name="like-post-frame-3549518-4914-59ae3c672e2ac">

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

.. rubric:: 15 Responses to “The Yoneda Lemma”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-52458">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52458">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `Robert Harper <http://www.cs.cmu.edu/~rwh>`__ Says:

   .. raw:: html

      </div>

   `September 2, 2015 at 5:12
   am <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-52458>`__
   When explaining Yoneda I find it useful to do the pre-order case
   first. It states that in any pre-order, x<=y iff for all z, z<=x
   implies z<=y. The proof is trivial, but the utility of this little
   lemma is amazing. One example is in the proof that every Heyting
   algebra (exponentiated lattice) is distributive. It is a dawdle using
   Yoneda; I don’t know how to do it without it.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52483">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52483">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `September 2, 2015 at 7:06
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-52483>`__
   I was thinking of including the pre-order example, which I saw
   mentioned in your Oregon lectures (which I keep re-watching, every
   time understanding a little more). I wasn’t sure how to explain the
   “implies” part though. But you’re right, it is trivial.

   The sets in question can only be a singleton and an empty set. There
   can be no function from singleton (corresponding to z<=x being true)
   to an empty set (corresponding to z<=y being false), so the existence
   of a natural transformation is equivalent to the implication z<=x =>
   z<=y.

   The cool thing about blogging is that I can modify a post in response
   to suggestions. So I will revise this post to include this example.
   And I’ll include a poset example in my next post on Yoneda embedding.

   As for Heyting algebras, that would require a blog post or two just
   to introduce the concept. I don’t assume any particular math
   knowledge beyond high school from my readers, so I avoid examples
   that involve formal logic, topological spaces, etc.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52485">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52485">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `Robert Harper <http://www.cs.cmu.edu/~rwh>`__ Says:

   .. raw:: html

      </div>

   `September 2, 2015 at 8:06
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-52485>`__
   Yes, I had thought of remarking about the “implies” as a degenerate
   natural transformation between hom sets, but decided to be brief.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52932">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52932">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| kram1032 Says:

   .. raw:: html

      </div>

   `September 8, 2015 at 4:50
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-52932>`__
   Since it looks like the introduction to Monads is coming closer: I
   know too little about all this but there is a recent paper here:
   http://okmij.org/ftp/Haskell/extensible/ describing computationally
   efficient, composable methods to handle side-effects. Maybe some of
   it would be valuable for Monads? Or maybe for later topics.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-57042">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-57042">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| benmoussa Says:

   .. raw:: html

      </div>

   `November 6, 2015 at 12:10
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-57042>`__
   i think there is a mistake in last diagram c(a;g) not c(x,g)

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-57045">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-57045">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `November 6, 2015 at 2:59
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-57045>`__
   @benmoussa: Good catch! I didn’t like the colors in this diagram
   anyway, so I’m going to redo it.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-65895">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-65895">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| Alex Says:

   .. raw:: html

      </div>

   `July 1, 2016 at 11:43
   am <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-65895>`__
   Does this mean type system is boolean? I.e. double negation
   (A->*\|*)->\ *\|* can produce A for all A.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-65896">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-65896">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| vpatryshev Says:

   .. raw:: html

      </div>

   `July 1, 2016 at 1:58
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-65896>`__
   Is it okay that a theorem proven in Sets is applied to Hask?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-65897">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-65897">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 1, 2016 at 4:33
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-65897>`__
   @vpatryshev: I decided from the very beginning to ignore the Hask vs
   Set distinction, unless otherwise noted. There are some good reasons
   for using Set arguments in Hask, although one should always be
   careful. See for instance `Fast and Loose Reasoning is Morally
   Correct <http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html>`__.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67474">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67474">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| `John Armstrong <http://drmathochist.wordpress.com/>`__
   Says:

   .. raw:: html

      </div>

   `October 31, 2016 at 7:56
   am <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-67474>`__
   As a quick side-note, I’d go further than saying Cayley’s theorem is
   the closest analogue to Yoneda. I’d say that it *is* Yoneda, in the
   special case of a category with one object and all isomorphisms.
   Representable functors in that case are nothing but G-torsors!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67892">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67892">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `December 2, 2016 at 11:15
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-67892>`__
   Is it precomposition or postcomposition? If we do f . h, which is f
   after h, shouldn’t we say that we post-compose with f (or we
   pre-compose with h)?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67899">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67899">

   .. raw:: html

      <div class="comment-author vcard">

   |image16| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `December 3, 2016 at 11:01
   am <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-67899>`__
   Yes, pre and post are very ambiguous in this context. Normally we
   consider putting something on the left as “pre”, but the left
   function is executed “after” the right function so, in this sense,
   it’s “post”. There doesn’t seem to be a consensus, as seen in `this
   discussion <http://math.stackexchange.com/questions/304339/terminology-question-precompose-vs-compose>`__.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73667">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73667">

   .. raw:: html

      <div class="comment-author vcard">

   |image17| stevemao Says:

   .. raw:: html

      </div>

   `August 13, 2017 at 10:07
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-73667>`__
   I’ve heard that Coyoneda is just the Free Functor. How can I
   understand it?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73669">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73669">

   .. raw:: html

      <div class="comment-author vcard">

   |image18| stevemao Says:

   .. raw:: html

      </div>

   `August 14, 2017 at 12:10
   am <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-73669>`__
       Notice that in some literature it’s the contravariant version
       that’s called the Yoneda lemma.

   And they call covariant version Co-Yoneda?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-73694">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-73694">

   .. raw:: html

      <div class="comment-author vcard">

   |image19| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `August 14, 2017 at 4:05
   pm <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/#comment-73694>`__
   I discuss the free functor in the post on `Kan
   extensions <https://bartoszmilewski.com/2017/04/17/kan-extensions/>`__.
   It is indeed identical to co-Yoneda. See also Haskell library
   `Data.Functor.Coyoneda <https://hackage.haskell.org/package/kan-extensions-5.0.2/docs/Data-Functor-Coyoneda.html>`__.

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
   reply </2015/09/01/the-yoneda-lemma/#respond>`__
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
-  September 1, 2015 at 7:45 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Programming <https://bartoszmilewski.com/category/programming/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-58dfeb7db21bb8a5c6aa108b804078fd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-02b8f7ed2c25ec237e56603cd2669b4e">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-27873eea53430385e0629360524ab7fe">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-0e4d0d6c5ce5e9350cacc818ee350347">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-dae11b30544529b3c0836855a14cd653">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ed8df1b934fbb8259a5d1f369e168172">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-b4a7426cee3700d21354b77b4a29fddd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-084668018e4752a76a838f27e041d182">

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

|image26|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |Yoneda1| image:: https://bartoszmilewski.files.wordpress.com/2015/08/yoneda1-e1440290035365.png?w=263&h=259
   :class: alignnone wp-image-4983
   :width: 263px
   :height: 259px
   :target: https://bartoszmilewski.files.wordpress.com/2015/08/yoneda1.png
.. |Yoneda2| image:: https://bartoszmilewski.files.wordpress.com/2015/08/yoneda2.png?w=300&h=275
   :class: alignnone size-medium wp-image-4987
   :width: 300px
   :height: 275px
   :target: https://bartoszmilewski.files.wordpress.com/2015/08/yoneda2.png
.. |Yoneda3| image:: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda3.png?w=510
   :class: alignnone wp-image-5217 size-full
   :target: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda3.png
.. |Yoneda4| image:: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda4.png?w=510
   :class: alignnone wp-image-5218 size-full
   :target: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda4.png
.. |Yoneda5| image:: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda5.png?w=510
   :class: alignnone wp-image-5219 size-full
   :target: https://bartoszmilewski.files.wordpress.com/2015/09/yoneda5.png
.. |image5| image:: https://2.gravatar.com/avatar/58dfeb7db21bb8a5c6aa108b804078fd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://2.gravatar.com/avatar/58dfeb7db21bb8a5c6aa108b804078fd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/02b8f7ed2c25ec237e56603cd2669b4e?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://2.gravatar.com/avatar/27873eea53430385e0629360524ab7fe?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://0.gravatar.com/avatar/0e4d0d6c5ce5e9350cacc818ee350347?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://1.gravatar.com/avatar/dae11b30544529b3c0836855a14cd653?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://2.gravatar.com/avatar/ed8df1b934fbb8259a5d1f369e168172?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://2.gravatar.com/avatar/b4a7426cee3700d21354b77b4a29fddd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image16| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image17| image:: https://0.gravatar.com/avatar/084668018e4752a76a838f27e041d182?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image18| image:: https://0.gravatar.com/avatar/084668018e4752a76a838f27e041d182?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image19| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image26| image:: https://pixel.wp.com/b.gif?v=noscript

