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
   class="post-4445 post type-post status-publish format-standard hentry category-category-theory">

April 15, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Limits and Colimits
   :name: limits-and-colimits
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__
`[29]
Comments <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4445" class="pd-rating">

.. raw:: html

   </div>

    This is part 12 of Categories for Programmers. Previously:
    `Declarative
    Programming <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

It seems like in category theory everything is related to everything and
everything can be viewed from many angles. Take for instance the
universal construction of the
`product <https://bartoszmilewski.com/2015/01/07/products-and-coproducts/>`__.
Now that we know more about
`functors <https://bartoszmilewski.com/2015/01/20/functors/>`__ and
`natural
transformations <https://bartoszmilewski.com/2015/04/07/natural-transformations/>`__,
can we simplify and, possibly, generalize it? Let us try.

|ProductPattern|

The construction of a product starts with the selection of two objects
``a`` and ``b``, whose product we want to construct. But what does it
mean to *select objects*? Can we rephrase this action in more
categorical terms? Two objects form a pattern — a very simple pattern.
We can abstract this pattern into a category — a very simple category,
but a category nevertheless. It’s a category that we’ll call **2**. It
contains just two objects, 1 and 2, and no morphisms other than the two
obligatory identities. Now we can rephrase the selection of two objects
in *C* as the act of defining a functor D from the category **2** to
*C*. A functor maps objects to objects, so its image is just two objects
(or it could be one, if the functor collapses objects, which is fine
too). It also maps morphisms — in this case it simply maps identity
morphisms to identity morphisms.

|Two|

What’s great about this approach is that it builds on categorical
notions, eschewing the imprecise descriptions like “selecting objects,”
taken straight from the hunter-gatherer lexicon of our ancestors. And,
incidentally, it is also easily generalized, because nothing can stop us
from using categories more complex than **2** to define our patterns.

But let’s continue. The next step in the definition of a product is the
selection of the candidate object ``c``. Here again, we could rephrase
the selection in terms of a functor from a singleton category. And
indeed, if we were using Kan extensions, that would be the right thing
to do. But since we are not ready for Kan extensions yet, there is
another trick we can use: a constant functor Δ from the same category
**2** to *C*. The selection of ``c`` in *C* can be done with
Δ\ :sub:`c`. Remember, Δ\ :sub:`c` maps all objects into ``c`` and all
morphisms into ``idc``.

|TwoDelta|

Now we have two functors, Δ\ :sub:`c` and D going between **2** and *C*
so it’s only natural to ask about natural transformations between them.
Since there are only two objects in **2**, a natural transformation will
have two components. Object 1 in **2** is mapped to ``c`` by Δ\ :sub:`c`
and to ``a`` by D. So the component of a natural transformation between
Δ\ :sub:`c` and D at 1 is a morphism from ``c`` to ``a``. We can call it
``p``. Similarly, the second component is a morphism ``q`` from ``c`` to
``b`` — the image of the object 2 in **2** under D. But these are
exactly like the two projections we used in our original definition of
the product. So instead of talking about selecting objects and
projections, we can just talk about picking functors and natural
transformations. It so happens that in this simple case the naturality
condition for our transformation is trivially satisfied, because there
are no morphisms (other than the identities) in **2**.

|ProductCone|

A generalization of this construction to categories other than **2** —
ones that, for instance, contain non-trivial morphisms — will impose
naturality conditions on the transformation between Δ\ :sub:`c` and D.
We call such transformation a *cone*, because the image of Δ is the apex
of a cone/pyramid whose sides are formed by the components of the
natural transformation. The image of D forms the base of the cone.

In general, to build a cone, we start with a category *I* that defines
the pattern. It’s a small, often finite category. We pick a functor D
from *I* to *C* and call it (or its image) a *diagram*. We pick some
``c`` in *C* as the apex of our cone. We use it to define the constant
functor Δ\ :sub:`c` from *I* to *C*. A natural transformation from
Δ\ :sub:`c` to D is then our cone. For a finite *I* it’s just a bunch of
morphisms connecting ``c`` to the diagram: the image of *I* under D.

|Cone|

Naturality requires that all triangles (the walls of the pyramid) in
this diagram commute. Indeed, take any morphism ``f`` in *I*. The
functor D maps it to a morphism ``D f`` in *C*, a morphism that forms
the base of some triangle. The constant functor Δ\ :sub:`c` maps ``f``
to the identity morphism on ``c``. Δ squishes the two ends of the
morphism into one object, and the naturality square becomes a commuting
triangle. The two arms of this triangle are the components of the
natural transformation.

|ConeNaturality|

So that’s one cone. What we are interested in is the *universal cone* —
just like we picked a universal object for our definition of a product.

There are many ways to go about it. For instance, we may define a
*category of cones* based on a given functor D. Objects in that category
are cones. Not every object ``c`` in *C* can be an apex of a cone,
though, because there may be no natural transformation between
Δ\ :sub:`c` and D.

To make it a category, we also have to define morphisms between cones.
These would be fully determined by morphisms between their apexes. But
not just any morphism will do. Remember that, in our construction of the
product, we imposed the condition that the morphisms between candidate
objects (the apexes) must be common factors for the projections. For
instance:

::

    p' = p . m
    q' = q . m

|ProductRanking|

This condition translates, in the general case, to the condition that
the triangles whose one side is the factorizing morphism all commute.

.. raw:: html

   <div id="attachment_4487" class="wp-caption alignnone"
   data-shortcode="caption" style="width: 249px">

|Cone Commutativity|
The commuting triangle connecting two cones, with the factorizing
morphism ``h``  (here, the lower cone is the universal one, with
``Lim D`` as its apex).

.. raw:: html

   </div>

We’ll take those factorizing morphisms as the morphisms in our category
of cones. It’s easy to check that those morphisms indeed compose, and
that the identity morphism is a factorizing morphism as well. Cones
therefore form a category.

Now we can define the universal cone as the *terminal object* in the
category of cones. The definition of the terminal object states that
there is a unique morphism from any other object to that object. In our
case it means that there is a unique factorizing morphism from the apex
of any other cone to the apex of the universal cone. We call this
universal cone the *limit* of the diagram D, ``Lim D`` (in the
literature, you’ll often see a left arrow pointing towards *I* under the
``Lim`` sign). Often, as a shorthand, we call the apex of this cone the
limit (or the limit object).

The intuition is that the limit embodies the properties of the whole
diagram in a single object. For instance, the limit of our two-object
diagram is the product of two objects. The product (together with the
two projections) contains the information about both objects. And being
universal means that it has no extraneous junk.

.. rubric:: Limit as a Natural Isomorphism
   :name: limit-as-a-natural-isomorphism

There is still something unsatisfying about this definition of a limit.
I mean, it’s workable, but we still have this commutativity condition
for the triangles that are linking any two cones. It would be so much
more elegant if we could replace it with some naturality condition. But
how?

We are no longer dealing with one cone but with a whole collection (in
fact, a category) of cones. If the limit exists (and — let’s make it
clear — there’s no guarantee of that), one of those cones is the
universal cone. For every other cone we have a unique factorizing
morphism that maps its apex, let’s call it ``c``, to the apex of the
universal cone, which we named ``Lim D``. (In fact, I can skip the word
“other,” because the identity morphism maps the universal cone to itself
and it trivially factorizes through itself.) Let me repeat the important
part: given any cone, there is a unique morphism of a special kind. We
have a mapping of cones to special morphisms, and it’s a one-to-one
mapping.

This special morphism is a member of the hom-set ``C(c, Lim D)``. The
other members of this hom-set are less fortunate, in the sense that they
don’t factorize the mapping of cones. What we want is to be able to
pick, for each ``c``, one morphism from the set ``C(c, Lim D)`` — a
morphism that satisfies the particular commutativity condition. Does
that sound like defining a natural transformation? It most certainly
does!

But what are the functors that are related by this transformation?

One functor is the mapping of ``c`` to the set ``C(c, Lim D)``. It’s a
functor from *C* to **Set** — it maps objects to sets. In fact it’s a
contravariant functor. Here’s how we define its action on morphisms:
Let’s take a morphism ``f`` from ``c'`` to ``c``:

::

    f :: c' -> c

Our functor maps ``c'`` to the set ``C(c', Lim D)``. To define the
action of this functor on ``f`` (in other words, to lift ``f``), we have
to define the corresponding mapping between ``C(c, Lim D)`` and
``C(c', Lim D)``. So let’s pick one element ``u`` of ``C(c, Lim D)`` and
see if we can map it to some element of ``C(c', Lim D)``. An element of
a hom-set is a morphism, so we have:

::

    u :: c -> Lim D

We can precompose ``u`` with ``f`` to get:

::

    u . f :: c' -> Lim D

And that’s an element of ``C(c', Lim D)``— so indeed, we have found a
mapping of morphisms:

::

    contramap :: (c' -> c) -> (c -> Lim D) -> (c' -> Lim D)
    contramap f u = u . f

Notice the inversion in the order of ``c`` and ``c'`` characteristic of
a *contravariant* functor.

|HomSetMapping|

To define a natural transformation, we need another functor that’s also
a mapping from *C* to **Set**. But this time we’ll consider a set of
cones. Cones are just natural transformations, so we are looking at the
set of natural transformations ``Nat(Δc, D)``. The mapping from ``c`` to
this particular set of natural transformations is a (contravariant)
functor. How can we show that? Again, let’s define its action on a
morphism:

::

    f :: c' -> c

The lifting of ``f`` should be a mapping of natural transformations
between two functors that go from *I* to *C*:

::

    Nat(Δc, D) -> Nat(Δc', D)

How do we map natural transformations? Every natural transformation is a
selection of morphisms — its components — one morphism per element of
*I*. A component of some α (a member of ``Nat(Δc, D)``) at ``a`` (an
object in *I*) is a morphism:

::

    αa :: Δca -> D a

or, using the definition of the constant functor Δ,

::

    αa :: c -> D a

Given ``f`` and α, we have to construct a β, a member of
``Nat(Δc', D)``. Its component at ``a`` should be a morphism:

::

    βa :: c' -> D a

We can easily get the latter from the former by precomposing it with
``f``:

::

    βa = αa . f

| It’s relatively easy to show that those components indeed add up to a
  natural transformation.
| |NatMapping|

Given our morphism ``f``, we have thus built a mapping between two
natural transformations, component-wise. This mapping defines
``contramap`` for the functor:

::

    c -> Nat(Δc, D)

What I have just done is to show you that we have two (contravariant)
functors from *C* to **Set**. I haven’t made any assumptions — these
functors always exist.

Incidentally, the first of these functors plays an important role in
category theory, and we’ll see it again when we talk about Yoneda’s
lemma. There is a name for contravariant functors from any category *C*
to **Set**: they are called “presheaves.” This one is called a
*representable presheaf*. The second functor is also a presheaf.

Now that we have two functors, we can talk about natural transformations
between them. So without further ado, here’s the conclusion: A functor
``D`` from *I* to *C* has a limit ``Lim D`` if and only if there is a
natural isomorphism between the two functors I have just defined:

::

    C(c, Lim D) ≃ Nat(Δc, D)

Let me remind you what a natural isomorphism is. It’s a natural
transformation whose every component is an isomorphism, that is to say
an invertible morphism.

I’m not going to go through the proof of this statement. The procedure
is pretty straightforward if not tedious. When dealing with natural
transformations, you usually focus on components, which are morphisms.
In this case, since the target of both functors is **Set**, the
components of the natural isomorphism will be functions. These are
higher order functions, because they go from the hom-set to the set of
natural transformations. Again, you can analyze a function by
considering what it does to its argument: here the argument will be a
morphism — a member of ``C(c, Lim D)`` — and the result will be a
natural transformation — a member of ``Nat(Δc, D)``, or what we have
called a cone. This natural transformation, in turn, has its own
components, which are morphisms. So it’s morphisms all the way down, and
if you can keep track of them, you can prove the statement.

The most important result is that the naturality condition for this
isomorphism is exactly the commutativity condition for the mapping of
cones.

As a preview of coming attractions, let me mention that the set
``Nat(Δc, D)`` can be thought of as a hom-set in the functor category;
so our natural isomorphism relates two hom-sets, which points at an even
more general relationship called an adjunction.

.. rubric:: Examples of Limits
   :name: examples-of-limits

We’ve seen that the categorical product is a limit of a diagram
generated by a simple category we called **2**.

There is an even simpler example of a limit: the terminal object. The
first impulse would be to think of a singleton category as leading to a
terminal object, but the truth is even starker than that: the terminal
object is a limit generated by an empty category. A functor from an
empty category selects no object, so a cone shrinks to just the apex.
The universal cone is the lone apex that has a unique morphism coming to
it from any other apex. You will recognize this as the definition of the
terminal object.

The next interesting limit is called the *equalizer*. It’s a limit
generated by a two-element category with two parallel morphisms going
between them (and, as always, the identity morphisms). This category
selects a diagram in *C* consisting of two objects, ``a`` and ``b``, and
two morphisms:

::

    f :: a -> b
    g :: a -> b

To build a cone over this diagram, we have to add the apex, ``c`` and
two projections:

::

    p :: c -> a
    q :: c -> b

|EqualizerCone|

We have two triangles that must commute:

::

    q = f . p
    q = g . p

This tells us that ``q`` is uniquely determined by one of these
equations, say, ``q = f . p``, and we can omit it from the picture. So
we are left with just one condition:

::

    f . p = g . p

The way to think about it is that, if we restrict our attention to
**Set**, the image of the function ``p`` selects a subset of ``a``. When
restricted to this subset, the functions ``f`` and ``g`` are equal.

For instance, take ``a`` to be the two-dimensional plane parameterized
by coordinates ``x`` and ``y``. Take ``b`` to be the real line, and
take:

::

    f (x, y) = 2 * y + x
    g (x, y) = y - x

The equalizer for these two functions is the set of real numbers (the
apex, ``c``) and the function:

::

    p t = (t, (-2) * t)

Notice that ``(p t)`` defines a straight line in the two-dimensional
plane. Along this line, the two functions are equal.

Of course, there are other sets ``c'`` and functions ``p'`` that may
lead to the equality:

::

    f . p' = g . p'

but they all uniquely factor out through ``p``. For instance, we can
take the singleton set ``()`` as ``c'`` and the function:

::

    p'() = (0, 0)

It’s a good cone, because ``f (0, 0) = g (0, 0)``. But it’s not
universal, because of the unique factorization through ``h``:

::

    p' = p . h

with

::

    h () = 0

| |EquilizerLimit|
| An equalizer can thus be used to solve equations of the type
  ``f x = g x``. But it’s much more general, because it’s defined in
  terms of objects and morphisms rather than algebraically.

An even more general idea of solving an equation is embodied in another
limit — the pullback. Here, we still have two morphisms that we want to
equate, but this time their domains are different. We start with a
three-object category of the shape: ``1->2<-3``. The diagram
corresponding to this category consists of three objects, ``a``, ``b``,
and ``c``, and two morphisms:

::

    f :: a -> b
    g :: c -> b

This diagram is often called a *cospan*.

A cone built on top of this diagram consists of the apex, ``d``, and
three morphisms:

::

    p :: d -> a
    q :: d -> c
    r :: d -> b

|PullbackCone|

Commutativity conditions tell us that ``r`` is completely determined by
the other morphisms, and can be omitted from the picture. So we are only
left with the following condition:

::

    g . q = f . p

A pullback is a universal cone of this shape.

|PullbackLimit|

Again, if you narrow your focus down to sets, you can think of the
object ``d`` as consisting of pairs of elements from ``a`` and ``c`` for
which ``f`` acting on the first component is equal to ``g`` acting on
the second component. If this is still too general, consider the special
case in which ``g`` is a constant function, say ``g _ = 1.23`` (assuming
that ``b`` is a set of real numbers). Then you are really solving the
equation:

::

    f x = 1.23

In this case, the choice of ``c`` is irrelevant (as long as it’s not an
empty set), so we can take it to be a singleton set. The set ``a``
could, for instance, be the set of three-dimensional vectors, and ``f``
the vector length. Then the pullback is the set of pairs ``(v, ())``,
where ``v`` is a vector of length 1.23 (a solution to the equation
``sqrt (x2+y2+z2) = 1.23``), and ``()`` is the dummy element of the
singleton set.

But pullbacks have more general applications, also in programming. For
instance, consider C++ classes as a category in which morphism are
arrows that connect subclasses to superclasses. We’ll consider
inheritance a transitive property, so if C inherits from B and B
inherits from A then we’ll say that C inherits from A (after all, you
can pass a pointer to C where a pointer to A is expected). Also, we’ll
assume that C inherits from C, so we have the identity arrow for every
class. This way subclassing is aligned with subtyping. C++ also supports
multiple inheritance, so you can construct a diamond inheritance diagram
with two classes B and C inheriting from A, and a fourth class D
multiply inheriting from B and C. Normally, D would get two copies of A,
which is rarely desirable; but you can use virtual inheritance to have
just one copy of A in D.

What would it mean to have D be a pullback in this diagram? It would
mean that any class E that multiply inherits from B and C is also a
subclass of D. This is not directly expressible in C++, where subtyping
is nominal (the C++ compiler wouldn’t infer this kind of class
relationship — it would require “duck typing”). But we could go outside
of the subtyping relationship and instead ask whether a cast from E to D
would be safe or not. This cast would be safe if D were the bare-bone
combination of B and C, with no additional data and no overriding of
methods. And, of course, there would be no pullback if there is a name
conflict between some methods of B and C.

|Classes|

There’s also a more advanced use of a pullback in type inference. There
is often a need to *unify* types of two expressions. For instance,
suppose that the compiler wants to infer the type of a function:

::

    twice f x = f (f x)

It will assign preliminary types to all variables and sub-expressions.
In particular, it will assign:

::

    f       :: t0
    x       :: t1
    f x     :: t2
    f (f x) :: t3

from which it will deduce that:

::

    twice :: t0 -> t1 -> t3

It will also come up with a set of constraints resulting from the rules
of function application:

::

    t0 = t1 -> t2 -- because f is applied to x
    t0 = t2 -> t3 -- because f is applied to (f x)

These constraints have to be unified by finding a set of types (or type
variables) that, when substituted for the unknown types in both
expressions, produce the same type. One such substitution is:

::

    t1 = t2 = t3 = Int
    twice :: (Int -> Int) -> Int -> Int

but, obviously, it’s not the most general one. The most general
substitution is obtained using a pullback. I won’t go into the details,
because they are beyond the scope of this book, but you can convince
yourself that the result should be:

::

    twice :: (t -> t) -> t -> t

with ``t`` a free type variable.

.. rubric:: Colimits
   :name: colimits

Just like all constructions in category theory, limits have their dual
image in opposite categories. When you invert the direction of all
arrows in a cone, you get a co-cone, and the universal one of those is
called a colimit. Notice that the inversion also affects the factorizing
morphism, which now flows from the universal co-cone to any other
co-cone.

.. raw:: html

   <div id="attachment_4494" class="wp-caption alignnone"
   data-shortcode="caption" style="width: 160px">

|Colimit|
Cocone with a factorizing morphism ``h`` connecting two apexes.

.. raw:: html

   </div>

A typical example of a colimit is a coproduct, which corresponds to the
diagram generated by **2**, the category we’ve used in the definition of
the product.

|CoproductRanking|

Both the product and the coproduct embody the essence of a pair of
objects, each in a different way.

Just like the terminal object was a limit, so the initial object is a
colimit corresponding to the diagram based on an empty category.

The dual of the pullback is called the *pushout*. It’s based on a
diagram called a span, generated by the category ``1<-2->3``.

.. rubric:: Continuity
   :name: continuity

I said previously that functors come close to the idea of continuous
mappings of categories, in the sense that they never break existing
connections (morphisms). The actual definition of a *continuous functor*
``F`` from a category *C* to *C’* includes the requirement that the
functor preserve limits. Every diagram ``D`` in *C* can be mapped to a
diagram ``F ∘ D`` in *C’* by simply composing two functors. The
continuity condition for ``F`` states that, if the diagram ``D`` has a
limit ``Lim D``, then the diagram ``F ∘ D`` also has a limit, and it is
equal to ``F (Lim D)``.

|Continuity|

Notice that, because functors map morphisms to morphisms, and
compositions to compositions, an image of a cone is always a cone. A
commuting triangle is always mapped to a commuting triangle (functors
preserve composition). The same is true for the factorizing morphisms:
the image of a factorizing morphism is also a factorizing morphism. So
every functor is *almost* continuous. What may go wrong is the
uniqueness condition. The factorizing morphism in *C’* might not be
unique. There may also be other “better cones” in *C’* that were not
available in *C*.

A hom-functor is an example of a continuous functor. Recall that the
hom-functor, ``C(a, b)``, is contravariant in the first variable and
covariant in the second. In other words, it’s a functor:

::

    Cop × C -> Set

When its second argument is fixed, the hom-set functor (which becomes
the representable presheaf) maps colimits in *C* to limits in **Set**;
and when its first argument is fixed, it maps limits to limits.

In Haskell, a hom-functor is the mapping of any two types to a function
type, so it’s just a parameterized function type. When we fix the second
parameter, let’s say to ``String``, we get the contravariant functor:

::

    newtype ToString a = ToString (a -> String)
    instance Contravariant ToString where
        contramap f (ToString g) = ToString (g . f)

Continuity means that when ``ToString`` is applied to a colimit, for
instance a coproduct ``Either b c``, it will produce a limit; in this
case a product of two function types:

::

    ToString (Either b c) ~ (b -> String, c -> String)

Indeed, any function of ``Either b c`` is implemented as a case
statement with the two cases being serviced by a pair of functions.

Similarly, when we fix the first argument of the hom-set, we get the
familiar reader functor. Its continuity means that, for instance, any
function returning a product is equivalent to a product of functions; in
particular:

::

    r -> (a, b) ~ (r -> a, r -> b)

I know what you’re thinking: You don’t need category theory to figure
these things out. And you’re right! Still, I find it amazing that such
results can be derived from first principles with no recourse to bits
and bytes, processor architectures, compiler technologies, or even
lambda calculus.

If you’re curious where the names “limit” and “continuity” come from,
they are a generalization of the corresponding notions from calculus. In
calculus limits and continuity are defined in terms of open
neighborhoods. Open sets, which define topology, form a category (a
poset).

.. rubric:: Challenges
   :name: challenges

#. How would you describe a pushout in the category of C++ classes?
#. Show that the limit of the identity functor ``Id :: C -> C`` is the
   initial object.
#. Subsets of a given set form a category. A morphism in that category
   is defined to be an arrow connecting two sets if the first is the
   subset of the second. What is a pullback of two sets in such a
   category? What’s a pushout? What are the initial and terminal
   objects?
#. Can you guess what a coequalizer is?
#. Show that, in a category with a terminal object, a pullback towards
   the terminal object is a product.
#. Similarly, show that a pushout from an initial object (if one exists)
   is the coproduct.

Next: `Free
Monoids <https://bartoszmilewski.com/2015/07/21/free-monoids/>`__.

.. rubric:: Acknowledgments
   :name: acknowledgments

| I’d like to thank Gershom Bazerman for checking my math and logic, and
  André van Meulebrouck, who has been volunteering his editing help.
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

   <div id="crt-467066400" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-616227263" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4445-59ae3c2ed831c"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4445&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4445-59ae3c2ed831c"
   data-name="like-post-frame-3549518-4445-59ae3c2ed831c">

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

.. rubric:: 29 Responses to “Limits and Colimits”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-45704">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45704">

   .. raw:: html

      <div class="comment-author vcard">

   |image18| `Rasmus Svensson <http://gravatar.com/raekmannen>`__ Says:

   .. raw:: html

      </div>

   `May 5, 2015 at 9:14
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-45704>`__
   Shouldn’t the double-colon be an equals sign here?

   ::

       t0 :: t1 -> t2

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-45705">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45705">

   .. raw:: html

      <div class="comment-author vcard">

   |image19| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `May 5, 2015 at 9:51
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-45705>`__
   Oops! Fixed, thanks.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-45822">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-45822">

   .. raw:: html

      <div class="comment-author vcard">

   |image20| `kimolas <http://kimolas.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `May 8, 2015 at 6:42
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-45822>`__
   “A generalization of this construction to categories other tha[n] 2”

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-46286">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-46286">

   .. raw:: html

      <div class="comment-author vcard">

   |image21| `bhall <http://bhall.wordpress.com/>`__ Says:

   .. raw:: html

      </div>

   `May 18, 2015 at 5:23
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-46286>`__
   Do limits have to be unique? I’m guessing the answer is “no” since
   products only have to be “unique up to isomorphism”. Or is there some
   way the limit gets around this?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-46296">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-46296">

   .. raw:: html

      <div class="comment-author vcard">

   |image22| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `May 18, 2015 at 10:42
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-46296>`__
   As with all universal constructions, they are unique up to a unique
   isomorphism.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50737">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50737">

   .. raw:: html

      <div class="comment-author vcard">

   |image23| ahala Says:

   .. raw:: html

      </div>

   `July 27, 2015 at 3:32
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-50737>`__
   Do these words “limits”, “Continuity” come from analysis? How do they
   coincide when taking analysis from the the point of view of Category?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50763">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50763">

   .. raw:: html

      <div class="comment-author vcard">

   |image24| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 27, 2015 at 12:37
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-50763>`__
   @ahala: Yes, indeed. More specifically from topology. Open subsets
   form a category in a topological space, with inclusions playing the
   role of morphisms. **Top** is a category of topological spaces with
   continuous functions as morphisms.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54806">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54806">

   .. raw:: html

      <div class="comment-author vcard">

   |image25| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 2, 2015 at 6:15
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-54806>`__
   Let G be the functor from C to Set that maps every c in C to the set
   of cones (for functor D) with apex c. IOW, ``G(c) = Nat(Δc, D)``.
   Next, for *any* X in C, let ``FX`` be the functor from C to Set that
   maps every c in C to the set C(c, X). Also, for any X in C, let
   ``PX`` stand for the assertion “there exists a natural isomorphism
   between the functors ``FX`` and G.” Finally, let T denote the theorem
   stated in the “Limit as natural isomorphism” section (right after “So
   without further ado, here’s the conclusion:…”. I can paraphrase T (or
   rather, my understanding of it) in two not-obviously-equivalent ways.
   Here’s the first one: “The functor D has a limit with apex X if and
   only if ``PX``.” And the second one: “\ *Any* cone with apex X is a
   limit of D if and only if ``PX``.” Maybe these two formulations can
   be made to coincide with a suitable insertion of “up to [unique]
   isomorphism” somewhere, but the question still remains: is either of
   them indeed equivalent to T?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54861">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54861">

   .. raw:: html

      <div class="comment-author vcard">

   |image26| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 3, 2015 at 5:48
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-54861>`__
   In the example on pullbacks (the one where one of the morphisms is a
   function with image {42}), it’s not clear to me what the pullback d
   is in the end. AFAICT, it is the product e \\times c, where e is the
   set {t \| t \\in a and f t = 42}. Is this correct? Either way, I
   think the example would be more useful to your readers if the
   pullback was fully spelled out. (BTW, sorry for the inept notation; I
   have not figured out how to render math in these comments.)

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54887">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54887">

   .. raw:: html

      <div class="comment-author vcard">

   |image27| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 2:31
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-54887>`__
   @weekendwarrior: Your first question boils down to: Is the limiting
   cone unique? The answer is: Like every universal construction, it’s
   unique up to unique isomorphism.

   As for your second question, I added a parenthetical remark that
   should clear the confusion (the image of g is not a set — it’s a
   number — an element of the set of numbers).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54893">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54893">

   .. raw:: html

      <div class="comment-author vcard">

   |image28| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 3:58
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-54893>`__
   We must be using different definitions. I’ve always seen the “image
   of a function” g defined as the *subset* of the function’s codomain
   consisting of the elements g(x), as x ranges over the function’s
   domain. In the example it so happens that the set corrsponding to
   this description is a singleton, but it is still a set. But maybe
   we’re talking past each other. I think your point is that in the
   example, the category C where the pullback lives is not the category
   Set, but rather something else. Haskell types? If so, I’m even more
   curious now to know what exactly is the pullback (i.e. the object d
   and morphisms p and q) in that example.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-54941">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-54941">

   .. raw:: html

      <div class="comment-author vcard">

   |image29| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 4, 2015 at 8:51
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-54941>`__
   You’re right about the image. Thank you for keeping me on my toes.

   I rewrote the example so it’s more explicit. I hope it helps.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-55000">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-55000">

   .. raw:: html

      <div class="comment-author vcard">

   |image30| weekendwarrior Says:

   .. raw:: html

      </div>

   `October 6, 2015 at 12:44
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-55000>`__
   Yes, the new version is very clear. Thanks!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66057">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66057">

   .. raw:: html

      <div class="comment-author vcard">

   |image31| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `July 13, 2016 at 2:26
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-66057>`__
   When you say that the terminal object is the limit making I the empty
   category, how does delta\_c picks the apex of the cone? If I is the
   empty category no object of I can be used to get to c using delta\_c.
   What am I missing?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66061">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66061">

   .. raw:: html

      <div class="comment-author vcard">

   |image32| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `July 13, 2016 at 9:52
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-66061>`__
   @Juan: Good point! This is one of these tricky limiting cases, like
   dividing zero by zero. On the one hand, you can’t get to ``c``
   because the source category is empty. On the other hand, the constant
   functor ignores its argument, so it shouldn’t matter.

   The best explanation is that the const functor trick is more of a
   motivation for the rigorous definition of a limit as a natural
   isomorphism:

   ::

       C(c, Lim D) ≃ Nat(Δc, D)

   Let’s analyze the right hand side. There is only one functor from the
   empty category to *C* — the empty functor. So both ``Δc`` and ``D``
   are empty functors. There is only one natural transformation between
   empty functors — the identity natural transformation. The right hand
   side is therefore a singleton set, for any choice of ``c``. Which
   means that all the hom-sets ``C(c, Lim D)`` must be singletons. That
   makes ``Lim D`` the terminal object.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66075">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66075">

   .. raw:: html

      <div class="comment-author vcard">

   |image33| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__
   Says:

   .. raw:: html

      </div>

   `July 14, 2016 at 12:02
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-66075>`__
   Thanks for your explanation. I can’t say I fully understand it but at
   least makes perfect sense.

   Somehow this reminds me of an scene of the film Matrix when a child
   says: “there is no spoon”. But here we have an identity natural
   transformation between two no-spoons (empty functors).

   Mindboggling.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66783">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66783">

   .. raw:: html

      <div class="comment-author vcard">

   |image34| Shimin Guo Says:

   .. raw:: html

      </div>

   `September 7, 2016 at 2:27
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-66783>`__
   How is it that a cone is entirely determined by its apex when D is
   fixed? Couldn’t there be multiple natural transformations from Δc to
   D, which means multiple cones corresponding to the same c?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-66785">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-66785">

   .. raw:: html

      <div class="comment-author vcard">

   |image35| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `September 7, 2016 at 4:20
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-66785>`__
   Shimin Guo: You’re absolutely right. In fact I later talk about a set
   of natural transformations ``Nat(Δc, D)``. I’m removing this
   statement from the post. Thank you for spotting this.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67403">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67403">

   .. raw:: html

      <div class="comment-author vcard">

   |image36| `John Armstrong <http://drmathochist.wordpress.com/>`__
   Says:

   .. raw:: html

      </div>

   `October 23, 2016 at 1:22
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67403>`__
   Sorry to come in so long after the post went up, but I wanted to
   point out a slight error in terminology. When discussing pull-backs,
   the diagram you’re talking about is related to spans, but is not
   ITSELF a span.

   A span in a category is a diagram of the form

   |A\_1\\leftarrow B\\rightarrow A\_2|

   (I’m hoping that works, but just in case: A1 A2)

   Given A1 and A2, the spans between them form a category as you might
   expect: morphisms are the ones taken from hom(B, B’) that make both
   side triangles commute. They look like product cones, actually.

   What does this have to do with pull-backs? well, spans can be
   composed! Say we’ve got two of them:

   | |A\_1\\leftarrow B\_1\\rightarrow A\_2|
   | |A\_2\\leftarrow B\_2\\rightarrow A\_3|

   We can chain them up in the middle:

   |A\_1\\leftarrow B\_1\\rightarrow A\_2\\leftarrow B\_2\\rightarrow
   A\_3|

   And now we can pull back the middle part to get an object C with
   morphisms to B1 and B2 that make the square in the middle commute.
   Composing with the outer morphisms we get morphisms from C to A1 and
   A3, which makes a span:

   |A\_1\\leftarrow C\\rightarrow A\_3|

   Does this mean that spans form a category? well, not quite. The
   problem is that this composition by pull-backs isn’t quite
   associative. But if we consider the morphisms between spans (the ones
   that look like product cone morphisms from before), then this
   composition of spans (as 1-morphisms) is associative *up to a
   canonical 2-morphism*, making this a “weak” 2-category. The specific
   2-morphism is defined by a natural isomorphism called the
   “associator”, the existence of which should follow from other
   material in this very post!

   Similarly, in push-outs the diagrams you mention are not quite
   co-spans, but the ideas are related, mutatis mutandis, as above.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67415">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67415">

   .. raw:: html

      <div class="comment-author vcard">

   |image42| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 23, 2016 at 10:01
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67415>`__
   You’re right, I confused spans with cospans. Fixed!

   I just talked about bicategories in my last lecture. This would be a
   good example.

   BTW, I had no idea latex would work in WordPress. Must be a new
   thing.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67417">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67417">

   .. raw:: html

      <div class="comment-author vcard">

   |image43| `John Armstrong <http://drmathochist.wordpress.com/>`__
   Says:

   .. raw:: html

      </div>

   `October 24, 2016 at 5:56
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67417>`__
   I used to use it when I was still math-blogging at
   unapologetic.wordpress.com. Seems to be on by default here!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67420">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67420">

   .. raw:: html

      <div class="comment-author vcard">

   |image44| eschnett Says:

   .. raw:: html

      </div>

   `October 24, 2016 at 5:11
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67420>`__
   You mention “category 13” in your post where you introduce spans. I
   assume that’s an html quoting error, where the arrows have turned
   into comments and eaten the object “2”.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67421">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67421">

   .. raw:: html

      <div class="comment-author vcard">

   |image45| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `October 24, 2016 at 7:17
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67421>`__
   @eschnett: Exactly! Thanks for noticing.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67772">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67772">

   .. raw:: html

      <div class="comment-author vcard">

   |image46| capnfreako Says:

   .. raw:: html

      </div>

   `November 25, 2016 at 7:16
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67772>`__
   After reviewing the Wikipedia article on *coequalizer*, as well as a
   few of the references it points to, I’m still not clear on its
   significance in programming. Can you offer any hints?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67774">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67774">

   .. raw:: html

      <div class="comment-author vcard">

   |image47| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `November 25, 2016 at 1:55
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-67774>`__
   This `blog
   post <http://blog.functorial.com/posts/2012-02-19-What-If-Haskell-Had-Equalizers.html>`__
   by Phil Freeman provides some intuitions about equalizers and
   coequalizers.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69876">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69876">

   .. raw:: html

      <div class="comment-author vcard">

   |image48| Mark Says:

   .. raw:: html

      </div>

   `April 5, 2017 at 2:20
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-69876>`__
   I would like to dive deeper into your Hindley-Milner pullback
   example. Do you have any references I can look at?

   I’m really enjoying the videos, thanks for the good work!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-69917">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-69917">

   .. raw:: html

      <div class="comment-author vcard">

   |image49| veix Says:

   .. raw:: html

      </div>

   `April 8, 2017 at 5:56
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-69917>`__
   “The intuition is that the limit embodies the properties of the whole
   diagram in a single object.”

   this intuition seems misleading to me in the case of equalizer

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70976">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70976">

   .. raw:: html

      <div class="comment-author vcard">

   |image50| `karkunow <http://karkunow.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `May 23, 2017 at 3:38
   pm <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-70976>`__
   | @Bartosz,
   | It seems that there is a problem with ‘precomposing’ word. You have
     two statements:
   | 1). We can precompose f with u to get: u . f :: c’ -> Lim D
   | 2). We can easily get the latter from the former by precomposing it
     with f: βa = αa . f

   | And obviously one of them must be wrong.
   | To solve the problem you need to change, for example, the second
     one to:
   | “We can easily get the latter from the former by composing it with
     f: βa = αa . f”
   | or
   | “We can easily get the latter from the former by precomposing f
     with αa: βa = αa . f”

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-71007">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-71007">

   .. raw:: html

      <div class="comment-author vcard">

   |image51| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `May 24, 2017 at 8:21
   am <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/#comment-71007>`__
   @karkunow: Yes, this is an ongoing problem. I think I will, from now
   on, use precompose f to mean “first act with f” which, incidentally
   means that f will be on the right of the dot (that’s where the
   confusion comes from: it’s “post” dot).

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
   reply </2015/04/15/limits-and-colimits/#respond>`__
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
-  April 15, 2015 at 8:46 am
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-4c62f44155a43ed215dd680683e8dce7">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-9e9968f17084069910490607aae744c8">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c28371a3b467d45b93aed57252229d89">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-8ba5c32605adc61b0bc9b6396482c7ac">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-4aa6db921795b84b986eb4aac8ffd569">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-b4a7426cee3700d21354b77b4a29fddd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-4b9e90ffb9e35c53596e1234c34a92f0">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ed8df1b934fbb8259a5d1f369e168172">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ef3ac5dd2108e05f505ff9ed04740196">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-90b33f49f6f5de5cf8e80f0a7aac6caa">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-9ccc8e38bbfb38b77d468bfc5d9e3307">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-ce17817b1139b9d248ff46805e263b8d">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-6996fe77db9f65db1834b998b5222f9b">

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

|image58|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |ProductPattern| image:: https://bartoszmilewski.files.wordpress.com/2014/12/productpattern.jpg?w=150&h=99
   :class: alignnone wp-image-3767 size-thumbnail
   :width: 150px
   :height: 99px
   :target: https://bartoszmilewski.files.wordpress.com/2014/12/productpattern.jpg
.. |Two| image:: https://bartoszmilewski.files.wordpress.com/2015/04/two.jpg?w=300&h=220
   :class: alignnone wp-image-4482 size-medium
   :width: 300px
   :height: 220px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/two.jpg
.. |TwoDelta| image:: https://bartoszmilewski.files.wordpress.com/2015/04/twodelta.jpg?w=300&h=212
   :class: alignnone size-medium wp-image-4483
   :width: 300px
   :height: 212px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/twodelta.jpg
.. |ProductCone| image:: https://bartoszmilewski.files.wordpress.com/2015/04/productcone.jpg?w=300&h=203
   :class: alignnone size-medium wp-image-4498
   :width: 300px
   :height: 203px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/productcone.jpg
.. |Cone| image:: https://bartoszmilewski.files.wordpress.com/2015/04/cone.jpg?w=300&h=216
   :class: alignnone size-medium wp-image-4485
   :width: 300px
   :height: 216px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/cone.jpg
.. |ConeNaturality| image:: https://bartoszmilewski.files.wordpress.com/2015/04/conenaturality.jpg?w=300&h=232
   :class: alignnone size-medium wp-image-4486
   :width: 300px
   :height: 232px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/conenaturality.jpg
.. |ProductRanking| image:: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg?w=205&h=167
   :class: alignnone wp-image-3772
   :width: 205px
   :height: 167px
   :target: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg
.. |Cone Commutativity| image:: https://bartoszmilewski.files.wordpress.com/2015/04/conecommutativity.jpg?w=239&h=214
   :class: wp-image-4487
   :width: 239px
   :height: 214px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/conecommutativity.jpg
.. |HomSetMapping| image:: https://bartoszmilewski.files.wordpress.com/2015/04/homsetmapping.jpg?w=249&h=200
   :class: alignnone wp-image-4488
   :width: 249px
   :height: 200px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/homsetmapping.jpg
.. |NatMapping| image:: https://bartoszmilewski.files.wordpress.com/2015/04/natmapping.jpg?w=300&h=194
   :class: alignnone size-medium wp-image-4489
   :width: 300px
   :height: 194px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/natmapping.jpg
.. |EqualizerCone| image:: https://bartoszmilewski.files.wordpress.com/2015/04/equalizercone.jpg?w=218&h=201
   :class: alignnone wp-image-4490
   :width: 218px
   :height: 201px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/equalizercone.jpg
.. |EquilizerLimit| image:: https://bartoszmilewski.files.wordpress.com/2015/04/equilizerlimit.jpg?w=211&h=223
   :class: alignnone wp-image-4491
   :width: 211px
   :height: 223px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/equilizerlimit.jpg
.. |PullbackCone| image:: https://bartoszmilewski.files.wordpress.com/2015/04/pullbackcone.jpg?w=238&h=172
   :class: alignnone wp-image-4492
   :width: 238px
   :height: 172px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/pullbackcone.jpg
.. |PullbackLimit| image:: https://bartoszmilewski.files.wordpress.com/2015/04/pullbacklimit.jpg?w=204&h=227
   :class: alignnone wp-image-4493
   :width: 204px
   :height: 227px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/pullbacklimit.jpg
.. |Classes| image:: https://bartoszmilewski.files.wordpress.com/2015/04/classes.jpg?w=155&h=222
   :class: alignnone wp-image-4500
   :width: 155px
   :height: 222px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/classes.jpg
.. |Colimit| image:: https://bartoszmilewski.files.wordpress.com/2015/04/colimit.jpg?w=150&h=150
   :class: wp-image-4494 size-thumbnail
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/colimit.jpg
.. |CoproductRanking| image:: https://bartoszmilewski.files.wordpress.com/2014/12/coproductranking.jpg?w=150&h=125
   :class: alignnone wp-image-3775 size-thumbnail
   :width: 150px
   :height: 125px
   :target: https://bartoszmilewski.files.wordpress.com/2014/12/coproductranking.jpg
.. |Continuity| image:: https://bartoszmilewski.files.wordpress.com/2015/04/continuity.jpg?w=300&h=86
   :class: alignnone wp-image-4495 size-medium
   :width: 300px
   :height: 86px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/continuity.jpg
.. |image18| image:: https://1.gravatar.com/avatar/4c62f44155a43ed215dd680683e8dce7?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image19| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image20| image:: https://0.gravatar.com/avatar/9e9968f17084069910490607aae744c8?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image21| image:: https://0.gravatar.com/avatar/c28371a3b467d45b93aed57252229d89?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image22| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image23| image:: https://2.gravatar.com/avatar/8ba5c32605adc61b0bc9b6396482c7ac?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image24| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image25| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image26| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image27| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image28| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image29| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image30| image:: https://1.gravatar.com/avatar/4aa6db921795b84b986eb4aac8ffd569?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image31| image:: https://i1.wp.com/pbs.twimg.com/profile_images/452017421855907841/W65GNlUV_normal.jpeg?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image32| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image33| image:: https://i1.wp.com/pbs.twimg.com/profile_images/452017421855907841/W65GNlUV_normal.jpeg?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image34| image:: https://1.gravatar.com/avatar/4b9e90ffb9e35c53596e1234c34a92f0?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image35| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image36| image:: https://2.gravatar.com/avatar/ed8df1b934fbb8259a5d1f369e168172?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |A\_1\\leftarrow B\\rightarrow A\_2| image:: https://s0.wp.com/latex.php?latex=A_1%5Cleftarrow+B%5Crightarrow+A_2&bg=ffffff&fg=29303b&s=0
   :class: latex
.. |A\_1\\leftarrow B\_1\\rightarrow A\_2| image:: https://s0.wp.com/latex.php?latex=A_1%5Cleftarrow+B_1%5Crightarrow+A_2&bg=ffffff&fg=29303b&s=0
   :class: latex
.. |A\_2\\leftarrow B\_2\\rightarrow A\_3| image:: https://s0.wp.com/latex.php?latex=A_2%5Cleftarrow+B_2%5Crightarrow+A_3&bg=ffffff&fg=29303b&s=0
   :class: latex
.. |A\_1\\leftarrow B\_1\\rightarrow A\_2\\leftarrow B\_2\\rightarrow A\_3| image:: https://s0.wp.com/latex.php?latex=A_1%5Cleftarrow+B_1%5Crightarrow+A_2%5Cleftarrow+B_2%5Crightarrow+A_3&bg=ffffff&fg=29303b&s=0
   :class: latex
.. |A\_1\\leftarrow C\\rightarrow A\_3| image:: https://s0.wp.com/latex.php?latex=A_1%5Cleftarrow+C%5Crightarrow+A_3&bg=ffffff&fg=29303b&s=0
   :class: latex
.. |image42| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image43| image:: https://2.gravatar.com/avatar/ed8df1b934fbb8259a5d1f369e168172?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image44| image:: https://2.gravatar.com/avatar/ef3ac5dd2108e05f505ff9ed04740196?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image45| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image46| image:: https://0.gravatar.com/avatar/90b33f49f6f5de5cf8e80f0a7aac6caa?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image47| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image48| image:: https://0.gravatar.com/avatar/9ccc8e38bbfb38b77d468bfc5d9e3307?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image49| image:: https://0.gravatar.com/avatar/ce17817b1139b9d248ff46805e263b8d?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image50| image:: https://0.gravatar.com/avatar/6996fe77db9f65db1834b998b5222f9b?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image51| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image58| image:: https://pixel.wp.com/b.gif?v=noscript

