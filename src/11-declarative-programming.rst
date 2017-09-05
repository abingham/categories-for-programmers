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
   class="post-4415 post type-post status-publish format-standard hentry category-category-theory">

April 15, 2015

.. raw:: html

   <div class="post-info">

.. rubric:: Category Theory and Declarative Programming
   :name: category-theory-and-declarativeprogramming
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__
`[11]
Comments <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_4415" class="pd-rating">

.. raw:: html

   </div>

    This is part 11 of Categories for Programmers. Previously: `Natural
    Transformations <https://bartoszmilewski.com/2015/04/07/natural-transformations/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

.. rubric:: Introduction to Part II
   :name: introduction-to-part-ii

In the first part of the book I argued that both category theory and
programming are about composability. In programming, you keep
decomposing a problem until you reach the level of detail that you can
deal with, solve each subproblem in turn, and re-compose the solutions
bottom-up. There are, roughly speaking, two ways of doing it: by telling
the computer what to do, or by telling it how to do it. One is called
declarative and the other imperative.

You can see this even at the most basic level. Composition itself may be
defined declaratively; as in, ``h`` is a composite of ``g`` after ``f``:

::

    h = g . f

or imperatively; as in, call ``f`` first, remember the result of that
call, then call ``g`` with the result:

::

    h x = let y = f x
          in g y

The imperative version of a program is usually described as a sequence
of actions ordered in time. In particular, the call to ``g`` cannot
happen before the execution of ``f`` completes. At least, that’s the
conceptual picture — in a lazy language, with *call-by-need* argument
passing, the actual execution may proceed differently.

In fact, depending on the cleverness of the compiler, there may be
little or no difference between how declarative and imperative code is
executed. But the two methodologies differ, sometimes drastically, in
the way we approach problem solving and in the maintainability and
testability of the resulting code.

The main question is: when faced with a problem, do we always have the
choice between a declarative and imperative approaches to solving it?
And, if there is a declarative solution, can it always be translated
into computer code? The answer to this question is far from obvious and,
if we could find it, we would probably revolutionize our understanding
of the universe.

Let me elaborate. There is a similar duality in physics, which either
points at some deep underlying principle, or tells us something about
how our minds work. Richard Feynman mentions this duality as an
inspiration in his own work on quantum electrodynamics.

There are two forms of expressing most laws of physics. One uses local,
or infinitesimal, considerations. We look at the state of a system
around a small neighborhood, and predict how it will evolve within the
next instant of time. This is usually expressed using differential
equations that have to be integrated, or summed up, over a period of
time.

Notice how this approach resembles imperative thinking: we reach the
final solution by following a sequence of small steps, each depending on
the result of the previous one. In fact, computer simulations of
physical systems are routinely implemented by turning differential
equations into difference equations and iterating them. This is how
spaceships are animated in the asteroids game. At each time step, the
position of a spaceship is changed by adding a small increment, which is
calculated by multiplying its velocity by the time delta. The velocity,
in turn, is changed by a small increment proportional to acceleration,
which is given by force divided by mass.

|Asteroids|

These are the direct encodings of the differential equations
corresponding to Newton’s laws of motion:

::

    F = m dv/dt
    v = dx/dt

Similar methods may be applied to more complex problems, like the
propagation of electromagnetic fields using Maxwell’s equations, or even
the behavior of quarks and gluons inside a proton using lattice QCD
(quantum chromodynamics).

This local thinking combined with discretization of space and time that
is encouraged by the use of digital computers found its extreme
expression in the heroic attempt by Stephen Wolfram to reduce the
complexity of the whole universe to a system of cellular automata.

The other approach is global. We look at the initial and the final state
of the system, and calculate a trajectory that connects them by
minimizing a certain functional. The simplest example is the Fermat’s
principle of least time. It states that light rays propagate along paths
that minimize their flight time. In particular, in the absence of
reflecting or refracting objects, a light ray from point A to point B
will take the shortest path, which is a straight line. But light
propagates slower in dense (transparent) materials, like water or glass.
So if you pick the starting point in the air, and the ending point under
water, it’s more advantageous for light to travel longer in the air and
then take a shortcut through water. The path of minimum time makes the
ray refract at the boundary of air and water, resulting in Snell’s law
of refraction:

::

    sin θ1 / sin θ2 =  v1 / v2

where ``v1`` is the speed of light in the air and ``v2`` is the speed of
light in the water.

|Snell|

All of classical mechanics can be derived from the principle of least
action. The action can be calculated for any trajectory by integrating
the Lagrangian, which is the difference between kinetic and potential
energy (notice: it’s the difference, not the sum — the sum would be the
total energy). When you fire a mortar to hit a given target, the
projectile will first go up, where the potential energy due to gravity
is higher, and spend some time there racking up negative contribution to
the action. It will also slow down at the top of the parabola, to
minimize kinetic energy. Then it will speed up to go quickly through the
area of low potential energy.

|Mortar|

Feynman’s greatest contribution was to realize that the principle of
least action can be generalized to quantum mechanics. There, again, the
problem is formulated in terms of initial state and final state. The
Feynman path integral between those states is used to calculate the
probability of transition.

|Feynman|

The point is that there is a curious unexplained duality in the way we
can describe the laws of physics. We can use the local picture, in which
things happen sequentially and in small increments. Or we can use the
global picture, where we declare the initial and final conditions, and
everything in between just follows.

The global approach can be also used in programming, for instance when
implementing ray tracing. We declare the position of the eye and the
positions of light sources, and figure out the paths that the light rays
may take to connect them. We don’t explicitly minimize the time of
flight for each ray, but we do use Snell’s law and the geometry of
reflection to the same effect.

The biggest difference between the local and the global approach is in
their treatment of space and, more importantly, time. The local approach
embraces the immediate gratification of here and now, whereas the global
approach takes a long-term static view, as if the future had been
preordained, and we were only analyzing the properties of some eternal
universe.

Nowhere is it better illustrated than in the Functional Reactive
Programming approach to user interaction. Instead of writing separate
handlers for every possible user action, all having access to some
shared mutable state, FRP treats external events as an infinite list,
and applies a series of transformations to it. Conceptually, the list of
all our future actions is there, available as the input data to our
program. From a program’s perspective there’s no difference between the
list of digits of π, a list of pseudo-random numbers, or a list of mouse
positions coming through computer hardware. In each case, if you want to
get the nth item, you have to first go through the first n-1 items. When
applied to temporal events, we call this property *causality*.

So what does it have to do with category theory? I will argue that
category theory encourages a global approach and therefore supports
declarative programming. First of all, unlike calculus, it has no
built-in notion of distance, or neighborhood, or time. All we have is
abstract objects and abstract connections between them. If you can get
from A to B through a series of steps, you can also get there in one
leap. Moreover, the major tool of category theory is the universal
construction, which is the epitome of a global approach. We’ve seen it
in action, for instance, in the definition of the categorical product.
It was done by specifying its properties — a very declarative approach.
It’s an object equipped with two projections, and it’s the best such
object — it optimizes a certain property: the property of factorizing
the projections of other such objects.

| |ProductRanking|
| Compare this with Fermat’s principle of minimum time, or the principle
  of least action.

Conversely, contrast this with the traditional definition of a cartesian
product, which is much more imperative. You describe how to create an
element of the product by picking one element from one set and another
element from another set. It’s a recipe for creating a pair. And there’s
another for disassembling a pair.

In almost every programming language, including functional languages
like Haskell, product types, coproduct types, and function types are
built in, rather than being defined by universal constructions; although
there have been attempts at creating categorical programming languages
(see, e.g., `Tatsuya Hagino’s
thesis <http://synrc.com/publications/cat/Category%20Theory/Type%20Theory/Hagino%20T.%20A%20Categorical%20Programming%20Language.pdf>`__).

Whether used directly or not, categorical definitions justify
pre-existing programming constructs, and give rise to new ones. Most
importantly, category theory provides a meta-language for reasoning
about computer programs at a declarative level. It also encourages
reasoning about problem specification before it is cast into code.

Next `Limits and
Colimits <https://bartoszmilewski.com/2015/04/15/limits-and-colimits/>`__.

.. rubric:: Acknowledgments
   :name: acknowledgments

| I’d like to thank Gershom Bazerman for checking my math and logic, and
  André van Meulebrouck, who has been volunteering his editing help.
| `Follow @BartoszMilewski <https://twitter.com/BartoszMilewski>`__

.. raw:: html

   <div id="geo-post-4415" class="geo geo-post" style="display: none">

43.193051 11.286150

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

   <div id="crt-1079936371" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-1245219981" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/?share=email>`__
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

   <div id="like-post-wrapper-3549518-4415-59ae3c196ac2a"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=4415&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-4415-59ae3c196ac2a"
   data-name="like-post-frame-3549518-4415-59ae3c196ac2a">

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

.. rubric:: 11 Responses to “Category Theory and
   Declarative Programming”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-44794">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-44794">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| Mateusz Says:

   .. raw:: html

      </div>

   `April 15, 2015 at 2:15
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-44794>`__
   You Sir are doing AWESOME job!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-44829">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-44829">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `suhail shergill <http://shergill.su>`__ Says:

   .. raw:: html

      </div>

   `April 16, 2015 at 5:20
   am <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-44829>`__
       There is a similar duality in physics, which either points at
       some deep underlying principle

   said deep underlying principle might be something which generalizes
   noether’s theorem along with the fact that symmetries abound in our
   explanations of processes (be it due to there being a deeper reason
   for symmetries to almost surely exist, or for our minds to be wired
   to recognise them as “principle components” of sorts, i.e. for us to
   be able to almost surely recognize processes as being
   near-symmetries)

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-50515">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-50515">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `karkunow <http://karkunow.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `July 23, 2015 at 12:11
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-50515>`__
   Great analogy about local and global reasoning! It was just at the
   fingertips, but I didn’t notice that before.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-52877">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-52877">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| kram1032 Says:

   .. raw:: html

      </div>

   `September 8, 2015 at 1:03
   am <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-52877>`__
   | @suhail shergill
   | according to this http://bentnib.org/conservation-laws.pdf the
     Noether Theorem appears to essentially be a special case of the
     notion of theorems “for free”

   @Bartosz Milewski you forgot to add a link to Limits and Colimits –
   also, very nice analogy!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60300">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60300">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| phomer Says:

   .. raw:: html

      </div>

   `January 11, 2016 at 6:05
   am <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60300>`__
   My intuitive guess is that the top-down approach has the potential to
   be more expressive than Turing Machines. That is, there are ‘larger’
   abstract formal systems we can’t instantiate in physical reality.
   What do you think of Wolfram’s rule 110? That seems to mess with my
   intuition…

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60322">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60322">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 11, 2016 at 1:19
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60322>`__
   So far the top-down (global) and local approaches have been shown to
   be equivalent (at least in physics). Wolfram’s automata seem to be
   too simplistic to describe reality — good try, though. They are
   Turing complete, so they can simulate, digitally, more complex
   systems. But if we believe that space-time is continuous, the can
   only provide an approximation. But who knows, maybe reality is
   discrete below Planck scale.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60347">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60347">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| anonymous Says:

   .. raw:: html

      </div>

   `January 11, 2016 at 11:19
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60347>`__
   Doesn’t the existence of the Planck length itself strongly suggest
   that?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60360">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60360">

   .. raw:: html

      <div class="comment-author vcard">

   |image12| `Alaoui Solaimani
   Abdellah <https://www.facebook.com/app_scoped_user_id/582296345254427/>`__
   Says:

   .. raw:: html

      </div>

   `January 12, 2016 at 2:14
   am <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60360>`__
   I see here an analogy between strategy and operations

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60377">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60377">

   .. raw:: html

      <div class="comment-author vcard">

   |image13| phomer Says:

   .. raw:: html

      </div>

   `January 12, 2016 at 7:27
   am <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60377>`__
   Do you know if the expressiveness of category theory and/or set
   theory is beyond Turing Machines? I’ve kinda assumed that they were
   both broader formal systems, but I am just guessing. Software like
   Maple, Mathematica and Coq might actually shed some light on that.

   Another intuitive view that I’ve been headed towards is that if we do
   really live within a spacetime object, that it is essentially
   discrete. That is, notions of continums and infinity are really just
   our own abstract symbolic creations. i could be way wrong about that,
   but funny things like IP most likely being in a different complexity
   class than LP seem to keep tilting me in that direction.

   What disrupted me about rule 110 is not that I expect automata to be
   able to model the world, but rather I expected them to be contained
   and not burst through into being Turing-complete. In a sense, if they
   can flow upwards like that, then other formal systems can as well
   (which I sort of suspect, but it’s hazy).

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60406">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60406">

   .. raw:: html

      <div class="comment-author vcard">

   |image14| Zans Tangle Says:

   .. raw:: html

      </div>

   `January 12, 2016 at 3:08
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60406>`__
   Hi Bartosz, excellent post.

   Two questions:

   1) what method do you use for writing specifications of your
   software?

   2) what are your thoughts on the Hagino thesis?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-60407">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-60407">

   .. raw:: html

      <div class="comment-author vcard">

   |image15| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `January 12, 2016 at 3:09
   pm <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/#comment-60407>`__
   Citing from Wikipedia: “There is currently no proven physical
   significance of the Planck length.”

   We have a “natural” or “universal” unit of speed, the speed of light.
   We have a natural quantum unit, the Planck constant. And we have a
   natural unit of gravitational interaction G. A particular combination
   of these natural units can be used to construct a unit of length —
   the Planck length. However, because if mixes quantum theory with
   gravitational theory — two theories that refuse to be unified with
   each other — its physical significance is purely speculative. There’s
   nothing to show that there’s “no physics” below Planck length.

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
   reply </2015/04/15/category-theory-and-declarative-programming/#respond>`__
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
-  April 15, 2015 at 1:55 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2015/04/15/category-theory-and-declarative-programming/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-4e4c30251e549e7579eb6c7159cd25bc">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-6bdd1786d54fc62d1323968042398b35">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-6996fe77db9f65db1834b998b5222f9b">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-02b8f7ed2c25ec237e56603cd2669b4e">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-d92b64b0bbc0f2b7297924e76c4a4a84">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-2b35e0daa555e0c43375fb9ff4ffad36">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-46c6245f7fbf34e1209fb7f940a7df80">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-afc01c068bd26b2e44e66cdd6d2355fc">

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

.. |Asteroids| image:: https://bartoszmilewski.files.wordpress.com/2015/04/asteroids.png?w=300&h=225
   :class: alignnone size-medium wp-image-4432
   :width: 300px
   :height: 225px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/asteroids.png
.. |Snell| image:: https://bartoszmilewski.files.wordpress.com/2015/04/snell.jpg?w=224&h=300
   :class: alignnone size-medium wp-image-4437
   :width: 224px
   :height: 300px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/snell.jpg
.. |Mortar| image:: https://bartoszmilewski.files.wordpress.com/2015/04/mortar.jpg?w=300&h=158
   :class: alignnone size-medium wp-image-4438
   :width: 300px
   :height: 158px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/mortar.jpg
.. |Feynman| image:: https://bartoszmilewski.files.wordpress.com/2015/04/feynman.jpg?w=300&h=225
   :class: alignnone size-medium wp-image-4439
   :width: 300px
   :height: 225px
   :target: https://bartoszmilewski.files.wordpress.com/2015/04/feynman.jpg
.. |ProductRanking| image:: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg?w=300&h=244
   :class: alignnone size-medium wp-image-3772
   :width: 300px
   :height: 244px
   :target: https://bartoszmilewski.files.wordpress.com/2014/12/productranking.jpg
.. |image5| image:: https://1.gravatar.com/avatar/4e4c30251e549e7579eb6c7159cd25bc?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/6bdd1786d54fc62d1323968042398b35?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://0.gravatar.com/avatar/6996fe77db9f65db1834b998b5222f9b?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://0.gravatar.com/avatar/02b8f7ed2c25ec237e56603cd2669b4e?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://1.gravatar.com/avatar/d92b64b0bbc0f2b7297924e76c4a4a84?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://2.gravatar.com/avatar/2b35e0daa555e0c43375fb9ff4ffad36?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image12| image:: https://1.gravatar.com/avatar/46c6245f7fbf34e1209fb7f940a7df80?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image13| image:: https://1.gravatar.com/avatar/d92b64b0bbc0f2b7297924e76c4a4a84?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image14| image:: https://1.gravatar.com/avatar/afc01c068bd26b2e44e66cdd6d2355fc?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image15| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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

