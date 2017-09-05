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
   class="post-5226 post type-post status-publish format-standard hentry category-category-theory category-functional-programming category-haskell category-monads">

November 21, 2016

.. raw:: html

   <div class="post-info">

.. rubric:: Monads: Programmer’s Definition
   :name: monads-programmers-definition
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Functional
Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Monads <https://bartoszmilewski.com/category/monads/>`__
`[12]
Comments <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_5226" class="pd-rating">

.. raw:: html

   </div>

    This is part 20 of Categories for Programmers. Previously:
    `Free/Forgetful
    Adjunctions <https://bartoszmilewski.com/2016/06/15/freeforgetful-adjunctions/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

Programmers have developed a whole mythology around monads. It’s
supposed to be one of the most abstract and difficult concepts in
programming. There are people who “get it” and those who don’t. For
many, the moment when they understand the concept of the monad is like a
mystical experience. The monad abstracts the essence of so many diverse
constructions that we simply don’t have a good analogy for it in
everyday life. We are reduced to groping in the dark, like those blind
men touching different parts of the elephant end exclaiming
triumphantly: “It’s a rope,” “It’s a tree trunk,” or “It’s a burrito!”

Let me set the record straight: The whole mysticism around the monad is
the result of a misunderstanding. The monad is a very simple concept.
It’s the diversity of applications of the monad that causes the
confusion.

As part of research for this post I looked up duct tape (a.k.a., duck
tape) and its applications. Here’s a little sample of things that you
can do with it:

-  sealing ducts
-  fixing CO\ :sub:`2` scrubbers on board Apollo 13
-  wart treatment
-  fixing Apple’s iPhone 4 dropped call issue
-  making a prom dress
-  building a suspension bridge

Now imagine that you didn’t know what duct tape was and you were trying
to figure it out based on this list. Good luck!

So I’d like to add one more item to the collection of “the monad is
like…” clichés: The monad is like duct tape. Its applications are widely
diverse, but its principle is very simple: it glues things together.
More precisely, it composes things.

This partially explains the difficulties a lot of programmers,
especially those coming from the imperative background, have with
understanding the monad. The problem is that we are not used to thinking
of programing in terms of function composition. This is understandable.
We often give names to intermediate values rather than pass them
directly from function to function. We also inline short segments of
glue code rather than abstract them into helper functions. Here’s an
imperative-style implementation of the vector-length function in C:

::

    double vlen(double * v) {
      double d = 0.0;
      int n;
      for (n = 0; n < 3; ++n)
        d += v[n] * v[n];
      return sqrt(d);
    }

Compare this with the (stylized) Haskell version that makes function
composition explicit:

::

    vlen = sqrt . sum . fmap  (flip (^) 2)

(Here, to make things even more cryptic, I partially applied the
exponentiation operator ``(^)`` by setting its second argument to
``2``.)

I’m not arguing that Haskell’s point-free style is always better, just
that function composition is at the bottom of everything we do in
programming. And even though we are effectively composing functions,
Haskell does go to great lengths to provide imperative-style syntax
called the ``do`` notation for monadic composition. We’ll see its use
later. But first, let me explain why we need monadic composition in the
first place.

.. rubric:: The Kleisli Category
   :name: the-kleisli-category

We have previously arrived at the `writer
monad <https://bartoszmilewski.com/2014/12/23/kleisli-categories/>`__ by
embellishing regular functions. The particular embellishment was done by
pairing their return values with strings or, more generally, with
elements of a monoid. We can now recognize that such embellishment is a
functor:

::

    newtype Writer w a = Writer (a, w)

    instance Functor (Writer w) where
      fmap f (Writer (a, w)) = Writer (f a, w)

We have subsequently found a way of composing embellished functions, or
Kleisli arrows, which are functions of the form:

::

    a -> Writer w b

It was inside the composition that we implemented the accumulation of
the log.

We are now ready for a more general definition of the Kleisli category.
We start with a category *C* and an endofunctor ``m``. The corresponding
Kleisli category *K* has the same objects as *C*, but its morphisms are
different. A morphism between two objects ``a`` and ``b`` in *K* is
implemented as a morphism:

::

    a -> m b

in the original category *C*. It’s important to keep in mind that we
treat a Kleisli arrow in *K* as a morphism between ``a`` and ``b``, and
not between ``a`` and ``m b``.

In our example, ``m`` was specialized to ``Writer w``, for some fixed
monoid ``w``.

Kleisli arrows form a category only if we can define proper composition
for them. If there is a composition, which is associative and has an
identity arrow for every object, then the functor ``m`` is called a
*monad*, and the resulting category is called the Kleisli category.

In Haskell, Kleisli composition is defined using the fish operator
``>=>``, and the identity arrrow is a polymorphic function called
``return``. Here’s the definition of a monad using Kleisli composition:

::

    class Monad m where
      (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
      return :: a -> m a

Keep in mind that there are many equivalent ways of defining a monad,
and that this is not the primary one in the Haskell ecosystem. I like it
for its conceptual simplicity and the intuition it provides, but there
are other definitions that are more convenient when programming. We’ll
talk about them momentarily.

In this formulation, monad laws are very easy to express. They cannot be
enforced in Haskell, but they can be used for equational reasoning. They
are simply the standard composition laws for the Kleisli category:

::

    (f >=> g) >=> h = f >=> (g >=> h) -- associativity
    return >=> f = f                  -- left unit
    f >=> return = f                  -- right unit

This kind of a definition also expresses what a monad really is: it’s a
way of composing embellished functions. It’s not about side effects or
state. It’s about composition. As we’ll see later, embellished functions
may be used to express a variety of effects or state, but that’s not
what the monad is for. The monad is the sticky duct tape that ties one
end of an embellished function to the other end of an embellished
function.

Going back to our ``Writer`` example: The logging functions (the Kleisli
arrows for the ``Writer`` functor) form a category because ``Writer`` is
a monad:

::

    instance Monoid w => Monad (Writer w) where
        f >=> g = \a -> 
            let Writer (b, s)  = f a
                Writer (c, s') = g b
            in Writer (c, s `mappend` s')
        return a = Writer (a, mempty)

Monad laws for ``Writer w`` are satisfied as long as monoid laws for
``w`` are satisfied (they can’t be enforced in Haskell either).

There’s a useful Kleisli arrow defined for the ``Writer`` monad called
``tell``. It’s sole purpose is to add its argument to the log:

::

    tell :: w -> Writer w ()
    tell s = Writer ((), s)

We’ll use it later as a building block for other monadic functions.

.. rubric:: Fish Anatomy
   :name: fish-anatomy

When implementing the fish operator for different monads you quickly
realize that a lot of code is repeated and can be easily factored out.
To begin with, the Kleisli composition of two functions must return a
function, so its implementation may as well start with a lambda taking
an argument of type ``a``:

::

    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
    f >=> g = \a -> ...

The only thing we can do with this argument is to pass it to ``f``:

::

    f >=> g = \a -> let mb = f a
                    in ...

At this point we have to produce the result of type ``m c``, having at
our disposal an object of type ``m b`` and a function ``g :: b -> m c``.
Let’s define a function that does that for us. This function is called
bind and is usually written in the form of an infix operator:

::

    (>>=) :: m a -> (a -> m b) -> m b

For every monad, instead of defining the fish operator, we may instead
define bind. In fact the standard Haskell definition of a monad uses
bind:

::

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a

Here’s the definition of bind for the ``Writer`` monad:

::

    (Writer (a, w)) >>= f = let Writer (b, w') = f a
                            in  Writer (b, w `mappend` w')

It is indeed shorter than the definition of the fish operator.

It’s possible to further dissect bind, taking advantage of the fact that
``m`` is a functor. We can use ``fmap`` to apply the function
``a -> m b`` to the contents of ``m a``. This will turn ``a`` into
``m b``. The result of the application is therefore of type ``m (m b)``.
This is not exactly what we want — we need the result of type ``m b`` —
but we’re close. All we need is a function that collapses or flattens
the double application of ``m``. Such function is called ``join``:

::

    join :: m (m a) -> m a

Using ``join``, we can rewrite bind as:

::

    ma >>= f = join (fmap f ma)

That leads us to the third option for defining a monad:

::

    class Functor m => Monad m where
        join :: m (m a) -> m a
        return :: a -> m a

Here we have explicitly requested that ``m`` be a ``Functor``. We didn’t
have to do that in the previous two definitions of the monad. That’s
because any type constructor ``m`` that either supports the fish or bind
operator is automatically a functor. For instance, it’s possible to
define ``fmap`` in terms of bind and ``return``:

::

    fmap f ma = ma >>= \a -> return (f a)

For completeness, here’s ``join`` for the ``Writer`` monad:

::

    join :: Monoid w => Writer w (Writer w a) -> Writer w a
    join (Writer ((Writer (a, w')), w)) = Writer (a, w `mappend` w')

.. rubric:: The ``do`` Notation
   :name: the-do-notation

One way of writing code using monads is to work with Kleisli arrows —
composing them using the fish operator. This mode of programming is the
generalization of the point-free style. Point-free code is compact and
often quite elegant. In general, though, it can be hard to understand,
bordering on cryptic. That’s why most programmers prefer to give names
to function arguments and intermediate values.

When dealing with monads it means favoring the bind operator over the
fish operator. Bind takes a monadic value and returns a monadic value.
The programmer may chose to give names to those values. But that’s
hardly an improvement. What we really want is to pretend that we are
dealing with regular values, not the monadic containers that encapsulate
them. That’s how imperative code works — side effects, such as updating
a global log, are mostly hidden from view. And that’s what the ``do``
notation emulates in Haskell.

You might be wondering then, why use monads at all? If we want to make
side effects invisible, why not stick to an imperative language? The
answer is that the monad gives us much better control over side effects.
For instance, the log in the ``Writer`` monad is passed from function to
function and is never exposed globally. There is no possibility of
garbling the log or creating a data race. Also, monadic code is clearly
demarcated and cordoned off from the rest of the program.

The ``do`` notation is just syntactic sugar for monadic composition. On
the surface, it looks a lot like imperative code, but it translates
directly to a sequence of binds and lambda expressions.

For instance, take the example we used previously to illustrate the
composition of Kleisli arrows in the ``Writer`` monad. Using our current
definitions, it could be rewritten as:

::

    process :: String -> Writer String [String]
    process = upCase >=> toWords

This function turns all characters in the input string to upper case and
splits it into words, all the while producing a log of its actions.

In the ``do`` notation it would look like this:

::

    process s = do
        upStr <- upCase s
        toWords upStr

Here, ``upStr`` is just a ``String``, even though ``upCase`` produces a
``Writer``:

::

    upCase :: String -> Writer String String
    upCase s = Writer (map toUpper s, "upCase ")

This is because the ``do`` block is desugared by the compiler to:

::

    process s = 
       upCase s >>= \ upStr ->
           toWords upStr

The monadic result of ``upCase`` is bound to a lambda that takes a
``String``. It’s the name of this string that shows up in the ``do``
block. When reading the line:

::

    upStr <- upCase s

we say that ``upStr`` *gets* the result of ``upCase s``.

The pseudo-imperative style is even more pronounced when we inline
``toWords``. We replace it with the call to ``tell``, which logs the
string ``"toWords "``, followed by the call to ``return`` with the
result of splitting the string ``upStr`` using ``words``. Notice that
``words`` is a regular function working on strings.

::

    process s = do
        upStr <- upStr s
        tell "toWords "
        return (words upStr)

Here, each line in the do block introduces a new nested bind in the
desugared code:

::

    process s = 
        upCase s >>= \upStr ->
          tell "toWords " >>= \() ->
            return (words upStr)

Notice that ``tell`` produces a unit value, so it doesn’t have to be
passed to the following lambda. Ignoring the contents of a monadic
result (but not its effect — here, the contribution to the log) is quite
common, so there is a special operator to replace bind in that case:

::

    (>>) :: m a -> m b -> m b
    m >> k = m >>= (\_ -> k)

The actual desugaring of our code looks like this:

::

    process s = 
        upCase s >>= \upStr ->
          tell "toWords " >>
            return (words upStr)

In general, ``do`` blocks consist of lines (or sub-blocks) that either
use the left arrow to introduce new names that are then available in the
rest of the code, or are executed purely for side-effects. Bind
operators are implicit between the lines of code. Incidentally, it is
possible, in Haskell, to replace the formatting in the ``do`` blocks
with braces and semicolons. This provides the justification for
describing the monad as a way of overloading the semicolon.

Notice that the nesting of lambdas and bind operators when desugaring
the ``do`` notation has the effect of influencing the execution of the
rest of the ``do`` block based on the result of each line. This property
can be used to introduce complex control structures, for instance to
simulate exceptions.

Interestingly, the equivalent of the ``do`` notation has found its
application in imperative languages, C++ in particular. I’m talking
about resumable functions or coroutines. It’s not a secret that C++
`futures form a
monad <https://bartoszmilewski.com/2014/02/26/c17-i-see-a-monad-in-your-future/>`__.
It’s an example of the continuation monad, which we’ll discuss shortly.
The problem with continuations is that they are very hard to compose. In
Haskell, we use the ``do`` notation to turn the spaghetti of “my handler
will call your handler” into something that looks very much like
sequential code. Resumable functions make the same transformation
possible in C++. And the same mechanism can be applied to turn the
`spaghetti of nested
loops <https://bartoszmilewski.com/2014/04/21/getting-lazy-with-c/>`__
into list comprehensions or “generators,” which are essentially the
``do`` notation for the list monad. Without the unifying abstraction of
the monad, each of these problems is typically addressed by providing
custom extensions to the language. In Haskell, this is all dealt with
through libraries.

Next: `Monads and
Effects <https://bartoszmilewski.com/2016/11/30/monads-and-effects/>`__.

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

   <div id="crt-1446018714" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-946171744" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/?share=email>`__
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

   <div id="like-post-wrapper-3549518-5226-59ae3cc4aef72"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=5226&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-5226-59ae3cc4aef72"
   data-name="like-post-frame-3549518-5226-59ae3cc4aef72">

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

.. rubric:: 12 Responses to “Monads: Programmer’s Definition”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-67718">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67718">

   .. raw:: html

      <div class="comment-author vcard">

   |image0| Tom Shacham Says:

   .. raw:: html

      </div>

   `November 22, 2016 at 6:08
   am <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67718>`__
   This is a particularly illuminating post, thanks Bartosz!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67725">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67725">

   .. raw:: html

      <div class="comment-author vcard">

   |image1| `lambda
   functions <http://dobegin.com/lambda-functions-everywhere/>`__ Says:

   .. raw:: html

      </div>

   `November 22, 2016 at 1:28
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67725>`__
   “sugared” “do” examples with “upStr” are broken. plz fix

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67726">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67726">

   .. raw:: html

      <div class="comment-author vcard">

   |image2| `Adam nemini <http://gmail.com>`__ Says:

   .. raw:: html

      </div>

   `November 22, 2016 at 3:09
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67726>`__
   Just fine right up to here, then off the cliff: “pairing their return
   values with strings or, more generally, with elements of a monoid. We
   can now recognize that such embellishment is a functor”

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67733">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67733">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `November 22, 2016 at 9:20
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67733>`__
   @Adam I’m assuming the reader is familiar with the previous
   discussion of Kleisli categories.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67734">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67734">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `November 22, 2016 at 9:29
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67734>`__
   @lambda functions: Damn WordPress silently eating less-than signs and
   everything that follows. Fixed!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67817">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67817">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `datalligator <http://datalligator.org>`__ Says:

   .. raw:: html

      </div>

   `November 28, 2016 at 8:21
   am <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67817>`__
   Love your thinking and development of this; indeed it is a motivating
   example to follow all the definitions that lead to it. Are you
   thinking of turning this series into a book: categories for the
   working programmer?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67840">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67840">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `dmitri14 <http://zaitsev77.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `November 29, 2016 at 3:25
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67840>`__
   “That’s because any type constructor m that either supports the fish
   or bind operator is automatically a functor. For instance, it’s
   possible to define fmap in terms of bind and return:”

   | I can see the case of bind, but how can you use the fish to get the
     functor
   | ``fmap:: (a -> b) -> ma -> mb`` ? The fish returns ``a -> mc``, so
     how to get a morphism starting at ``ma`` is not clear.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67848">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67848">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `November 30, 2016 at 12:53
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67848>`__
   @dmitri14: I was tempted to provide code for all possible
   translations between definitions, but then I would have to explain
   them. So here are some, without explanations. It’s pretty much an
   exercise in matching types. It’s sometimes called “type tetris.”

   ::

       fmap f = id >=> \a -> return (f a)

   ::

       join = id >=> id

   ::

       ma >>= f = (id >=> f) ma

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67874">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67874">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `dmitri14 <http://zaitsev77.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `December 1, 2016 at 1:46
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-67874>`__
   Thank you! It is remarkable that ``id`` is always the 1st argument of
   the fish in these relations. Does it mean, only part of the fish is
   used and a more general Kleisli product may not come from a monad
   (contrary to what is said without proof in
   https://ncatlab.org/nlab/show/monad+%28in+computer+science%29)?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-68390">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-68390">

   .. raw:: html

      <div class="comment-author vcard">

   |image9| Randall Says:

   .. raw:: html

      </div>

   `January 11, 2017 at 9:26
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-68390>`__
   Lovely, thanks!

   | Suggestion: ‘vlen = sqrt . sum . fmap (^ 2)’
   | (a bit briefer by avoiding ‘flip’)

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70627">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70627">

   .. raw:: html

      <div class="comment-author vcard">

   |image10| `karkunow <http://karkunow.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `May 14, 2017 at 5:53
   pm <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-70627>`__
   @dmitry14, what do you mean exactly by “a more general Kleisli
   product may not come from a monad”, ‘multiplication’ on n-Cat? Can’t
   see any contrary info there.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-74188">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-74188">

   .. raw:: html

      <div class="comment-author vcard">

   |image11| thomas Says:

   .. raw:: html

      </div>

   `August 29, 2017 at 8:40
   am <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/#comment-74188>`__
   | this is by far the best explaination I may ever have heard.
   | I am struggeling with monads and the surrounding concepts for about
     one week now… and this really helped me.
   | thank you very much

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
   reply </2016/11/21/monads-programmers-definition/#respond>`__
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
-  November 21, 2016 at 11:29 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Functional
   Programming <https://bartoszmilewski.com/category/functional-programming/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Monads <https://bartoszmilewski.com/category/monads/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/trackback/>`__
   from your own site.

.. raw:: html

   </div>

`Create a free website or blog at
WordPress.com. <https://wordpress.com/?ref=footer_website>`__

.. raw:: html

   <div style="display:none">

.. raw:: html

   <div class="grofile-hash-map-2cf44ff0b32311d9453701e027ae0778">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-aeac14b009a7f6f2418f8737e61ad5b8">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c6dffa98706669f858c82f6de9242fec">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-da9c721223da7980bded23529ca22e90">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-2f5283cc7b85e352c0f86ad8581ff371">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-fad78be0d39573f5b05e459624ac10bf">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-6996fe77db9f65db1834b998b5222f9b">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-8b24f1139471d1de56f7fc083eb90dc4">

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

|image18|

.. raw:: html

   </div>

.. raw:: html

   </div>

.. |image0| image:: https://2.gravatar.com/avatar/2cf44ff0b32311d9453701e027ae0778?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image1| image:: https://1.gravatar.com/avatar/aeac14b009a7f6f2418f8737e61ad5b8?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image2| image:: https://0.gravatar.com/avatar/c6dffa98706669f858c82f6de9242fec?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image3| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://1.gravatar.com/avatar/da9c721223da7980bded23529ca22e90?s=48&d=https%3A%2F%2F1.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://2.gravatar.com/avatar/2f5283cc7b85e352c0f86ad8581ff371?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image8| image:: https://2.gravatar.com/avatar/2f5283cc7b85e352c0f86ad8581ff371?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image9| image:: https://0.gravatar.com/avatar/fad78be0d39573f5b05e459624ac10bf?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image10| image:: https://0.gravatar.com/avatar/6996fe77db9f65db1834b998b5222f9b?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image11| image:: https://2.gravatar.com/avatar/8b24f1139471d1de56f7fc083eb90dc4?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
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
.. |image18| image:: https://pixel.wp.com/b.gif?v=noscript

