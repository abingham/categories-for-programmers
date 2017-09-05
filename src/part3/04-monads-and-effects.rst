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
   class="post-7853 post type-post status-publish format-standard hentry category-category-theory category-haskell category-monads">

November 30, 2016

.. raw:: html

   <div class="post-info">

.. rubric:: Monads and Effects
   :name: monads-and-effects
   :class: post-title

Posted by Bartosz Milewski under `Category
Theory <https://bartoszmilewski.com/category/category-theory/>`__,
`Haskell <https://bartoszmilewski.com/category/haskell/>`__,
`Monads <https://bartoszmilewski.com/category/monads/>`__
`[9]
Comments <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comments>`__ 

.. raw:: html

   </div>

.. raw:: html

   <div class="post-content">

.. raw:: html

   <div id="pd_rating_holder_2203687_post_7853" class="pd-rating">

.. raw:: html

   </div>

    This is part 21 of Categories for Programmers. Previously: `Monads:
    Programmer’s
    Definition <https://bartoszmilewski.com/2016/11/21/monads-programmers-definition/>`__.
    See the `Table of
    Contents <https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/>`__.

Now that we know what the monad is for — it lets us compose embellished
functions — the really interesting question is why embellished functions
are so important in functional programming. We’ve already seen one
example, the ``Writer`` monad, where embellishment let us create and
accumulate a log across multiple function calls. A problem that would
otherwise be solved using impure functions (e.g., by accessing and
modifying some global state) was solved with pure functions.

.. rubric:: The Problem
   :name: the-problem

Here is a short list of similar problems, copied from `Eugenio Moggi’s
seminal paper <https://core.ac.uk/download/pdf/21173011.pdf>`__, all of
which are traditionally solved by abandoning the purity of functions.

-  Partiality: Computations that may not terminate
-  Nondeterminism: Computations that may return many results
-  Side effects: Computations that access/modify state

   -  Read-only state, or the environment
   -  Write-only state, or a log
   -  Read/write state

-  Exceptions: Partial functions that may fail
-  Continuations: Ability to save state of the program and then restore
   it on demand
-  Interactive Input
-  Interactive Output

What really is mind blowing is that all these problems may be solved
using the same clever trick: turning to embellished functions. Of
course, the embellishment will be totally different in each case.

You have to realize that, at this stage, there is no requirement that
the embellishment be monadic. It’s only when we insist on composition —
being able to decompose a single embellished function into smaller
embellished functions — that we need a monad. Again, since each of the
embellishments is different, monadic composition will be implemented
differently, but the overall pattern is the same. It’s a very simple
pattern: composition that is associative and equipped with identity.

The next section is heavy on Haskell examples. Feel free to skim or even
skip it if you’re eager to get back to category theory or if you’re
already familiar with Haskell’s implementation of monads.

.. rubric:: The Solution
   :name: the-solution

First, let’s analyze the way we used the ``Writer`` monad. We started
with a pure function that performed a certain task — given arguments, it
produced a certain output. We replaced this function with another
function that embellished the original output by pairing it with a
string. That was our solution to the logging problem.

We couldn’t stop there because, in general, we don’t want to deal with
monolithic solutions. We needed to be able to decompose one
log-producing function into smaller log-producing functions. It’s the
composition of those smaller functions that led us to the concept of a
monad.

What’s really amazing is that the same pattern of embellishing the
function return types works for a large variety of problems that
normally would require abandoning purity. Let’s go through our list and
identify the embellishment that applies to each problem in turn.

.. rubric:: Partiality
   :name: partiality

We modify the return type of every function that may not terminate by
turning it into a “lifted” type — a type that contains all values of the
original type plus the special “bottom” value ``⊥``. For instance, the
``Bool`` type, as a set, would contain two elements: ``True`` and
``False``. The lifted ``Bool`` contains three elements. Functions that
return the lifted ``Bool`` may produce ``True`` or ``False``, or execute
forever.

The funny thing is that, in a lazy language like Haskell, a never-ending
function may actually return a value, and this value may be passed to
the next function. We call this special value the bottom. As long as
this value is not explicitly needed (for instance, to be pattern
matched, or produced as output), it may be passed around without
stalling the execution of the program. Because every Haskell function
may be potentially non-terminating, all types in Haskell are assumed to
be lifted. This is why we often talk about the category **Hask** of
Haskell (lifted) types and functions rather than the simpler **Set**. It
is not clear, though, that **Hask** is a real category (see this `Andrej
Bauer
post <http://math.andrej.com/2016/08/06/hask-is-not-a-category/>`__).

.. rubric:: Nondeterminism
   :name: nondeterminism

If a function can return many different results, it may as well return
them all at once. Semantically, a non-deterministic function is
equivalent to a function that returns a list of results. This makes a
lot of sense in a lazy garbage-collected language. For instance, if all
you need is one value, you can just take the head of the list, and the
tail will never be evaluated. If you need a random value, use a random
number generator to pick the n-th element of the list. Laziness even
allows you to return an infinite list of results.

In the list monad — Haskell’s implementation of nondeterministic
computations — ``join`` is implemented as ``concat``. Remember that
``join`` is supposed to flatten a container of containers — ``concat``
concatenates a list of lists into a single list. ``return`` creates a
singleton list:

::

    instance Monad [] where
        join = concat
        return x = [x]

The bind operator for the list monad is given by the general formula:
``fmap`` followed by ``join`` which, in this case gives:

::

    as >>= k = concat (fmap k as)

Here, the function ``k``, which itself produces a list, is applied to
every element of the list ``as``. The result is a list of lists, which
is flattened using ``concat``.

From the programmer’s point of view, working with a list is easier than,
for instance, calling a non-deterministic function in a loop, or
implementing a function that returns an iterator (although, `in modern
C++ <http://ericniebler.com/2014/04/27/range-comprehensions/>`__,
returning a lazy range would be almost equivalent to returning a list in
Haskell).

A good example of using non-determinism creatively is in game
programming. For instance, when a computer plays chess against a human,
it can’t predict the opponent’s next move. It can, however, generate a
list of all possible moves and analyze them one by one. Similarly, a
non-deterministic parser may generate a list of all possible parses for
a given expression.

Even though we may interpret functions returning lists as
non-deterministic, the applications of the list monad are much wider.
That’s because stitching together computations that produce lists is a
perfect functional substitute for iterative constructs — loops — that
are used in imperative programming. A single loop can be often rewritten
using ``fmap`` that applies the body of the loop to each element of the
list. The ``do`` notation in the list monad can be used to replace
complex nested loops.

My favorite example is the program that generates Pythagorean triples —
triples of positive integers that can form sides of right triangles.

::

    triples = do
        z <- [1..]
        x <- [1..z]
        y <- [x..z]
        guard (x^2 + y^2 == z^2)
        return (x, y, z)

The first line tells us that ``z`` gets an element from an infinite list
of positive numbers ``[1..]``. Then ``x`` gets an element from the
(finite) list ``[1..z]`` of numbers between 1 and ``z``. Finally ``y``
gets an element from the list of numbers between ``x`` and ``z``. We
have three numbers ``1 <= x <= y <= z`` at our disposal. The function
``guard`` takes a ``Bool`` expression and returns a list of units:

::

    guard :: Bool -> [()]
    guard True  = [()]
    guard False = []

This function (which is a member of a larger class called ``MonadPlus``)
is used here to filter out non-Pythagorean triples. Indeed, if you look
at the implementation of bind (or the related operator ``>>``), you’ll
notice that, when given an empty list, it produces an empty list. On the
other hand, when given a non-empty list (here, the singleton list
containing unit ``[()]``), bind will call the continuation, here
``return (x, y, z)``, which produces a singleton list with a verified
Pythagorean triple. All those singleton lists will be concatenated by
the enclosing binds to produce the final (infinite) result. Of course,
the caller of ``triples`` will never be able to consume the whole list,
but that doesn’t matter, because Haskell is lazy.

The problem that normally would require a set of three nested loops has
been dramatically simplified with the help of the list monad and the
``do`` notation. As if that weren’t enough, Haskell let’s you simplify
this code even further using list comprehension:

::

    triples = [(x, y, z) | z <- [1..]
                         , x <- [1..z]
                         , y <- [x..z]
                         , x^2 + y^2 == z^2]

This is just further syntactic sugar for the list monad (strictly
speaking, ``MonadPlus``).

You might see similar constructs in other functional or imperative
languages under the guise of generators and coroutines.

.. rubric:: Read-Only State
   :name: read-only-state

A function that has read-only access to some external state, or
environment, can be always replaced by a function that takes that
environment as an additional argument. A pure function ``(a, e) -> b``
(where ``e`` is the type of the environment) doesn’t look, at first
sight, like a Kleisli arrow. But as soon as we curry it to
``a -> (e -> b)`` we recognize the embellishment as our old friend the
reader functor:

::

    newtype Reader e a = Reader (e -> a)

You may interpret a function returning a ``Reader`` as producing a
mini-executable: an action that given an environment produces the
desired result. There is a helper function ``runReader`` to execute such
an action:

::

    runReader :: Reader e a -> e -> a
    runReader (Reader f) e = f e

It may produce different results for different values of the
environment.

Notice that both the function returning a ``Reader``, and the ``Reader``
action itself are pure.

To implement bind for the ``Reader`` monad, first notice that you have
to produce a function that takes the environment ``e`` and produces a
``b``:

::

    ra >>= k = Reader (\e -> ...)

Inside the lambda, we can execute the action ``ra`` to produce an ``a``:

::

    ra >>= k = Reader (\e -> let a = runReader ra e
                             in ...)

We can then pass the ``a`` to the continuation ``k`` to get a new action
``rb``:

::

    ra >>= k = Reader (\e -> let a  = runReader ra e
                                 rb = k a
                             in ...)

Finally, we can run the action ``rb`` with the environment ``e``:

::

    ra >>= k = Reader (\e -> let a  = runReader ra e
                                 rb = k a
                             in runReader rb e)

To implement ``return`` we create an action that ignores the environment
and returns the unchanged value.

Putting it all together, after a few simplifications, we get the
following definition:

::

    instance Monad (Reader e) where
        ra >>= k = Reader (\e -> runReader (k (runReader ra e)) e)
        return x = Reader (\e -> x)

.. rubric:: Write-Only State
   :name: write-only-state

This is just our initial logging example. The embellishment is given by
the ``Writer`` functor:

::

    newtype Writer w a = Writer (a, w)

For completeness, there’s also a trivial helper ``runWriter`` that
unpacks the data constructor:

::

    runWriter :: Writer w a -> (a, w)
    runWriter (Writer (a, w)) = (a, w)

As we’ve seen before, in order to make ``Writer`` composable, ``w`` has
to be a monoid. Here’s the monad instance for ``Writer`` written in
terms of the bind operator:

::

    instance (Monoid w) => Monad (Writer w) where 
        (Writer (a, w)) >>= k = let (a', w') = runWriter (k a)
                                in Writer (a', w `mappend` w')
        return a = Writer (a, mempty)

.. rubric:: State
   :name: state

Functions that have read/write access to state combine the
embellishments of the ``Reader`` and the ``Writer``. You may think of
them as pure functions that take the state as an extra argument and
produce a pair value/state as a result: ``(a, s) -> (b, s)``. After
currying, we get them into the form of Kleisli arrows
``a -> (s -> (b, s))``, with the embellishment abstracted in the
``State`` functor:

::

    newtype State s a = State (s -> (a, s))

Again, we can look at a Kleisli arrow as returning an action, which can
be executed using the helper function:

::

    runState :: State s a -> s -> (a, s)
    runState (State f) s = f s

Different initial states may not only produce different results, but
also different final states.

The implementation of bind for the ``State`` monad is very similar to
that of the ``Reader`` monad, except that care has to be taken to pass
the correct state at each step:

::

    sa >>= k = State (\s -> let (a, s') = runState sa s
                                sb = k a
                            in runState sb s')

Here’s the full instance:

::

    instance Monad (State s) where
        sa >>= k = State (\s -> let (a, s') = runState sa s 
                                in runState (k a) s')
        return a = State (\s -> (a, s))

There are also two helper Kleisli arrows that may be used to manipulate
the state. One of them retrieves the state for inspection:

::

    get :: State s s
    get = State (\s -> (s, s))

and the other replaces it with a completely new state:

::

    put :: s -> State s ()
    put s' = State (\s -> ((), s'))

.. rubric:: Exceptions
   :name: exceptions

An imperative function that throws an exception is really a partial
function — it’s a function that’s not defined for some values of its
arguments. The simplest implementation of exceptions in terms of pure
total functions uses the ``Maybe`` functor. A partial function is
extended to a total function that returns ``Just a`` whenever it makes
sense, and ``Nothing`` when it doesn’t. If we want to also return some
information about the cause of the failure, we can use the ``Either``
functor instead (with the first type fixed, for instance, to
``String``).

Here’s the ``Monad`` instance for ``Maybe``:

::

    instance Monad Maybe where
        Nothing >>= k = Nothing
        Just a  >>= k = k a
        return a = Just a

Notice that monadic composition for ``Maybe`` correctly short-circuits
the computation (the continuation ``k`` is never called) when an error
is detected. That’s the behavior we expect from exceptions.

.. rubric:: Continuations
   :name: continuations

It’s the “Don’t call us, we’ll call you!” situation you may experience
after a job interview. Instead of getting a direct answer, you are
supposed to provide a handler, a function to be called with the result.
This style of programming is especially useful when the result is not
known at the time of the call because, for instance, it’s being
evaluated by another thread or delivered from a remote web site. A
Kleisli arrow in this case returns a function that accepts a handler,
which represents “the rest of the computation”:

::

    data Cont r a = Cont ((a -> r) -> r)

The handler ``a -> r``, when it’s eventually called, produces the result
of type ``r``, and this result is returned at the end. A continuation is
parameterized by the result type. (In practice, this is often some kind
of status indicator.)

There is also a helper function for executing the action returned by the
Kleisli arrow. It takes the handler and passes it to the continuation:

::

    runCont :: Cont r a -> (a -> r) -> r
    runCont (Cont k) h = k h

The composition of continuations is notoriously difficult, so its
handling through a monad and, in particular, the ``do`` notation, is of
extreme advantage.

Let’s figure out the implementation of bind. First let’s look at the
stripped down signature:

::

    (>>=) :: ((a -> r) -> r) -> 
             (a -> (b -> r) -> r) -> 
             ((b -> r) -> r)

Our goal is to create a function that takes the handler ``(b -> r)`` and
produces the result ``r``. So that’s our starting point:

::

    ka >>= kab = Cont (\hb -> ...)

Inside the lambda, we want to call the function ``ka`` with the
appropriate handler that represents the rest of the computation. We’ll
implement this handler as a lambda:

::

    runCont ka (\a -> ...)

In this case, the rest of the computation involves first calling ``kab``
with ``a``, and then passing ``hb`` to the resulting action ``kb``:

::

    runCont ka (\a -> let kb = kab a
                      in runCont kb hb)

As you can see, continuations are composed inside out. The final handler
``hb`` is called from the innermost layer of the computation. Here’s the
full instance:

::

    instance Monad (Cont r) where
        ka >>= kab = Cont (\hb -> runCont ka (\a -> runCont (kab a) hb))
        return a = Cont (\ha -> ha a)

.. rubric:: Interactive Input
   :name: interactive-input

This is the trickiest problem and a source of a lot of confusion.
Clearly, a function like ``getChar``, if it were to return a character
typed at the keyboard, couldn’t be pure. But what if it returned the
character inside a container? As long as there was no way of extracting
the character from this container, we could claim that the function is
pure. Every time you call ``getChar`` it would return exactly the same
container. Conceptually, this container would contain the superposition
of all possible characters.

If you’re familiar with quantum mechanics, you should have no problem
understanding this analogy. It’s just like the box with the
Schrödinger’s cat inside — except that there is no way to open or peek
inside the box. The box is defined using the special built-in ``IO``
functor. In our example, ``getChar`` could be declared as a Kleisli
arrow:

::

    getChar :: () -> IO Char

(Actually, since a function from the unit type is equivalent to picking
a value of the return type, the declaration of ``getChar`` is simplified
to ``getChar :: IO Char``.)

Being a functor, ``IO`` lets you manipulate its contents using ``fmap``.
And, as a functor, it can store the contents of any type, not just a
character. The real utility of this approach comes to light when you
consider that, in Haskell, ``IO`` is a monad. It means that you are able
to compose Kleisli arrows that produce ``IO`` objects.

You might think that Kleisli composition would allow you to peek at the
contents of the ``IO`` object (thus “collapsing the wave function,” if
we were to continue the quantum analogy). Indeed, you could compose
``getChar`` with another Kleisli arrow that takes a character and, say,
converts it to an integer. The catch is that this second Kleisli arrow
could only return this integer as an ``(IO Int)``. Again, you’ll end up
with a superposition of all possible integers. And so on. The
Schrödinger’s cat is never out of the bag. Once you are inside the
``IO`` monad, there is no way out of it. There is no equivalent of
``runState`` or ``runReader`` for the ``IO`` monad. There is no
``runIO``!

So what can you do with the result of a Kleisli arrow, the ``IO``
object, other than compose it with another Kleisli arrow? Well, you can
return it from ``main``. In Haskell, ``main`` has the signature:

::

    main :: IO ()

and you are free to think of it as a Kleisli arrow:

::

    main :: () -> IO ()

From that perspective, a Haskell program is just one big Kleisli arrow
in the ``IO`` monad. You can compose it from smaller Kleisli arrows
using monadic composition. It’s up to the runtime system to do something
with the resulting ``IO`` object (also called ``IO`` action).

Notice that the arrow itself is a pure function — it’s pure functions
all the way down. The dirty work is relegated to the system. When it
finally executes the ``IO`` action returned from ``main``, it does all
kinds of nasty things like reading user input, modifying files, printing
obnoxious messages, formatting a disk, and so on. The Haskell program
never dirties its hands (well, except when it calls ``unsafePerformIO``,
but that’s a different story).

Of course, because Haskell is lazy, ``main`` returns almost immediately,
and the dirty work begins right away. It’s during the execution of the
``IO`` action that the results of pure computations are requested and
evaluated on demand. So, in reality, the execution of a program is an
interleaving of pure (Haskell) and dirty (system) code.

There is an alternative interpretation of the ``IO`` monad that is even
more bizarre but makes perfect sense as a mathematical model. It treats
the whole Universe as an object in a program. Notice that, conceptually,
the imperative model treats the Universe as an external global object,
so procedures that perform I/O have side effects by virtue of
interacting with that object. They can both read and modify the state of
the Universe.

We already know how to deal with state in functional programming — we
use the state monad. Unlike simple state, however, the state of the
Universe cannot be easily described using standard data structures. But
we don’t have to, as long as we never directly interact with it. It’s
enough that we assume that there exists a type ``RealWorld`` and, by
some miracle of cosmic engineering, the runtime is able to provide an
object of this type. An ``IO`` action is just a function:

::

    type IO a  =  RealWorld -> (a, RealWorld)

Or, in terms of the ``State`` monad:

::

    type IO = State RealWorld

However, ``>=>`` and ``return`` for the ``IO`` monad have to be built
into the language.

.. rubric:: Interactive Output
   :name: interactive-output

The same ``IO`` monad is used to encapsulate interactive output.
``RealWorld`` is supposed to contain all output devices. You might
wonder why we can’t just call output functions from Haskell and pretend
that they do nothing. For instance, why do we have:

::

    putStr :: String -> IO ()

rather than the simpler:

::

    putStr :: String -> ()

Two reasons: Haskell is lazy, so it would never call a function whose
output — here, the unit object — is not used for anything. And, even if
it weren’t lazy, it could still freely change the order of such calls
and thus garble the output. The only way to force sequential execution
of two functions in Haskell is through data dependency. The input of one
function must depend on the output of another. Having ``RealWorld``
passed between ``IO`` actions enforces sequencing.

Conceptually, in this program:

::

    main :: IO ()
    main = do
        putStr "Hello "
        putStr "World!"

the action that prints “World!” receives, as input, the Universe in
which “Hello ” is already on the screen. It outputs a new Universe, with
“Hello World!” on the screen.

.. rubric:: Conclusion
   :name: conclusion

Of course I have just scratched the surface of monadic programming.
Monads not only accomplish, with pure functions, what normally is done
with side effects in imperative programming, but they also do it with a
high degree of control and type safety. They are not without drawbacks,
though. The major complaint about monads is that they don’t easily
compose with each other. Granted, you can combine most of the basic
monads using the monad transformer library. It’s relatively easy to
create a monad stack that combines, say, state with exceptions, but
there is no formula for stacking arbitrary monads together.

Next: `Monads
Categorically <https://bartoszmilewski.com/2016/12/27/monads-categorically/>`__.

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

   <div id="crt-1708705316" style="width:300px;height:250px;">

.. raw:: html

   </div>

.. raw:: html

   <div id="crt-932010275" style="width:300px;height:250px;">

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

-  `Reddit <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=reddit>`__
-  `More <#>`__
-  

.. raw:: html

   <div class="sharing-hidden">

.. raw:: html

   <div class="inner" style="display: none;">

-  `Twitter <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=twitter>`__
-  `LinkedIn <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=linkedin>`__
-  
-  `Google <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=google-plus-1>`__
-  `Pocket <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=pocket>`__
-  
-  `Facebook <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=facebook>`__
-  `Email <https://bartoszmilewski.com/2016/11/30/monads-and-effects/?share=email>`__
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

   <div id="like-post-wrapper-3549518-7853-59ae3cd07abd6"
   class="sharedaddy sd-block sd-like jetpack-likes-widget-wrapper jetpack-likes-widget-unloaded"
   data-src="//widgets.wp.com/likes/#blog_id=3549518&amp;post_id=7853&amp;origin=bartoszmilewski.wordpress.com&amp;obj_id=3549518-7853-59ae3cd07abd6"
   data-name="like-post-frame-3549518-7853-59ae3cd07abd6">

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

.. rubric:: 9 Responses to “Monads and Effects”
   :name: comments

#. 

   .. raw:: html

      <div id="comment-67865">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67865">

   .. raw:: html

      <div class="comment-author vcard">

   |image0| `Robert Harper <http://www.cs.cmu.edu/~rwh>`__ Says:

   .. raw:: html

      </div>

   `December 1, 2016 at 6:42
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67865>`__
   But for the pretentious terminology, what you have is little more
   than what was present in Algol-60, the only difference being commands
   having a return type. Haskell is but a dialect of Algol, a fine old
   imperative language, a vast improvement on its successors. It even
   had call-by-name, not need, because they hadn’t yet realized that the
   command structure forms a well-behaved modality (not a monad),
   including the encapsulation of unexecuted commands as values.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67867">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67867">

   .. raw:: html

      <div class="comment-author vcard">

   |image1| `Juan Manuel (@babui\_) <http://twitter.com/babui_>`__ Says:

   .. raw:: html

      </div>

   `December 1, 2016 at 8:02
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67867>`__
   Why are functions that take a continuation as a parameter consideren
   not pure?

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67870">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67870">

   .. raw:: html

      <div class="comment-author vcard">

   |image2| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `December 1, 2016 at 11:12
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67870>`__
   @Juan Manuel: I guess I described the problem in terms of the
   solution. The original problem in imperative programming is to
   execute some fragment of code with the option to jump out of it and
   continue with the rest of the computation. Just think of the
   continuation as the code that follows the specific fragment of code —
   it’s literally the rest of the program. This continuation is reified
   as a handler in continuation passing style. Calling the handler is
   like jumping out of the routine and proceeding with the rest. So it’s
   a sophisticated flow of control mechanism. It can be implemented in C
   using setjmp/longjump.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67872">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67872">

   .. raw:: html

      <div class="comment-author vcard">

   |image3| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `December 1, 2016 at 11:26
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67872>`__
   @Robert Harper: I’d like to be language agnostic, but for practical
   purposes I had to pick a specific programming language to illustrate
   the use of monads. Algol-60 would be a pretty obscure choice. `Of
   course, ML has
   monads <https://existentialtype.wordpress.com/2011/05/01/of-course-ml-has-monads/>`__
   and, nowadays, even Java has monads (in fact, there is a claim that
   monads are used in Java by more programmers than the total of Haskell
   programmers 😉 ). I’m partial to Haskell syntax though.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67886">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67886">

   .. raw:: html

      <div class="comment-author vcard">

   |image4| `Steve Downey (@sdowney) <http://twitter.com/sdowney>`__
   Says:

   .. raw:: html

      </div>

   `December 2, 2016 at 10:29
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67886>`__
   The triples code examples for the list monad seem to have gotten
   dropped.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67887">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67887">

   .. raw:: html

      <div class="comment-author vcard">

   |image5| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `December 2, 2016 at 12:08
   pm <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67887>`__
   @Steve Downey: Damn WordPress! Fixed!

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-67928">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-67928">

   .. raw:: html

      <div class="comment-author vcard">

   |image6| `Niriel <http://niriel.wordpress.com>`__ Says:

   .. raw:: html

      </div>

   `December 6, 2016 at 4:25
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-67928>`__
   Idris seems to have a way of easily stacking monads, through its
   “effects” library. The return type is of type “Effect [list of effect
   types]”. It still confuses me greatly. Maybe it is merely a syntactic
   convenience that dependent typing provides, and is just a bunch of
   monads transformers in the background. You might be interested.
   http://www.idris-lang.org/documentation/effects/

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70155">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70155">

   .. raw:: html

      <div class="comment-author vcard">

   |image7| `Henry
   Chern <https://www.facebook.com/app_scoped_user_id/1344392675639647/>`__
   Says:

   .. raw:: html

      </div>

   `April 21, 2017 at 1:11
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-70155>`__
   Using the list comprehension, the code for the Pythagorean triples
   was not completed.

   .. raw:: html

      <div class="reply">

   .. raw:: html

      </div>

   .. raw:: html

      </div>

#. 

   .. raw:: html

      <div id="comment-70168">

   .. raw:: html

      </div>

   .. raw:: html

      <div id="div-comment-70168">

   .. raw:: html

      <div class="comment-author vcard">

   |image8| `Bartosz Milewski <http://BartoszMilewski.com>`__ Says:

   .. raw:: html

      </div>

   `April 21, 2017 at 6:49
   am <https://bartoszmilewski.com/2016/11/30/monads-and-effects/#comment-70168>`__
   Sorry, it’s the brain-dead WordPress. I already complained about it
   removing less-than signs, but they refuse to fix it. Fixed for now!

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
   reply </2016/11/30/monads-and-effects/#respond>`__
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
-  November 30, 2016 at 1:21 pm
-  **Category :**
-  `Category
   Theory <https://bartoszmilewski.com/category/category-theory/>`__,
   `Haskell <https://bartoszmilewski.com/category/haskell/>`__,
   `Monads <https://bartoszmilewski.com/category/monads/>`__
-  **Do More :**
-  You can `leave a response <#respond>`__, or
   `trackback <https://bartoszmilewski.com/2016/11/30/monads-and-effects/trackback/>`__
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

   <div class="grofile-hash-map-b4a7426cee3700d21354b77b4a29fddd">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-c018f213204496b4bbf481e7c8e6c15c">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-b2c303a92e0fa1792ac8f619e9933a3d">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-67810704d44f2474e9eeff64a052078d">

.. raw:: html

   </div>

.. raw:: html

   <div class="grofile-hash-map-5c42ee0fb147266be2c21e05ac4bc58a">

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

.. |image0| image:: https://2.gravatar.com/avatar/58dfeb7db21bb8a5c6aa108b804078fd?s=48&d=https%3A%2F%2F2.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image1| image:: https://i1.wp.com/pbs.twimg.com/profile_images/452017421855907841/W65GNlUV_normal.jpeg?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image2| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image3| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image4| image:: https://i2.wp.com/pbs.twimg.com/profile_images/932910946/Picture_318_normal.jpg?resize=48%2C48
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image5| image:: https://0.gravatar.com/avatar/c018f213204496b4bbf481e7c8e6c15c?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image6| image:: https://0.gravatar.com/avatar/67810704d44f2474e9eeff64a052078d?s=48&d=https%3A%2F%2F0.gravatar.com%2Favatar%2Fad516503a11cd5ca435acc9bb6523536%3Fs%3D48&r=G
   :class: avatar avatar-48
   :width: 48px
   :height: 48px
.. |image7| image:: https://i0.wp.com/graph.facebook.com/v2.2/1344392675639647/picture?q=type%3Dlarge%26_md5%3D2f5bf20a57a614960d5e6fe6554c5e9f&resize=48%2C48
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

