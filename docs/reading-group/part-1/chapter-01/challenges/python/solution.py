import random

# 1. Define a higher-order function (or a function object) ``memoize`` in
# your favorite language. This function takes a pure function ``f`` as
# an argument and returns a function that behaves almost exactly like
# ``f``, except that it only calls the original function once for every
# argument, stores the result internally, and subsequently returns this
# stored result every time it’s called with the same argument.
def memoize(f):
        results = {}
    def m(*x):
        try:
            return results[x]
        except KeyError:
            r = f(*x)
            results[x] = r
            return r
    return m


# 2. Try to memoize a function from your standard library that you
# normally use to produce random numbers. Does it work?
rand = memoize(lambda x: random.random() * x)


def test_rand():
    assert rand(10) == rand(10)


# 3. Most random number generators can be initialized with a seed.
#    Implement a function that takes a seed, calls the random number
#    generator with that seed, and returns the result. Memoize that
#    function. Does it work?
def _seed_and_random(a):
    random.seed(a)
    return random.random()

seed_and_random = memoize(_seed_and_random)

def test_seed_and_random():
    assert seed_and_random(42) == seed_and_random(42)
