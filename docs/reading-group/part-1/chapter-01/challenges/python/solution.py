def identity(x):
    """1. Implement, as best as you can, the identity function in your favorite
    language (or the second favorite, if your favorite language happens to be
    Haskell).
    """
    return x


def compose(f, g):
    """2. Implement the composition function in your favorite language. It takes
    two functions as arguments and returns a function that is their
    composition.
    """
    return lambda x: g(f(x))


def test_identity_under_composition():
    """3. Write a program that tries to test that your composition function
    respects identity.
    """
    assert compose(chr, identity)(123) == chr(123)
    assert compose(identity, chr)(456) == chr(456)
