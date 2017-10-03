#![crate_type = "lib"]
#![allow(dead_code)]
#![feature(conservative_impl_trait)]
#![feature(try_from)]

/// 1. Implement, as best as you can, the identity function in your favorite
/// language (or the second favorite, if your favorite language happens to be
/// Haskell).
fn identity<T>(x: T) -> T {
    x
}

/// 2. Implement the composition function in your favorite language.
/// It takes two functions as arguments and returns a function that
/// is their composition.
fn compose<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(A) -> B,
    G: Fn(B) -> C,
{
    move |x| g(f(x))
}

/// 3. Write a program that tries to test that your composition function
/// respects identity.
fn test_identity_under_composition() {
	let chr = <char as std::convert::TryFrom<u32>>::try_from;

	assert_eq!(compose(identity, chr)(123), chr(123));
	assert_eq!(compose(chr, identity)(456), chr(456));
}
