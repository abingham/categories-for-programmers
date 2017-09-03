# Categories for Programmers

This is a [Sphinx build](http://www.sphinx-doc.org/) for Bartosz
Milewski's
[excellent series of blog posts "Category Theory for Programmers"](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).
My main goal is to create an EPUB version of the series that is easy to consume
on e-readers.

## Building the EPUB

The main tool you need for building the output
is [Sphinx](http://www.sphinx-doc.org/). The Sphinx website does a fine just
explaining how to install it, so I won't go into any detail here. On linux or OS
X you'll also need to install `make`.

Once Sphinx is installed, open a terminal and go to the directory containing
this file. Once in that directory run:

```
make epub
```

This should work on OS X, linux, and windows.

Once this is done, the output will be in `_build/epub`.

## Contributing

I'd love any help people want to give! My basic approach is pretty simple. I run pandoc over a single chapter (blog post), converting it to reStructuredText. For example, to convert the first chapter I used this command:

```
pandoc -o 01-category-the-essence-of-computation.rst https://bartoszmilewski.com/2014/11/04/category-the-essence-of-composition/
```

The output goes into the `rst` directory. I do a few small edits to the file:

1. Remove the comments
2. Remove the image links for comment avatars
3. Turn section rubrics into RST section headers

And then I link the new file into `index.rst`.

### Images sources

I'm currently using the wordpress-hosted images of the original blog posts as
the image sources. This works just fine, even though Sphinx warns about it. It
might be best in the long term to copy the images into this repository, but it's
not strictly necessary so I haven't bothered. If anyone wants to pull those
files in (and I imagine it should be pretty automatable), I'm all in favor of
it.
