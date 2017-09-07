# Categories for Programmers

This is a [Sphinx build](http://www.sphinx-doc.org/) for Bartosz
Milewski's
[excellent series of blog posts "Category Theory for Programmers"](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).
My main goal is to create an EPUB version of the series that is easy to consume
on e-readers.

## Just want to read the book?

The most recent build of the book is in the `latest` directory. You
can
[download it with this link](https://github.com/abingham/categories-for-programmers/raw/master/latest/CategoriesforProgrammers.epub).


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

At this point the bulk of the conversion work is done. There are some open
issues in the tracker that you could look at. Or basic editorial work would
probably be useful as well.
