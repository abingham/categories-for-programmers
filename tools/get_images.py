"""Fetch all images in an RST file.

In practice you'll use it something like this:

    for F in *rst; do python get_images.py $F; done

This uses wget's recursive download support to build a directory structure
containing all of the downloaded files.
"""

import subprocess
import sys


def clean_uri(uri):
    "Remove query parameters from a URI."
    return uri.split('?')[0]


def uris(filename):
    "Sequence of image URIs in an RST file."
    with open(filename, 'rt') as f:
        for line in f:
            if 'image::' in line:
                uri = line.split('image::')[-1]
                yield clean_uri(uri)


def main(filename):
    "Use wget to fetch all images from an RST file."
    for uri in uris(filename):
        subprocess.run('wget -m {}'.format(uri).split())


if __name__ == '__main__':
    main(sys.argv[1])
