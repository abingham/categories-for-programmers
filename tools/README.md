# Tools

Eventually we need to copy all of the images from the blog locally. This way the
reader won't need internet access to see them. We'll need some tooling to do that.

## get_images.py

This scans an RST input file, identifies any image directives, and downloads the
images.

## update_links.py (not written)

This updates the RST files with new links. Perhaps this is best done with a sed
script or something instead of a Python file.
