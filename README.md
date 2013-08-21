# site-generator
site-generator is a static site generator written in Common Lisp.

Command line args parsed with (Clon)[http://www.lrde.epita.fr/~didier/software/lisp/clon.php]. Shell commands run through (inferior-shell)[http://www.cliki.net/inferior-shell].

Tests can be run with `(asdf:test-system :site-generator)`.

## Features
- Arbitrary data in content pages
- Support for multiple languages
- Conversion of markup with (Pandoc)[http://johnmacfarlane.net/pandoc/]

## Planned features
- Site previewing with (Hunchentoot)[http://weitz.de/hunchentoot/]
- Publishing with with (Rsync)[http://rsync.samba.org/]
- Support for automatic blog-like aggregate pages
- RSS feeds
- Automated Javascript and CSS compilation
- Create new content just through the command line???

## Using site-generator
