# site-generator
site-generator is a static site generator written in Common Lisp. See the section "Using site-generator" for a short tutorial on its use, or check out the examples in the `examples` directory.

This program would not be possible without the many excellent Common Lisp libraries it uses. Many thanks to their creators!

Tests can be run with `(asdf:test-system :site-generator)`.

## Features
- Arbitrary data in content pages
- Support for multiple languages
- Templates expand Common Lisp expressions
- Conversion of markup with (Pandoc)[http://johnmacfarlane.net/pandoc/]
- Site previewing with (Hunchentoot)[http://weitz.de/hunchentoot/]
- Publishing with with (Rsync)[http://rsync.samba.org/]

### Planned features
Features required for version 0.1.0:

- Automatic blog-like aggregate pages
- RSS feed support
- Execution of arbitrary commands before publishing (e.g. to compile Javascript or CSS)

### Possible features
Things that might be done if I ever feel the need to.

- Expand lisp html templates (CL-WHO)
- Create new content through the command line
- Auto-refresh of pages hosted on test server through Javascript injection

## Using site-generator
This tutorial will explain in detail the use of the most important commands you need to make a website. To see help regarding the commands offered by site-generator, use the command `site-generator --help`.
### Installation
Using a stand-alone version of site-generator is the easiest way to get started. Make sure you have (Rsync)[http://rsync.samba.org/] and (Pandoc)[http://johnmacfarlane.net/pandoc/] installed on your computer.
### Creating a stand-alone site-generator
If you want to use a development version of site-generator, you can create a stand-alone executable file by running the file `make.sh`. This script relies on (Quicklisp)[http://www.quicklisp.org/], (Buildapp)[http://www.xach.com/lisp/buildapp/], and consequentially (SBCL)[http://www.sbcl.org/]. The script must be modified to point to your Quicklisp directory.

### Initialization
site-generator uses a particular directory structure to organize your site. Specifically, it relies on the existence of three directories: `content`, `templates`, and `static`. Inside `content` it expects a file called `config`. Additionally, when your site is generated, a directory called `site` will be created. The directory that contains these folders is the sum of your site-generator site.

You can create a fresh site-generator site by running the command `site-generator --init [Directory]` or `site-generator -i [Directory]`. If the directory is omitted, the file from which the command was run will be initialized as a site generator site.

It might be a good idea to track and version the content of your site. Adding the contents of `content`, `templates`, and `static` to a source control system such as (git)[http:/gitscm.org] would accomplish this goal. `site` does not need to be added, as it is created by site-generator from the other directories.
### Creating a site
#### Content pages
##### Templates
#### Templates
#### Static files
### Generating the site
### Using the test server
### Publishing the site

## Known bugs
