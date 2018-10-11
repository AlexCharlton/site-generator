# FREEBSD HOWTO

Edit `make.sh`:

1. Change `#!/bin/bash` to `#!/bin/sh`
2. Change `CC=gcc` to `CC=cc` (clang)
3. Remove the line with `--compress-core`

And it should work out of the box with just sbcl and quicklisp installed.
