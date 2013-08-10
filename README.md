- Locale support
- Arbitrary data in content pages
- Site previewing (Hunchentoot)
- Conversion of markup with (Pandoc)[http://johnmacfarlane.net/pandoc/]
- Publishing with (Rsync)[http://rsync.samba.org/]

Command line args parsed with Clon. Shell commands run through inferior-shell.

Order of evaluation:
- Walk site directory
  - Set up variables from config files
  - Determine changed files
  - For each new or changed file:
	- Read file
	- Set up variables
	- For each locale:
      - Expand template calls recusively starting from outermost template
	  - Render markup
	  - Output page
	- If changed file affects other pages (how do we know?):
	  - Update affected page

Tests can be run with `(asdf:test-system :site-generator)`.
