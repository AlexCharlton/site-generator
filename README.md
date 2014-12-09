{{{toc}}}

# Introduction
*site-generator* is a command-line application for static site generation.

Static site generators combine content with HTML templates to create full web sites. They are useful because many websites don't need dynamic content. Dynamic doesn't refer to content that needs to be updated, but rather content that is either updated by the users of the site (instead of by the author or authors of the site), or updated by computers in some way. Many blogs, technical resources, and business websites only need to be updated by one or a handful of people, and not by the public. Even comments on blogs (if they are deemed to be necessary) can be added through external services, or Javascript.

If your site doesn't require dynamic updates, then a static site comes with benefits. Because there is no code that needs to be run after a request to the server, there is much less room for security problems. And since serving static pages requires less processing, static sites will be faster and use less memory, which means they can handle more traffic than a comparable dynamic site.

*site-generator* was motivated by the apparent lack of static site generators that are able to deal with arbitrary content. While being able to input a title, author, date, and content for each of your pages is nice if you have a blog, the ability to generate any pages that are complex becomes limited to how much you can cram into your content section. For pages with any complexity, this could mean adding a whole bunch of HTML into a file that is meant for content. If other pages were to share this HTML, it would have to be duplicated. This is clearly not solving the problem that a site generator should.

So *site-generator* solves this problem by making you decide what content your site is made of. Behind every page in your site is a *content file* in which you associate your content with variables. These variables can then be used pretty much anywhere[^templatevars] with *site-generator*'s powerful templating syntax. With this power comes responsibility, though. *site-generator* is much more powerful than most static site generators, and is therefore more dangerous.

The other area in which *site-generator* excels is its support for content in multiple languages. Content can be specified, in the same content file, in as many languages as you want, and *site-generator* will create a site for each language.

*site-generator* was written for advanced users. It does not do anything to safeguard against bad practises. It is only a tool for combining content with templates, with some conveniences. People who know Common Lisp will get the most out of *site-generator*, since CL is the language with which *site-generator* can be extended. Knowing Common Lisp is not a prerequisite for using *site-generator*, however. The documentation explains all of the CL required to build pretty much any site (it's not much).

*site-generator* was written in Common Lisp. It would not have been possible without the many excellent Common Lisp libraries it uses, not to mention the excellent open source CL implementations. Many thanks to their creators!

The *site-generator* test-suite can be run with `(asdf:test-system :site-generator)`.

This documentation is easiest to read at [*site-generator*'s home](http://alex-charlton.com/projects/site-generator/).

[^templatevars]: But they should probably be used mostly in *template files*.

# Features
- Accepts arbitrary data in content pages
- Support for content written in multiple languages
- Templates can expand Common Lisp expressions in arbitrary CL environments making the functionality of *site-generator* infinitely expandable (to people who can program in Common Lisp, at least)
- Conversion of markup with [Pandoc](http://johnmacfarlane.net/pandoc/)
- Support for aggregate pages like archives and RSS feeds 
- Site previewing with [Hunchentoot](http://weitz.de/hunchentoot/)
- Publishing with [Rsync](http://rsync.samba.org/)
- HTML/XML can be generated with the lightweight syntax of [CL-WHO](http://weitz.de/cl-who/)
- Execution of arbitrary commands before publishing (e.g. to compile Javascript or CSS)


# Getting *site-generator*
Using a stand-alone version of *site-generator* is the easiest way to get started. Just download the executable file ([Linux (64 bit)](http://alex-charlton.com/static/downloads/Linux/site-generator), [Mac](http://alex-charlton.com/static/downloads/Mac/site-generator)) and place it in a directory that is in your `PATH`. Make sure you have [Rsync](http://rsync.samba.org/) and [Pandoc](http://johnmacfarlane.net/pandoc/) installed on your computer if you want to use the publish or markup features.

## Source
The BSD licenced source code for *site-generator* can be found on [GitHub](https://github.com/AlexCharlton/site-generator).

## Creating a stand-alone *site-generator*
If you want to use a development version of *site-generator*, you can create a stand-alone executable file by running the file `make.sh`. This script relies on [Quicklisp](http://www.quicklisp.org/), [Buildapp](http://www.xach.com/lisp/buildapp/), and consequentially [SBCL](http://www.sbcl.org/). The script must be modified to point to your Quicklisp directory[^ql].

[^ql]: Otherwise, you could overhaul the script.

## Bugs
To see known bugs or to report new ones, go to *site-generator*'s [issue tracker](https://github.com/AlexCharlton/site-generator/issues).


# Using *site-generator*
This tutorial will explain in detail the use of the most important commands you need to make a website. To see help regarding the commands offered by *site-generator*, use the command `site-generator --help`. For more information on any of the variables or functions used in the tutorial, see the appendices.

Two example sites will be worked through in this tutorial: an *example site* and an *example blog*. These sites can be found in the [`examples/`](https://github.com/AlexCharlton/site-generator/tree/master/examples) directory of *site-generator*'s source.

## Initialization
*site-generator* uses a particular directory structure to organize your site. Specifically, it relies on the existence of three directories: `content`, `templates`, and `static`. Inside `content` it expects a file called `config`. Additionally, when your site is generated, a directory called `site` will be created. The directory that contains these folders is the sum of your *site-generator* site.

You can create a fresh *site-generator* site by running the command `site-generator --init [DIRECTORY]` or `site-generator -i [DIRECTORY]`. For all *site-generator* commands, if the directory is omitted, the path from which the command was run will be considered the directory.

So, the *site-generator* directory of the *example site* could be created by running the command:

```
site-generator --init example-site
```

It might be a good idea to track and version the content of your site. Adding the contents of `content`, `templates`, and `static` to a source control system such as [git](http:/gitscm.org) would accomplish this goal. `site` does not need to be added, as it is created by *site-generator* from the other directories. Tracking the file `.database`, which is created after the first run of *site-generator*, is also not a bad idea.

## Creating sites
As previously hinted at, a *site-generator* site is made of three kinds of files: *content* files *template* files, and *static* files.

Content files are the files that contain the actual content of your site. These generally have a one-to-one relationship with the pages of the site that you want to generate. For instance, each blog post you create will have a content file of its own, which specifies the contents of that blog post.

Template files are mostly plain HTML files that have special *template variables* or *template expressions*. Each content file has an associated template file. When the content file is turned into a page, the page is built by taking the contents of the template file and replacing all the template variables and expressions with the relevant data.

Static files are the files that don't change when the site is generated. This includes images, Javascript, and CSS files.

We will discuss these three types of files in more detail in the remainder of this section.

### Our first content file
For each web page you want, you need a content file. Content files describe the content of the page you want to create. So for the *example site*, we decide we want to have an about page, and we want the page to have a title, so we create the file `example-site/content/about` and in it, we write:

```
:title
About site-generator
```

Here we are defining a content variable called `title`, and assigning the string `About site-generator` to that variable. In content files, variable definitions begin with a `:` followed immediately by the name of the variable we want to define.

Since we want this site to be bilingual, we add more to the file:

```
:title
About site-generator

:title lang=fr
À propos de site-generator
```

The `title` variable now has some new information. Namely, that the French component of the title is the string `À propos de site-generator`. The previous information assigned to title (`About site-generator`) is still there, since it was assigned to the default language (which is English, a.k.a. `en`). Note that variable definitions (or in this case the addition of new content in a different language) must be preceded by a blank line.

We'd also like to add some sort of primary content to the file. we decide that the content should be the following:

```
:content lang=en markup=none
<p>This is *another* (the asterisks are deliberate) page
that uses a different template.</p>

:content lang=fr
Ceci est un *autre* (il n'y a pas d'astérisques, ici)
page qui utilise un template différent.
```

Here we're defining the English part of the content (which we assign to the variable `content`), and we're saying that we want its markup to be `none` -- that is, we want its string to be placed directly into the template without any marking up occurring. We're also redundantly defining the language of this content as `en`, but we could leave that out since `en` is the default language. We also define some French content. Since we haven't said anything about its markup, it will be marked up in the default fashion. We'll talk about markup in a bit.

Now we want to specify the template that this page will use. We decide that, in a little bit, we'll create a template called `other.html`, and we want to use that template for our about page. So, still in our content file, we write:

```
:template
other.html
```

While this looks the same as the content variable definitions that we made before, this is a special variable called a *configuration variable*. A fixed set of variable names are reserved by *site-generator*, and they are used to configure the way a site behaves. `template` is one such configuration variable, and we'll see more soon. Almost all configuration variables differ from content variables in that they are the same for all languages. Because of this, if we were to write `:template lang=fr`, the `lang=fr` part would just be ignored.. The complete list of configuration variables can be seen in [appendix A](#appendix-a-configuration-variables).

Now, assuming that we had made the template `example-site/templates/other.html`, what would happen when this one-page site is generated? The template `other.html` would be filled in with the values from our file `example-site/content/about`, and the result is output in the file `example-site/site/About_site-generator/index.html` (a French file is generated, too, but we'll get back to that).

There's a few things going on with the way that the output file's name was chosen. First, we see that the file being output is called `index.html`, and it's the directory that this file is in that has a distinctive name. This is so that, when we visit the site at `www.example-site.com/About_site-generator/`, we get the page that we want and the URL is slightly prettier than `www.example-site.com/About_site-generator.html`. If you do want the latter behaviour, it can be controlled with the configuration variable `pages-as-directories` (see [appendix A](#appendix-a-configuration-variables)). Second, we notice that the name of the page was inherited from the title that was set in the content file (albeit with an underscore in the place of the space, since you can't have spaces in URLs). *site-generator* recognizes the page's `title` as special, and will set it as the output page's path. If there was no `title` variable set, then the content file's file name would have been used instead (e.g. `example-site/site/about/index.html`).

Say we decide that `A_propos_de_site-generator` is too long a name for the page, in French. The special configuration variable `slug` can be used to override the path to the output page. `slug` is special because its content *is* associated with different languages.

```
:slug lang=fr
A_propos
```

This sets the French language content of the variable `slug` to `A_propos`. So when we generate this content file, we get two pages in the site. One English language page, and one French language page at `example-site/site/fr/A_propos/index.html`. We see that the French page is at a new directory: `example-site/site/fr/`. Every non-default language gets its own directory named after its language code.

### Config files
There is one content file, for each directory in `content/`, that is special. Files named `config` are treated differently from the other content files. *Config files* are used to set variables that are accessible to all of the content files in its directory (and sub-directories). So say we want the entire site to know its name; A config file is the place to put it -- specifically the top-level config file, which is the config file that's in the `content` directory of your site. The file `example-site/content/config` was already created when we initialized the site, so now we're going to put stuff in it:

```
:site-name
site-generator
```

Now all of the content files in the site know that `site-name` is equal to `site-generator`.

We should also tell the site what languages it should be generating. By default, *site-generator* will only generate one language -- the default language. We can set the languages it should generate to :

```
:languages
en fr
```

`languages` is a configuration variable that can only be set in the top-level config file. It's noteworthy that the language codes we're using are arbitrary. By default, *site-generator* only knows the code `en`, and you don't even have to use it. The language codes are just indicators, for you, of what language a particular thing is. Any string can be used for a language code (although case won't be preserved). The default language can be set with the configuration variable `default-language` (which, again, can only be set in the top-level config file).

### Markup
We decide that every page in the site is going to have, or at least might want to have, access to the same navigation bar. We also decide that we can't be bothered to write out the HTML for this navigation bar, so we're going to create this bar in [Markdown](http://daringfireball.net/projects/markdown/). Markdown is just one of the many markup languages that [Pandoc](http://johnmacfarlane.net/pandoc/) -- *site-generator*'s markup tool -- can parse. The navigation bar will consist of an unordered list of links that we will write in our `example-site/content/config` file like this:

```
:nav lang=en 
* [About site-generator]($(page-address "about"))

:nav lang=fr
* [À propos de site-generator]($(page-address "about"))
```

The asterisk is markdown's way of indicating items in an unordered list, while the `[link text](URL)` syntax indicate links. Right now our site only has one page, so our list of links only has one item -- the about page. Rather than derive the address for each page, in every language, we've added in some code that will be replaced by the address of the desired page, based on its content file name. We'll visit the syntax of this code later.

Now since we want the `nav` variable to be interpreted as Markdown, we could have written it like this:

```
:nav lang=en markup=markdown
* [About site-generator]($(page-address "about"))

:nav lang=fr markup=markdown
* [À propos de site-generator]($(page-address "about"))
```

But instead we'll add some new lines:

```
:default
:nav markup=markdown
:content markup=markdown
```

This tells *site-generator* that the default values of `markup` for  `nav` (and `content`) will be `markdown`.

Alternately we could have written:

```
:markup
markdown
```

To set the global value of `markup`.

Pandoc can do a good deal of things and interpret a lot of markup languages. *site-generator* provides a number of configuration variables (or arguments to content variables) to affect its input. The main two are `markup` and `output-format` which tell Pandoc how to interpret its input, and what language to output to. The values of these can be almost anything that [Pandoc supports](http://johnmacfarlane.net/pandoc/README.html) as input and output formats.[^output]

The remainder of the Pandoc configuration variables try to cover most of the other options that are both supported by Pandoc and that make sense in the context of *site-generator*. The full list of them can be seen in [appendix C](#appendix-c-pandoc-configuration-variables).

[^output]: Some output formats obviously won't make much sense -- why would you be outputting to a Word doc?

### Wrapping up the example site content
In order to flesh out our example site, we're going to add a couple more pages. First will be `example-site/content/index`, which was actually already created when we initialized the site. This is the page that will appear when you visit the top-level of the example site domain. To it we will add some basic content.

```
:content
This content is the same for both the English and French
pages (désolé!).
```

When no version of a piece of content is specified for a given language, the content of the default language will be used. So this content will look the same for both the English and the French versions of the site.

For our next file we decide to put it in a new folder, because perhaps we have more pages of the site that we'll want to group together in this folder. Because we're running out of names for pages of this site, we'll make our new content file at `example-site/content/foo/bar`.

```
:content
Content

:slug
Bar
```

This bare-bones content file should be easy to understand. We also want to change the way that the name of the folder is rendered in English and in French, so we create the file `example-site/content/foo/config`.

```
:directory-slug lang=en
Foo

:directory-slug lang=fr
Quox
```

`directory-slug` is like `slug`, but it sets the directory URL string.

We'll also amend our navigation bar (in `example-site/content/config`) to include these new pages.

```
:nav lang=en 
* [Home]($(page-address "index"))
* [About site-generator]($(page-address "about"))
* [Foo]($(page-address "foo/bar"))

:nav lang=fr
* [Accueil]($(page-address "index"))
* [À propos de site-generator]($(page-address "about"))
* [Foo]($(page-address "foo/bar"))
```

Now when we generate this site, we'll be creating (in the directory `example-site/site/`) the pages `index.html`, `fr/index.html`, `About_site-generator/index.html`, `fr/A_propos/index.html`, `Foo/Bar/index.html`, and `fr/Quox/Bar/index.html`. Now all we need to do is make the template files for this site.

### Template files
We know we need to make at least two template files, `main.html` (which is the default template file, specified in the top-level config file) and `other.html` (which is used by `about`). Let's start with `main.html`. We make the file `example-site/templates/main.html` and in it we put the outline of what we want the HTML of this site to be:

```html
<!DOCTYPE HTML>
<html>
<head>
<meta charset="UTF-8">
<title><!-- PAGE TITLE GOES HERE --></title>
</head>

<body>
  <div><!-- LANGUAGE SELECTION GOES HERE --></div>
  <header>
    <h1><!-- SITE NAME GOES HERE --></h1>
  </header>
  <nav>
    <!-- NAV BAR GOES HERE -->
  </nav>
  <article>
    <!-- MAIN CONTENT GOES HERE -->
  </article>

  <footer>
    <!-- FOOTER STUFF GOES HERE -->
  </footer>
</body>

</html>

```

All these comments are place-holders for where we want content to be filled in. To fill in the content we need to add *template variables* or *template expressions*. Template variables are the simplest to understand. They are the name of a content variable that you have defined (or plan to define) in a content file, surrounded by dollar signs -- like `$content$`. We already know that several of these place-holders map directly to content variables that we defined in our content pages:

```html
<!DOCTYPE HTML>
<html>
<head>
<meta charset="UTF-8">
<title>$title$</title>
</head>

<body>
  <div><!-- LANGUAGE SELECTION GOES HERE --></div>
  <header>
    <h1>$site-name$</h1>
  </header>
  <nav>
    $nav$
  </nav>
  <article>
    $content$
  </article>

  <footer>
    <!-- FOOTER STUFF GOES HERE -->
  </footer>
</body>

</html>

```

### Template expressions, or A Lisp primer
Now, for the language selection, we want some code that will output a list of links that point to the current page but in a different language. *site-generator* provides a function to do so called `other-languages`. To call this function, we need to use a *template expression* which is a set of parentheses containing the desired expression (written in Common Lisp), preceded by a dollar sign. So our language selection will look like so:

```html
  <div>$(other-languages)</div>
```

And the HTML that will be output for the page `about` will look like this:

```html
  <div>
    <ul class='languages'>
      <li class='current-language'>EN</li>
      <li><a href='/fr/A_propos/'>FR</a></li>
    </ul>
  </div>
```

`other-languages` has assigned classes to both the unordered list and the item which represents the current language. Say we want to change the class name for the unordered list from `languages` to `langs`. The function `other-languages` gives us a way to do so. But first, a bit of a Lisp lesson.

The syntax for any Lisp expression is `(function ARGS)`, so `(+ 1 2)` is the Lisp way of writing `1 + 2`. There are also keyword arguments[^keyword] which are called by writing `(function :keyword arg)`. Because the arguments are named, they can be placed in any order. So `(function :key1 foo :key2 bar)` is the same as `(function :key2 bar :key1 foo)`. Keyword arguments can also be left out and they should default to something sensible, so `(function :key2 bar)` is also allowed.

`other-languages` provides two keyword arguments: `ul-class` and `selected-class`. We want to change the `ul-class`, so we'll write

```html
  <div>$(other-languages :ul-class "langs")</div>
```

`langs` is surrounded by double quotes because it needs to be interpreted as a string. Otherwise, Lisp would think it referred to a variable.

Say we realize that we don't want the `<title>` of the page to be just the variable `title`, but we also want to include the `site-name`. We also realize that not all pages have the variable `title` set, so how are we going to get it to work? We need to use a conditional expression:

```html
<title>$(when (bound? title)
          (echo title " — "))
       $site-name$
</title>
```

Here we're using the Lisp conditional expression `when`. The syntax for `when` is `(when TEST-EXPRESSION TRUE-EXPRESSIONS)`, meaning when `TEST-EXPRESSION` evaluates to true (anything that's not `nil`, the canonical Common Lisp false value), `TRUE-EXPRESSIONS` are run. So in the above template expression we're saying that when the variable `title` is bound (`(bound? title)`), then `echo` (combine the arguments into one string) the value of the variable `title` and the string `" — "`. The value of `site-name` is going to appear no matter what.[^if]

One thing to note is that newlines and indentation have no effect on Lisp code, it's just there to make it easier to read.

With our new-found Lisp skills, we decide to write the footer. We realize that we want to have two versions of the footer -- one in English and one in French. This means that it is content (since it is associated with a language), so we'll add the following to `example-site/content/config`:

```commonlisp
:footer-text lang=en
This is the end of the
page$(when (bound? title)
       (echo " "
             (markup
              (echo "\"" title
                    "\" (these should be curly quotes)")
              :output-format :markdown
              :markup :markdown))). 

:footer-text
Ceci est la fin de la
page$(when (bound? title)
       (echo " \"" title "\"")).
```

The French footer should be pretty easy to understand. The template expression is saying: When the variable `title` is bound, echo the text ` " TITLE-TEXT"`. The only tricky bit there are the backslashes in front of the quotation marks. They are there because we want to output literal quotation marks and we don't want to prematurely end the string that contains them, so we escape the quotation marks with a backslash.

Speaking of escaping with backslashes, this is also how we escape template variables and expressions. So `\$hi$` will be output as `$hi$` when it gets run through *site-generator*, and the variable `hi` won't be expanded. Most of the time, you don't need to escape dollar signs, though. The only times dollar signs need to be escaped is when they might be interpreted as a template variable or expression, and you don't want them to be. Template expressions always begin with `$(`, and template variables are only considered when they have two dollar signs surrounding a string without whitespace.

We've gone a bit crazy with the English footer. The first part is the same as the French footer-- we're only doing something when `title` is bound -- but the rest includes a call to `markup`. `markup` is the function that *site-generator* uses to run text through Pandoc. In this case, we're passing it the string `"TITLE-TEXT" (these should be curly quotes)`, and we're setting the `output-format` to `markdown` and the `markup` to `markdown`. Why would we be reading and outputting markdown? Well, we're taking advantage of the Pandoc `smart` option, which automatically creates directional quotations where appropriate. Since we don't want this text to be surrounded by `<p>` tags (which would happen if we set the `output-format` to `html`) we output to markdown and the only change to the text is the directional quotes. Leading and trailing whitespace are stripped by Pandoc which is why we have the extra `echo` with a space.

[^if]: We could have also written the above `when` expression using `if`, which has the syntax `(if TEST-EXPRESSION TRUE-EXPRESSION FALSE-EXPRESSION)`:

    ```html
<title>$(if (bound? title)
            (echo title " — " site-name)
            site-name)
</title>
```

[^keyword]: Keyword arguments are generally known as "named arguments" in other languages.

### Breaking up templates with `include`
Now that we've finished one template, we will move on to the other -- aptly named `other.html`. We realize that we want to reuse the header and footer structure of `index.html`. To do this, we will create two pages -- `example-site/templates/header.html`:

```html
<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>$(when (bound? title)
            (echo title " — "))
	     $site-name$
  </title>
</head>

<body>
  <div>$(other-languages)</div>
  <header>
    <h1>$site-name$</h1>
  </header>
  <nav>
    $nav$
  </nav>
```

And `example-site/templates/footer.html`:

```html
  <footer>
    $footer-text$
  </footer>
</body>

</html>
```

And modify `main.html`:

```html
$(include "header.html")
  <article>
    $content$
  </article>
$(include "footer.html")
```

The templates `header.html` and `footer.html` are used in `main.html` through the function `include`, which accepts one argument: the name of a template file, relative to the template directory. The `include` expression gets replaced with the contents of the template file that is named.

The contents of `example-site/templates/other.html` will be similar to `main.html`.

```html
$(include "header.html")
<div class="otherstuff">
  $content$
  <p>This text is part of the template!</p>
</div>
$(include "footer.html")
```

Instead of the `<article>` tag, we have put the content in side a `<div class="otherstuff">`, and we have added a paragraph to the end of that `div`, that will always appear in that template.

That concludes our first site! The full site can be seen in the [`examples/example-site/`](https://github.com/AlexCharlton/site-generator/tree/master/examples/example-site) directory of the source. Go to the section [Generating the site](#generating-the-site) to learn how to generate the actual site, or continue on to see how you can use *site-generator* to create a blog.

### An example blog
We're going to approach this *example blog* from the opposite end, compared to how we made the *example site*. We'll start with the templates then move onto the content.

We will again have a template called `main.html`. This `main.html` is going to look quite different from the other one. Rather that writing out the HTML by hand, like a savage, we'll be generating the HTML using Lisp. Specifically, using [CL-WHO](http://weitz.de/cl-who/). The file `example-blog/templates/main.html` will contain the following:

```commonlisp
$(xml
   (:html
    (:head
     (:meta :charset "UTF-8")
     (:title (str (when (bound? title)
                    (echo title " — ")))
             (str site-name)))
    (:body
     (:header (:h1 (str site-name)))
     (str nav)
   (str contents))))
```

So what's going on here? First, the template expression consists of `$(xml ...)`. `xml` is *site-generator*'s way of denoting that the following code is going to represent a tree of XML (which HTML is). All of the "functions" in that tree (`:html` `:head`, `:body`, etc.) will become HTML tags. Like HTML, these expressions are nested. Attributes of the tag are denoted like keyword arguments: `:attribute value`. So `(:meta :charset "UTF-8")` will turn into `<meta charset="UTF-8" />` The final elements in an HTML expression (that doesn't belong to an attribute) will become the content of that tag. So `(:div "Hi, there!" " Bye!")` would become `<div>Hi, there! Bye!</div>`. When these final values are not a literal string, but instead some other value (like a variable or a function call), we need to tell CL-WHO to treat it like a string. We do this with `str`. `(:body (str contents))` results in the HTML `<body>Whatever the variable contents is</body>`.

In the above template, we see that we have three variables that we'll need to define in the site's content (look at the content of the `str` expressions): `title`, `site-name`, `nav`, and `contents`. So in order to make our blog, we'll need to fill in those variables.

In our top-level config file `example-blog/content/config`, we'll add the following content:

```commonlisp
:site-name
Example Blog

:template
main.html

:nav
$(xml (:nav
       (:ul 
        (:li (:a :href (page-address "index")
                 "Home"))
        (:li (:a :href (page-address "archive")
                 "Archives")))))
```

We're defining the `site-name` and `nav`, which are both required by `main.html`. We're also setting the default template to be `main.html`, as expected. The `nav` variable is using the same CL-WHO syntax to generate its HTML.

So what is the actual content of the site we want to make? We want this to be a blog, so there must need to be some blog posts. We also promised in our `nav` variable that there will be at least two other pages: one named `index` and one named `archive`. Let's work on the blog posts for now.

We know that each blog post is going to be part of a logical set of all the blog posts, and that these posts will probably be different from the other pages on this site. Sounds like we need a new folder: `example-blog/content/pages/`, and in it we will make a config file:

```commonlisp
:contents
$(xml (:article (:h2 (str title))
                (:div :class "article-info" 
                      (:div :class "author"
                            (str author))
                      (:div :class "date"
                            (str (page-date :current))))
                (str (content))
                (:span :class "prev"
                       (let ((prev (prev-page "pages")))
                         (when prev
                           (htm (:a :href (page-address prev)
                                    "Previous post")))))
                (:span :class "next"
                       (let ((next (next-page "pages")))
                         (when next
                           (htm (:a :href (page-address next)
                                    "Next post")))))))

:author
Alex
```

Now we've filled in the variable `contents`. Ignoring for a moment the details of the big block of XML, we'll just look at what variables are present there (again, look at the `str` expressions): `title`, `author`, and `content`. From this we know what variables we'll need to define when we make a blog post. We've also set `author` to default to `Alex` for all files in this directory.

Now all we need in order to create a blog post is to create a file in the directory `example-blog/content/pages/` and fill in some simple values. We do this for the pages `first`, `second` and `third`. For instance `example-blog/content/pages/second`:

```
:title
Second post

:date
day=17

:content 
Lorem ipsum...
```

Getting back to that big chunk of HTML in `example-blog/content/pages/config`, we see a number of functions that we don't recognize. The first is `page-date`, which is being called with the keyword `:current`. What this function does is return the formatted date of the specified page. In this case the page being specified is the current page, but we could refer to another page, such as `"index"`.

### Dates
Dates in *site-generator* are complicated enough that they deserve some more discussion. First, you might notice that in `second`, we define `date` to be `day=17`. In `third` we decline to set `date` to anything at all. By default, *site-generator* will set the date of the page to the modification time of the file when the site was first generated. That means that the date output by `page-date` will stay the same even after the content file is modified, as long as the site has been generated with that content file at least once.[^lastmodified] In your content file you can modify any element of the default date. Setting any of `second`, `minute`, `hour`, `day`, `month`, or `year` in the `date` configuration variable will change that element of the default date. For instance setting `date` to `day=17 month=11` will set the date of that file to the 17th of November, retaining the year and time from the default date of the file.

Dates can also be formatted in any way you want. `page-date` accepts the keyword argument `:format`. `:format` is a list of strings and keywords that specify what you want the format of the date to be. In order to write a literal list in Lisp you can use the `list` function (e.g. `(list 1 2 3)`) or, if the list does not contain any elements that need to be evaluated, the quote syntactic sugar (e.g. `'(1 2 3)`).

The following keywords are accepted by the format argument to `page-date`:[^local-time]

- `:year`: *year 
- `:month`: *numeric month 
- `:day`: *day of month 
- `:weekday`: *numeric day of week, starting from 0 which means Sunday 
- `:hour`: *hour 
- `:min`: *minutes 
- `:sec`: *seconds 
- `:msec`: *milliseconds 
- `:iso-week-year`: *year for ISO week date (can be different from regular calendar year) 
- `:iso-week-number`: *ISO week number (i.e. 1 through 53) 
- `:iso-week-day`: *ISO compatible weekday number (i.e. monday=1, sunday=7) 
- `:ordinal-day`: day of month as an ordinal (e.g. 1st, 23rd) 
- `:long-weekday`: long form of weekday (e.g. Sunday, Monday) 
- `:short-weekday`: short form of weekday (e.g. Sun, Mon) 
- `:long-month`: long form of month (e.g. January, February) 
- `:short-month`: short form of month (e.g. Jan, Feb) 
- `:hour12`: hour on a 12-hour clock 
- `:ampm`: am/pm marker in lowercase 
- `:gmt-offset`: the gmt-offset of the time, in +00:00 form 
- `:gmt-offset-or-z`: like :gmt-offset, but is Z when UTC 
- `:timezone`: timezone abbreviation for the time

Elements marked by * can be placed in a list in the form: `(:keyword PADDING &optional (PADCHAR #\0))`, where `PADDING` is the number of digits that the element should be padded to and `PADCHAR` is the optional character with which to pad, defaulting to `#\0` (a literal `0` character). So `(:seconds 2)` would format as the number of seconds with two digits, e.g. `03` or `24` seconds.

The default format string is `'(:long-month " " :ordinal-day ", " :year " " :hour ":" (:min 2) " " :timezone)`, which looks like: August 26th, 2013 21:15 EDT.

[^lastmodified]: The last modification time of the file can be accessed with the similar function `page-last-modified`.

[^local-time]: These keywords are inherited from the underlying time library that *site-generator* uses:  [local-time](http://common-lisp.net/project/local-time/manual.html).

### Accessing information about other pages
As a reminder, `example-blog/content/pages/config` contains the following:

```commonlisp
:contents
$(xml (:article (:h2 (str title))
                (:div :class "article-info" 
                      (:div :class "author"
                            (str author))
                      (:div :class "date"
                            (str (page-date :current))))
                (str (content))
                (:span :class "prev"
                       (let ((prev (prev-page "pages")))
                         (when prev
                           (htm (:a :href (page-address prev)
                                    "Previous post")))))
                (:span :class "next"
                       (let ((next (next-page "pages")))
                         (when next
                           (htm (:a :href (page-address next)
                                    "Next post")))))))

:author
Alex
```

We still have to explain a couple of functions that were used in `contents`. First is `(content)`. While this is referring to the content variable `content`, this is using the syntax of a function call. Why is that? For every content variable that we create, a function[^fn] with the same name is also created. This function is responsible for doing the markup of the contents of the variable, as well as expanding any template variables or expressions that it might contain. So inside a template expression such as `$(xml ...)`, when we refer to `title`, we're actually referring to the "raw" data of title -- the unprocessed string. Whenever that string doesn't include any template variables or expressions, and it isn't supposed to be marked up, we can use that "raw" data and there will be no difference. When the content *should* be marked up, like the variable `content`, we must use its function call, e.g. `(content)`. Since we generally expect things to be marked up and expanded, a template variable like `$title` is actually the same as the template expansion `$(title)`. 

Underneath the `(str (content))` there are two spans with the classes `prev` and `next` which are used to point to the previous and next blog posts. The content of these spans is some code that begins with `(let ...)`. `let` is a lisp expression that has the syntax `(let (BINDINGS) EXPRESSIONS)` where bindings are any number of `(VARIABLE VALUE)` pairs. `let` establishes these variables within the scope of its body. So,

```commonlisp
(let ((a 1)
      (b 2))
  (+ a b))
```

Is equal to `3`. In the above `(:span :class "prev" ...)`, we're setting the local variable `prev` to `(prev-page "pages")`. `(prev-page DIRECTORY)` is a function that returns the previous page, chronologically from the current page, out of the pages in `DIRECTORY`. Now, anywhere in the `let` we can refer to `prev`. So the rest of the `let` expression,

```commonlisp
(when prev
  (htm (:a :href (page-address prev)
           "Previous post")))
```

Is saying that, when the variable `prev` exists (because there isn't always going to be a previous page, in the case that you are rendering the most recent page), output a link to the address of that previous page with the link text `"Previous post"`.

The one last thing we haven't talked about is the `(htm ...)` here. Remember `str` that was used when you wanted the non-string-literal contents of a bit of CL-WHO HTML to be output as a string? You might wonder why `str` was not placed around the `let`. In this case, since we got back to using CL-WHO HTML syntax, we didn't need a `str`[^htm]. Instead we use `htm` to let CL-WHO know that we wanted to get back to using its syntax.

The next span, with class `next`, is the same as the one with class `prev`, but it makes reference to the posts that were made prior to the current page.

[^fn]: Actually a macro is created, but this is of little consequence.

[^htm]: We could choose to do so, but then we'd need another `xml` function in place of the `htm`

### Custom Lisp functions

Based on the `nav` variable that we set in the top-level config file, we know we have at least two more pages that we want to create: `index` and `archive`. `example-blog/content/index` will be a very simple page, with one twist:

```commonlisp
:contents
HI! $(foo)

:cl-environment
(defun foo ()
  "I'm a function!")
```

We're defining `contents` as expected, but in it we're referencing the function `foo`. What's `foo`? `foo` isn't provided by *site-generator*. In fact, it doesn't exist until it is defined later in that content file:

```commonlisp
:cl-environment
(defun foo ()
  "I'm a function!")
```

The configuration variable `cl-environment` sets up a custom Common Lisp environment that is created before your content files are turned into web pages. In the above environment, we're defining the function `foo` using `defun` which has the syntax `(defun FUNCTION-NAME (ARGS) EXPRESSIONS)`. `foo` just returns the string `"I'm a function!"`, so it isn't particularly useful. You could use any Common Lisp functions you want, in `cl-environment`, including loading Lisp files. By doing so, *site-generator* can be extended to do pretty much anything. While we can't give a full tutorial on how to use Common Lisp, we recommend reading [Practical Common Lisp](http://www.gigamonkeys.com/book/) in order to learn more about the language.

### Creating aggregate pages
So now we need to create `example-blog/content/archive`. We know that this should be a list of all of the articles we put in `example-blog/content/pages/`. In order to get this information, we'll use the function `(get-pages DIRECTORY)` to get a list of all the pages in the directory `pages/`. We'll then need to loop over this list, creating HTML for each page.

```commonlisp
:title
Archive

:contents
$(xml 
   (loop for page in (get-pages "pages")
      do (htm
          (:article
           (:h2 (:a :href (page-address page)
                    (str (page-title page))))
           (:div :class "article-info"
                 (:div :class "author"
                       (str (page-author page)))
                 (:div :class "date"
                       (str (page-date page))))
           (:p (str (first-line
                     (get-content page :content))))
           (:p (:a :href (page-address page)
                   "Keep reading..."))))))

:depends
pages/
```

`loop` uses the syntax `(loop for X in LIST do EXPRESSION)` to loop over a list.[^loop] For each element of `LIST`, it assigns the value to `X` and then executes the `EXPRESSION`. In this case, for each element in `(get-pages "pages")`, we're creating an `<article>` tag and in it we're putting a header containing the title of the article, a div containing the page's author and date, a paragraph containing the `first-line` of `(get-content page :content)` (which gets the supplied content variable from the given page, so in this case, we are getting the first paragraph of the `content` of each page), and a paragraph containing a link to the rest of the article.

Finally we see that the page `depends` on `pages/`. Hopefully it makes some sense that a page that uses information from the directory `pages/` should depend on that directory. The full meaning of `depends` is explained in [Generating the site](#generating-the-site).

[^loop]: The full syntax of `loop` is [pretty complex](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm), but this should suffice for most sites. Practical Common Lisp has a [good introduction](http://www.gigamonkeys.com/book/loop-for-black-belts.html) to this crazy macro.

### Creating RSS feeds
How about an RSS feed for this blog? We create the file `example-blog/content/rss`:

```
:extension
xml

:template
rss.lisp

:depends
pages/
```

Simple enough. The only new thing is the `extension`. This configuration variable prevents *site-generator* from outputting an `.html` file and will instead force it to output a `.EXTENSION` file. In this case, we'll be generating the page `example-blog/site/rss.xml`.

Also notable is that we set the template to `rss.lisp`. Templates don't need to have any particular extension, so since we'll be writing this template mostly in Lisp (even though it is still a *site-generator* template file) we might as well let our editor know how to handle it. In `example-blog/templates/rss.lisp`, we write:

```commonlisp
$(xml
   (:rss :version "2.0"
         (:channel 
          (:title "Example Blog")
          (:link "http://example-blog-url.com/")
          (:description "An example blog for site-generator")
          (:lastBuildDate (str (build-time)))
          (:language "en-us")
          (loop for page in (get-pages "pages" :number 2)
             do (htm
                 (:item
                  (:title (str (page-title page)))
                  (:link (str (page-address page)))
                  (:guid (str (page-address page)))
                  (:pubDate (str (page-date
                                  page
                                  :format +rfc+)))
                  (:description
                   "<![CDATA[ "
                   (str (markup (get-content page
                                             :content)
                                :markup :markdown))
                   " ]]>")))))))

:depends
pages/
```

The only new function here is `build-time` which returns the string representing the time at which it is called, formatted to the RFC 3339 Internet standard. We also see `(page-date page :format +rfc+)` where `+rfc+` is the format list which corresponds to the aforementioned standard. 

Now that we have our RSS feed, we should add it to the header of our `main.html` template. While we're at it, why don't we add a reference to a style sheet!:

```commonlisp
$(xml
   (:html
    (:head
     (:meta :charset "UTF-8")
     (:link :href "/static/style.css"
            :rel "stylesheet"
            :type "text/css")
     (:link :href "/rss.xml"
            :rel "alternate"
            :type "application/rss+xml"
            :title "Example blog RSS feed")
     (:title (str (when (bound? title)
                    (echo title " — ")))
             (str site-name)))
    (:body (str contents))))
```
	 
### Static files
The link of the style sheet refers to it being located in `/static/`. This is because the style sheet is a static file. We add it to `example-blog/static/`, and when we generate our site that folder will be linked to `example-blog/site/static/`, where any page can access it. The static folder is consequentially the place to put any CSS files, Javascript files, and images.

Our blog is now ready to be generated! The full sources for this example blog can be found in the [`examples/example-blog/`](https://github.com/AlexCharlton/site-generator/tree/master/examples/example-blog) directory of the source.

## Generating the site
Once you have created a site, generating it is easy! Simply run the command `site-generator [DIRECTORY]`. When any changes are made to the site, run it again.

This command will only regenerate the files that need to be. So if you modify the file `example-site/content/index`, only the file `example-site/site/index.html` will be regenerated. Modifying config files will trigger the regeneration of all of the files in their directory (and sub-directories), and modifying template files will trigger the regeneration of all the files that use them. This means that modifying the top-level config file will cause the entire site to be regenerated.

The configuration variable `:depends` will alter this behaviour. `:depends` takes a line-separated list of paths. Pages for which `:depends` is set will be updated when any files named in that list of paths, or that exist in a directory name by those paths, is changed. For example, since the files `example-blog/content/rss` and `example-blog/content/archives` both have the lines:

```
:depends
pages/
```

Modifying the file `example-blog/content/pages/first` will trigger the regeneration of `example-blog/site/First_Post!/index.html`, `example-blog/site/Archive/index.html`, and `example-blog/site/rss.xml`. This behaviour is critical for pages that should be updated whenever a particular set of content changes.

## Using the test server
When *site-generator* is run with the `--test-server` flag (`site-generator -s[PORT] [DIRECTORY]` or `site-generator --test-server=[PORT] [DIRECTORY]`), it will launch a test server that hosts your site at the optionally specified PORT. You can access this site through the address that is printed out to the command-line. Previewing your site in this manner is a handy way of seeing what the pages of your site look like before publishing.

When the test server is running, your site will be constantly scanned for changes. If changes are detected, then the relevant pages will be regenerated. If you have any of the modified pages open in your browser, they will need to be refreshed for the changes to take effect.

Typing `quit` or `exit` (or inserting an end-of-file character) into the command-line, while the server is running, will cleanly exit the server.

## Publishing the site
When *site-generator* is run with the `--publish` flag (`site-generator -p [DIRECTORY]`), it will generate your site then push it to the server specified in your top-level config file by the variable `:server`. The server string should be an Rsync compatible string specifying your username, server address, and the directory on the server into which the site should be loaded. This string is in the form of: `username@server:dir`. E.g.:

```
:server
alexcharlton@alex-charlton.com:alex-charlton.com/
```

Be careful, though: *site-generator* deletes any content that is present in that directory on the server! If you have files you don’t want to touch, you can add one or more files or directories to the `:exclude` variable. This should be a line separated list of Rsync patterns. `:exclude` will also prevent *site-generator* from uploading any files or directories that you don’t want to. E.g.:

```
:exclude
*.php
secret-local-file.html
```

This example could be used in the case that you have PHP files on the server that you don’t want deleted, and you also don’t want to upload `secret-local-file.html`.


## Executing additional commands
Sometimes, your site may depend on having some other commands run for it to be built properly. A common example of this is needing to compile CoffeeScript, et al., to Javascript. *site-generator* makes it easy to automate these commands through the `:commands` variable, which must be placed in the top-level config file.

`:commands` is processed as a list of command-line commands, separated by newlines. Each command is run with the *site-generator* directory as the current directory. Newlines can be escaped with backslashes. E.g.:

```
:commands
coffee --compile coffee-script/hello.coffee \
       --output static/js/
```

Commands are executed asynchronously. If you wish to run multiple commands in order, separate them with a semicolon (possibly followed with an escaped newline, if desired). E.g.:

```
:commands
coffee --compile coffee-script/hello.coffee \
       --output static/js/                 ;\
echo "CoffeeScript file compiled"
```

When *site-generator* is run with the `--run-commands` flag (`site-generator -r [DIRECTORY]`), the commands specified by `:commands` will be run. From the previous example, this means that the file `coffee-script/hello.coffee` will be compiled and output to the directory `static/js/`, after which the string `CoffeeScript file compiled` will be printed out to the terminal.


# Appendices
## Appendix A -- Configuration variables
Configuration variables are variables that are defined in config or content files that have special meaning to *site-generator*. The list below describes how the content of each of these variables is interpreted.

- `:cl-environment`: Common Lisp code that is evaluated before the content file is generated into a page.
- `:commands`: Line separated command-line commands that are executed when *site-generator* is passed the `--run-commands` flag. Newlines can be backslash escaped. May only be defined in the top-level config file.
- `:date`: A list of `unit=value` pairs which are used to modify the default date of the content file. Supported units are `second`, `minute`, `hour`, `day`, `month`, and `year`. May only be defined in a content file.
- `:default`: Accepts lines with a syntax similar to the definition of content variables. Used to set the default Pandoc arguments for specific content variables. This variable is special in the way it is inherited between config files -- the default values are merged together rather than overwritten. The default value of `default` is `:content markup=markdown`, which makes `markdown` the default `markup` value for all `content` values.
- `:default-language`: A language code that is set to be the default language for the site. Defaults to `en`. May only be defined in the top-level config file.
- `:depends`: The list of paths and files, relative to the content directory, that the files for which this variable applies depend upon for generation. Every time a file that is depended upon is modified, the file that depends on it will be regenerated.
- `:directory-slug`: The string that will be used to represent the URL of the directory of the config file where `directory-slug` was defined. May only be defined in a config file.
- `:exclude`: A list of line-separated Rsync pattern strings, indicating which files or directories should be excluded while publishing the site. This can be helpful when there are files on the server that you don’t want to be touched.
- `:extension`: The file extension that will be used for the affected pages, when they are generated.
- `:highlight`: `true` or `false` -- whether or not Pandoc will highlight code blocks that have a language specified. The highlighting will only be visible with an appropriate CSS file. See the [example code highlighting CSS file](https://github.com/AlexCharlton/site-generator/tree/master/examples/code-highlight.css). Defaults to `true`.
- `:languages`: A space or comma separated list of language codes. A site for every language code listed will be generated. Defaults to `en`. May only be defined in the top-level config file.
- `:markup`: The name that represents the type of markup that the affected content should be interpreted as. Any value that [Pandoc understands](http://johnmacfarlane.net/pandoc/README.html) is permissible. For `markdown`, extensions can be added and removed with + and - (see the Pandoc README). Defaults to `none`.
- `number-sections`: `true` or `false` -- number section headings. Defaults to `false`.
- `:output-format`: The name that represents the desired output format of the affected, marked up content. May be any output format that Pandoc understands, but only some will be useful. Defaults to `html5`.
- `:pages-as-directories`: `true` or `false` -- whether or not to output a page to an `index.html` file in the directory that represents the page's name, thus creating "pretty" URLs. Defaults to `true`. May only be defined in the top-level config file.
- `:server`: The string understood by Rsync that represents the `username@server-address:directory` to which the site will be uploaded when *site-generator* is passed the `--publish` flag.
- `:slug`: The string that will be used to represent the URL of the page of the content file in which `slug` was defined. May only be defined in a content file.
- `:smart`: `true` or `false` -- whether or not Pandoc will create typographically correct output. Defaults to `true`.
- `:template`: The file name (relative to the template directory) of the template that will be used for the affected files.
- `:toc`: `true` or `false` -- whether or not to generate a table of contents with the Pandoc output. The resulting table of contents can be accessed in the content that was processed to create the table using the special symbol `{{{toc}}}` (which is impotent when placed between `pre` tags). The variable `toc` will also be set to the resulting table of contents, but this variable will only be bound after the content has been processed, making this of limited use if the table of contents is to go before the content. Defaults to `false`.
- `:toc-depth`: The number of sections levels that will be included in the table of contents. Defaults to `3`.
- `:use`: The space or comma separated list of Lisp packages to `use` in the generation environment. Defaults to `cl site-generator cl-who`.

Additionally, the variables `lang` and `current-file` are reserved by *site-generator* and may not be defined in any content file.

## Appendix B -- Supplied functions
On top of the functions supplied by [Common Lisp](http://www.lispworks.com/documentation/HyperSpec/Front/) and [CL-WHO](http://weitz.de/cl-who/), the following functions are available when writing template and content files:

- `(bound? VARIABLE)`: Return the value of `VARIABLE` if the variable is bound, and `nil` otherwise.
- `(build-time)`: Return the RFC 3339 formatted time string of the time that this function is called.
- `(echo &rest STRINGS)`: Combine the list of `STRINGS` into one string.
- `(first-line STRING)`: Return the first line of the `STRING`.
- `(get-content PAGE CONTENT-VARIABLE)`: Get the value of the `CONTENT-VARIABLE` (in keyword form, so `title` would be `:title`) from the `PAGE` (relative to the content directory).
- `(get-pages DIRECTORY &key NUMBER START ORDER)`: Return the date sorted pages that are present in `DIRECTORY`, `ORDER`ed by `:descending` or `:ascending`. `NUMBER`, if supplied, limits the number of pages returned. `START` offsets the start of the returned list by the given number. E.g. `(getpages "foo" :number 3 :start 5)` returns the path names of the 6th, 7th, and 8th pages from the directory `content/foo/`.
- `(include TEMPLATE)`: Reads the file `TEMPLATE` (relative to the template directory) and returns its contents.
- `(join-strings SEPARATOR &rest STRINGS)`: Joins the `STRINGS` together with `SEPARATOR` in between. E.g. `(join-strings " " "foo" "bar" "baz")` results in `"foo bar baz"`.
- `(lines STRING)`: Returns a list of strings that correspond to the lines in STRING, with empty lines removed.
- `(markup CONTENT &rest ARGS)`: Markup the content with Pandoc. While this function will default to whatever Pandoc configuration has been set for the current environment, additional `ARGS` can be supplied to override these defaults. E.g. `(markup thing :markup :restructuredtext)` will markup the thing as reStructuredText.
- `(next-page PAGE-DIRECTORY &optional PAGE)`: Return the string representing the path of the content file (relative to the content-directory) that chronologically follows the current page (or `PAGE`) and that is located in `PAGE-DIRECTORY`.
- `(other-languages &key UL-CLASS SELECTED-CLASS)`: Produces an unordered list of the links to the pages corresponding to the current page, but in different languages. `UL-CLASS` will modify the class that the unordered list possess (defaults to `"languages"`), and `SELECTED-CLASS` will modify the class of the list element corresponding to the language of the current page (defaults to `"current-language"`).
- `(page-address PAGE &key LANGUAGE)`: Return the string corresponding to the domain-relative address of the content file `PAGE` (relative to the content directory) for `LANGUAGE` (which defaults to the current language). A word of warning: This does not track the `PAGE` for changes. This means that if the `PAGE` changes address, the page that called `page-address` will not be updated with that new address. When changing page addresses, it's therefore a good idea to regenerate the whole site to be safe.
- `(page-author PAGE &key LANGUAGE) `: Return the string corresponding to the author of the content file `PAGE` (relative to the content directory) for `LANGUAGE` (which defaults to the current language)
- `(page-date PAGE &key LANGUAGE FORMAT)`: Return the string corresponding to the formatted date of the content file `PAGE` (which may be a string represeding the path of the page relative to the content directory, or the keyword `:current`). `LANGUAGE` has no effect on this function. For a full description of the `FORMAT` variable, see the [Dates](#dates) section.
- `(page-last-modified PAGE &key LANGUAGE FORMAT)`: Return the string corresponding to the formatted date of the last modification time of the content file `PAGE` (relative to the content directory). `LANGUAGE` has no effect on this function. For a full description of the `FORMAT` variable, see the [Dates](#dates) section.
- `(page-title PAGE &key LANGUAGE)`: Return the string corresponding to the title of the content file `PAGE` (relative to the content directory) for `LANGUAGE` (which defaults to the current language)
- `(prev-page PAGE-DIRECTORY &optional PAGE)`: Return the string representing the path of the content file (relative to the content-directory) that chronologically precedes the current page (or `PAGE`) and that is located in `PAGE-DIRECTORY`.
- `(static-files DIRECTORY)`: Return a list strings representing all of the files (and directories) in the given `DIRECTORY`, relative to the static directory.
- `(words STRING)`: Return the list of strings that are separated by whitespace in `STRING`.
- `(xml EXPRESSION)`: A macro wrapping CL-WHO's `WITH-HTML-OUTPUT-TO-STRING`, so that only the XML content is required, and the appropriate prologue is output when the leading keyword is `:HTML` or `RSS`.

## Appendix C -- Pandoc configuration variables
These are both arguments that can be appended to variable definition (e.g. `:var smart=true`) or can be defined on their own (with two separate lines). They are explained in more detail in appendix A.

- `additional-pandoc-args`
- `highlight`
- `markup`
- `number-sections`
- `output-format`
- `smart`
- `toc`
- `toc-depth`

## Appendix D -- *site-generator* syntax
Templates contain `$template-variables$` and `$(template expressions)`. When templates are expanded, these are replaced with the value of the variable or the result of the expression. Templates are located in the `template/` directory.

Content files define configuration variables and content variables. The list of configuration variables can be seen in [appendix A](#appendix-a-configuration-variables). Variables are defined by starting a line with the name of the variable, prepended by a colon. Variables must be preceded by a blank line. Content variables can have additional arguments, appearing on the same line as the variable, in the form of key-value pairs separated by equal signs. E.g.:

```
:template
main.html

:content-variable lang=en markup=markdown
This is some content that is assigned to the variable
content-variable.

This is more of the same content
```

Lines that begin with a semi-colon are ignored. This includes lines that would otherwise be part of content. If you want some content to begin with a semi-colon, just add a space before it. E.g.:

```
;; This is a site generator file
:content-variable lang=en markup=markdown
This is some content that is assigned to the variable
content-variable.
; This will not show up in content-variable

 ; This will show up in content-variable
```


`nil` is a special content value, that will set the value of the content to Common Lisp's `nil`, e.g.:

```
:some-content
nil

:other-content
$(when (bound? some-content)
   "Even though some-content is bound, this will still
    never be printed, because some-content is set to nil"
```

Content files are located in the `content/` directory. They may possess any name except for those starting with a `.` (i.e. hidden files) or bracketed by `#` symbols (i.e. Emacs auto-save files).

## Appendix E -- Changelog
### v0.8.0
- Add `static-files`

### v0.7.0
- Add ability to exclude files/directories while publishing

### v0.6.0
- Commands are run asynchronously

### v0.5.0
- Test server listens to 0.0.0.0 rather than 127.0.0.1

### v0.4.0
- Config files can now contain comments

### v0.3.0
- Make sure `static` directory stays current
- Fix bug with time setting

### v0.2.0
- Add support for moving and deleting content files
- Bugfixes

### v0.1.0
- Initial release
