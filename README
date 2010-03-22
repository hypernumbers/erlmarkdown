DEVELOPER NOTES
===============

Erlmarkdown uses generated tests. There are 2 levels of generation:
`./tests/index.html` uses `generate_tests.js` to take a set of input test cases
and output an EUnit test file for Erlang.

Some of the tests in `generate_tests.js` are hand written and some have been generated from the MarkdownSharp test suites.

The MarkdownSharp test suites were originally  downloaded from:
`http://code.google.com/p/markdownsharp/`

They can be found in `./tests/markdowndownsharp/`

The code that generates tests from them in in the escript file:
`./priv/make_tests`


Release Notes
=============

Version 1.1.12 Production Bug Fix
---------------------------------

Bug fix for "<>"

Version 1.1.11 Production Bug Fix
---------------------------------

Bug Fix for tags with a couple more shonky unfixed whitespace bugs


Version 1.1.10 Production Bug Fix
---------------------------------

Shonk Alert! A couple of whitespace bugs remain unfixed

Version 1.1.9 Production Bug Fix
--------------------------------

Wasn't handling special white space inserted by the ***PARSER*** (don't ask!)

Version 1.1.8 Production Bug Fix
--------------------------------

Some blocktag/html errrors fixed

Version 1.1.7 Production Bug Fix
--------------------------------

URl's were not html encoded.

Failing tests of 1.1.5 not addressed in this release

Version 1.1.6 UTF8 Support
--------------------------

Can now specify a UTF 8 entry point - this addresses the non-breaking space/ascii 160 problem of Version 1.1.3

Failing tests of 1.1.5 not addressed in this release

Version 1.1.5 (Interim)
-----------------------

Integration of additional tests from markdownsharp:
http://code.google.com/p/markdownsharp/source/browse/trunk/MarkdownSharpTests/#MarkdownSharpTests/testfiles%3Fstate%3Dclosed

17 currently failing which is why this is an interim release

Version 1.1.4 Production Bug Fix
--------------------------------

Fixes 4 sets of bugs:
* you can now put an image inside an href 
  ` (eg [![alt](/img/some.png)](http://example.com/dir/)`
* text in ordered and unordered lists now renders correctly
  eg bold and italic etc
* the 'you can't have 2 code segments in a single line' bug is fixed

Version 1.1.3 Production Bug Fix
--------------------------------

Under certain circumstances non-breaking spaces (ascii 160) could wedge
the server.

fuzz.erl rejigged to make for better diagnostics and run with 1,000,000 random
characters against markdown.

***CAVEAT***: having serious doubts as to how erlmarkdown will handle unicode

Version 1.1.2 Production Bug Fix
--------------------------------

A number of failures have been noticed in production of bits of markdown
failing to convert.

A fuzz generator has been added to produce large amounts or random characters
with particular emphasis on characters that are 'structural' in markdown.
50,000 random character strings found 3 or 4 new bugs which have been fixed
in this release.

Version 1.1.1 Production Bug Fix
--------------------------------

Fixed a bug involving underscores/formatting characters inside inline
references and images

Version 1.1 Production Bug Fix
------------------------------

In the beginning erlmarkdown was an implementation of markdown written to spec
- the spec being the daring-fireball syntax page.

BUT it soon transpired that there were significant differences between *our*
implementation of the spec and that of showdown (from attacklabs.net). Given
that the client-side markdown and the server-side markdown need to work together
we switched the erlang code from being an implementation of the spec to being
a server-side implementation of showdown.

BUT the WMD dialog box produces (as input) code which is 'off spec' (but which
showdown supports), **so**...

Version 1.1 is tested for compatibility with:
* showdown
* the markdown input produced by the WMD dialog box editor

We are tracking the release of WMD **and** showdown from this version on github:
<http://github.com/derobins/wmd/commit/980f68797307d28f0541868c740974cb2eeb1209>

We are no longer tracking the showdown code on attacklabs.net

As a result of this is that the list of ERATTA AND KNOWN BUGS is a bit longer

Version 1.0 Production Release
------------------------------

This is a major rewrite. There is little point writing servers-side markdown
without a client-side markdown to preview it.

This markdown will now track showdown from Attack Labs as its twin 
implementation.

<http://attacklab.net/showdown/>

To that end the test suite has been repurposed to test
compatibility with showdown. The current release of showdown used is V0.9

The directory `/tests` now contains an webpage `index.html` which generates
the tests (ie the file `markdown_tests.erl` in `/src`).

`index.html` loads a javascript file `generate_tests.js` which holds a list of 
strings which it generates the tests from (ie testing if markdown produces the
same output as showdown...)

There are a number of places where markdown is not *whitespace compatible* with showdown - you can inspect these by looking for commented out tests in 
`generate_tests.js`. There are also a number of showdown bugs or other 
inconsistencies which also show up as commented out tests in 
`generate_tests.js`.

The biggest single difference is that showdown doesn't escape any html tags
by default whereas markdown has a whitelist of blocklevel tags which it doesn't
escape (see the function `markdown:is_block_tag/1`).

(Other) Known Bugs and Errata:
as per the previous versions (see bottom of document)

There are 261 Unit Tests.

Version 1.0 RC1.1 First Point Release
-------------------------------------

Various bug fixes:
* supports short inline URL references of the form 'some text [id] some more'
* fixes bugs with lines containing markdown only like '#', '>', '=' and '-'
* fixes a bug where a terminating '\n\n' would mean each line came back reversed
* fixes various 'atx header' bugs
* fixes bugs where lines start with an emphasis or strong marker (thanks to Tom McNulty)

Unit tests bumped up to 255.

Version 1.0 RC1 Production Release Candidate
--------------------------------------------

This version comes with 226 Unit Tests and is being released for pre-production testing.

It also has 1 System Test - this document :)

Known Bugs And Errata
=====================

1  This is a valid markdown for references
------------------------------------------

    [id]: http://sub.dom.tld:1234/some/path\\t\\n(Title)

The title wrap to a new line is not supported in this release.

2 Character Escaping
--------------------

I'm not entirely clear how character escaping should work for:

    \   backslash
    \`   backtick
    *   asterisk
    _   underscore
    {}  curly braces
    []  square brackets
    ()  parentheses
    #   hash mark
    +   plus sign
    -   minus sign (hyphen)
    .   dot
    !   exclamation mark

I escape them only in context.

3 Anti-spam mailto's
--------------------

The official release obfuscated e-mail addresses, this don't :(

4 Nesting
---------

Showdown supports nested <ul>'s, <ol>'s and <blockquote>'s - erlmarkdown doesnt (yet)...

What It Is
==========

This is an Erlang implementation of Markdown.

See http://daringfireball.net/projects/markdown/ for details

License
=======

The license is the same as the original Markdown


Musical Colophon
----------------

'My Song' when I wrote this was:
<http://open.spotify.com/track/6mqdunuFFSODHKcpDTFvAj>

'Generic Song' to play when you find a bug in my code:
<http://open.spotify.com/track/5YLa8jWik5OqgBoSix3NUp>

'Generic Song' about how I feel about myself when you find a bug:
<http://open.spotify.com/track/2RbJj7D5pRff82NtDDSwah>