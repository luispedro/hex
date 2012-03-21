===================
HeX: TeX in Haskell
===================

HeX is a reimplementation of TeX in Haskell.

It is currently pre-pre-alpha: it doesn't do much.

The plan is to first write a faithful reimplementation of TeX and then, *make
it better*.

What needs to be better?
------------------------

Here are my ideas:

1. Better error messages.
2. Better package management.
3. Document at-once-typesetting (instead of page-per-page).
4. Single pass implementation of cross-references.
5. Better handling of floats.
6. Better handling of images.
7. Faster.
8. Better scripting.
9. In-built (or script written) facilities for things like references (instead
   of relying on external programmes).
10. Use of multi-core machines.
11. More output formats (ePub, HTML, ...?)

Some of these are traditionally built *on top* of TeX instead of modifying it.
While that is a good way to do some things, it also means that the results are
not as good as they should be. There is no reason to not have the handling of
floats at the TeX level, for example.

What needs to stay the same?
----------------------------

Good quality output.

What can change?
----------------

The console output. Coming from a Unix background, I find TeX unbearably
verbose. If no errors are found, it should output *nothing*. Also, there might
be no functionality for interactive use when an error occurs.

Files that trigger an error in TeX will not necessarily work the same way in
hex.

Dependencies
------------

cabal install \
            cmdargs \
            list-extras \
            vector \
            vector-algorithms \
            binary \
            convertible \
            parsec \
            dlist \
            test-framework-th \
            test-framework-quickcheck2 \
            test-framework-hunit

Meta-Information
----------------

Author: Luis Pedro Coelho <luis@luispedro.org>

License: GPLv3 or later

Website: http://luispedro.org/software/hex
