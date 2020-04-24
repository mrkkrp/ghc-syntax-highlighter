# GHC syntax highligher

[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-2-Clause)
[![Hackage](https://img.shields.io/hackage/v/ghc-syntax-highlighter.svg?style=flat)](https://hackage.haskell.org/package/ghc-syntax-highlighter)
[![Stackage Nightly](http://stackage.org/package/ghc-syntax-highlighter/badge/nightly)](http://stackage.org/nightly/package/ghc-syntax-highlighter)
[![Stackage LTS](http://stackage.org/package/ghc-syntax-highlighter/badge/lts)](http://stackage.org/lts/package/ghc-syntax-highlighter)
[![Build Status](https://travis-ci.org/mrkkrp/ghc-syntax-highlighter.svg?branch=master)](https://travis-ci.org/mrkkrp/ghc-syntax-highlighter)

This is a syntax highlighter library for Haskell using lexer of GHC itself.

Here is a blog post announcing the package, the readme is mostly derived
from it:

* https://markkarpov.com/post/announcing-ghc-syntax-highlighter.html

## Motivation

Parsing Haskell is hard, because Haskell is a complex language with
countless features. The only way to get it right 100% is to use parser of
GHC itself. Fortunately, now there is the [`ghc`][ghc] package, which as of
version 8.4.1 exports enough of GHC's source code to allow us use its lexer.

Alternative approaches, even decent ones like [`highlight.js`][hljs] either
don't support cutting-edge features or do their work without sufficient
precision so that many tokens end up combined and the end result is
typically still hard to read.

## How to use it in your blog

Depends on your markdown processor. If you're an [`mmark`][mmark] user, good
news, since version 0.2.1.0 of [`mmark-ext`][mmark-ext] it includes the
`ghcSyntaxHighlighter` extension. Due to flexibility of MMark, it's possible
to use this highlighter for Haskell and [`skylighting`][skylighting] as a
fall-back for everything else. Consult [the docs][mmark-ext-docs] for more
information.

[skylighting][skylighting] is what Pandoc uses. And from what I can tell
it's hardcoded to use only that library for highlighting, so some creativity
may be necessary to get it work.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/ghc-syntax-highlighter/issues).

Pull requests are also welcome.

## License

Copyright © 2018–present Mark Karpov

Distributed under BSD 3 clause license.

[ghc]: https://hackage.haskell.org/package/ghc
[hljs]: https://highlightjs.org/
[mmark]: https://hackage.haskell.org/package/mmark
[mmark-ext]: https://hackage.haskell.org/package/mmark-ext
[skylighting]: https://hackage.haskell.org/package/skylighting
[mmark-ext-docs]: https://hackage.haskell.org/package/mmark-ext/docs/Text-MMark-Extension-GhcSyntaxHighlighter.html
