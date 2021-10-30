## GHC syntax highlighter 0.0.8.0

* Uses `ghc-lib-parser-9.2.1.x`.

## GHC syntax highlighter 0.0.7.0

* Uses `ghc-lib-parser-9.0.1.x`.

## GHC syntax highlighter 0.0.6.0

* Uses `ghc-lib-parser-8.10.x`.

## GHC syntax highlighter 0.0.5.0

* Uses `ghc-lib-parser`.

## GHC syntax highlighter 0.0.4.1

* Works with GHC 8.8.

* Dropped support for GHC 8.2.

## GHC syntax highlighter 0.0.4.0

* Implemented highlighting of file header pragmas such as `OPTIONS_GHC` and
  `LANGUAGE`. They are not handled by the GHC lexer, so custom code were
  added for this purpose.

## GHC syntax highlighter 0.0.3.1

* Fixed the bug when certain extensions such as `-XLambdaCase` were not
  enabled when the code was compiled with GHC 8.6.

## GHC syntax highlighter 0.0.3.0

* Compiles with GHC 8.6.

## GHC syntax highlighter 0.0.2.0

* Added `Loc` and `tokenizeHaskellLoc`.

## GHC syntax highlighter 0.0.1.0

* Initial release.
