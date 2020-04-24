{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighterSpec (spec) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.SyntaxHighlighter
import Test.Hspec

spec :: Spec
spec = describe "tokenizeHaskell" $ do
  it "parses a basic module" (withFile "data/Main.hs" basicModule)
  it "parses a type family" (withFile "data/TypeFamily.hs" typeFamily)
  it "parses an explicit forall" (withFile "data/Forall.hs" explicitForall)
  it "parses lambda case correctly" (withFile "data/LambdaCase.hs" lambdaCase)
  it "parses file header pragmas correctly" (withFile "data/Pragmas.hs" pragmas)

withFile :: FilePath -> [(Token, Text)] -> Expectation
withFile path toks = do
  txt <- T.readFile path
  tokenizeHaskell txt `shouldBe` Just toks

----------------------------------------------------------------------------
-- Expected token streams

basicModule :: [(Token, Text)]
basicModule =
  [ (KeywordTok, "module"),
    (SpaceTok, " "),
    (ConstructorTok, "Main"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (VariableTok, "main"),
    (SymbolTok, ")"),
    (SpaceTok, " "),
    (KeywordTok, "where"),
    (SpaceTok, "\n\n"),
    (KeywordTok, "import"),
    (SpaceTok, " "),
    (ConstructorTok, "Data.Bits"),
    (SpaceTok, "\n\n"),
    (CommentTok, "-- | Program's entry point."),
    (SpaceTok, "\n"),
    (VariableTok, "main"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (ConstructorTok, "IO"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (SymbolTok, ")"),
    (SpaceTok, "\n"),
    (VariableTok, "main"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (VariableTok, "return"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (SymbolTok, ")"),
    (SpaceTok, "\n")
  ]

typeFamily :: [(Token, Text)]
typeFamily =
  [ (KeywordTok, "type"),
    (SpaceTok, " "),
    (KeywordTok, "family"),
    (SpaceTok, " "),
    (ConstructorTok, "All"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (VariableTok, "c"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (VariableTok, "k"),
    (SpaceTok, " "),
    (SymbolTok, "->"),
    (SpaceTok, " "),
    (ConstructorTok, "Constraint"),
    (SymbolTok, ")"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (VariableTok, "xs"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (SymbolTok, "["),
    (VariableTok, "k"),
    (SymbolTok, "]"),
    (SymbolTok, ")"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (ConstructorTok, "Constraint"),
    (SpaceTok, " "),
    (KeywordTok, "where"),
    (SpaceTok, "\n  "),
    (ConstructorTok, "All"),
    (SpaceTok, " "),
    (VariableTok, "c"),
    (SpaceTok, " "),
    (SymbolTok, "'"),
    (SymbolTok, "["),
    (SymbolTok, "]"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (SymbolTok, ")"),
    (SpaceTok, "\n  "),
    (ConstructorTok, "All"),
    (SpaceTok, " "),
    (VariableTok, "c"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (VariableTok, "x"),
    (SpaceTok, " "),
    (SymbolTok, "'"),
    (SymbolTok, ":"),
    (SpaceTok, " "),
    (VariableTok, "xs"),
    (SymbolTok, ")"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (VariableTok, "c"),
    (SpaceTok, " "),
    (VariableTok, "x"),
    (SymbolTok, ","),
    (SpaceTok, " "),
    (ConstructorTok, "All"),
    (SpaceTok, " "),
    (VariableTok, "c"),
    (SpaceTok, " "),
    (VariableTok, "xs"),
    (SymbolTok, ")"),
    (SpaceTok, "\n")
  ]

explicitForall :: [(Token, Text)]
explicitForall =
  [ (VariableTok, "x"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (KeywordTok, "forall"),
    (SpaceTok, " "),
    (VariableTok, "a"),
    (OperatorTok, "."),
    (SpaceTok, " "),
    (VariableTok, "a"),
    (SpaceTok, "\n"),
    (VariableTok, "x"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (VariableTok, "undefined"),
    (SpaceTok, "\n")
  ]

lambdaCase :: [(Token, Text)]
lambdaCase =
  [ (VariableTok, "foo"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (ConstructorTok, "Foo"),
    (SpaceTok, " "),
    (SymbolTok, "->"),
    (SpaceTok, " "),
    (ConstructorTok, "Maybe"),
    (SpaceTok, " "),
    (ConstructorTok, "Bar"),
    (SpaceTok, "\n"),
    (VariableTok, "foo"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (SymbolTok, "\\"),
    (SymbolTok, "case"),
    (SpaceTok, "\n  "),
    (ConstructorTok, "Foo"),
    (SpaceTok, " "),
    (SymbolTok, "->"),
    (SpaceTok, " "),
    (ConstructorTok, "Just"),
    (SpaceTok, " "),
    (ConstructorTok, "Bar"),
    (SpaceTok, "\n  "),
    (SymbolTok, "_"),
    (SpaceTok, " "),
    (SymbolTok, "->"),
    (SpaceTok, " "),
    (ConstructorTok, "Nothing"),
    (SpaceTok, "\n")
  ]

pragmas :: [(Token, Text)]
pragmas =
  [ (PragmaTok, "{-# LANGUAGE OverloadedStrings #-}"),
    (SpaceTok, "\n"),
    (PragmaTok, "{-# OPTIONS_GHC -fno-warn-unused-matches #-}"),
    (SpaceTok, "\n\n"),
    (VariableTok, "main"),
    (SpaceTok, " "),
    (SymbolTok, "::"),
    (SpaceTok, " "),
    (ConstructorTok, "IO"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (SymbolTok, ")"),
    (SpaceTok, "\n"),
    (VariableTok, "main"),
    (SpaceTok, " "),
    (SymbolTok, "="),
    (SpaceTok, " "),
    (VariableTok, "return"),
    (SpaceTok, " "),
    (SymbolTok, "("),
    (SymbolTok, ")"),
    (SpaceTok, "\n")
  ]
