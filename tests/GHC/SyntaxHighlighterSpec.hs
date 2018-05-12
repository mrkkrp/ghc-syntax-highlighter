{-# LANGUAGE OverloadedStrings #-}

module GHC.SyntaxHighlighterSpec
  ( spec )
where

import GHC.SyntaxHighlighter
import Test.Hspec
import qualified Data.Text.IO as T

spec :: Spec
spec = describe "tokenizeHaskell" $
  it "works" $ do
    txt <- T.readFile "data/Main.hs"
    tokenizeHaskell txt `shouldBe` Just
      [ (KeywordTok,"module")
      , (SpaceTok," ")
      , (ConstructorTok,"Main")
      , (SpaceTok," ")
      , (SymbolTok,"(")
      , (VariableTok,"main")
      , (SymbolTok,")")
      , (SpaceTok," ")
      , (KeywordTok,"where")
      , (SpaceTok,"\n\n")
      , (KeywordTok,"import")
      , (SpaceTok," ")
      , (ConstructorTok,"Data.Bits")
      , (SpaceTok,"\n\n")
      , (CommentTok,"-- | Program's entry point.")
      , (SpaceTok,"\n\n")
      , (VariableTok,"main")
      , (SpaceTok," ")
      , (SymbolTok,"::")
      , (SpaceTok," ")
      , (ConstructorTok,"IO")
      , (SpaceTok," ")
      , (SymbolTok,"(")
      , (SymbolTok,")")
      , (SpaceTok,"\n")
      , (VariableTok,"main")
      , (SpaceTok," ")
      , (SymbolTok,"=")
      , (SpaceTok," ")
      , (VariableTok,"return")
      , (SpaceTok," ")
      , (SymbolTok,"(")
      , (SymbolTok,")")
      , (SpaceTok,"\n")
      ]
