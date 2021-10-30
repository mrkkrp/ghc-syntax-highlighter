{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- |
-- Module      :  GHC.SyntaxHighlighter
-- Copyright   :  © 2018–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module allows you to decompose a 'Text' stream containing Haskell
-- source code into a stream of 'Text' chunks tagged with 'Token'.
--
-- This library uses the GHC's lexer, so the result is guaranteed to be 100%
-- correct, as if it were parsed by GHC itself.
module GHC.SyntaxHighlighter
  ( Token (..),
    Loc (..),
    tokenizeHaskell,
    tokenizeHaskellLoc,
  )
where

import Control.Monad
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as ES
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer
import GHC.LanguageExtensions
import qualified GHC.Parser.Lexer as L
import GHC.Types.SrcLoc

----------------------------------------------------------------------------
-- Data types

-- | Token types that are used as tags to mark spans of source code.
data Token
  = -- | Keyword
    KeywordTok
  | -- | Pragmas
    PragmaTok
  | -- | Symbols (punctuation that is not an operator)
    SymbolTok
  | -- | Variable name (term level)
    VariableTok
  | -- | Data\/type constructor
    ConstructorTok
  | -- | Operator
    OperatorTok
  | -- | Character
    CharTok
  | -- | String
    StringTok
  | -- | Integer
    IntegerTok
  | -- | Rational number
    RationalTok
  | -- | Comment (including Haddocks)
    CommentTok
  | -- | Space filling
    SpaceTok
  | -- | Something else?
    OtherTok
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The start and end positions of a span. The arguments of the data
-- constructor contain in order:
--
--     * Line number of start position of a span
--     * Column number of start position of a span
--     * Line number of end position of a span
--     * Column number of end position of a span
--
-- @since 0.0.2.0
data Loc = Loc !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------
-- High-level API

-- | Tokenize Haskell source code. If the code cannot be parsed, return
-- 'Nothing'. Otherwise return the original input tagged by 'Token's.
-- 'Nothing' is rarely returned, if ever, because it looks like the lexer is
-- capable of interpreting almost any text as a stream of GHC tokens.
--
-- The parser does not require the input source code to form a valid Haskell
-- program, so as long as the lexer can decompose your input (most of the
-- time), it'll return something in 'Just'.
tokenizeHaskell :: Text -> Maybe [(Token, Text)]
tokenizeHaskell input = sliceInputStream input <$> tokenizeHaskellLoc input

-- | Replace 'Loc' locations with actual chunks of input 'Text'.
sliceInputStream :: Text -> [(Token, Loc)] -> [(Token, Text)]
sliceInputStream input toks = unfoldr sliceOnce (initText' input, toks)
  where
    sliceOnce (txt, []) = do
      (txt', chunk) <- tryFetchRest txt
      return ((SpaceTok, chunk), (txt', []))
    sliceOnce (txt, tss@((t, l) : ts)) =
      case tryFetchSpace txt l of
        Nothing ->
          let (txt', chunk) = fetchSpan txt l
              t' = case t of
                CommentTok ->
                  if isHeaderPragma chunk
                    then PragmaTok
                    else CommentTok
                tok -> tok
           in Just ((t', chunk), (txt', ts))
        Just (txt', chunk) ->
          Just ((SpaceTok, chunk), (txt', tss))

-- | Similar to 'tokenizeHaskell', but instead of 'Text' chunks provides
-- locations of corresponding spans in the given input stream.
--
-- @since 0.0.2.0
tokenizeHaskellLoc :: Text -> Maybe [(Token, Loc)]
tokenizeHaskellLoc input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer (T.unpack input)
    parseState = L.initParserState parserOpts buffer location
    parserOpts =
      L.mkParserOpts
        ES.empty
        (ES.fromList enabledExts)
        True -- safe imports
        True -- keep Haddock tokens
        True -- keep comment tokens
        False -- lex LINE and COLUMN pragmas

-- | The Haskell lexer.
pLexer :: L.P [(Token, Loc)]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case r of
        L _ L.ITeof -> return []
        _ ->
          case fixupToken r of
            Nothing -> go
            Just x -> (x :) <$> go

-- | Convert @'Located' 'L.Token'@ representation into a more convenient for
-- us form.
fixupToken :: Located L.Token -> Maybe (Token, Loc)
fixupToken (L srcSpan tok) = (classifyToken tok,) <$> srcSpanToLoc srcSpan

-- | Convert 'SrcSpan' into 'Loc'.
srcSpanToLoc :: SrcSpan -> Maybe Loc
srcSpanToLoc (RealSrcSpan s _) =
  let srcSpanSLine = srcSpanStartLine s
      srcSpanSCol = srcSpanStartCol s
      srcSpanELine = srcSpanEndLine s
      srcSpanECol = srcSpanEndCol s
      start = (srcSpanSLine, srcSpanSCol)
      end = (srcSpanELine, srcSpanECol)
   in if start == end
        then Nothing -- NOTE Some magic auto-generated tokens that do not
        -- actually appear in the input stream. Drop them.
        else
          Just $
            Loc
              srcSpanSLine
              srcSpanSCol
              srcSpanELine
              srcSpanECol
srcSpanToLoc _ = Nothing

-- | Classify a 'L.Token' in terms of 'Token'.
classifyToken :: L.Token -> Token
classifyToken = \case
  -- Keywords
  L.ITas -> KeywordTok
  L.ITcase -> KeywordTok
  L.ITclass -> KeywordTok
  L.ITdata -> KeywordTok
  L.ITdefault -> KeywordTok
  L.ITderiving -> KeywordTok
  L.ITdo _ -> KeywordTok
  L.ITelse -> KeywordTok
  L.IThiding -> KeywordTok
  L.ITforeign -> KeywordTok
  L.ITif -> KeywordTok
  L.ITimport -> KeywordTok
  L.ITin -> KeywordTok
  L.ITinfix -> KeywordTok
  L.ITinfixl -> KeywordTok
  L.ITinfixr -> KeywordTok
  L.ITinstance -> KeywordTok
  L.ITlet -> KeywordTok
  L.ITmodule -> KeywordTok
  L.ITnewtype -> KeywordTok
  L.ITof -> KeywordTok
  L.ITqualified -> KeywordTok
  L.ITthen -> KeywordTok
  L.ITtype -> KeywordTok
  L.ITwhere -> KeywordTok
  L.ITforall _ -> KeywordTok
  L.ITexport -> KeywordTok
  L.ITlabel -> KeywordTok
  L.ITdynamic -> KeywordTok
  L.ITsafe -> KeywordTok
  L.ITinterruptible -> KeywordTok
  L.ITunsafe -> KeywordTok
  L.ITstdcallconv -> KeywordTok
  L.ITccallconv -> KeywordTok
  L.ITcapiconv -> KeywordTok
  L.ITprimcallconv -> KeywordTok
  L.ITjavascriptcallconv -> KeywordTok
  L.ITmdo _ -> KeywordTok
  L.ITfamily -> KeywordTok
  L.ITrole -> KeywordTok
  L.ITgroup -> KeywordTok
  L.ITby -> KeywordTok
  L.ITusing -> KeywordTok
  L.ITpattern -> KeywordTok
  L.ITstatic -> KeywordTok
  L.ITstock -> KeywordTok
  L.ITanyclass -> KeywordTok
  L.ITvia -> KeywordTok
  L.ITunit -> KeywordTok
  L.ITsignature -> KeywordTok
  L.ITdependency -> KeywordTok
  L.ITrequires -> KeywordTok
  -- Pragmas
  L.ITinline_prag {} -> PragmaTok
  L.ITspec_prag _ -> PragmaTok
  L.ITspec_inline_prag {} -> PragmaTok
  L.ITsource_prag _ -> PragmaTok
  L.ITrules_prag _ -> PragmaTok
  L.ITwarning_prag _ -> PragmaTok
  L.ITdeprecated_prag _ -> PragmaTok
  L.ITline_prag _ -> PragmaTok
  L.ITcolumn_prag _ -> PragmaTok
  L.ITscc_prag _ -> PragmaTok
  L.ITunpack_prag _ -> PragmaTok
  L.ITnounpack_prag _ -> PragmaTok
  L.ITann_prag _ -> PragmaTok
  L.ITcomplete_prag _ -> PragmaTok
  L.ITclose_prag -> PragmaTok
  L.IToptions_prag _ -> PragmaTok
  L.ITinclude_prag _ -> PragmaTok
  L.ITlanguage_prag -> PragmaTok
  L.ITminimal_prag _ -> PragmaTok
  L.IToverlappable_prag _ -> PragmaTok
  L.IToverlapping_prag _ -> PragmaTok
  L.IToverlaps_prag _ -> PragmaTok
  L.ITincoherent_prag _ -> PragmaTok
  L.ITctype _ -> PragmaTok
  L.ITcomment_line_prag -> PragmaTok
  -- Reserved symbols
  L.ITdotdot -> SymbolTok
  L.ITcolon -> SymbolTok
  L.ITdcolon _ -> SymbolTok
  L.ITequal -> SymbolTok
  L.ITlam -> SymbolTok
  L.ITlcase -> SymbolTok
  L.ITvbar -> SymbolTok
  L.ITlarrow _ -> SymbolTok
  L.ITrarrow _ -> SymbolTok
  L.ITlolly -> SymbolTok
  L.ITat -> SymbolTok
  L.ITtilde -> SymbolTok
  L.ITdarrow _ -> SymbolTok
  L.ITbang -> SymbolTok
  L.ITstar _ -> SymbolTok
  L.ITbiglam -> SymbolTok
  L.ITocurly -> SymbolTok
  L.ITccurly -> SymbolTok
  L.ITvocurly -> SymbolTok
  L.ITvccurly -> SymbolTok
  L.ITobrack -> SymbolTok
  L.ITopabrack -> SymbolTok
  L.ITcpabrack -> SymbolTok
  L.ITcbrack -> SymbolTok
  L.IToparen -> SymbolTok
  L.ITcparen -> SymbolTok
  L.IToubxparen -> SymbolTok
  L.ITcubxparen -> SymbolTok
  L.ITsemi -> SymbolTok
  L.ITcomma -> SymbolTok
  L.ITunderscore -> SymbolTok
  L.ITbackquote -> SymbolTok
  L.ITsimpleQuote -> SymbolTok
  L.ITpercent -> SymbolTok
  L.ITproj _ -> SymbolTok
  -- NOTE GHC thinks these are reserved symbols, but I classify them as
  -- operators.
  L.ITminus -> OperatorTok
  L.ITprefixminus -> OperatorTok
  L.ITdot -> OperatorTok
  -- Identifiers
  L.ITvarid _ -> VariableTok
  L.ITconid _ -> ConstructorTok
  L.ITvarsym _ -> OperatorTok
  L.ITconsym _ -> OperatorTok
  L.ITqvarid _ -> VariableTok
  L.ITqconid _ -> ConstructorTok
  L.ITqvarsym _ -> OperatorTok
  L.ITqconsym _ -> OperatorTok
  L.ITdupipvarid _ -> VariableTok
  L.ITlabelvarid _ -> VariableTok
  -- Basic types
  L.ITchar _ _ -> CharTok
  L.ITstring _ _ -> StringTok
  L.ITinteger _ -> IntegerTok
  L.ITrational _ -> RationalTok
  L.ITprimchar _ _ -> CharTok
  L.ITprimstring _ _ -> StringTok
  L.ITprimint _ _ -> IntegerTok
  L.ITprimword _ _ -> IntegerTok
  L.ITprimfloat _ -> RationalTok
  L.ITprimdouble _ -> RationalTok
  -- Template Haskell extension tokens
  L.ITopenExpQuote _ _ -> SymbolTok
  L.ITopenPatQuote -> SymbolTok
  L.ITopenDecQuote -> SymbolTok
  L.ITopenTypQuote -> SymbolTok
  L.ITcloseQuote _ -> SymbolTok
  L.ITopenTExpQuote _ -> SymbolTok
  L.ITcloseTExpQuote -> SymbolTok
  L.ITtyQuote -> SymbolTok
  L.ITquasiQuote _ -> SymbolTok
  L.ITqQuasiQuote _ -> SymbolTok
  L.ITdollar -> SymbolTok
  L.ITdollardollar -> SymbolTok
  -- Arrow notation
  L.ITproc -> KeywordTok
  L.ITrec -> KeywordTok
  L.IToparenbar _ -> SymbolTok
  L.ITcparenbar _ -> SymbolTok
  L.ITlarrowtail _ -> SymbolTok
  L.ITrarrowtail _ -> SymbolTok
  L.ITLarrowtail _ -> SymbolTok
  L.ITRarrowtail _ -> SymbolTok
  -- Type application
  L.ITtypeApp -> SymbolTok
  -- Special
  L.ITunknown _ -> OtherTok
  L.ITeof -> OtherTok -- normally is not included in results
  -- Documentation annotations
  L.ITdocCommentNext {} -> CommentTok
  L.ITdocCommentPrev {} -> CommentTok
  L.ITdocCommentNamed {} -> CommentTok
  L.ITdocSection {} -> CommentTok
  L.ITdocOptions {} -> CommentTok
  L.ITlineComment {} -> CommentTok
  L.ITblockComment {} -> CommentTok

----------------------------------------------------------------------------
-- Text traversing

-- | A type for 'Text' with line\/column location attached.
data Text'
  = Text'
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Text
  deriving (Show)

-- | Create 'Text'' from 'Text'.
initText' :: Text -> Text'
initText' = Text' 1 1

-- | Try to fetch white space before start of span at 'Loc'.
tryFetchSpace :: Text' -> Loc -> Maybe (Text', Text)
tryFetchSpace txt (Loc sl sc _ _) =
  let (txt', r) = reachLoc txt sl sc
   in if T.null r
        then Nothing
        else Just (txt', r)

-- | Try to fetch the rest of 'Text'' stream.
tryFetchRest :: Text' -> Maybe (Text', Text)
tryFetchRest (Text' l c txt) =
  if T.null txt
    then Nothing
    else Just (Text' l c "", txt)

-- | Fetch a span at 'Loc'.
fetchSpan :: Text' -> Loc -> (Text', Text)
fetchSpan txt (Loc _ _ el ec) = reachLoc txt el ec

-- | Reach given line\/column location and return 'Text' that has been
-- traversed.
reachLoc ::
  Text' ->
  -- | Line number to reach
  Int ->
  -- | Column number to reach
  Int ->
  (Text', Text)
reachLoc txt@(Text' _ _ original) l c =
  let chunk = T.unfoldr f txt
      f (Text' l' c' s) = do
        guard (l' < l || c' < c)
        (ch, s') <- T.uncons s
        let (l'', c'') = case ch of
              '\n' -> (l' + 1, 1)
              '\t' -> (l', c' + 8 - ((c' - 1) `rem` 8))
              _ -> (l', c' + 1)
        return (ch, Text' l'' c'' s')
   in (Text' l c (T.drop (T.length chunk) original), chunk)

----------------------------------------------------------------------------
-- Pragmas detection

-- | Detect file header pragma.
isHeaderPragma :: Text -> Bool
isHeaderPragma txt0 = isJust $ do
  txt1 <- T.stripStart <$> T.stripPrefix "{-#" txt0
  guard (T.isPrefixOf "LANGUAGE" txt1 || T.isPrefixOf "OPTIONS_GHC" txt1)

----------------------------------------------------------------------------
-- Language extensions

-- | Language extensions we enable by default.
enabledExts :: [Extension]
enabledExts =
  [ ForeignFunctionInterface,
    InterruptibleFFI,
    CApiFFI,
    Arrows,
    TemplateHaskell,
    TemplateHaskellQuotes,
    ImplicitParams,
    OverloadedLabels,
    ExplicitForAll,
    BangPatterns,
    PatternSynonyms,
    MagicHash,
    RecursiveDo,
    UnicodeSyntax,
    UnboxedTuples,
    UnboxedSums,
    DatatypeContexts,
    TransformListComp,
    QuasiQuotes,
    LambdaCase,
    BinaryLiterals,
    NegativeLiterals,
    HexFloatLiterals,
    TypeApplications,
    StaticPointers,
    NumericUnderscores,
    StarIsType
  ]
