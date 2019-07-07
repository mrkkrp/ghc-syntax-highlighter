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
-- This library uses GHC's lexer, so the result is guaranteed to be 100%
-- correct, as if it was parsed by GHC itself.

{-# LANGUAGE CPP                           #-}
{-# LANGUAGE LambdaCase                    #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE TupleSections                 #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module GHC.SyntaxHighlighter
  ( Token (..)
  , Loc (..)
  , tokenizeHaskell
  , tokenizeHaskellLoc )
where

import Control.Monad
import Data.Bits
import Data.List (unfoldr, foldl')
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Word (Word64)
import FastString (mkFastString)
import Module (newSimpleUnitId, ComponentId (..))
import SrcLoc
import StringBuffer
import qualified Data.Text as T
import qualified EnumSet   as ES
import qualified Lexer     as L

----------------------------------------------------------------------------
-- Data types

-- | Token types that are used as tags to mark spans of source code.

data Token
  = KeywordTok         -- ^ Keyword
  | PragmaTok          -- ^ Pragmas
  | SymbolTok          -- ^ Symbols (punctuation that is not an operator)
  | VariableTok        -- ^ Variable name (term level)
  | ConstructorTok     -- ^ Data\/type constructor
  | OperatorTok        -- ^ Operator
  | CharTok            -- ^ Character
  | StringTok          -- ^ String
  | IntegerTok         -- ^ Integer
  | RationalTok        -- ^ Rational number
  | CommentTok         -- ^ Comment (including Haddocks)
  | SpaceTok           -- ^ Space filling
  | OtherTok           -- ^ Something else?
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Start and end positions of a span. The arguments of the data
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
    sliceOnce (txt, tss@((t, l):ts)) =
      case tryFetchSpace txt l of
        Nothing ->
          let (txt', chunk) = fetchSpan txt l
              t' = case t of
                CommentTok -> if isHeaderPragma chunk
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
    L.POk    _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer (T.unpack input)
    parseState = L.mkPStatePure parserFlags buffer location
    parserFlags = L.ParserFlags
      { L.pWarningFlags = ES.empty
      , L.pExtensionFlags = ES.empty
      , L.pThisPackage = newSimpleUnitId (ComponentId (mkFastString ""))
      , L.pExtsBitmap = extsBitmap
      }

-- | Haskell lexer.

pLexer :: L.P [(Token, Loc)]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case r of
        L _ L.ITeof -> return []
        _           ->
          case fixupToken r of
            Nothing -> go
            Just  x -> (x:) <$> go

-- | Convert @'Located' 'L.Token'@ representation to a more convenient for
-- us form.

fixupToken :: Located L.Token -> Maybe (Token, Loc)
fixupToken (L srcSpan tok) = (classifyToken tok,) <$> srcSpanToLoc srcSpan

-- | Convert 'SrcSpan' to 'Loc'.

srcSpanToLoc :: SrcSpan -> Maybe Loc
srcSpanToLoc (RealSrcSpan rss) =
  let start = realSrcSpanStart rss
      end   = realSrcSpanEnd   rss
  in if start == end
       then Nothing -- NOTE Some magic auto-generated tokens that do not
            -- actually appear in the input stream. Drop them.
       else Just $ Loc (srcLocLine start)
                       (srcLocCol start)
                       (srcLocLine end)
                       (srcLocCol end)
srcSpanToLoc _ = Nothing

-- | Classify a 'L.Token' in terms of 'Token'.

classifyToken :: L.Token -> Token
classifyToken = \case
  -- Keywords
  L.ITas        -> KeywordTok
  L.ITcase      -> KeywordTok
  L.ITclass     -> KeywordTok
  L.ITdata      -> KeywordTok
  L.ITdefault   -> KeywordTok
  L.ITderiving  -> KeywordTok
  L.ITdo        -> KeywordTok
  L.ITelse      -> KeywordTok
  L.IThiding    -> KeywordTok
  L.ITforeign   -> KeywordTok
  L.ITif        -> KeywordTok
  L.ITimport    -> KeywordTok
  L.ITin        -> KeywordTok
  L.ITinfix     -> KeywordTok
  L.ITinfixl    -> KeywordTok
  L.ITinfixr    -> KeywordTok
  L.ITinstance  -> KeywordTok
  L.ITlet       -> KeywordTok
  L.ITmodule    -> KeywordTok
  L.ITnewtype   -> KeywordTok
  L.ITof        -> KeywordTok
  L.ITqualified -> KeywordTok
  L.ITthen      -> KeywordTok
  L.ITtype      -> KeywordTok
  L.ITwhere     -> KeywordTok
  L.ITforall _  -> KeywordTok
  L.ITexport    -> KeywordTok
  L.ITlabel     -> KeywordTok
  L.ITdynamic   -> KeywordTok
  L.ITsafe      -> KeywordTok
  L.ITinterruptible -> KeywordTok
  L.ITunsafe    -> KeywordTok
  L.ITstdcallconv -> KeywordTok
  L.ITccallconv -> KeywordTok
  L.ITcapiconv  -> KeywordTok
  L.ITprimcallconv -> KeywordTok
  L.ITjavascriptcallconv -> KeywordTok
  L.ITmdo       -> KeywordTok
  L.ITfamily    -> KeywordTok
  L.ITrole      -> KeywordTok
  L.ITgroup     -> KeywordTok
  L.ITby        -> KeywordTok
  L.ITusing     -> KeywordTok
  L.ITpattern   -> KeywordTok
  L.ITstatic    -> KeywordTok
  L.ITstock     -> KeywordTok
  L.ITanyclass  -> KeywordTok
#if MIN_VERSION_ghc(8,6,1)
  L.ITvia       -> KeywordTok
#endif
  L.ITunit      -> KeywordTok
  L.ITsignature -> KeywordTok
  L.ITdependency -> KeywordTok
  L.ITrequires  -> KeywordTok
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
  L.ITgenerated_prag _ -> PragmaTok
  L.ITcore_prag _ -> PragmaTok
  L.ITunpack_prag _ -> PragmaTok
  L.ITnounpack_prag _ -> PragmaTok
  L.ITann_prag _ -> PragmaTok
  L.ITcomplete_prag _ -> PragmaTok
  L.ITclose_prag -> PragmaTok
  L.IToptions_prag _ -> PragmaTok
  L.ITinclude_prag _ -> PragmaTok
  L.ITlanguage_prag -> PragmaTok
#if !MIN_VERSION_ghc(8,6,1)
  L.ITvect_prag _ -> PragmaTok
  L.ITvect_scalar_prag _ -> PragmaTok
  L.ITnovect_prag _ -> PragmaTok
#endif
  L.ITminimal_prag _ -> PragmaTok
  L.IToverlappable_prag _ -> PragmaTok
  L.IToverlapping_prag _ -> PragmaTok
  L.IToverlaps_prag _ -> PragmaTok
  L.ITincoherent_prag _ -> PragmaTok
  L.ITctype _ -> PragmaTok
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
  L.ITat -> SymbolTok
  L.ITtilde -> SymbolTok
#if !MIN_VERSION_ghc(8,6,1)
  L.ITtildehsh -> SymbolTok
#endif
  L.ITdarrow _ -> SymbolTok
  L.ITbang -> SymbolTok
#if MIN_VERSION_ghc(8,6,1)
  L.ITstar _ -> SymbolTok
#endif
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
  -- NOTE GHC thinks these are reserved symbols, but I classify them as
  -- operators.
  L.ITminus -> OperatorTok
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
  L.ITidEscape _ -> SymbolTok
  L.ITparenEscape -> SymbolTok
  L.ITidTyEscape _ -> SymbolTok
  L.ITparenTyEscape -> SymbolTok
  L.ITtyQuote -> SymbolTok
  L.ITquasiQuote _ -> SymbolTok
  L.ITqQuasiQuote _ -> SymbolTok
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
  L.ITdocCommentNext _ -> CommentTok
  L.ITdocCommentPrev _ -> CommentTok
  L.ITdocCommentNamed _ -> CommentTok
  L.ITdocSection _ _ -> CommentTok
  L.ITdocOptions _ -> CommentTok
  L.ITlineComment _ -> CommentTok
  L.ITblockComment _ -> CommentTok

----------------------------------------------------------------------------
-- Text traversing

-- | A type for 'Text' with line\/column location attached.

data Text' = Text'
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

reachLoc
  :: Text'
  -> Int               -- ^ Line number to reach
  -> Int               -- ^ Column number to reach
  -> (Text', Text)
reachLoc txt@(Text' _ _ original) l c =
  let chunk = T.unfoldr f txt
      f (Text' l' c' s) = do
        guard (l' < l || c' < c)
        (ch, s') <- T.uncons s
        let (l'', c'') = case ch of
              '\n' -> (l' + 1, 1)
              '\t' -> (l', c' + 8 - ((c' - 1) `rem` 8))
              _    -> (l', c' + 1)
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
-- Exts bitmap hack

-- | Extension bitmap we use in this library.

extsBitmap :: Word64
extsBitmap = mkExtsBitmap enabledExts
{-# NOINLINE extsBitmap #-}

-- | Create extension bitmap similarly to how it's done in GHC.

mkExtsBitmap :: [ExtBits] -> Word64
mkExtsBitmap = foldl' f 0
  where
    f w x = bit (fromEnum x) .|. w

-- | Copied from GHC sources that are not yet available in the @ghc@
-- package.

data ExtBits
  = FfiBit
  | InterruptibleFfiBit
  | CApiFfiBit
#if !MIN_VERSION_ghc(8,6,0)
  | ParrBit
#endif
  | ArrowsBit
  | ThBit
  | ThQuotesBit
  | IpBit
  | OverloadedLabelsBit -- #x overloaded labels
  | ExplicitForallBit -- the 'forall' keyword and '.' symbol
  | BangPatBit -- Tells the parser to understand bang-patterns
               -- (doesn't affect the lexer)
  | PatternSynonymsBit -- pattern synonyms
  | HaddockBit-- Lex and parse Haddock comments
  | MagicHashBit -- "#" in both functions and operators
  | RecursiveDoBit -- mdo
  | UnicodeSyntaxBit -- the forall symbol, arrow symbols, etc
  | UnboxedTuplesBit -- (# and #)
  | UnboxedSumsBit -- (# and #)
  | DatatypeContextsBit
  | TransformComprehensionsBit
  | QqBit -- enable quasiquoting
  | InRulePragBit
  | RawTokenStreamBit -- producing a token stream with all comments included
  | SccProfilingOnBit
  | HpcBit
  | AlternativeLayoutRuleBit
  | RelaxedLayoutBit
  | NondecreasingIndentationBit
  | SafeHaskellBit
  | TraditionalRecordSyntaxBit
  | ExplicitNamespacesBit
  | LambdaCaseBit
  | BinaryLiteralsBit
  | NegativeLiteralsBit
  | HexFloatLiteralsBit
  | TypeApplicationsBit
  | StaticPointersBit
  | NumericUnderscoresBit
#if MIN_VERSION_ghc(8,6,0)
  | StarIsTypeBit
#endif
  deriving Enum

-- | Extension we enable for the best user experience.

enabledExts :: [ExtBits]
enabledExts =
  [ FfiBit
  , InterruptibleFfiBit
  , CApiFfiBit
#if !MIN_VERSION_ghc(8,6,0)
  , ParrBit
#endif
  , ArrowsBit
  , ThBit
  , ThQuotesBit
  , IpBit
  , OverloadedLabelsBit
  , ExplicitForallBit
  , BangPatBit
  , PatternSynonymsBit
  , HaddockBit
  , MagicHashBit
  , RecursiveDoBit
  , UnicodeSyntaxBit
  , UnboxedTuplesBit
  , UnboxedSumsBit
  , DatatypeContextsBit
  , TransformComprehensionsBit
  , QqBit
  , InRulePragBit
  , RawTokenStreamBit
  , SafeHaskellBit
  , LambdaCaseBit
  , BinaryLiteralsBit
  , NegativeLiteralsBit
  , HexFloatLiteralsBit
  , TypeApplicationsBit
  , StaticPointersBit
  , NumericUnderscoresBit
#if MIN_VERSION_ghc(8,6,0)
  , StarIsTypeBit
#endif
  ]
