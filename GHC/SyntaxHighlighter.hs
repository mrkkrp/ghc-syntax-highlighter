module GHC.SyntaxHighlighter
  ( -- parseDecl
  )
where

-- import DynFlags
-- import GHC (runGhc)
-- import HsDecls
-- import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word64)
import FastString (mkFastString)
import HsExpr
import HsExtension (GhcPs)
import Lexer
import Module (newSimpleUnitId, ComponentId (..))
import Parser
import SrcLoc
import StringBuffer
import Unsafe.Coerce
import qualified EnumSet as ES

-- dynFlags :: DynFlags
-- dynFlags = unsafePerformIO (runGhc Nothing getDynFlags)
-- {-# NOINLINE dynFlags #-}

runParser :: String -> Either () [LTok]
runParser input =
  case unP (lexer True foo) parseState of
    PFailed _ _ _ -> Left ()
    POk _ x -> Right x
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 0 0
    buffer = stringToStringBuffer input
    parseState = mkPStatePure parserFlags buffer location
    parserFlags = ParserFlags
      { pWarningFlags = ES.empty
      , pExtensionFlags = ES.empty
      , pThisPackage = newSimpleUnitId (ComponentId (mkFastString ""))
      , pExtsBitmap = unsafeCoerce (0 :: Word64) -- figure out how to deal
                      -- with it
      }

data LTok = LTok !Int !Int !Int !Int Token
  deriving (Show)

foo :: Located Token -> P [LTok]
foo l@(L _ e) =
  case e of
    ITeof -> return []
    _     -> (fixup l:) <$> lexer True foo

fixup :: Located Token -> LTok
fixup (L srcSpan e) = LTok sl sc el ec e
  where
    (sl,sc,el,ec) = case srcSpan of
      RealSrcSpan rss ->
        let start = realSrcSpanStart rss
            end   = realSrcSpanEnd   rss
        in ( srcLocLine start
           , srcLocCol  start
           , srcLocLine end
           , srcLocCol  end
           )

-- parseDecl ::
