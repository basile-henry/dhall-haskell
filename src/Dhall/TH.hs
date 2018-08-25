{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module provides `staticDhallExpression` which can be used to resolve
    all of an expression’s imports at compile time, allowing one to reference
    Dhall expressions from Haskell without having a runtime dependency on the
    location of Dhall files.

    For example, given a file “Some/Type.dhall” containing

        < This : Natural | Other : ../Other/Type.dhall >

    rather than duplicating the AST manually in a Haskell `Type`, you can do

        Dhall.Type
        (\case
            UnionLit "This" _ _  -> ...
            UnionLit "Other" _ _ -> ...)
        $(staticDhallExpression "../../Some/Type.dhall")

    This would create the Dhall Expr AST from the `Type.dhall` file at compile
    time with all imports resolved, making it easy to keep your Dhall configs
    and Haskell interpreters in sync.
-}
module Dhall.TH
    ( -- * Template Haskell
      staticDhallExpression
    , dhall
    ) where

import Data.Typeable
import Dhall (Type)
import Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ) -- 7.10 compatibility.
import Language.Haskell.TH.Syntax hiding (Type)
import Lens.Family ((.~))

import qualified Data.Text as Text
import qualified Dhall
import qualified GHC.IO.Encoding
import qualified System.IO

-- | This fully resolves, type checks, and normalizes the expression, so the
--   resulting AST is self-contained.
staticDhallExpression :: Text.Text -> Q Exp
staticDhallExpression text = do
    runIO (GHC.IO.Encoding.setLocaleEncoding System.IO.utf8)
    expression <- runIO (Dhall.inputExpr text)
    dataToExpQ (\a -> liftText <$> cast a) expression
  where
    -- A workaround for a problem in TemplateHaskell (see
    -- https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable)
    liftText = fmap (AppE (VarE 'Text.pack)) . lift . Text.unpack

{- | QuasiQuoter to use inline Dhall expressions in Haskell.

The expression is resolved, type checked and normalized at compile time.

>>> :set -XQuasiQuotes
>>> dhallBool = dhall Dhall.bool
>>> [dhallBool| Natural/even 42 |]
True
-}
dhall :: Lift a => Type a -> QuasiQuoter
dhall returnType =
  QuasiQuoter
    { quoteExp  = dhallQuoteExp returnType
    , quoteDec  = quoteErr "Dec"
    , quoteType = quoteErr "Type"
    , quotePat  = quoteErr "Pat"
    }
  where
    quoteErr x =
      fail $
        "Dhall.TH.dhall: The QuasiQuoter only handles \"Exp\", not \""
        <> x <> "\"."

dhallQuoteExp :: Lift a => Type a -> String -> Q Exp
dhallQuoteExp returnType input = do
    Loc {..} <- location

    let source = "(" <> loc_filename <> ":" <> show (fst loc_start) <> ")"
        settings = (Dhall.sourceName .~ source) Dhall.defaultInputSettings

    haskellValue <- runIO $ do
      GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
      Dhall.inputWithSettings settings returnType (Text.pack input)

    [e|haskellValue|]
