{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpsTH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse (parseExp)
import Data.Char (isSpace)

-- | Parse a Haskell code string at compile time.
-- Returns both the original string (for code generation) and the parsed expression.
-- This validates that the code string is syntactically valid Haskell.
--
-- Usage: $(dualCode "reverse") produces ("reverse", reverse)
dualCode :: String -> Q Exp
dualCode code = case parseExp trimmed of
  Left err -> fail $ "OpsTH.dualCode: Failed to parse Haskell expression: "
                     ++ show trimmed ++ "\nError: " ++ err
  Right expr -> [| ($(litE (stringL trimmed)), $(return expr)) |]
  where
    trimmed = strip code

-- | Like dualCode but only returns the string (for compile-only ops)
codeOnly :: String -> Q Exp
codeOnly code = litE (stringL (strip code))

-- | Validate that a code string is syntactically valid Haskell at compile time
validateCode :: String -> Q ()
validateCode code = case parseExp (strip code) of
  Left err -> fail $ "OpsTH.validateCode: Invalid Haskell: " ++ code ++ "\nError: " ++ err
  Right _ -> return ()

-- | Strip leading/trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Quasi-quoter for dual code definitions
-- Usage: [dual|reverse|] expands to ("reverse", reverse)
dual :: QuasiQuoter
dual = QuasiQuoter
  { quoteExp = dualCode
  , quotePat = error "dual: patterns not supported"
  , quoteType = error "dual: types not supported"
  , quoteDec = error "dual: declarations not supported"
  }

-- | Type for dual implementations: code string paired with runtime function
data DualCode a = DualCode
  { dcCode :: String  -- ^ Haskell code string for compilation
  , dcFn   :: a       -- ^ Runtime function for interpretation
  }

-- | Create a DualCode from a code string and function
-- The function type is polymorphic to allow different arities
mkDual :: String -> a -> DualCode a
mkDual = DualCode

-- | Extract just the code string (for compilation mode)
getCode :: DualCode a -> String
getCode = dcCode

-- | Extract just the function (for interpretation mode)
getFn :: DualCode a -> a
getFn = dcFn
