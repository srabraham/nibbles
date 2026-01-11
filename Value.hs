{-# LANGUAGE FlexibleInstances #-}

module Value where

import Types
import Data.Char (chr, ord, isSpace)
import Data.List (intercalate, genericLength)

-- Runtime value representation for interpreter mode
data RValue
  = RInt Integer
  | RChr Integer       -- Stored as Integer internally (like Nibbles does)
  | RList [RValue]
  | RFn (RValue -> RValue)
  | RTuple [RValue]
  | RUnit

instance Show RValue where
  show (RInt n) = show n
  show (RChr n) = show (chr $ fromIntegral n)
  show (RList vs) = "[" ++ intercalate "," (map show vs) ++ "]"
  show (RFn _) = "<function>"
  show (RTuple vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show RUnit = "()"

instance Eq RValue where
  RInt a == RInt b = a == b
  RChr a == RChr b = a == b
  RList a == RList b = a == b
  RTuple a == RTuple b = a == b
  RUnit == RUnit = True
  _ == _ = False

instance Ord RValue where
  compare (RInt a) (RInt b) = compare a b
  compare (RChr a) (RChr b) = compare a b
  compare (RList a) (RList b) = compare a b
  compare (RTuple a) (RTuple b) = compare a b
  compare RUnit RUnit = EQ
  compare _ _ = EQ  -- Functions can't be compared

-- Check if VT matches RValue type
vtMatchesValue :: VT -> RValue -> Bool
vtMatchesValue VInt (RInt _) = True
vtMatchesValue VChr (RChr _) = True
vtMatchesValue (VList [et]) (RList vs) = all (vtMatchesValue et) vs
vtMatchesValue _ _ = False

-- Value extractors
rToInteger :: RValue -> Integer
rToInteger (RInt n) = n
rToInteger (RChr n) = n
rToInteger v = error $ "Expected integer, got: " ++ show v

rToList :: RValue -> [RValue]
rToList (RList xs) = xs
rToList v = error $ "Expected list, got: " ++ show v

rToFn :: RValue -> (RValue -> RValue)
rToFn (RFn f) = f
rToFn v = error $ "Expected function, got: " ++ show v

-- Truthiness (matches Header.hs logic)
rTruthy :: RValue -> Bool
rTruthy (RInt n) = n > 0
rTruthy (RChr n) = n > 0 && not (isSpace (chr (fromIntegral n)))
rTruthy (RList xs) = not (null xs)
rTruthy (RTuple (x:_)) = rTruthy x
rTruthy _ = False

-- Convert string to RValue list of chars
stringToRValue :: String -> RValue
stringToRValue s = RList $ map (RChr . fromIntegral . ord) s

-- Convert RValue to output string (matches finish logic in Polylib.hs)
rValueToString :: RValue -> String
rValueToString (RInt n) = show n
rValueToString (RChr n) = [chr $ fromIntegral n]
rValueToString (RList xs) = concatMap rValueToChar xs
  where
    rValueToChar (RChr n) = [chr $ fromIntegral n]
    rValueToChar v = rValueToString v
rValueToString (RTuple vs) = intercalate "," $ map rValueToString vs
rValueToString (RFn _) = "<function>"
rValueToString RUnit = ""

-- Finish output with newline (matches finishLn in Header.hs)
rFinishLn :: RValue -> String
rFinishLn v =
  let s = rValueToString v
  in if null s || last s == '\n' then s else s ++ "\n"

-- Get length of RValue list
rLength :: RValue -> Integer
rLength (RList xs) = genericLength xs
rLength v = error $ "length expects list, got: " ++ show v
