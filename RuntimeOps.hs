{-# LANGUAGE FlexibleInstances #-}

module RuntimeOps where

import Value
import Data.List (genericLength, genericTake, genericDrop, genericReplicate,
                  sort, sortBy, nub, nubBy, transpose, intercalate, findIndex,
                  elemIndex, delete, (\\), union, intersect, inits, tails,
                  isPrefixOf, isSuffixOf, isInfixOf, group, permutations)
import Data.Char (chr, ord, isSpace, isAlpha, isDigit, toUpper, toLower,
                  isUpper, isLower, isPunctuation, isSymbol)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Ord (comparing)
import qualified Data.Set as Set

-- ============================================================
-- List Operations
-- ============================================================

rReverse :: RValue -> RValue
rReverse (RList xs) = RList (reverse xs)
rReverse v = error $ "reverse expects list, got: " ++ show v

rHead :: RValue -> RValue
rHead (RList (x:_)) = x
rHead (RList []) = RInt 0  -- Default value for empty list
rHead v = error $ "head expects list, got: " ++ show v

rTail :: RValue -> RValue
rTail (RList (_:xs)) = RList xs
rTail (RList []) = RList []
rTail v = error $ "tail expects list, got: " ++ show v

rInit :: RValue -> RValue
rInit (RList []) = RList []
rInit (RList xs) = RList (init xs)
rInit v = error $ "init expects list, got: " ++ show v

rLast :: RValue -> RValue
rLast (RList []) = RInt 0  -- Default value
rLast (RList xs) = last xs
rLast v = error $ "last expects list, got: " ++ show v

rTake :: RValue -> RValue -> RValue
rTake (RInt n) (RList xs) = RList $ if n < 0
  then genericDrop (genericLength xs + n) xs
  else genericTake n xs
rTake n v = error $ "take expects (int, list), got: " ++ show (n, v)

rDrop :: RValue -> RValue -> RValue
rDrop (RInt n) (RList xs) = RList $ if n < 0
  then genericTake (genericLength xs + n) xs
  else genericDrop n xs
rDrop n v = error $ "drop expects (int, list), got: " ++ show (n, v)

rConcat :: RValue -> RValue
rConcat (RList xs) = RList $ concatMap rToList xs
rConcat v = error $ "concat expects list of lists, got: " ++ show v

rAppend :: RValue -> RValue -> RValue
rAppend (RList a) (RList b) = RList (a ++ b)
rAppend a b = error $ "append expects two lists, got: " ++ show (a, b)

rCons :: RValue -> RValue -> RValue
rCons x (RList xs) = RList (x : xs)
rCons x v = error $ "cons expects (elem, list), got: " ++ show (x, v)

rSnoc :: RValue -> RValue -> RValue
rSnoc (RList xs) x = RList (xs ++ [x])
rSnoc v x = error $ "snoc expects (list, elem), got: " ++ show (v, x)

rReplicate :: RValue -> RValue -> RValue
rReplicate (RInt n) v = RList $ genericReplicate n v
rReplicate n v = error $ "replicate expects (int, value), got: " ++ show (n, v)

rRange :: RValue -> RValue
rRange (RInt n) = RList $ map RInt [1..n]
rRange v = error $ "range expects int, got: " ++ show v

rRange0 :: RValue -> RValue
rRange0 (RInt n) = RList $ map RInt [0..n-1]
rRange0 v = error $ "range0 expects int, got: " ++ show v

rSubscript :: RValue -> RValue -> RValue
rSubscript (RInt i) (RList xs)
  | null xs = RInt 0  -- Default for empty
  | otherwise = xs !! fromIntegral ((i - 1) `mod` genericLength xs)
rSubscript i v = error $ "subscript expects (int, list), got: " ++ show (i, v)

rAt :: RValue -> RValue -> RValue
rAt (RList xs) (RInt i)
  | i < 0 = RInt 0
  | i >= genericLength xs = RInt 0
  | otherwise = xs !! fromIntegral i
rAt v i = error $ "at expects (list, int), got: " ++ show (v, i)

rTranspose :: RValue -> RValue
rTranspose (RList xs) = RList $ map RList $ transpose $ map rToList xs
rTranspose v = error $ "transpose expects list of lists, got: " ++ show v

rSort :: RValue -> RValue
rSort (RList xs) = RList $ sortBy rCompare xs
  where
    rCompare (RInt a) (RInt b) = compare a b
    rCompare (RChr a) (RChr b) = compare a b
    rCompare (RList a) (RList b) = compare a b
    rCompare _ _ = EQ
rSort v = error $ "sort expects list, got: " ++ show v

rNub :: RValue -> RValue
rNub (RList xs) = RList $ nubBy rEq xs
  where rEq a b = a == b
rNub v = error $ "nub expects list, got: " ++ show v

rGroup :: RValue -> RValue
rGroup (RList xs) = RList $ map RList $ groupBy rEq xs
  where
    rEq a b = a == b
    groupBy _ [] = []
    groupBy eq (x:xs) = (x : takeWhile (eq x) xs) : groupBy eq (dropWhile (eq x) xs)
rGroup v = error $ "group expects list, got: " ++ show v

rZip :: RValue -> RValue -> RValue
rZip (RList a) (RList b) = RList $ zipWith (\x y -> RTuple [x, y]) a b
rZip a b = error $ "zip expects two lists, got: " ++ show (a, b)

rZipWith :: RValue -> RValue -> RValue -> RValue
rZipWith (RFn f) (RList a) (RList b) = RList $ zipWith (\x y -> f (RTuple [x, y])) a b
rZipWith f a b = error $ "zipWith expects (fn, list, list), got: " ++ show (f, a, b)

rInits :: RValue -> RValue
rInits (RList xs) = RList $ map RList $ inits xs
rInits v = error $ "inits expects list, got: " ++ show v

rTails :: RValue -> RValue
rTails (RList xs) = RList $ map RList $ tails xs
rTails v = error $ "tails expects list, got: " ++ show v

rPermutations :: RValue -> RValue
rPermutations (RList xs) = RList $ map RList $ permutations xs
rPermutations v = error $ "permutations expects list, got: " ++ show v

-- ============================================================
-- Higher-Order List Operations
-- ============================================================

rFilter :: RValue -> RValue -> RValue
rFilter (RFn f) (RList xs) = RList $ filter (rTruthy . f) xs
rFilter f v = error $ "filter expects (fn, list), got: " ++ show v

rMap :: RValue -> RValue -> RValue
rMap (RFn f) (RList xs) = RList $ map f xs
rMap f v = error $ "map expects (fn, list), got: " ++ show v

rFoldr :: RValue -> RValue -> RValue -> RValue
rFoldr (RFn f) ini (RList xs) = foldr (\x acc -> f (RTuple [x, acc])) ini xs
rFoldr f ini v = error $ "foldr expects (fn, init, list), got: " ++ show (f, ini, v)

rFoldl :: RValue -> RValue -> RValue -> RValue
rFoldl (RFn f) ini (RList xs) = foldl (\acc x -> f (RTuple [acc, x])) ini xs
rFoldl f ini v = error $ "foldl expects (fn, init, list), got: " ++ show (f, ini, v)

rFoldr1 :: RValue -> RValue -> RValue
rFoldr1 (RFn f) (RList []) = RInt 0  -- Default
rFoldr1 (RFn f) (RList xs) = foldr1 (\x acc -> f (RTuple [x, acc])) xs
rFoldr1 f v = error $ "foldr1 expects (fn, list), got: " ++ show (f, v)

rScanl :: RValue -> RValue -> RValue -> RValue
rScanl (RFn f) ini (RList xs) = RList $ scanl (\acc x -> f (RTuple [acc, x])) ini xs
rScanl f ini v = error $ "scanl expects (fn, init, list), got: " ++ show (f, ini, v)

rScanr :: RValue -> RValue -> RValue -> RValue
rScanr (RFn f) ini (RList xs) = RList $ scanr (\x acc -> f (RTuple [x, acc])) ini xs
rScanr f ini v = error $ "scanr expects (fn, init, list), got: " ++ show (f, ini, v)

rTakeWhile :: RValue -> RValue -> RValue
rTakeWhile (RFn f) (RList xs) = RList $ takeWhile (rTruthy . f) xs
rTakeWhile f v = error $ "takeWhile expects (fn, list), got: " ++ show (f, v)

rDropWhile :: RValue -> RValue -> RValue
rDropWhile (RFn f) (RList xs) = RList $ dropWhile (rTruthy . f) xs
rDropWhile f v = error $ "dropWhile expects (fn, list), got: " ++ show (f, v)

rAny :: RValue -> RValue -> RValue
rAny (RFn f) (RList xs) = RInt $ if any (rTruthy . f) xs then 1 else 0
rAny f v = error $ "any expects (fn, list), got: " ++ show (f, v)

rAll :: RValue -> RValue -> RValue
rAll (RFn f) (RList xs) = RInt $ if all (rTruthy . f) xs then 1 else 0
rAll f v = error $ "all expects (fn, list), got: " ++ show (f, v)

-- ============================================================
-- Arithmetic Operations
-- ============================================================

rAdd :: RValue -> RValue -> RValue
rAdd (RInt a) (RInt b) = RInt (a + b)
rAdd (RChr a) (RInt b) = RChr (a + b)
rAdd (RInt a) (RChr b) = RChr (a + b)
rAdd (RChr a) (RChr b) = RInt (a + b)  -- char + char = int
rAdd a (RList bs) = RList $ map (rAdd a) bs  -- Vectorization
rAdd (RList as) b = RList $ map (`rAdd` b) as
rAdd a b = error $ "add type mismatch: " ++ show (a, b)

rSub :: RValue -> RValue -> RValue
rSub (RInt a) (RInt b) = RInt (a - b)
rSub (RChr a) (RInt b) = RChr (a - b)
rSub (RChr a) (RChr b) = RInt (a - b)
rSub (RInt a) (RChr b) = RChr (a - b)
rSub a (RList bs) = RList $ map (rSub a) bs
rSub (RList as) b = RList $ map (`rSub` b) as
rSub a b = error $ "subtract type mismatch: " ++ show (a, b)

rMul :: RValue -> RValue -> RValue
rMul (RInt a) (RInt b) = RInt (a * b)
rMul a (RList bs) = RList $ map (rMul a) bs
rMul (RList as) b = RList $ map (`rMul` b) as
rMul a b = error $ "multiply type mismatch: " ++ show (a, b)

rDiv :: RValue -> RValue -> RValue
rDiv (RInt a) (RInt 0) = RInt (signum a * 2^128)  -- Safe div from Header.hs
rDiv (RInt a) (RInt b) = RInt (a `div` b)
rDiv a b = error $ "divide type mismatch: " ++ show (a, b)

rMod :: RValue -> RValue -> RValue
rMod (RInt _) (RInt 0) = RInt 0
rMod (RInt a) (RInt b) = RInt (a `mod` b)
rMod a b = error $ "modulo type mismatch: " ++ show (a, b)

rPow :: RValue -> RValue -> RValue
rPow (RInt a) (RInt b)
  | b < 0 = RInt (rNthRoot (-b) a)
  | otherwise = RInt (a ^ b)
rPow a b = error $ "pow type mismatch: " ++ show (a, b)

rNthRoot :: Integer -> Integer -> Integer
rNthRoot _ 0 = 0
rNthRoot n x = rNthRootGuess n x x
  where
    rNthRootGuess n guess x = case ((x `div` guess^(n-1)) + guess*(n-1)) `div` n of
      r | r >= guess -> guess
        | otherwise -> rNthRootGuess n r x

rAbs :: RValue -> RValue
rAbs (RInt n) = RInt (abs n)
rAbs v = error $ "abs expects int, got: " ++ show v

rSignum :: RValue -> RValue
rSignum (RInt n) = RInt (signum n)
rSignum v = error $ "signum expects int, got: " ++ show v

rNegate :: RValue -> RValue
rNegate (RInt n) = RInt (-n)
rNegate v = error $ "negate expects int, got: " ++ show v

rMax :: RValue -> RValue -> RValue
rMax (RInt a) (RInt b) = RInt (max a b)
rMax (RChr a) (RChr b) = RChr (max a b)
rMax a b = error $ "max type mismatch: " ++ show (a, b)

rMin :: RValue -> RValue -> RValue
rMin (RInt a) (RInt b) = RInt (min a b)
rMin (RChr a) (RChr b) = RChr (min a b)
rMin a b = error $ "min type mismatch: " ++ show (a, b)

-- ============================================================
-- Aggregation Operations
-- ============================================================

rSum :: RValue -> RValue
rSum (RList xs) = RInt $ sum $ map rToInteger xs
rSum v = error $ "sum expects list of ints, got: " ++ show v

rProduct :: RValue -> RValue
rProduct (RList xs) = RInt $ product $ map rToInteger xs
rProduct v = error $ "product expects list of ints, got: " ++ show v

rMaximum :: RValue -> RValue
rMaximum (RList []) = RInt 0
rMaximum (RList xs) = foldr1 rMax2 xs
  where rMax2 a b = if rToInteger a >= rToInteger b then a else b
rMaximum v = error $ "maximum expects list, got: " ++ show v

rMinimum :: RValue -> RValue
rMinimum (RList []) = RInt 0
rMinimum (RList xs) = foldr1 rMin2 xs
  where rMin2 a b = if rToInteger a <= rToInteger b then a else b
rMinimum v = error $ "minimum expects list, got: " ++ show v

-- ============================================================
-- Character/String Operations
-- ============================================================

rOrd :: RValue -> RValue
rOrd (RChr c) = RInt c
rOrd v = error $ "ord expects char, got: " ++ show v

rChr :: RValue -> RValue
rChr (RInt n) = RChr n
rChr v = error $ "chr expects int, got: " ++ show v

rToUpper :: RValue -> RValue
rToUpper (RChr c) = RChr $ fromIntegral $ ord $ toUpper $ chr $ fromIntegral c
rToUpper v = error $ "toUpper expects char, got: " ++ show v

rToLower :: RValue -> RValue
rToLower (RChr c) = RChr $ fromIntegral $ ord $ toLower $ chr $ fromIntegral c
rToLower v = error $ "toLower expects char, got: " ++ show v

rIsUpper :: RValue -> RValue
rIsUpper (RChr c) = RInt $ if isUpper (chr $ fromIntegral c) then 1 else 0
rIsUpper v = error $ "isUpper expects char, got: " ++ show v

rIsLower :: RValue -> RValue
rIsLower (RChr c) = RInt $ if isLower (chr $ fromIntegral c) then 1 else 0
rIsLower v = error $ "isLower expects char, got: " ++ show v

rIsDigit :: RValue -> RValue
rIsDigit (RChr c) = RInt $ if isDigit (chr $ fromIntegral c) then 1 else 0
rIsDigit v = error $ "isDigit expects char, got: " ++ show v

rIsAlpha :: RValue -> RValue
rIsAlpha (RChr c) = RInt $ if isAlpha (chr $ fromIntegral c) then 1 else 0
rIsAlpha v = error $ "isAlpha expects char, got: " ++ show v

rIsSpace :: RValue -> RValue
rIsSpace (RChr c) = RInt $ if isSpace (chr $ fromIntegral c) then 1 else 0
rIsSpace v = error $ "isSpace expects char, got: " ++ show v

-- ============================================================
-- Comparison Operations
-- ============================================================

rEqual :: RValue -> RValue -> RValue
rEqual a b = RInt $ if a == b then 1 else 0

rNotEqual :: RValue -> RValue -> RValue
rNotEqual a b = RInt $ if a /= b then 1 else 0

rLess :: RValue -> RValue -> RValue
rLess (RInt a) (RInt b) = RInt $ if a < b then 1 else 0
rLess (RChr a) (RChr b) = RInt $ if a < b then 1 else 0
rLess a b = error $ "less type mismatch: " ++ show (a, b)

rGreater :: RValue -> RValue -> RValue
rGreater (RInt a) (RInt b) = RInt $ if a > b then 1 else 0
rGreater (RChr a) (RChr b) = RInt $ if a > b then 1 else 0
rGreater a b = error $ "greater type mismatch: " ++ show (a, b)

rLessEq :: RValue -> RValue -> RValue
rLessEq (RInt a) (RInt b) = RInt $ if a <= b then 1 else 0
rLessEq (RChr a) (RChr b) = RInt $ if a <= b then 1 else 0
rLessEq a b = error $ "lessEq type mismatch: " ++ show (a, b)

rGreaterEq :: RValue -> RValue -> RValue
rGreaterEq (RInt a) (RInt b) = RInt $ if a >= b then 1 else 0
rGreaterEq (RChr a) (RChr b) = RInt $ if a >= b then 1 else 0
rGreaterEq a b = error $ "greaterEq type mismatch: " ++ show (a, b)

-- ============================================================
-- Base Conversion
-- ============================================================

rFromBase :: RValue -> RValue -> RValue
rFromBase (RInt b) (RList xs) = RInt $ foldl (\x y -> x * b + rToInteger y) 0 xs
rFromBase b v = error $ "fromBase expects (int, list), got: " ++ show (b, v)

rToBase :: RValue -> RValue -> RValue
rToBase (RInt b) (RInt n) = RList $ reverse $ map (RInt . (`mod` b)) $
                             takeWhile (> 0) $ iterate (`div` b) n
rToBase b n = error $ "toBase expects (int, int), got: " ++ show (b, n)

-- ============================================================
-- Set Operations
-- ============================================================

rUnion :: RValue -> RValue -> RValue
rUnion (RList a) (RList b) = RList $ rToList (rNub (RList (a ++ b)))
rUnion a b = error $ "union expects two lists, got: " ++ show (a, b)

rIntersect :: RValue -> RValue -> RValue
rIntersect (RList a) (RList b) = RList $ filter (`elem` b) a
rIntersect a b = error $ "intersect expects two lists, got: " ++ show (a, b)

rDiff :: RValue -> RValue -> RValue
rDiff (RList a) (RList b) = RList $ filter (`notElem` b) a
rDiff a b = error $ "diff expects two lists, got: " ++ show (a, b)

-- ============================================================
-- Misc Operations
-- ============================================================

rId :: RValue -> RValue
rId = id

rConst :: RValue -> RValue -> RValue
rConst a _ = a

rFlip :: RValue -> RValue -> RValue -> RValue
rFlip (RFn f) a b = f (RTuple [b, a])
rFlip f a b = error $ "flip expects function, got: " ++ show f

rCompose :: RValue -> RValue -> RValue -> RValue
rCompose (RFn f) (RFn g) x = f (g x)
rCompose f g x = error $ "compose expects two functions, got: " ++ show (f, g)

rIf :: RValue -> RValue -> RValue -> RValue
rIf cond thenV elseV = if rTruthy cond then thenV else elseV

rFst :: RValue -> RValue
rFst (RTuple (x:_)) = x
rFst (RList (x:_)) = x
rFst v = error $ "fst expects tuple or list, got: " ++ show v

rSnd :: RValue -> RValue
rSnd (RTuple (_:y:_)) = y
rSnd (RList (_:y:_)) = y
rSnd v = error $ "snd expects tuple or list with 2+ elements, got: " ++ show v

rPair :: RValue -> RValue -> RValue
rPair a b = RTuple [a, b]

rDup :: RValue -> RValue
rDup x = RTuple [x, x]

rStep :: RValue -> RValue -> RValue
rStep (RInt n) (RList xs) = RList $ map fst $ filter ((== 0) . (`mod` absN) . snd) $
                            zip (if n < 0 then reverse xs else xs) [0..]
  where absN = abs n
rStep n v = error $ "step expects (int, list), got: " ++ show (n, v)

rChunksOf :: RValue -> RValue -> RValue
rChunksOf (RInt n) (RList xs) = RList $ map RList $ chunksOf (fromIntegral n) xs
rChunksOf n v = error $ "chunksOf expects (int, list), got: " ++ show (n, v)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

rIntercalate :: RValue -> RValue -> RValue
rIntercalate (RList sep) (RList xss) = RList $ intercalate sep $ map rToList xss
rIntercalate sep v = error $ "intercalate expects (list, list of lists), got: " ++ show (sep, v)

rWords :: RValue -> RValue
rWords (RList xs) = RList $ map (RList . map (RChr . fromIntegral . ord)) $
                    words $ map (chr . fromIntegral . rToInteger) xs
rWords v = error $ "words expects string, got: " ++ show v

rLines :: RValue -> RValue
rLines (RList xs) = RList $ map (RList . map (RChr . fromIntegral . ord)) $
                    lines $ map (chr . fromIntegral . rToInteger) xs
rLines v = error $ "lines expects string, got: " ++ show v

rShow :: RValue -> RValue
rShow (RInt n) = RList $ map (RChr . fromIntegral . ord) $ show n
rShow v = RList $ map (RChr . fromIntegral . ord) $ show v
