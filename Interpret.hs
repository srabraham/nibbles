{-# LANGUAGE RecordWildCards #-}

module Interpret (
    InterpCtx(..),
    InterpFn,
    emptyCtx,
    pushArg,
    getArg,
    runInterpreter,
    evalWithInput
) where

import Value
import RuntimeOps
import qualified Data.Map as Map
import Data.Char (ord, chr)
import Data.List (intercalate, genericLength)

-- | Interpreter function type: takes context and returns a value
type InterpFn = InterpCtx -> RValue

-- | Interpreter context - tracks arguments and input
data InterpCtx = InterpCtx
    { ctxArgs    :: Map.Map Int RValue  -- De Bruijn indexed arguments
    , ctxDepth   :: Int                 -- Current lambda depth
    , ctxInput   :: RValue              -- Program input (stdin as string)
    , ctxRawArgs :: [RValue]            -- Raw program arguments
    } deriving (Show)

-- | Create empty context with given input
emptyCtx :: String -> [String] -> InterpCtx
emptyCtx input rawArgs = InterpCtx
    { ctxArgs = Map.empty
    , ctxDepth = 0
    , ctxInput = stringToRValue input
    , ctxRawArgs = map stringToRValue rawArgs
    }

-- | Push an argument onto the context (for lambda entry)
pushArg :: InterpCtx -> RValue -> InterpCtx
pushArg ctx@InterpCtx{..} val = ctx
    { ctxArgs = Map.insert (ctxDepth + 1) val ctxArgs
    , ctxDepth = ctxDepth + 1
    }

-- | Get argument by de Bruijn index (1-indexed from current depth)
getArg :: InterpCtx -> Int -> RValue
getArg InterpCtx{..} idx =
    case Map.lookup (ctxDepth - idx + 1) ctxArgs of
        Just v -> v
        Nothing -> error $ "Undefined arg index: " ++ show idx ++
                          " (depth=" ++ show ctxDepth ++ ")"

-- | Pop an argument from the context (for lambda exit)
popArg :: InterpCtx -> InterpCtx
popArg ctx@InterpCtx{..} = ctx
    { ctxArgs = Map.delete ctxDepth ctxArgs
    , ctxDepth = ctxDepth - 1
    }

-- | Run the full interpreter pipeline
runInterpreter :: InterpFn -> String -> [String] -> IO String
runInterpreter interpFn input rawArgs = do
    let ctx = emptyCtx input rawArgs
        result = interpFn ctx
    return $ rFinishLn result

-- | Evaluate with pre-created context
evalWithInput :: InterpFn -> InterpCtx -> RValue
evalWithInput fn ctx = fn ctx

-- ============================================================
-- Built-in Input Accessors (mirror Compile.hs letArgs)
-- ============================================================

-- | Get first integer from input
getFstInt :: InterpCtx -> RValue
getFstInt ctx = case getInts ctx of
    RList (x:_) -> x
    _ -> RInt 100  -- Default

-- | Get second integer from input
getSndInt :: InterpCtx -> RValue
getSndInt ctx = case getInts ctx of
    RList (_:y:_) -> y
    _ -> RInt 1000  -- Default

-- | Get all integers from input
getInts :: InterpCtx -> RValue
getInts ctx = case ctxRawArgs ctx of
    [] -> parseInts (ctxInput ctx)
    args -> RList $ concatMap (rToList . parseInts) args

-- | Get first line of input
getFstLine :: InterpCtx -> RValue
getFstLine ctx = case getLines ctx of
    RList (x:_) -> x
    _ -> RList $ map (RChr . fromIntegral . ord) $ ' ':['a'..'z']++
                 ".,!?_\n"++['A'..'Z']++['0'..'9']++
                 "-+:;\"'~`@#$%^&*()[]{}<>\\/=|"  -- printables

-- | Get second line of input
getSndLine :: InterpCtx -> RValue
getSndLine ctx = case getLines ctx of
    RList (_:y:_) -> y
    _ -> RList []

-- | Get all lines of input
getLines :: InterpCtx -> RValue
getLines ctx = case ctxRawArgs ctx of
    [] -> rLines (ctxInput ctx)
    args -> RList args

-- | Get all input as string
getAllInput :: InterpCtx -> RValue
getAllInput = ctxInput

-- | Get integer matrix from input
getIntMatrix :: InterpCtx -> RValue
getIntMatrix ctx = case ctxRawArgs ctx of
    [] -> RList $ filter (not . null . rToList) $
          rToList $ rMap (RFn parseInts) (rLines (ctxInput ctx))
    args -> RList $ filter (not . null . rToList) $
            map parseInts args

-- | Parse integers from a string RValue
parseInts :: RValue -> RValue
parseInts (RList xs) = RList $ parseIntsFromString $ map toChar xs
  where
    toChar (RChr c) = chr $ fromIntegral c
    toChar (RInt n) = chr $ fromIntegral n
    toChar _ = ' '
parseInts v = RList []

parseIntsFromString :: String -> [RValue]
parseIntsFromString "" = []
parseIntsFromString "-" = []
parseIntsFromString ('-':c:rest)
    | isDigit c = RInt (read ('-':c:num)) : parseIntsFromString after
    | otherwise = parseIntsFromString rest
    where
        (num, after) = span isDigit rest
        isDigit c = c >= '0' && c <= '9'
parseIntsFromString (c:rest)
    | isDigit c = RInt (read (c:num)) : parseIntsFromString after
    | otherwise = parseIntsFromString rest
    where
        (num, after) = span isDigit rest
        isDigit c = c >= '0' && c <= '9'

-- ============================================================
-- Combinator Builders for Operations
-- ============================================================

-- | Create a unary operation interpreter
unaryOp :: (RValue -> RValue) -> InterpFn -> InterpFn
unaryOp f argFn ctx = f (argFn ctx)

-- | Create a binary operation interpreter
binaryOp :: (RValue -> RValue -> RValue) -> InterpFn -> InterpFn -> InterpFn
binaryOp f arg1Fn arg2Fn ctx = f (arg1Fn ctx) (arg2Fn ctx)

-- | Create a ternary operation interpreter
ternaryOp :: (RValue -> RValue -> RValue -> RValue)
          -> InterpFn -> InterpFn -> InterpFn -> InterpFn
ternaryOp f arg1Fn arg2Fn arg3Fn ctx = f (arg1Fn ctx) (arg2Fn ctx) (arg3Fn ctx)

-- | Create a constant value interpreter
constVal :: RValue -> InterpFn
constVal v _ = v

-- | Create an argument reference interpreter
argRef :: Int -> InterpFn
argRef idx ctx = getArg ctx idx

-- | Create a lambda interpreter
lambdaFn :: (InterpFn -> InterpFn) -> InterpFn
lambdaFn bodyBuilder ctx = RFn $ \arg ->
    let ctx' = pushArg ctx arg
        bodyFn = bodyBuilder (argRef 1)  -- bodyBuilder gets access to the arg
    in bodyFn ctx'

-- | Create an application interpreter
applyFn :: InterpFn -> InterpFn -> InterpFn
applyFn fnInterp argInterp ctx =
    case fnInterp ctx of
        RFn f -> f (argInterp ctx)
        v -> error $ "Cannot apply non-function: " ++ show v

-- | Create a let binding interpreter
letFn :: InterpFn -> (InterpFn -> InterpFn) -> InterpFn
letFn valFn bodyBuilder ctx =
    let val = valFn ctx
        ctx' = pushArg ctx val
        bodyFn = bodyBuilder (argRef 1)
    in bodyFn ctx'
