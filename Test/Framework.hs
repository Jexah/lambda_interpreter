{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Framework where
import Lambda.Core.Eval
import Lambda.Core.Parser
import Lambda.Core.Lexer
import Lambda.Core.Pretty
import Lambda.Core.Exp


-------------------------------------------------------------------------------
-- | The sorts of tests we use.
data Test 
        -- | A test where we check that two computed values are the same.
        =  forall a. (Eq a, Show a) 
        => Same  String a a

        -- | A test where we check that two functions produce the same
        --   result when applied to some argument.
        | forall  a b. (Eq b, Show b)
        => Match String (a -> b) (a -> b) a


-- | Run a single test, printint the result to the console.
testOne :: Test -> IO Bool
testOne (Same name x1 x2)
 | x1 == x2
 = do   putStrLn $ "OK     " ++ name
        return True

 | otherwise
 = do   putStrLn $ "FAILED " ++ name
        putStrLn $ " expected: " ++ show x2
        putStrLn $ "      got: " ++ show x1
        return False

testOne (Match name f g x)
 | f x == g x
 = do   putStrLn $ "OK     " ++ name
        return True

 | otherwise
 = do   putStrLn $ "FAILED " ++ name
        putStrLn $ " expected: " ++ show (f x)
        putStrLn $ "      got: " ++ show (g x)
        return False


-------------------------------------------------------------------------------
-- | Tokenize a string.
tokenizes :: String -> [Token]
tokenizes 
 = tokenize


-------------------------------------------------------------------------------
-- | Tokenise and parse the type in this string.
sparset  :: String -> Maybe Type
sparset str
 = parseType (tokenize str)


-- | Like `pparset`, but pretty print the result.
pparset :: String -> Maybe String
pparset str = fmap prettyType $ sparset str


-------------------------------------------------------------------------------
-- | Tokenise and parse the expression in this string.
sparsex  :: String -> Maybe Exp
sparsex str
 = parseExp (tokenize str)


-- | Like `sparse`, but pretty print the result.
pparsex :: String -> Maybe String
pparsex str = fmap prettyExp $ sparsex str


-------------------------------------------------------------------------------
-- | Single step reduction of the expression in this string.
sstep :: String -> Either ErrorEval Exp
sstep str 
 = case sparsex str of
        Nothing
         -> Left ErrorEvalNoParse

        Just xx  
         -> case step [] xx of
                StepNone        -> Right xx
                StepSome xx'    -> Right xx'
                StepError err   -> Left $ ErrorEvalStep err


-- | Like `sstep`, but pretty print the result.
pstep :: String -> Either ErrorEval String
pstep str = fmap prettyExp $ sstep str


-------------------------------------------------------------------------------
-- | Reduce an expression at most the given number of steps.
ssteps :: Int -> String -> Either ErrorEval Exp
ssteps n str
 = case sparsex str of
        Nothing
         -> Left ErrorEvalNoParse

        Just xx
         -> case steps n [] xx of
                StepNone        -> Right xx
                StepSome xx'    -> Right xx'
                StepError err   -> Left $ ErrorEvalStep err


-- | Like `ssteps`, but pretty print the result.
psteps :: Int -> String -> Either ErrorEval String
psteps n str = fmap prettyExp $ ssteps n str


-------------------------------------------------------------------------------
-- | Evalaute the expression in this string to normal form.
seval :: String -> Either ErrorEval Exp
seval str
 = case sparsex str of
        Nothing
         -> Left ErrorEvalNoParse

        Just xx
         -> case eval [] xx of
                StepNone        -> Right xx
                StepSome xx'    -> Right xx'
                StepError err   -> Left $ ErrorEvalStep err


-- | Like `seval`, but pretty print the result.
peval :: String -> Either ErrorEval String
peval str = fmap prettyExp $ seval str


-- | Errors that can arise when reducing an expression.
data ErrorEval
        = ErrorEvalNoParse
        | ErrorEvalStep ErrorStep
        deriving (Show, Eq)

