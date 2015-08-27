{-# OPTIONS_GHC -Wall #-}

module Main 
        ( module Test.Framework
        , test)
where
import Test.Framework
import Lambda.Core.Exp
import Lambda.Core.Check
import Data.Maybe
import Lambda.Core.Parser
import Lambda.Core.Lexer

-- | Run all the tests, printing the results to the console.
--
--   You can either load this file in ghci and evaluate 'test',
--   or run it from the unix command-line with:
--
-- @
--   ghc Test.hs -e "test"
-- @
--
test :: IO Bool
test = fmap and $ mapM testOne tests


teval :: String -> Exp
teval s = fromJust $ parseExp $ tokenize s

tests :: [Test]
tests = 
 -- Type parser.
 [ Match "parsing Nat type"   
        pparset Just 
        "Nat"

 , Match "parsing function type"
        pparset Just
        "(Nat -> Bool) -> Nat"

 -- Expression parser.
 , Match "parsing expressions"
        pparsex Just
        "if (equals (add 2 2) 4) 10 (\\x : Nat. x)"

 -- Reduction.
 , Same  "eval add 2 3"
        (peval "add 2 3")
        (Right "5")

 , Same  "eval add 2 (add 4 5)"
        (peval "add 2 (add 4 5)")
        (Right "11")

 , Same  "eval add (add 2 4) (add 4 5)"
        (peval "add (add 2 4) (add 4 5)")
        (Right "15")

 , Same  "check (teval 'add 2 (add 4 5)'"
        (check [] [] (teval "add 2 (add 4 5)"))
        (Right (TPrim TNat))

 , Same  "check (teval 'add (add 4 5) 2')"
        (check [] [] (teval "add (add 4 5) 2"))
        (Right (TPrim TNat))

 , Same  "eval equals 1 1"
        (peval "equals 1 1")
        (Right "true")

 , Same  "eval equals 1 2"
        (peval "equals 1 2")
        (Right "false")

 , Same  "check (teval 'equals 1 1')"
        (check [] [] (teval "equals 1 1"))
        (Right (TPrim TBool))

 , Same  "eval equals 1 false"
        (check [] [] (teval "equals 1 false"))
        (Left ErrorTypeAppArgumentNotSameType)

 , Same  "eval and true true"
        (peval "and true true")
        (Right "true")

 , Same  "eval and false true"
        (peval "and false true")
        (Right "false")

 , Same  "eval or false true"
        (peval "or false true")
        (Right "true")

 , Same  "eval or false false"
        (peval "or false false")
        (Right "false")

 , Same  "eval or true false"
        (peval "or true false")
        (Right "true")

 , Same  "eval or true true"
        (peval "or true true")
        (Right "true")

 , Same  "eval not true"
        (peval "not true")
        (Right "false")

 , Same  "eval not false"
        (peval "not false")
        (Right "true")

 , Same  "eval not (not true)"
        (peval "not (not true)")
        (Right "true")

 , Same  "check (peval 'and true 4')"
        (check [] [] (teval "and true 4"))
        (Left ErrorTypeAppArgumentNotSameType)

 , Same  "check (peval 'add 5 (and true false)'"
        (check [] [] (teval "add 5 (and true false)"))
        (Left ErrorTypeAppArgumentNotSameType)

 , Same  "check undefined variable x"
        (check [] [] (XVar (V "x")))
        (Left (ErrorTypeUnboundVar (V "x")))

 , Same  "pair 1 2"
        (check [] [] (teval "pair 1 2"))
        (Right (TPair (TPrim TNat) (TPrim TNat)))

 , Same  "pair 5 true"
        (check [] [] (teval "pair 5 true"))
        (Right (TPair (TPrim TNat) (TPrim TBool)))

 , Same  "eval fst (pair 1 true)"
        (peval "fst (pair 1 true)")
        (Right "1")

 , Same  "eval fst (pair (if false 5 1) true)"
        (peval "fst (pair (if false 5 1) true)")
        (Right "1")

 , Same  "eval fst (pair (snd (pair 1 2)) true)"
        (peval "fst (pair (snd (pair 1 2)) true)")
        (Right "2")

 -- This expression is ill-typed and stuck.
 -- The evaluator cannot reduce it further.
 , Same  "eval bad 1"
        (peval "add 2 (add 3 not) false")
        (Right "add 2 (add 3 not) false")

 -- If-expressions don't work in the assignment distribution.
 , Same  "eval if"
        (peval "if (equals (add 2 3) 5) 10 20")
        (Right "10")

 , Same  "eval if"
        (peval "if (true) (if (equals (add 2 3) 5) 10 20) false")
        (Right "10")
        
 , Same  "check conditional"
        (Right (TPrim TBool))
        (check [] [] (XApp (XApp (XApp (XPrim PIf) (XPrim (PBool True))) (XPrim (PBool True))) (XPrim (PBool False))))
  ]




