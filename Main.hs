{-# OPTIONS_GHC -Wall #-}

-- | A simple Lambda Calculus Interpreter.
module Main where
import Lambda.Repl.Macros
import Lambda.Repl
import qualified System.Console.Haskeline       as H


-- | Entry point for the interpreter REPL.
--
--   You can either load this file into ghci, and evaluate 'main', 
--   or run it from the unix command-line with:
--
-- @
--    ghc Main.hs -e "main"
-- @
--
--   * The 'IO ()' is a type signature that says this computation
--     may perform some IO actions, and returns a unit value.
--
main :: IO ()
main 
 = do   -- Load macros from a file in the working directory.
        macros  <- loadMacros "Prelude.macros"

        -- Start the REPL.
        --  We use an external library called "Haskeline" to manage input.
        --  http://hackage.haskell.org/package/haskeline
        H.runInputT H.defaultSettings (repl macros)



