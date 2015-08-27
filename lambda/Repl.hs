{-# OPTIONS_GHC -Wall #-}

module Lambda.Repl where
import Lambda.Core.Lexer
import Lambda.Core.Parser
import Lambda.Core.Eval
import Lambda.Core.Check
import Lambda.Core.Pretty
import Lambda.Core.Exp
import qualified Control.Monad.IO.Class         as C
import qualified System.Console.Haskeline       as H
import qualified Data.List                      as List


-- | The Read-Eval-Print-Loop.
--
--   * The 'InputT IO ()' means this computation gets some input from the
--     console, as well as performing some IO actions.
--
repl :: [(Macro, Exp)] -> H.InputT IO ()
repl macros
 = loop
 where  loop
         = do   -- Get a line from the console, using the given prompt.
                minput <- H.getInputLine "> "
                case minput of

                 -- User closed stdin, so we're done.
                 Nothing        -> return ()

                 -- Explicit quit command.
                 Just ":quit"   -> return ()

                 -- Check the expression and report its type.
                 -- For example:
                 -- 
                 --    > :type (\x : Nat. x)
                 --    Nat -> Nat
                 --
                 Just str
                  |  Just str'  <- List.stripPrefix ":type " str
                  -> do 
                        _ <- C.liftIO
                          $  parseIO str'
                          $  \x -> checkIO macros x
                          $  \t -> do putStrLn (prettyType t)
                                      return (Just ())
                        loop

                 -- If no particular command has been specified then evaluate
                 -- the expression.
                 Just str     
                  -> do 
                        _ <- C.liftIO
                          $  parseIO str
                          $  \x -> checkIO  macros x
                          $  \_ -> reduceIO macros x
                          $  \_ -> return (Just ())

                        loop


-- | Try to parse an expression,
--   printing an error message if there is one.
parseIO :: String                       -- ^ String to parse.
        -> (Exp -> IO (Maybe b))        -- ^ Continue with parsed expression.
        -> IO (Maybe b)

parseIO str continue
 = case tokenize str of
        -- No tokens. Input was empty.
        [] 
         ->     return Nothing

        tokens 
         -> case parseExp tokens of
                Nothing
                 -> do  putStrLn $ "Parse Error\n"
                        return Nothing
                Just xx
                 ->     continue xx


-- | Try to type check an expression,
--   printing an error message if there is one.
checkIO :: [(Macro, Exp)]               -- ^ Macros.
        -> Exp                          -- ^ Expression to check.
        -> (Type -> IO (Maybe b))       -- ^ Continue with expression type.
        -> IO (Maybe b)

checkIO macros xx continue
 = case check macros [] xx of
        Left err
         -> do  putStrLn $ "error: " ++ show err
                return Nothing
        Right t
         ->     continue t


-- | Reduce an expression,
--   printing each intermediate step in the reduction.
reduceIO
        :: [(Macro, Exp)]               -- ^ Macros.
        -> Exp                          -- ^ Expression to check.
        -> (Exp -> IO (Maybe b))        -- ^ Continue with normal form.
        -> IO (Maybe b)

reduceIO macros xx0 continue
 = loop (0 :: Int) xx0
 where
        loop count xx
         = case step macros xx of
                StepNone
                 | count == 0
                 -> do  putStrLn $ prettyExp xx
                        putStr "\n"
                        continue xx

                 | otherwise        
                 -> do  putStrLn $ "[" ++ show count ++ " steps]"
                        putStr "\n"
                        continue xx

                StepSome xx'
                 -> do  putStrLn $ prettyExp xx'

                        -- If the evaluator alleges the expression as stepped, 
                        -- but it's still the same as before then we've diverged.
                        -- Bail out now to avoid running forever.
                        if xx == xx'
                         then do putStrLn "...\n"
                                 continue xx

                         else loop (count + 1) xx'

                StepError err
                 -> do  -- Some runtime error (crash).
                        -- This shouldn't happen with well typed expressions.
                        putStrLn $ "ERROR: " ++ show err
                        putStr "\n"                                               
                        return Nothing

