{-# OPTIONS_GHC -Wall #-}

-- | Lambda Core Calculus type checker.
module Lambda.Core.Check where
import Lambda.Core.Exp

-- | Check an expression for problems.
check   :: [(Macro, Exp)]       -- ^ Table of macros.
        -> [(Var,   Type)]      -- ^ Types of free variables (the type environment)
        -> Exp                  -- ^ Expression to check.
        -> Either ErrorType Type
 

-- | This is just a placeholder return value.
--   Not all expressions have type Bool.
check _macros _env _xx
 = case _xx of 
    -- | Lambda abstraction case
    --   XAbs (value (Maybe (Type)) Exp)
    XAbs v mt e 
     -> case mt of
        -- | Variable type not defined in abstraction, return err.
        Nothing  -> Left ErrorTypeAbsLocalTypeNotDefined
        -- | Type is specified, add variable and type to environment table.
        Just t   -> case check _macros (_env ++ [(v, t)]) e of
                        Left err -> Left err
                        Right et -> Right (TFun t et)

    -- | Variable case
    --   XVar variableName
    XVar vn
     -> case lookup vn _env of
             -- Variable name not found, return err.
             Nothing     -> Left (ErrorTypeUnboundVar vn)
             -- Variable name found in environment table, return type.
             Just t      -> Right t
        
    -- | If case
    --   XApp (XApp (XApp (XPrim PIf) Boolean) Exp) Exp
    XApp (XApp (XApp (XPrim PIf) b) e1) e2
     -> case (check _macros _env b) of
            -- | Err found in conditional statement, return err.
            Left err -> Left err
            -- | Conditional statement is a TBool, check Expression 2.
            Right (TPrim TBool)
             -> case check _macros _env e2 of
                -- | Expression 2 evaluates to an error, return err.
                Left err -> Left err
                -- | Expression 2 evaluates to a type, check Expression 1.
                Right t2
                 -> case check _macros _env e1 of
                    -- | Expression 1 evaluates to an error, return err.
                    Left err -> Left err
                    -- | Expression 2 evaluates to a type
                    Right t1
                     -> if (t1 == t2) 
                         -- | The two types are equal, and the conditional is a bool, so the return
                         --   type == t1 == t2.
                         then Right t1 
                         -- | t1 and t2 are not equal, so the return type of the if statement is not
                         --   valid.
                         else Left ErrorTypeIfResolvedTypesNotMatched
            -- | The conditional statement does not return a error, and is not a bool, so return
            --   a new error.
            Right _ -> Left ErrorTypeIfNotBool
            

    XApp (XApp (XPrim PPairCons) e1) e2
     -> case check _macros _env e1 of
             Left err -> Left err
             Right t1 -> case check _macros _env e2 of
                             Left err -> Left err
                             Right t2 -> Right (TPair t1 t2)
            
    XApp (XPrim PFst) (XPrim (PPair p))
     -> check _macros _env (fst p)

    XApp (XPrim PFst) (XApp (XApp (XPrim PPairCons) e1) _)
     -> check _macros _env (e1)

    XApp (XPrim PSnd) (XPrim (PPair p))
     -> check _macros _env (snd p)

    XApp (XPrim PSnd) (XApp (XApp (XPrim PPairCons) _) e2)
     -> check _macros _env (e2)


    -- | All XApp occurances besides correctly formatted if.
    --   XApp Exp Exp
    XApp e1 e2
     -> case check _macros _env e2 of
            -- | Expression 2 is evaluates to an err.
            Left err -> Left err
            -- | The resolved type of Expression 2 is t2, so check the input type of Expression 1
            Right t2
             -> case check _macros _env e1 of
                    -- | Expression 1 evaluates to an err.
                    Left err -> Left err
                    -- | Expression 1 is of the type Function(t1):t2'
                    Right (TFun t1 t2')
                     -> if(t2 == t2')
                         -- | The input type of Expression 1 is equal to the type of Expression 2, so 
                         --   both expressions are valid and return type == t1
                         then Right t1
                         -- | The input type for Expression 2 is different to the type of Expression 1
                         --   so return an error.
                         else Left (ErrorTypeAppArgumentNotSameType)
                    -- | Type resolution of Expression 1 is not of the format (TFun t1 t2') and is
                    --   not an error, so return a new error.
                    Right _ -> Left ErrorTypeAppNotFunction


    -- | Macro case
    --   XMacro Macro
    XMacro m
     -> case lookup m _macros of
            -- | No macro found, return err.
            Nothing -> Left (ErrorTypeNoSuchMacro m)
            -- | Macro found, return type evaluation of expression.
            Just e  -> check _macros _env e

    -- | Primitive case
    --   XPrim Exp
    XPrim e
     -> case e of
            PNat _    -> Right (TPrim TNat)
            PBool _   -> Right (TPrim TBool)
            PPair p   -> case check _macros _env (fst p) of
                               Left err -> Left err
                               Right t1 -> case check _macros _env (snd p) of
                                            Left err -> Left err
                                            Right t2 -> Right (TPair t1 t2)
            PNot      -> Right $ TFun (TPrim TBool) (TPrim TBool)
            POr       -> Right $ TFun (TFun (TPrim TBool) (TPrim TBool)) (TPrim TBool)
            PAnd      -> Right $ TFun (TFun (TPrim TBool) (TPrim TBool)) (TPrim TBool)
            PAdd      -> Right $ TFun (TFun (TPrim TNat) (TPrim TNat)) (TPrim TNat)
            PEquals   -> Right $ TFun (TFun (TPrim TBool) (TPrim TNat)) (TPrim TNat)
            PIf       -> Left ErrorTypeIfMalformed
            PPairCons -> Left ErrorTypePairMalformed
            PFst      -> Left ErrorTypePairMalformedFst                                     -- =========================
            PSnd      -> Left ErrorTypePairMalformedSnd

-- | Problems we can detect when type checking expressions.
data ErrorType
        -- | Variable is not present in the environment.
        = ErrorTypeUnboundVar Var

        -- | Scrutinee of an if-expression is not a Bool.
        | ErrorTypeIfNotBool

        | ErrorTypePairMalformed
        | ErrorTypePairMalformedFst
        | ErrorTypePairMalformedSnd

        -- | Abstraction local variable type not defined.
        | ErrorTypeAbsLocalTypeNotDefined

        -- | The first expression in XApp is not a function of type TFun Exp Exp.
        | ErrorTypeAppNotFunction

        -- | The two expressions in the if statement do not resolve to the same type.
        | ErrorTypeIfResolvedTypesNotMatched

        -- | The type of the local variable in a lambda abstraction does not match the input type.
        | ErrorTypeAppArgumentNotSameType

        -- | The macro is not found in the macros list.
        | ErrorTypeNoSuchMacro Macro

        -- | If statement does not match the if (TBool) (Exp) (Exp) format.
        | ErrorTypeIfMalformed

        deriving (Eq, Show)
