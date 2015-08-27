{-# OPTIONS_GHC -Wall #-}

module Lambda.Core.Eval where
import Lambda.Core.Exp
import Lambda.Core.Substitute


-- | Reduce the given expression to normal form.
eval  :: [(Macro, Exp)] -> Exp -> StepResult Exp
eval macros xxStart
 = loop False xxStart
 where
        loop progress xx
         = case step macros xx of
                StepNone        
                 | progress     -> StepSome xx
                 | otherwise    -> StepNone

                StepSome xx'    -> loop True xx'
                StepError err   -> StepError err


-- | Perform at most the given number of steps.
steps :: Int -> [(Macro, Exp)] -> Exp -> StepResult Exp
steps nMax macros xxStart
 = loop nMax False xxStart
 where
        loop 0 progress xx
         | progress     = StepSome xx
         | otherwise    = StepNone

        loop n progress xx
         = case step macros xx of
                StepNone        -> loop 0       progress xx
                StepSome xx'    -> loop (n - 1) True     xx'
                StepError err   -> StepError err


-- | Perform a single step reduction.
--   Left-to-right evaluation order, reducing under lambdas.
--
--   The only runtime error we can encounter is discovering a macro with
--   no definition. If this happens we return the offending macro name.
-- 
step :: [(Macro, Exp)] -> Exp -> StepResult Exp
step macros xx
 = case xx of
        -- | Variables and macro names are already in normal form.
        XVar _            -> StepNone
        XMacro _          -> StepNone

        -- | Reduce under lambda abstractions.
        XAbs v mt x
         -> case step macros x of
                StepNone        -> StepNone
                StepSome x'     -> StepSome (XAbs v mt x')
                StepError err   -> StepError err

        -- | When the left of an application is an abstraction 
        --   we can perform a (capture avoiding) substitution.
        XApp (XAbs v _ x11) x2    
         -> StepSome $ subNoCap v x2 x11

        -- | If the left of an application is a macro,
        --   then expand the macro in a separate step.
        XApp (XMacro m) x2    
         -> case lookup m macros of
                Nothing  -> StepError $ ErrorStepUnknownMacro m
                Just x1' -> StepSome  $ XApp x1' x2 

        -- | Reduce a negation.
        XApp (XPrim PNot) (XPrim (PBool b))
         -> StepSome $ XPrim (PBool $ not b)

        -- | Reduce a disjunction.
        XApp (XApp (XPrim POr) (XPrim (PBool b1))) (XPrim (PBool b2))
         -> StepSome $ XPrim (PBool $ b1 || b2)

        -- | Reduce a conjunction.
        XApp (XApp (XPrim PAnd) (XPrim (PBool b1))) (XPrim (PBool b2))
         -> StepSome $ XPrim (PBool $ b1 && b2)

        -- | Reduce an addition.
        XApp (XApp (XPrim PAdd) (XPrim (PNat n1))) (XPrim (PNat n2))
         -> StepSome $ XPrim (PNat (n1 + n2))

        -- | Reduce an equality.
        XApp (XApp (XPrim PEquals) (XPrim (PNat n1))) (XPrim (PNat n2))
         -> StepSome $ XPrim (PBool (n1 == n2))

        -- | Reduce a branch.
        XApp (XApp (XApp (XPrim PIf) (XPrim (PBool b))) e1) e2
         -> StepSome $ if b then e1 else e2

        -- | Reduce a pair.
        XApp (XApp (XPrim PPairCons) e1) e2
         -> StepSome $ XPrim $ PPair (e1, e2)

        -- | Reduce a fst.
        XApp (XPrim PFst) (XPrim (PPair p))
         -> StepSome $ fst p

        -- | Reduce a snd.
        XApp (XPrim PSnd) (XPrim (PPair p))
         -> StepSome $ snd p



        -- | Try and reduce expressions deep with applications,
        --   but still only one-at-a-time.
        XApp x1 x2 
         -> case step macros x1 of
                StepNone        
                 -> case step macros x2 of
                        StepNone        -> StepNone
                        StepSome x2'    -> StepSome $ XApp x1 x2'
                        StepError err   -> StepError err

                StepSome x1'            -> StepSome $ XApp x1' x2
                StepError err           -> StepError err

        XPrim _        
         -> StepNone


-- | Result of stepping an expression.
data StepResult a
        -- | We don't have a rule to reduce the expression any further.
        = StepNone  

        -- | We reduced the expression to this simpler form.
        | StepSome      a

        -- | Reducing this expression caused an error.
        | StepError     ErrorStep
        deriving Show


-- | Things that can go wrong when stepping an expression.
data ErrorStep
        = ErrorStepUnknownMacro Macro
        deriving (Show, Eq)


instance Functor StepResult where
        fmap _ StepNone         = StepNone
        fmap f (StepSome x)     = StepSome (f x)
        fmap _ (StepError  err) = StepError err

