{-# OPTIONS_GHC -Wall #-}

-- | Lambda Core Calculus Expressions.
module Lambda.Core.Exp where
import Data.Set                 (Set)
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
-- | A variable name represented as a String.
data Var        
        = V String
        deriving (Show, Eq, Ord)


-- | A macro name represented as a String.
data Macro
        = M String
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
-- | Type abstract syntax.
data Type
        = TFun  Type Type
        | TPrim TypePrim
        | TPair Type Type
        | TAnd  Type Type
        | TOr   Type Type
        deriving (Show, Eq)


-- | Type primitives.
data TypePrim
        = TBool
        | TNat
        deriving (Show, Eq)


-- | Primitive types and their names.
typePrimitives :: [(String, TypePrim)]
typePrimitives 
 =      [ ("Bool",      TBool)
        , ("Nat",       TNat) ]


-------------------------------------------------------------------------------
-- | Expressions abstract syntax.
data Exp
        -- | Variables.
        = XVar Var

        -- | Lambda abstractions,
        --   with an optional type annotation.
        | XAbs Var (Maybe Type) Exp 

        -- | Function applications.
        | XApp Exp Exp

        -- | A macro expansion.
        | XMacro Macro

        -- | A primitive operator.
        | XPrim  ExpPrim
        deriving (Show, Eq)


-- | Primitives.
data ExpPrim
        = PNat  Int
        | PBool Bool
        | PPair (Exp, Exp)
        | PPairCons
        | PNot
        | POr
        | PAnd
        | PAdd
        | PEquals
        | PIf
        | PFst
        | PSnd
        deriving (Show, Eq)


-- | Primitive operators and their names.
expPrimitives :: [(String, ExpPrim)]    
expPrimitives
 =      [ ("true",      PBool True)
        , ("false",     PBool False)
        , ("not",       PNot)
        , ("or",        POr)
        , ("and",       PAnd)
        , ("add",       PAdd) 
        , ("equals",    PEquals) 
        , ("if",        PIf) 
        , ("pair",      PPairCons) 
        , ("fst",       PFst) 
        , ("snd",       PSnd) ]


-- | Check if an expression is a variable.
isXVar :: Exp -> Bool
isXVar xx
 = case xx of
        XVar _          -> True
        _               -> False


-- | Check if an expression is a macro name.
isXMacro :: Exp -> Bool
isXMacro xx
 = case xx of
        XMacro _        -> True
        _               -> False


-- | Get all the variables mentioned in an expression,
--   both free and bount.
allVars  :: Exp -> Set Var
allVars xx
 = case xx of
        XVar v          -> Set.singleton v
        XAbs v _ x1     -> Set.insert v (allVars x1)
        XApp x1 x2      -> Set.union (allVars x1) (allVars x2)
        XMacro{}        -> Set.empty
        XPrim{}         -> Set.empty


-- | Get a set of free variables in this expression.
freeVars :: Exp -> Set Var
freeVars xx
 = case xx of
        XVar v          -> Set.singleton v
        XAbs v _ x1     -> Set.delete v (freeVars x1)
        XApp x1 x2      -> Set.union (freeVars x1) (freeVars x2)
        XMacro{}        -> Set.empty
        XPrim{}         -> Set.empty

