{-# OPTIONS_GHC -Wall #-}

-- | Pretty printing of lambda expressions
module Lambda.Core.Pretty where
import Lambda.Core.Exp
import qualified Data.List              as List


-------------------------------------------------------------------------------
-- | Pretty print a type.
prettyType :: Type -> String
prettyType xx
 = case xx of
        TFun t1 t2
         -> prettyTypeLeft t1 ++ " -> " ++ prettyType t2

        TPrim p
         -> case List.find (\t -> snd t == p) typePrimitives of
             Just (name, _) -> name
             Nothing        -> error $ "pretty: no name defined for " ++ show p

        TPair e1 e2
         -> "(" ++ prettyType e1 ++ ", " ++ prettyType e2 ++ ")"

        TAnd t1 t2
         -> "(" ++ prettyType t1 ++ ", " ++ prettyType t2 ++ ")"

        TOr t1 t2
         -> "(" ++ prettyType t1 ++ ", " ++ prettyType t2 ++ ")"

-- | Pretty print a type on the left of an arrow.
prettyTypeLeft :: Type -> String
prettyTypeLeft tt
 = case tt of
        TFun{}          -> parens (prettyType tt)
        _               -> prettyType tt


-------------------------------------------------------------------------------
-- | Pretty print an expression.
prettyExp :: Exp -> String
prettyExp xx
 = case xx of
        -- Variables.
        XVar (V var)    
         -> var

        -- Abstractions.
        XAbs (V var) (Just t) e  
         -> "\\" ++ var ++ " : " ++ prettyType t ++ ". " ++ prettyExp e

        XAbs (V var) Nothing e  
         -> "\\" ++ var ++ ". " ++ prettyExp e

        -- The expressions in an application are printed differently
        -- depending on if they are compound expressions.
        XApp e1 e2      
         -> prettyExpLeft e1 ++ " " ++ prettyExpRight e2

        -- Macros are printed with a leading '#'.
        XMacro (M str)  
         -> "#" ++ str

        -- Literal values.
        XPrim (PNat n)      -> show n
        XPrim (PBool True)  -> "true"
        XPrim (PBool False) -> "false"
        XPrim (PPair p) -> show p

        -- For primitives which are not literal values, 
        -- lookup their names in the primitives table.
        XPrim p
         -> case List.find (\t -> snd t == p) expPrimitives of
             Just (name, _) -> name
             Nothing        -> error $ "pretty: no name defined for " ++ show p


-- | Pretty print an expression on the left of an application.
prettyExpLeft :: Exp -> String
prettyExpLeft xx
 = case xx of
        XAbs{}          -> parens (prettyExp xx)
        _               -> prettyExp xx


-- | Pretty print an expression on the right of an application.
prettyExpRight :: Exp -> String
prettyExpRight xx
 = case xx of
        XAbs{}          -> parens (prettyExp xx)
        XApp{}          -> parens (prettyExp xx)
        _               -> prettyExp xx


-- | Wrap a string in parenthesis.
parens :: String -> String
parens ss
 = "(" ++ ss ++ ")"

