{-# OPTIONS_GHC -Wall #-}

-- | A simple LL(k) parser for lambda expressions.
module Lambda.Core.Parser where
import Lambda.Core.Exp 
import Lambda.Core.Lexer


-- | Tokenise a string and then try to parse it.
loadExp  :: String  -> Maybe Exp
loadExp = parseExp . tokenize


-------------------------------------------------------------------------------
parseType :: [Token] -> Maybe Type
parseType ts
 = case parseTypeSome ts of
        -- We completely parsed the tokens.
        Just (tt, [])   -> Just tt

        -- We parse some prefix of the tokens, 
        -- but there is junk at the end that isn't part of the type.
        Just (_, _)     -> Nothing

        -- Some other parse error.
        Nothing         -> Nothing


-- | Try to parse some tokens as a type.
parseTypeSome :: [Token] -> Maybe (Type, [Token])
parseTypeSome ks
 = case parseTypeBit ks of
        Just (t1, KArrow : ks' ) 
         -> case parseTypeSome ks' of
                Nothing         -> Nothing
                Just (t2, ks'') -> Just (TFun t1 t2, ks'')

        result -> result


-- | Parse a type.
--   or `Nothing` if there was a parse error.
parseTypeBit :: [Token] -> Maybe (Type, [Token])
parseTypeBit (KBra : ks)
        | Just (t, KKet : ks')  <- parseTypeSome ks
        = Just (t, ks')

parseTypeBit (KCon str : ks)
 = let
        tPrim 
         = case lookup str typePrimitives of
                Just p  -> p
                Nothing -> error $ "parseTypeBit: no name for " ++ show str

   in   Just (TPrim tPrim, ks)

parseTypeBit _
        = Nothing


-------------------------------------------------------------------------------
-- | Parse a lambda expression, 
--   or `Nothing` if there was a parse error.
parseExp :: [Token] -> Maybe Exp
parseExp ks
 = case parseExpSome ks of
        -- We completely parsed the tokens.
        Just (xx, [])   -> Just xx

        -- We parsed some prefix of the tokens,
        -- but there was junk at the end that isn't part of the expression.
        Just (_, _)     -> Nothing

        -- Some other parse error.
        Nothing         -> Nothing
        

-- | Try to parse some tokens as an expression.
--
--   The standard grammar for lambda calculus is left recursive in the 
--   production for applications, which doesn't work with top-down LL
--   parsers. We get around this by using the standard trick of parsing
--   each component of the application independently then building
--   the application structure as a post process.
--   
parseExpSome :: [Token] -> Maybe (Exp, [Token])
parseExpSome ks
 = case parseExpBits ks of
        (x : xs, ks')   -> Just (buildExpApp x xs, ks')
        ([],     _)     -> Nothing

 
-- | Parse a sequence of non-application expressions.
parseExpBits :: [Token] -> ([Exp], [Token])
parseExpBits ks
 = case parseExpBit ks of
        Nothing         
         -> ([], ks)

        Just (e, ks')   
         -> let (es, rest)      = parseExpBits ks'
            in  (e : es, rest)


-- | Parse some non-application expressions.
parseExpBit :: [Token] -> Maybe (Exp, [Token])

-- Parse a compound expression, like (exp).
parseExpBit (KBra : ks)
        | Just (e, KKet : ks')  <- parseExpSome ks
        = Just (e, ks')

-- Parse a variable.
parseExpBit (KVar str : ks)        
        = Just (XVar (V str), ks)

-- Try to parse the untyped version, 
-- without an annotation on the variable.
parseExpBit (KLam : KVar str : KDot : ks)
        | Just (e, ks')         <- parseExpSome ks
        = Just (XAbs (V str) Nothing e, ks')

-- Try to parse the type version,
-- with an annotation on the variable.
parseExpBit (KLam : KVar str : KColon : ks)
        | Just (t, KDot : ks')  <- parseTypeSome ks
        , Just (e, ks'')        <- parseExpSome  ks'
        = Just (XAbs (V str) (Just t) e, ks'')

parseExpBit (KMacro str : ks)      
        = Just (XMacro (M str), ks)

parseExpBit (KInt n : ks)
        = Just (XPrim (PNat n), ks)

parseExpBit (KPrim str : ks)
 = let  
        xPrim   
         = case lookup str expPrimitives of
                Just p  -> p
                Nothing -> error $ "parseExpBit: no name for " ++ show str

   in   Just (XPrim xPrim, ks)

parseExpBit _
        = Nothing


-- | Create some left-associated applications from a
--   list of expressions.
--   
--    Eg: buildExpApp [x1, x2, x3] => (x1 x2) x3
--
buildExpApp :: Exp -> [Exp] -> Exp
buildExpApp x0 xx0
 = buildExpApp' $ reverse (x0 : xx0)
 where 
       buildExpApp' xx
        = case xx of
           []           -> error "buildApp': list should be non-empty"
           x : []       -> x
           x : xs       -> XApp (buildExpApp' xs) x

        