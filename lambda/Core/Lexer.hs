{-# OPTIONS_GHC -Wall #-}

-- | A lexer for lambda expression syntax.
module Lambda.Core.Lexer where
import Lambda.Core.Exp
import qualified Data.Char      as C


-- | A token in lambda expression syntax.
data Token
        = KError Char           -- ^ Some junk or invalid character.
        | KBra                  -- ^ Open braket (
        | KKet                  -- ^ Close braket )
        | KLam                  -- ^ Lambda (\)
        | KDot                  -- ^ Dot (.)
        | KColon                -- ^ Colon (:)

        | KAnd                  -- ^ And
        | KAmpersand            -- ^ Ampersand (&)

        | KArrow                -- ^ Arrow (->)

        | KVar   String         -- ^ Variable names.
        | KCon   String         -- ^ Constructor names.
        | KMacro String         -- ^ Macro names.

        | KInt   Int            -- ^ Integer.
        | KPrim  String         -- ^ Primitive value.
        deriving (Show, Eq)


-- | Tokenize a string.
tokenize :: String -> [Token]
tokenize []             =  []
tokenize (c:cs)

        -- Ignore whitespace.
        | C.isSpace c     = tokenize cs

        -- Multiple character symbols.
        | '-' : '>' : cs' <- (c : cs)
        = KArrow : tokenize cs'

        -- Single character symbols.
        | c == '('      = KBra   : tokenize cs
        | c == ')'      = KKet   : tokenize cs
        | c == '\\'     = KLam   : tokenize cs
        | c == '.'      = KDot   : tokenize cs
        | c == ':'      = KColon : tokenize cs

        -- Macro names start with a '#' symbol.
        | c == '#'
        , name          <- takeWhile C.isAlphaNum cs
        , rest          <- drop (length name) cs
        = KMacro name : tokenize rest

        -- Integer
        | C.isDigit c
        = let   restOfNat       = takeWhile C.isDigit cs
                str             = c : restOfNat

                restOfString    = drop (length restOfNat) cs
          in    KInt (read str) : tokenize restOfString

        -- Constructor names must start with an uppercase alphabetic character.
        -- Subsequent characters can be alpha or numeric.
        | C.isUpper c
        = let   restOfName      = takeWhile C.isAlphaNum cs
                name            = c : restOfName

                restOfString    = drop (length restOfName) cs

          in    (if elem name primNames
                  then KPrim name
                  else KCon  name) : tokenize restOfString

        -- Variable names must start with a lowercase alphabetic charater.
        -- Subsequent characters can be alpha or numeric.
        | C.isLower c     
        = let   restOfName      = takeWhile C.isAlphaNum cs
                name            = c : restOfName

                restOfString    = drop (length restOfName) cs

          in    (if elem name primNames
                  then KPrim name
                  else KVar  name) : tokenize restOfString

        -- If we see a junk character then stop scanning.
        | otherwise
        = [KError c]

 where  primNames = map fst expPrimitives

