{-# OPTIONS_GHC -Wall #-}

module Lambda.Core.Substitute where
import Lambda.Core.Exp
import Data.Set         (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Do a capture avoiding substitution,
--   renaming binders as needed.
subNoCap :: Var -> Exp -> Exp -> Exp
subNoCap vSub xSub xx0
 = down Map.empty xx0
 where
        -- Collect the free variables in the expression to substitute.
        -- We do this once at the start instead of at every node
        -- on the way down.
        freeX 
         = freeVars xSub

        -- Decend into the expression looking for variables that
        --   match the one we're subtituting for.
        -- We also keep a map of variables names we've needed to 
        -- freshen to avoid variable capture. 
        down rw xx
         = case xx of
            XVar v  
             -> case Map.lookup v rw of
                 -- Variable has been freshened to avoid capture.
                 Just vRewrite  -> XVar vRewrite

                 Nothing 
                  -- Variable is the one we're substituting for.
                  | v == vSub   -> xSub

                  -- Some other variable.
                  | otherwise   -> xx

            -- Descend into abstractions.
            XAbs v mt x1
             -- The binder is being rewritten to avoid capture. 
             | v == vSub      -> xx

             -- The binder is the same as one of the free variables
             -- in the expression we're subtituting for, so we need to 
             -- freshen it (give it a new name) to avoid capture.
             |  Set.member v freeX
             -> let -- Choose a new variable name that is different from
                    -- all the over variables we've seen so far.
                    newVar = chooseNewVar 
                           $ Set.unions 
                                [ Set.singleton vSub, allVars xSub
                                , Set.singleton v,    allVars x1
                                , Set.fromList $ Map.keys  rw
                                , Set.fromList $ Map.elems rw ]

                    -- Remember we're rewriting the binder to a new name.
                    rw'    = Map.insert v newVar rw
                in XAbs newVar mt (down rw' x1)

             -- This abstraction isn't capturing any variables.
             | otherwise
             -> XAbs v mt (down rw x1)

            -- Descend into applications.
            XApp x1 x2
             -> XApp (down rw x1) (down rw x2)

            -- Macros and primitives don't have any variables that we need
            -- to substitute for.
            XMacro _ -> xx
            XPrim _  -> xx


-- | Choose a new variable that isn't in the given set.
chooseNewVar :: Set Var -> Var
chooseNewVar notVs
        = head 
        $ filter (\v -> not $ Set.member v notVs) 
        $ someNewVars


-- | Infinite list of new variables.
--   We start with nice names like 'A', 'B' ... 'Z', then when we've used all
--   of those then try A0 A1 A2 ... and on forever.
someNewVars :: [Var]
someNewVars     
        =  (map (\c -> V [c]) ['A' .. 'Z'])
        ++  map (\n -> V ("A" ++ show n)) [0 :: Int ..]

