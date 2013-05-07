{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -Wall #-}
module Language.Lens.Dispatch (
  Dispatch (..)
, Dispatchable (..)
, deriveTagDispatch
) where

import Data.Beamable.Internal
import Data.List
import GHC.Generics
import GHC.Prim (Constraint)
import Language.Haskell.TH
import Language.Lens.Tag

-- | Dispatch-based constraints

-- | Type-based dispatch driven by an enumeration value.
class Dispatch e where
    type DispatchCxt e b :: Constraint
    dispatch :: e -> (forall t. (IsTag t, Dispatchable t e, DispatchCxt e (Component (Full t) t)) => t -> r) -> r
    liftPair :: (DispatchCxt e l, DispatchCxt e r) => p0 e -> p1 (l, r) -> (DispatchCxt e (l, r) => k) -> k

-- | Generate an enumeration value to dispatch on this type.
class Dispatchable t e where
    signDispatch :: t -> e

-- | Create an enumeration type, using the provided name, that can be used to
-- dispatch on all `IsTag` instances currently in scope.
deriveTagDispatch :: String -> [Name] -> Q [Dec]
deriveTagDispatch enumNameStr contexts = do
    ClassI _ instances <- reify ''IsTag
    let tags = nub $ sort [ tagname | (InstanceD _ (AppT _ (ConT tagname)) _) <- instances]
        enumName = mkName enumNameStr

    enumConNms <- mapM (newName . (enumNameStr ++) . nameBase) tags

    let enumDeriv = [''Generic, ''Show, ''Eq, ''Ord, ''Enum, ''Bounded]
        enumCons  = map (flip NormalC []) enumConNms
        enumDec   = DataD [] enumName [] enumCons enumDeriv
        enumBeam  = InstanceD [] (ConT ''Beamable `AppT` ConT enumName) []

    let tag'constrNms = zip tags enumConNms
        dispatchables = map (mkDispatchable enumName) tag'constrNms
    dispatchI <- mkDispatchInst enumName tag'constrNms contexts
    
    return $ [enumDec, dispatchI, enumBeam] ++ dispatchables

mkDispatchInst :: Name -> [(Name,Name)] -> [Name] -> Q Dec
mkDispatchInst eTyp tconstrs contexts = do
    fnNm <- newName "f"
    aNm  <- newName "a"
    fooNm <- newName "cxtTypeVar"
    let iType = return $ ConT ''Dispatch `AppT` ConT eTyp
        newClause (tag,eConstr) = do
            (TyConI (DataD _ _ _ [NormalC tagConstr _] _)) <- reify tag
            clause [conP eConstr [], varP fnNm]
                   (normalB (varE fnNm `appE` conE tagConstr)) []
        dispatchFn = funD 'dispatch (map newClause tconstrs)
        liftFn     = funD 'liftPair [clause [wildP, wildP, varP aNm]
                        (normalB (varE aNm)) []]
        dispCxt    = return $ TySynInstD ''DispatchCxt [ConT eTyp, VarT fooNm] (mkContextSyn fooNm contexts)
    instanceD (return []) iType [dispCxt, dispatchFn, liftFn]

mkDispatchable :: Name -> (Name,Name) -> Dec
mkDispatchable eTyp (tag, eConNm) =
    let iType   = ConT ''Dispatchable `AppT` ConT tag `AppT` ConT eTyp
        dClause = Clause [WildP] (NormalB (ConE eConNm)) []
        dFn     = FunD 'signDispatch [dClause]
    in InstanceD [] iType [dFn]

mkContextSyn :: Name -> [Name] -> Type
mkContextSyn var [x]   = ConT x `AppT` VarT var -- since there's no single-element tuple yet, need to special-case it...
mkContextSyn var xs    = foldl (\r t -> AppT r (AppT t (VarT var))) (TupleT (length xs)) (map ConT xs)

