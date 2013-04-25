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
import Language.Haskell.TH
import Language.Lens.Tag

-- | Type-based dispatch driven by an enumeration value.
class Dispatch e where
    dispatch :: e -> (forall t. (IsTag t, Dispatchable t e, Beamable (Component (Full t) t)) => t -> r) -> r

-- | Generate an enumeration value to dispatch on this type.
class Dispatchable t e where
    signDispatch :: t -> e

-- | Create an enumeration type, using the provided name, that can be used to
-- dispatch on all `IsTag` instances currently in scope.
deriveTagDispatch :: String -> Q [Dec]
deriveTagDispatch enumNameStr = do
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
    dispatchI <- mkDispatchInst enumName tag'constrNms
    
    return $ [enumDec, dispatchI, enumBeam] ++ dispatchables

mkDispatchInst :: Name -> [(Name,Name)] -> Q Dec
mkDispatchInst eTyp tconstrs = do
    fnNm <- newName "f"
    let iType = return $ ConT ''Dispatch `AppT` ConT eTyp
        newClause (tag,eConstr) = do
            (TyConI (DataD _ _ _ [NormalC tagConstr _] _)) <- reify tag
            clause [conP eConstr [], varP fnNm]
                   (normalB (varE fnNm `appE` conE tagConstr)) []
        dispatchFn = funD 'dispatch (map newClause tconstrs)
    instanceD (return []) iType [dispatchFn]

mkDispatchable :: Name -> (Name,Name) -> Dec
mkDispatchable eTyp (tag, eConNm) =
    let iType   = ConT ''Dispatchable `AppT` ConT tag `AppT` ConT eTyp
        dClause = Clause [WildP] (NormalB (ConE eConNm)) []
        dFn     = FunD 'signDispatch [dClause]
    in InstanceD [] iType [dFn]
