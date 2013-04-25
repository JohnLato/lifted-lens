{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
module Language.Lens.Tag (
  Component
, IsTag (..)
, deriveTags
) where

import Control.Arrow (first)
import Data.Char (toUpper)
import GHC.Generics
import Language.Haskell.TH

import Data.Beamable

import Language.Lens.Naming

type family Component full tag

class IsTag tag where
    type Full t
    getTagName :: tag -> String
    getTag  :: tag -> Full tag -> Component (Full tag) tag
    setTag  :: tag -> Component (Full tag) tag -> Full tag -> Full tag

-- | Derive all tags and necessary instances for the named data type.
-- deriveTags requires the following extensions:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > {-# LANGUAGE DeriveGeneric #-}
--
-- All derived types are instances of Generic and Beamable.
deriveTags :: Name -> Q [Dec]
deriveTags typeName = do
        typeInfo <- reify typeName

        case typeInfo of
            TyConI (DataD _ name _ [constr] _) ->
                mkDecls Nothing name (getPrimFields constr)

            TyConI (NewtypeD _ name _ ntConstr _) -> do
                (cName, constr) <- unpackNTConstr ntConstr
                mkDecls (Just name) cName (getPrimFields constr)

            _ -> error invalid

    where

        -- I'm unhappy about this mess
        invalid :: String
        invalid = "You must specify datatype with a single constructor with record syntax."

        unpackNTConstr :: Con -> Q (Name, Con)
        unpackNTConstr (RecC _ [(_, _, ConT underlyingType)]) = do
            typeInfo <- reify underlyingType
            case typeInfo of
                TyConI (DataD _ name _ [constr] _) -> return (name, constr)
                _ -> error invalid
        unpackNTConstr _ = error invalid

        mkDecls :: Maybe Name -> Name -> [(String, Type)] -> Q [Dec]
        mkDecls mNewType name prim = do
                let tags = map (mkTagType mNewType . toConstrName . fst) prim
                    compInsts = map (mkCompInst mNewType name . first toConstrName) prim
                    beamInsts = map (mkBeamInst mNewType . mkName . fst) prim
                tagInsts <- mapM (mkTagInst mNewType name . mkName . fst) prim
                return $ concat [tags, compInsts, tagInsts, beamInsts]

        mkTagType :: Maybe Name -> Name -> Dec
        mkTagType mNewType name = let name' = preNT mNewType name
                                  in DataD [] name' [] [NormalC name' []] [''Show, ''Generic]

        mkBeamInst :: Maybe Name -> Name -> Dec
        mkBeamInst mNewType acc =
            let name' = preNT mNewType (toConstrName $ nameBase acc)
                iType = ConT ''Beamable `AppT` ConT name'
            in InstanceD [] iType []

        mkCompInst :: Maybe Name -> Name -> (Name, Type) -> Dec
        mkCompInst mNewType dName (fName, t) =
            let name' = preNT mNewType fName
                base' = maybe dName id mNewType
            in TySynInstD ''Component [ConT base', ConT name'] t

        mkTagInst :: Maybe Name -> Name -> Name -> Q Dec
        mkTagInst mNewType pref acc =
            let name' = preNT mNewType (toConstrName $ nameBase acc)
                base' = maybe pref id mNewType
                cxt'  = return []
                iType = return $ ConT ''IsTag `AppT` ConT name'
                decTp = return $ TySynInstD ''Full [ConT name'] (ConT base')
                body  = return $ LitE . StringL $ makeFieldName (nameBase pref) (nameBase acc)
                decFn = funD 'getTagName [clause [wildP] (normalB body) []]
                decAFn= funD 'getTag [clause [wildP] (normalB (varE acc)) []]
                decSFn= do
                    compName <- newName "field"
                    fullName <- newName "full"
                    let pats = [wildP, varP compName, varP fullName]
                        body' = return $ RecUpdE (VarE fullName) [(acc,VarE compName)]
                    funD 'setTag [clause pats (normalB body') []]
            in instanceD cxt' iType [decTp, decFn, decAFn, decSFn]

        getPrimFields :: Con -> [(String, Type)]
        getPrimFields constr =
            case constr of
                RecC _ fields -> map (\(fn, _, tn) -> (nameBase fn, tn)) fields
                _ -> error "Sorry, but only record-type constructors are supported"

        toConstrName :: String -> Name
        toConstrName (c:cs) = mkName (toUpper c : cs)
        toConstrName _ = error "You can't have empty name"

        preNT :: Maybe Name -> Name -> Name
        preNT (Just prefix) name = mkName (nameBase prefix ++ nameBase name)
        preNT Nothing name = name
