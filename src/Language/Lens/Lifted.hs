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
module Language.Lens.Lifted (
-- * Typed accessor DSL
  AccessExp
-- ** Creating typed DSL values
, aeTag
, aeTup
-- ** Running/using the typed DSL
, runDSL
, unType
-- * Untyped accessor DSL
, AccessTree (..)
-- * running the untyped DSL
, runTree
, module Z
) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Beamable
import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

import Language.Lens.Dispatch as Z
import Language.Lens.Tag

-------------------------------------------------------------
-- typed DSL stuff

-- | Typed accessor DSL, for creating lenses
data AccessExp enum full component where
    AeId  :: AccessExp enum full full
    AeTag :: (IsTag tag, Dispatchable tag enum) => tag -> AccessExp enum full component
    AeCmp :: AccessExp enum p component -> AccessExp enum full p -> AccessExp enum full component
    AeTup :: AccessExp enum full cL -> AccessExp enum full cR -> AccessExp enum full (cL,cR)

instance Show (AccessExp enum full component) where
    show (AeId)      = "AeId"
    show (AeTag tag) = "AeTag " ++ getTagName tag
    show (AeCmp l r) = "AeCmp (" ++ show l ++ ") (" ++ show r ++ ")"
    show (AeTup l r) = "AeTup (" ++ show l ++ ") (" ++ show r ++ ")"

instance Category (AccessExp enum) where
    id = AeId
    (.) = AeCmp

-- | Get a field using the specified 'tag'.
aeTag :: (IsTag tag, Dispatchable tag enum) => tag -> AccessExp enum full component
aeTag = AeTag

aeTup :: AccessExp enum full left -> AccessExp enum full right -> AccessExp enum full (left,right)
aeTup = AeTup

-- | Run the typed DSL, accessing a field of type 'component' in the 'full'
-- data value.
runDSL :: AccessExp enum full component -> full -> component
runDSL AeId        = id
runDSL (AeTag tag) = unsafeCoerce . getTag tag . unsafeCoerce
runDSL (AeCmp l r) = runDSL l . runDSL r
runDSL (AeTup l r) = \f -> (runDSL l f, runDSL r f)

-------------------------------------------------------------
-- untyped DSL stuff

-- | untyped accessor DSL, for beaming/consuming expressions
data AccessTree enum =
    AtTag enum
  | AtId
  | AtCmp (AccessTree enum) (AccessTree enum)
  | AtTup (AccessTree enum) (AccessTree enum)
  deriving (Show, Eq, Ord, Generic)

instance Beamable enum => Beamable (AccessTree enum)

-- | Convert a typed accessor structure to an untyped structure.
--
-- One common use is for transferring to another system, as the untyped DSL can
-- be beamed and run remotely.
unType :: AccessExp enum full component -> AccessTree enum
unType AeId        = AtId
unType (AeTag tag) = AtTag $ signDispatch tag
unType (AeCmp l r) = AtCmp (unType l) (unType r)
unType (AeTup l r) = AtTup (unType l) (unType r)

-- | Run an untyped DSL.
--
-- Since the field type is generally not known, we have to process it using
-- the class interface 'DispatchCxt', which is defined when 'deriveTagDispatch'
-- is called.
runTree :: forall r enum full. (DispatchCxt enum full, Dispatch enum)
        => AccessTree enum
        -> (forall b. DispatchCxt enum b => b -> r)
        -> full
        -> r
runTree dsl consumer = case dsl of
    AtTag tag -> dispatch tag (\t -> consumer . getTag t . unsafeCoerce)
    AtId      -> consumer
    AtCmp l r -> runTree r (runTree l consumer)
    AtTup l r -> \f -> runTree l (\lC -> runTree r (\rC -> liftPair dsl (Just (lC,rC)) $ consumer (lC,rC)) f) f
