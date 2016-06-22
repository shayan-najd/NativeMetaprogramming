{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
module AnnotatedIndexedExtension where

import Annotated (Typ,SrcLoc)
import AnnotatedIndexed (Lbl(..))

data RowX lbl where
  VarX :: RowX 'VarL
  AbsX :: Typ -> SrcLoc -> RowX 'AbsL
  AppX :: Typ -> RowX 'AppL
  TplX :: [Typ] -> RowX 'TplL
  ExtX :: RowX 'ExtL
