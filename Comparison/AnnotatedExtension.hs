{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
module AnnotatedExtension where

import Annotated (Typ,SrcLoc)

-- Row Extension
data RowX
  = VarX
  | AbsX Typ SrcLoc
  | AppX Typ
  | TplX [Typ]
  | ExtX

-- Column Extension
data ColX exp
  = OutX Typ exp
