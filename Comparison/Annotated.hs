{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
module Annotated where

--------------------------------------------------------------------------------
-- Annotated expressions,
--   where annotations are baked into the definition
--  (like HsSyn AST)
--------------------------------------------------------------------------------
data Typ
data SrcLoc

data Exp id
  = Var id
  | Abs Typ SrcLoc id (Exp id)
  | App Typ (Exp id)  (Exp id)
  | Tpl [Typ] [Exp id]
  | Out Typ (Exp id)
