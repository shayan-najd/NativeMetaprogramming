{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PolyKinds                  #-}
module RecursionIndexed where

import qualified Unannotated                 as U
import qualified UnannotatedExtension        as U
import qualified Annotated                   as A
import qualified AnnotatedExtension          as A
import AnnotatedIndexed (Lbl(..))
import qualified AnnotatedIndexedExtension   as AI
import qualified UnannotatedIndexedExtension as UI

-- Recursion Annotations
------------------------
data Exp' (lbl :: Lbl) row col id where
  Var :: id             -> Exp' 'VarL row col id
    -- Variables
  Abs :: id             -> Exp row col id -> Exp' 'AbsL row col id
    -- Abstraction
  App :: Exp row col id -> Exp row col id -> Exp' 'AppL row col id
    -- Application
  Tpl :: [Exp row col id]     -> Exp' 'TplL row col id
    -- Tuples
  Ext :: col (Exp row col id) -> Exp' 'ExtL row col id
    -- Column Extension

data Exp row col id where
  Ann :: row lbl -> Exp' lbl row col id -> Exp row col id

--------------------------------------------------------------------------------
-- Row and Column Extensibility
--------------------------------------------------------------------------------

-- Unannotated Exp Synonyms
---------------------------

type ExpUS id = Exp UI.RowX U.ColX id
pattern VarUS :: id -> ExpUS id
pattern AbsUS :: id -> ExpUS id -> ExpUS id
pattern AppUS :: ExpUS id -> ExpUS id -> ExpUS id
pattern TplUS :: [ExpUS id] -> ExpUS id

pattern VarUS x   = Ann UI.None (Var x)
pattern AbsUS x n = Ann UI.None (Abs x n)
pattern AppUS l m = Ann UI.None (App l m)
pattern TplUS ms  = Ann UI.None (Tpl ms)

cnvExpUStoUExp :: ExpUS id -> U.Exp id
cnvExpUStoUExp = \case
  VarUS x   -> U.Var x
  AbsUS x n -> U.Abs x                  (cnvExpUStoUExp n)
  AppUS l m -> U.App (cnvExpUStoUExp l) (cnvExpUStoUExp m)
  TplUS ms  -> U.Tpl (map cnvExpUStoUExp ms)
  _         -> error "Impossible!" -- <--- One problem with this approach

cnvUExptoExpUS :: U.Exp id -> ExpUS id
cnvUExptoExpUS = \case
  U.Var x   -> VarUS x
  U.Abs x n -> AbsUS x                  (cnvUExptoExpUS n)
  U.App l m -> AppUS (cnvUExptoExpUS l) (cnvUExptoExpUS m)
  U.Tpl ms  -> TplUS (map cnvUExptoExpUS ms)


-- Annotated Exp Synonyms
-------------------------

type ExpAS id = Exp AI.RowX A.ColX id
pattern VarAS :: id -> ExpAS id
pattern AbsAS :: A.Typ -> A.SrcLoc -> id -> ExpAS id -> ExpAS id
pattern AppAS :: A.Typ -> ExpAS id -> ExpAS id -> ExpAS id
pattern TplAS :: [A.Typ] -> [ExpAS id] -> ExpAS id
pattern OutAS :: A.Typ -> ExpAS id -> ExpAS id

pattern VarAS      x   = Ann AI.VarX       (Var x)
pattern AbsAS t  s x n = Ann (AI.AbsX t s) (Abs x n)
pattern AppAS t    l m = Ann (AI.AppX t)   (App l m)
pattern TplAS ts   ms  = Ann (AI.TplX ts)  (Tpl ms)
pattern OutAS t    m   = Ann AI.ExtX       (Ext (A.OutX t m))

cnvExpAStoAExp :: ExpAS id -> A.Exp id
cnvExpAStoAExp = \case
  VarAS x       -> A.Var x
  AbsAS t s x n -> A.Abs t s x (cnvExpAStoAExp n)
  AppAS t   l m -> A.App t   (cnvExpAStoAExp l) (cnvExpAStoAExp m)
  TplAS ts  ms  -> A.Tpl ts  (map cnvExpAStoAExp ms)
  OutAS t   m   -> A.Out t   (cnvExpAStoAExp m)
  _             -> error "Impossible!"

cnvAExptoExpAS :: A.Exp id -> ExpAS id
cnvAExptoExpAS = \case
  A.Var     x   -> VarAS     x
  A.Abs t s x n -> AbsAS t s x (cnvAExptoExpAS n)
  A.App t   l m -> AppAS t   (cnvAExptoExpAS l) (cnvAExptoExpAS m)
  A.Tpl ts  ms  -> TplAS ts  (map cnvAExptoExpAS ms)
  A.Out t   m   -> OutAS t   (cnvAExptoExpAS m)
