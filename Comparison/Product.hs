{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
module Product where

import qualified Unannotated          as U
import qualified UnannotatedExtension as U
import qualified Annotated            as A
import qualified AnnotatedExtension   as A

--------------------------------------------------------------------------------
-- Product Annotations
--------------------------------------------------------------------------------
data Exp row col id
  = Var row id
    -- Variables
  | Abs row id               (Exp row col id)
    -- Abstraction
  | App row (Exp row col id) (Exp row col id)
    -- Application
  | Tpl row [Exp row col id]
    -- Tuples
  | Ext row (col (Exp row col id))

--------------------------------------------------------------------------------
-- Row and Column Extensibility
--------------------------------------------------------------------------------

-- Unannotated Exp Synonyms
---------------------------

type ExpUS id = Exp U.RowX U.ColX id
pattern VarUS :: id -> ExpUS id
pattern AbsUS :: id -> ExpUS id -> ExpUS id
pattern AppUS :: ExpUS id -> ExpUS id -> ExpUS id
pattern TplUS :: [ExpUS id] -> ExpUS id

pattern VarUS x   = Var U.None x
pattern AbsUS x n = Abs U.None x n
pattern AppUS l m = App U.None l m
pattern TplUS ms  = Tpl U.None ms

cnvExpUStoUExp :: ExpUS id -> U.Exp id
cnvExpUStoUExp = \case
  VarUS x   -> U.Var x
  AbsUS x n -> U.Abs x                  (cnvExpUStoUExp n)
  AppUS l m -> U.App (cnvExpUStoUExp l) (cnvExpUStoUExp m)
  TplUS ms  -> U.Tpl (map cnvExpUStoUExp ms)
  _         -> error "Impossible!"

cnvUExptoExpUS :: U.Exp id -> ExpUS id
cnvUExptoExpUS = \case
  U.Var x   -> VarUS x
  U.Abs x n -> AbsUS x                  (cnvUExptoExpUS n)
  U.App l m -> AppUS (cnvUExptoExpUS l) (cnvUExptoExpUS m)
  U.Tpl ms  -> TplUS (map cnvUExptoExpUS ms)


-- Annotated Exp Synonyms
-------------------------

type ExpAS id = Exp A.RowX A.ColX id
pattern VarAS :: id -> ExpAS id
pattern AbsAS :: A.Typ -> A.SrcLoc -> id -> ExpAS id -> ExpAS id
pattern AppAS :: A.Typ -> ExpAS id -> ExpAS id -> ExpAS id
pattern TplAS :: [A.Typ] -> [ExpAS id] -> ExpAS id
pattern OutAS :: A.Typ -> ExpAS id -> ExpAS id

pattern VarAS      x   = Var A.VarX x
pattern AbsAS t  s x n = Abs (A.AbsX t s) x n
pattern AppAS t    l m = App (A.AppX t) l m
pattern TplAS ts   ms  = Tpl (A.TplX ts) ms
pattern OutAS t    m   = Ext A.ExtX (A.OutX t m)

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
