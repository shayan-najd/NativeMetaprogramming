{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module Translations where

import Control.Monad.State
import qualified Product   as P
import qualified Sum       as S
import qualified Recursion as R
import qualified External  as E

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- Recusion <---> Sum
---------------------

-- Recusion to Sum
cnvRS :: Functor col => R.Exp row col id -> S.Exp row col id
cnvRS (R.Ann row m) = S.Ann row (cnvRS' m)

cnvRS' :: Functor col => R.Exp' row col id -> S.Exp row col id
cnvRS' = \case
  R.Var x   -> S.Var x
  R.Abs x n -> S.Abs x         (cnvRS n)
  R.App l m -> S.App (cnvRS l) (cnvRS m)
  R.Tpl ms  -> S.Tpl (fmap cnvRS ms)
  R.Ext m   -> S.Ext (fmap cnvRS m)

-- Sum to Recursion
cnvSR :: Functor col => S.Exp row col id -> R.Exp [row] col id
         -- can be `bag row` or `set row` instead of [row]
cnvSR = cnvSR' []

cnvSR' :: Functor col => [row] -> S.Exp row col id -> R.Exp [row] col id
         -- can be `bag row` or `set row` instead of [row]
cnvSR' as = \case
  S.Var x   -> R.Ann as (R.Var x)
  S.Abs x n -> R.Ann as (R.Abs x (cnvSR n))
  S.App l m -> R.Ann as (R.App   (cnvSR l) (cnvSR m))
  S.Tpl ms  -> R.Ann as (R.Tpl (fmap cnvSR ms))
  S.Ext m   -> R.Ann as (R.Ext (fmap cnvSR m))
  S.Ann a m -> cnvSR' (a : as) m


-- Recusion <---> Product
-------------------------

-- Recursion to Product
cnvRP :: Functor col => R.Exp row col id -> P.Exp row col id
cnvRP (R.Ann a m) = cnvRP' a m

cnvRP' :: Functor col => row -> R.Exp' row col id -> P.Exp row col id
cnvRP' a = \case
  R.Var x   -> P.Var a x
  R.Abs x n -> P.Abs a x         (cnvRP n)
  R.App l m -> P.App a (cnvRP l) (cnvRP m)
  R.Tpl ms  -> P.Tpl a (fmap cnvRP ms)
  R.Ext m   -> P.Ext a (fmap cnvRP m)

-- Product to Recursion
cnvPR :: Functor col => P.Exp row col id -> R.Exp row col id
cnvPR = \case
  P.Var a x   -> R.Ann a (R.Var x)
  P.Abs a x n -> R.Ann a (R.Abs x         (cnvPR n))
  P.App a l m -> R.Ann a (R.App (cnvPR l) (cnvPR m))
  P.Tpl a ms  -> R.Ann a (R.Tpl (fmap cnvPR ms))
  P.Ext a m   -> R.Ann a (R.Ext (fmap cnvPR m))

-- Sum <---> Product
-------------------------

-- Product to Sum
cnvPS :: Functor col => P.Exp row col id -> S.Exp row col id
cnvPS = \case
  P.Var a x   -> S.Ann a (S.Var x)
  P.Abs a x n -> S.Ann a (S.Abs x         (cnvPS n))
  P.App a l m -> S.Ann a (S.App (cnvPS l) (cnvPS m))
  P.Tpl a ms  -> S.Ann a (S.Tpl (fmap cnvPS ms))
  P.Ext a m   -> S.Ann a (S.Ext (fmap cnvPS m))

-- Sum to Product
cnvSP :: Functor col => S.Exp row col id -> P.Exp [row] col id
cnvSP = cnvSP' []

cnvSP' :: Functor col => [row] -> S.Exp row col id -> P.Exp [row] col id
          -- can be `bag row` or `set row` instead of [row]
cnvSP' as = \case
  S.Var x   -> P.Var as x
  S.Abs x n -> P.Abs as x         (cnvSP n)
  S.App l m -> P.App as (cnvSP l) (cnvSP m)
  S.Tpl ms  -> P.Tpl as (fmap cnvSP ms)
  S.Ext m   -> P.Ext as (fmap cnvSP m)
  S.Ann a m -> cnvSP' (a : as) m


-- External <---> Recursion
---------------------------

fmapAnn :: Functor col => (row -> row') -> R.Exp row col id -> R.Exp row' col id
fmapAnn f (R.Ann a m) = R.Ann (f a) (fmapRExp f m)

fmapRExp :: Functor col =>
            (row -> row') -> R.Exp' row col id -> R.Exp' row' col id
fmapRExp f = \case
  R.Var x   -> R.Var x
  R.Abs x n -> R.Abs x             (fmapAnn f n)
  R.App l m -> R.App (fmapAnn f l) (fmapAnn f m)
  R.Tpl ms  -> R.Tpl (fmap (fmapAnn f) ms)
  R.Ext m   -> R.Ext (fmap (fmapAnn f) m)

-- External to Recursion
cnvER :: Functor col => (E.Tbl row , E.Exp col id) -> R.Exp (Maybe row) col id
cnvER (as , R.Ann a  m) = R.Ann (lookup a as) (fmapRExp (flip lookup as) m)

-- Recursion to External
cnvRE :: Traversable col => R.Exp row col id -> (E.Tbl row , E.Exp col id)
cnvRE m = let (m' , (tbl, _)) = runState (cnvRE' m) ([] , 0)
          in  (tbl , m')

type MTbl row exp = State (E.Tbl row, E.UniqueIdentifier) exp

cnvRE' :: Traversable col => R.Exp row col id ->
          MTbl row (E.Exp col id)
cnvRE' (R.Ann a m) = do
  (tbl , i) <- get
  let newId = i + 1
  put ((newId , a) : tbl, newId)
  R.Ann newId <$> cnvRE'' m

cnvRE'' :: Traversable col => R.Exp' row col id -> MTbl row (E.Exp' col id)
cnvRE'' = \case
  R.Var x   -> R.Var <$> pure x
  R.Abs x n -> R.Abs <$> pure x   <*> cnvRE' n
  R.App l m -> R.App <$> cnvRE' l <*> cnvRE' m
  R.Tpl ms  -> R.Tpl <$> mapM cnvRE' ms
  R.Ext m   -> R.Ext <$> mapM cnvRE' m

-- External <---> Product
-------------------------

-- External to Product
cnvEP :: Functor col => (E.Tbl row , E.Exp col id) -> P.Exp (Maybe row) col id
cnvEP (tbl , R.Ann a m) = cnvEP' tbl (lookup a tbl) m

cnvEP' :: Functor col => E.Tbl row -> Maybe row -> E.Exp' col id ->
          P.Exp (Maybe row) col id
cnvEP' tbl a = \case
  R.Var x   -> P.Var a x
  R.Abs x n -> P.Abs a x (cnvEP (tbl , n))
  R.App l m -> P.App a (cnvEP (tbl , l)) (cnvEP (tbl , m))
  R.Tpl ms  -> P.Tpl a (fmap (curry cnvEP tbl) ms)
  R.Ext m   -> P.Ext a (fmap (curry cnvEP tbl) m)

-- Product to External
cnvPE :: Traversable col => P.Exp row col id -> (E.Tbl row , E.Exp col id)
cnvPE m = let (m' , (tbl, _)) = runState (cnvPE' m) ([] , 0)
          in  (tbl , m')

cnvPE' :: Traversable col => P.Exp row col id -> MTbl row (E.Exp col id)
cnvPE' m  = do
  (tbl , i) <- get
  let newId = i + 1
  let f a   = put ((newId , a) : tbl, newId)
  R.Ann newId <$> cnvPE'' f m

cnvPE'' :: Traversable col =>
           (row -> MTbl row ()) -> P.Exp row col id -> MTbl row (E.Exp' col id)
cnvPE'' f = \case
  P.Var a x   -> do f a
                    R.Var <$> pure x
  P.Abs a x n -> do f a
                    R.Abs <$> pure x <*> cnvPE' n
  P.App a l m -> do f a
                    R.App <$> cnvPE' l <*> cnvPE' m
  P.Tpl a ms  -> do f a
                    R.Tpl <$> mapM cnvPE' ms
  P.Ext a m   -> do f a
                    R.Ext <$> mapM cnvPE' m

-- External <---> Sum
---------------------

-- External to Sum
cnvES :: Functor col => (E.Tbl row , E.Exp col id) -> S.Exp row col id
cnvES (tbl , R.Ann a m) = (maybe id S.Ann (lookup a tbl)) (cnvES' tbl m)

cnvES' :: Functor col => E.Tbl row -> E.Exp' col id -> S.Exp row col id
cnvES' tbl = \case
  R.Var x   -> S.Var x
  R.Abs x n -> S.Abs x (cnvES (tbl , n))
  R.App l m -> S.App (cnvES (tbl , l)) (cnvES (tbl , m))
  R.Tpl ms  -> S.Tpl (fmap (curry cnvES tbl) ms)
  R.Ext m   -> S.Ext (fmap (curry cnvES tbl) m)

-- Sum to External
cnvSE :: Traversable col => S.Exp row col id -> (E.Tbl [row] , E.Exp col id)
cnvSE m = let (m' , (tbl, _)) = runState (cnvSE' m) ([] , 0)
          in  (tbl , m')

cnvSE' :: Traversable col => S.Exp row col id ->
          MTbl [row] (E.Exp col id)
cnvSE' = cnvSE'' []

cnvSE'' :: Traversable col =>
           [row] -> S.Exp row col id -> MTbl [row]  (E.Exp col id)
cnvSE'' as = \case
  S.Ann a m -> cnvSE'' (a : as) m
  rest      -> do
    (tbl , i) <- get
    let newId = i + 1
    put ((newId , as) : tbl, newId)
    R.Ann newId <$> cnvSE''' rest

cnvSE''' :: Traversable col => S.Exp row col id -> MTbl [row] (E.Exp' col id)
cnvSE''' = \case
  S.Var x   -> R.Var <$> pure x
  S.Abs x n -> R.Abs <$> pure x   <*> cnvSE' n
  S.App l m -> R.App <$> cnvSE' l <*> cnvSE' m
  S.Tpl ms  -> R.Tpl <$> mapM cnvSE' ms
  S.Ext m   -> R.Ext <$> mapM cnvSE' m
  S.Ann {}  -> error "Impossible!"
