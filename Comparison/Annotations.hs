{-# OPTIONS_GHC -Wall   #-}
{-# LANGUAGE LambdaCase #-}
module Annotations where

import Control.Monad.State

--------------------------------------------------------------------------------
-- Presentations
--------------------------------------------------------------------------------

-- Product Annotations
----------------------
data ExpP id ann
  = VarP ann id
    -- Variables
  | AbsP ann id            (ExpP id ann)
    -- Abstraction
  | AppP ann (ExpP id ann) (ExpP id ann)
    -- Application
  | TplP ann [ExpP id ann]
    -- Tuples

-- Sum Annotations
------------------
data ExpS id ann
  = VarS id
    -- Variables
  | AbsS id            (ExpS id ann)
    -- Abstraction
  | AppS (ExpS id ann) (ExpS id ann)
    -- Application
  | TplS [ExpS id ann]
    -- Tuples
  | AnnS ann (ExpS id ann)
    -- Annotations

-- Recursion Annotations
------------------------
data ExpR id ann
  = VarR id
    -- Variables
  | AbsR id             (AExpR id ann)
    -- Abstraction
  | AppR (AExpR id ann) (AExpR id ann)
    -- Application
  | TplR [AExpR id ann]
    -- Tuples

data AnnR  exp ann = AnnR ann exp

type AExpR id  ann = AnnR (ExpR id ann) ann


-- External Table (Alan's Solution)
-----------------------------------
type UniqueIdentifier = Int -- or (SrcSpan, ...) as in Alan's work

type ExpE id  = ExpR id UniqueIdentifier
type AExpE id = AExpR id UniqueIdentifier
type Anns ann = [(UniqueIdentifier, ann)]
-- it can be
--    `[(UniqueIdentifier, Maybe ann)]`
-- or even
--    `[ann]`
-- (where the UniqueIdentifier is based on the annotation index in the list)

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- Recusion <---> Sum
---------------------

-- Recusion to Sum
cnvAS :: AExpR id ann -> ExpS id ann
cnvAS (AnnR ann m) = AnnS ann (cnvRS m)

cnvRS :: ExpR id ann -> ExpS id ann
cnvRS = \case
  VarR x   -> VarS x
  AbsR x n -> AbsS x         (cnvAS n)
  AppR l m -> AppS (cnvAS l) (cnvAS m)
  TplR ms  -> TplS (map cnvAS ms)

-- Sum to Recursion
cnvSA :: ExpS id ann -> AExpR id [ann]
         -- can be `bag ann` or `set ann` instead of [ann]
cnvSA = cnvSA' []

cnvSA' :: [ann] -> ExpS id ann -> AExpR id [ann]
         -- can be `bag ann` or `set ann` instead of [ann]
cnvSA' as = \case
  VarS x   -> AnnR as (VarR x)
  AbsS x n -> AnnR as (AbsR x (cnvSA n))
  AppS l m -> AnnR as (AppR   (cnvSA l) (cnvSA m))
  TplS ms  -> AnnR as (TplR (map cnvSA ms))
  AnnS a m -> cnvSA' (a : as) m


-- Recusion <---> Product
-------------------------

-- Recursion to Product
cnvAP :: AExpR id ann -> ExpP id ann
cnvAP (AnnR a m) = cnvRP a m

cnvRP :: ann -> ExpR id ann -> ExpP id ann
cnvRP a = \case
  VarR x   -> VarP a x
  AbsR x n -> AbsP a x         (cnvAP n)
  AppR l m -> AppP a (cnvAP l) (cnvAP m)
  TplR ms  -> TplP a (map cnvAP ms)

-- Product to Recursion
cnvPA :: ExpP id ann -> AExpR id ann
cnvPA = \case
  VarP a x   -> AnnR a (VarR x)
  AbsP a x n -> AnnR a (AbsR x         (cnvPA n))
  AppP a l m -> AnnR a (AppR (cnvPA l) (cnvPA m))
  TplP a ms  -> AnnR a (TplR (map cnvPA ms))


-- Sum <---> Product
-------------------------

-- Product to Sum
cnvPS :: ExpP id ann -> ExpS id ann
cnvPS = \case
  VarP a x   -> AnnS a (VarS x)
  AbsP a x n -> AnnS a (AbsS x         (cnvPS n))
  AppP a l m -> AnnS a (AppS (cnvPS l) (cnvPS m))
  TplP a ms  -> AnnS a (TplS (map cnvPS ms))

-- Sum to Product
cnvSP :: ExpS id ann -> ExpP id [ann]
cnvSP = cnvSP' []

cnvSP' :: [ann] -> ExpS id ann -> ExpP id [ann]
          -- can be `bag ann` or `set ann` instead of [ann]
cnvSP' as = \case
  VarS x   -> VarP as x
  AbsS x n -> AbsP as x         (cnvSP n)
  AppS l m -> AppP as (cnvSP l) (cnvSP m)
  TplS ms  -> TplP as (map cnvSP ms)
  AnnS a m -> cnvSP' (a : as) m


-- External <---> Recursion
---------------------------
instance Functor (ExpR id) where
  fmap f = \case
    VarR x   -> VarR x
    AbsR x n -> AbsR x           (fmapAnn n)
    AppR l m -> AppR (fmapAnn l) (fmapAnn m)
    TplR ms  -> TplR (map fmapAnn ms)
   where
    fmapAnn (AnnR a m) = AnnR (f a) (fmap f m)

-- External to Recursion
cnvEA :: (Anns ann , AExpE id) -> AExpR id (Maybe ann)
cnvEA (as , AnnR a  m) = AnnR (lookup a as) (fmap (flip lookup as) m)

-- Recursion to External
cnvAE :: AExpR id (Maybe ann) -> AExpE id
cnvAE m = evalState (cnvAE' m) ([] , 0)

cnvAE' :: AExpR id (Maybe ann) -> State (Anns ann, UniqueIdentifier) (AExpE id)
cnvAE' (AnnR ma m) = do
  (tbl , i) <- get
  let newId = i + 1
  put (maybe tbl (\ a -> (newId , a) : tbl) ma, newId)
  AnnR newId <$> cnvAE'' m

cnvAE'' :: ExpR id (Maybe ann) -> State (Anns ann, UniqueIdentifier) (ExpE id)
cnvAE'' = \case
  VarR x   -> pure (VarR x)
  AbsR x n -> AbsR <$> pure x   <*> cnvAE' n
  AppR l m -> AppR <$> cnvAE' l <*> cnvAE' m
  TplR ms  -> TplR <$> mapM cnvAE' ms
