{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
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
cnvAE :: AExpR id ann -> (Anns ann , AExpE id)
cnvAE m = let (m' , (tbl, _)) = runState (cnvAE' m) ([] , 0)
          in  (tbl , m')

cnvAE' :: AExpR id ann -> State (Anns ann, UniqueIdentifier) (AExpE id)
cnvAE' (AnnR a m) = do
  (tbl , i) <- get
  let newId = i + 1
  put ((newId , a) : tbl, newId)
  AnnR newId <$> cnvAE'' m

cnvAE'' :: ExpR id ann -> State (Anns ann, UniqueIdentifier) (ExpE id)
cnvAE'' = \case
  VarR x   -> pure (VarR x)
  AbsR x n -> AbsR <$> pure x   <*> cnvAE' n
  AppR l m -> AppR <$> cnvAE' l <*> cnvAE' m
  TplR ms  -> TplR <$> mapM cnvAE' ms

-- External <---> Product
-------------------------

-- External to Product
cnvEP :: (Anns ann , AExpE id) -> ExpP id (Maybe ann)
cnvEP (tbl , AnnR a m) = cnvEP' tbl (lookup a tbl) m

cnvEP' :: Anns ann -> Maybe ann -> ExpE id -> ExpP id (Maybe ann)
cnvEP' tbl a = \case
  VarR x   -> VarP a x
  AbsR x n -> AbsP a x (cnvEP (tbl , n))
  AppR l m -> AppP a (cnvEP (tbl , l)) (cnvEP (tbl , m))
  TplR ms  -> TplP a (map (\ m -> cnvEP (tbl , m)) ms)

-- Product to External
cnvPE :: ExpP id ann -> (Anns ann , AExpE id)
cnvPE m = let (m' , (tbl, _)) = runState (cnvPE' m) ([] , 0)
          in  (tbl , m')

cnvPE' :: ExpP id ann -> State (Anns ann, UniqueIdentifier) (AExpE id)
cnvPE' mm  = do
  (tbl , i) <- get
  let newId = i + 1
  let f a   = put ((newId , a) : tbl, newId)
  let m'    = case mm of
                VarP a x   -> do f a
                                 pure (VarR x)
                AbsP a x n -> do f a
                                 AbsR <$> pure x <*> cnvPE' n
                AppP a l m -> do f a
                                 AppR <$> cnvPE' l <*> cnvPE' m
                TplP a ms  -> do f a
                                 TplR <$> mapM cnvPE' ms
  AnnR newId <$> m'


-- External <---> Sum
---------------------

-- External to Sum
cnvES :: (Anns ann , AExpE id) -> ExpS id ann
cnvES (tbl , AnnR a m) = (maybe id AnnS (lookup a tbl)) (cnvES' tbl m)

cnvES' :: Anns ann -> ExpE id  -> ExpS id ann
cnvES' tbl = \case
  VarR x   -> VarS x
  AbsR x n -> AbsS x (cnvES (tbl , n))
  AppR l m -> AppS (cnvES (tbl , l)) (cnvES (tbl , m))
  TplR ms  -> TplS (map (\ m -> cnvES (tbl , m)) ms)

-- Sum to External
cnvSE :: ExpS id ann -> (Anns [ann] , AExpE id)
cnvSE m = let (m' , (tbl, _)) = runState (cnvSE' m) ([] , 0)
          in  (tbl , m')

cnvSE' :: ExpS id ann -> State (Anns [ann], UniqueIdentifier) (AExpE id)
cnvSE' = cnvSE'' []

cnvSE'' :: [ann] -> ExpS id ann ->
           State (Anns [ann], UniqueIdentifier) (AExpE id)
cnvSE'' as = \case
  AnnS a m -> cnvSE'' (a : as) m
  rest     -> do
    (tbl , i) <- get
    let newId = i + 1
    put ((newId , as) : tbl, newId)
    AnnR newId <$> cnvSE''' rest

cnvSE''' :: ExpS id ann -> State (Anns [ann], UniqueIdentifier) (ExpE id)
cnvSE''' = \case
  VarS x   -> VarR <$> pure x
  AbsS x n -> AbsR <$> pure x   <*> cnvSE' n
  AppS l m -> AppR <$> cnvSE' l <*> cnvSE' m
  TplS ms  -> TplR <$> mapM cnvSE' ms
  AnnS {}  -> error "Impossible!"

--------------------------------------------------------------------------------
-- Row Extensibility Using Annotations
--------------------------------------------------------------------------------

-- Annotated expressions,
--   where annotations are baked into the definition
--  (like HsSyn AST)
----------------------------------------------------
data Typ
data SrcLoc

data Exp id
  = Var id
  | Abs Typ SrcLoc id (Exp id)
  | App Typ (Exp id)  (Exp id)
  | Tpl [Typ] [Exp id]


-- Annotated expressions
--  where annotations are *not* baked into the definition
--        (like HSE AST but with recursion annotation)
---------------------------------------------------------
data Ann = AVar
         | AAbs  Typ SrcLoc
         | AApp  Typ
         | ATpl  [Typ]

type ExpX id = AExpR id Ann
pattern VarX :: id -> ExpX id
pattern AbsX :: Typ -> SrcLoc -> id -> ExpX id -> ExpX id
pattern AppX :: Typ -> ExpX id -> ExpX id -> ExpX id
pattern TplX :: [Typ] -> [ExpX id] -> ExpX id

pattern VarX      x   = AnnR AVar       (VarR x)
pattern AbsX t  s x n = AnnR (AAbs t s) (AbsR x n)
pattern AppX t    l m = AnnR (AApp t)   (AppR l m)
pattern TplX ts   ms  = AnnR (ATpl ts)  (TplR ms)

cnvExpXtoExp :: ExpX id -> Exp id
cnvExpXtoExp = \case
  VarX x       -> Var x
  AbsX t s x n -> Abs t s x (cnvExpXtoExp n)
  AppX t   l m -> App t (cnvExpXtoExp l) (cnvExpXtoExp m)
  TplX ts  ms  -> Tpl ts (map cnvExpXtoExp ms)
  _            -> error "Impossible!" -- <--- One problem with this approach

cnvExptoExpX :: Exp id -> ExpX id
cnvExptoExpX = \case
  Var     x   -> VarX     x
  Abs t s x n -> AbsX t s x (cnvExptoExpX n)
  App t   l m -> AppX t (cnvExptoExpX l) (cnvExptoExpX m)
  Tpl ts  ms  -> TplX ts (map cnvExptoExpX ms)

-- Above but by using GADTs to make sure annotations are used properly
----------------------------------------------------------------------

-- constructor labels
data Lbl
  = VarL
  | AbsL
  | AppL
  | TplL

data ExpR' id ann where
  VarR' :: id                             -> ExpR' id (ann 'VarL)
  AbsR' :: id -> AExpR' id ann            -> ExpR' id (ann 'AbsL)
  AppR' :: AExpR' id ann -> AExpR' id ann -> ExpR' id (ann 'AppL)
  TplR' :: [AExpR' id ann]                -> ExpR' id (ann 'TplL)

data AExpR' id  ann where
  AnnR' :: ann l -> ExpR' id (ann l) -> AExpR' id ann

data Ann' lbl where
  AVar' :: Ann' 'VarL
  AAbs' :: Typ -> SrcLoc -> Ann' 'AbsL
  AApp' :: Typ -> Ann' 'AppL
  ATpl' :: [Typ] -> Ann' 'TplL

type ExpX' id = AExpR' id Ann'
pattern VarX' :: id -> ExpX' id
pattern AbsX' :: Typ -> SrcLoc -> id -> ExpX' id -> ExpX' id
pattern AppX' :: Typ -> ExpX' id -> ExpX' id -> ExpX' id
pattern TplX' :: [Typ] -> [ExpX' id] -> ExpX' id

pattern VarX'      x   = AnnR' AVar'       (VarR' x)
pattern AbsX' t  s x n = AnnR' (AAbs' t s) (AbsR' x n)
pattern AppX' t    l m = AnnR' (AApp' t)   (AppR' l m)
pattern TplX' ts   ms  = AnnR' (ATpl' ts)  (TplR' ms)

cnvExpX'toExp :: ExpX' id -> Exp id
cnvExpX'toExp = \case
  VarX'     x   -> Var     x
  AbsX' t s x n -> Abs t s x (cnvExpX'toExp n)
  AppX' t   l m -> App t (cnvExpX'toExp l) (cnvExpX'toExp m)
  TplX' ts  ms  -> Tpl ts (map cnvExpX'toExp ms)
  _             -> error "Impossible!" -- possibly not needed with the new checker

cnvExptoExpX' :: Exp id -> ExpX' id
cnvExptoExpX' = \case
  Var     x   -> VarX'     x
  Abs t s x n -> AbsX' t s x (cnvExptoExpX' n)
  App t   l m -> AppX' t (cnvExptoExpX' l) (cnvExptoExpX' m)
  Tpl ts  ms  -> TplX' ts (map cnvExptoExpX' ms)
