{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE GADTs,TypeFamilies,DataKinds, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes,KindSignatures #-}
module Example where

import Extensible (desugarExtensible,Extensible(..),Extends(..))
import GHC.TypeLits

data SrcLoc
  = SrcLoc Int Int

desugarExtensible "Ext"
           [d| {-# ANN type Exp Extensible #-}
               data Exp id
                 = Lit Lit
                 | Var id
                 | Abs (Pat id) (Exp id)
                 | App (Exp id) (Exp id)
                 | Tup [Exp id]
                 | Let [Dec id] (Exp id)

               {-# ANN type Pat Extensible #-}
               data Pat id
                 = VarP id
                 | TupP [Pat id]
                 | VieP (Exp id) (Pat id)

               {-# ANN type Dec Extensible #-}
               data Dec id
                 = Fun {name :: id, patterns :: [Pat id], body :: Exp id}
                 | Pat (Pat id)  (Exp id)
                 | Sig id Typ

               {-# ANN type Lit Extensible #-}
               data Lit
                 = Int Integer
                 | Rat Rational
                 | Str String

               {-# ANN type Typ Extensible #-}
               data Typ where
                 INT :: Typ
                 ARR :: {argument :: Typ, result :: Typ} -> Typ

               {-# ANN type HsExp (Extends "Exp") #-}
               data HsExp id
                 = HsAbs (Extends "Abs") (HsTyp id) SrcLoc
                   -- (Extends ...) is a dummy field
                   -- that I used for simulating syntax.
                 | HsApp (Extends "App") (HsTyp id)
                 | HsTup (Extends "Tup") (HsExp id)
                 | HsOut (HsTyp id)  (HsExp id)
                 | HsLoc SrcLoc (HsExp id)

               {-# ANN type HsPat (Extends "Pat") #-}
               data HsPat id
                 = HsVarP (Extends "VarP") (HsTyp id)

               {-# ANN type HsDec (Extends "Dec") #-}
               data HsDec id

               {-# ANN type HsLit (Extends "Lit") #-}
               data HsLit id

               {-# ANN type HsTyp (Extends "Typ") #-}
               data HsTyp id

               data Foo id = Bar (HsTyp id)
           |]

pattern HsLit :: HsLit id -> HsExp id
pattern HsVar :: id  -> HsExp id
pattern HsAbs :: HsPat id -> HsExp id -> HsTyp id -> SrcLoc -> HsExp id
pattern HsApp :: HsExp id -> HsExp id -> HsTyp id -> HsExp id
pattern HsTup :: [HsExp id] -> HsExp id -> HsExp id
pattern HsLet :: [HsDec id] -> HsExp id -> HsExp id

pattern HsLit i       = Lit LitX       i
pattern HsVar x       = Var VarX       x
pattern HsAbs p n a l = Abs (AbsX a l) p n
pattern HsApp l m a   = App (AppX a)   l m
pattern HsTup ms m    = Tup (TupX m)   ms
pattern HsLet ms n    = Let LetX       ms n
