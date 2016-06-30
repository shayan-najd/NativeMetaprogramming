{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
module ApiAnnotationsExample where

import qualified Unannotated                 as U
import qualified UnannotatedExtension        as U
import qualified Annotated                   as A
import qualified AnnotatedExtension          as A
import AnnotatedIndexed (Lbl(..))
import qualified AnnotatedIndexedExtension   as AI
import qualified UnannotatedIndexedExtension as UI

import qualified ProductIndexed              as PI

import Data.List

--------------------------------------------------------------------------------
-- API Annotations example based on Product (Indexed) Annotations
--------------------------------------------------------------------------------

{-

We have a running example of a simple lambda terms with n-ary tuples


data Exp id
  = Var id
  | Abs id (Exp id)
  | App (Exp id) (Exp id)
  | Tpl [Exp id]

If we assume a concrete syntax such as

  id = ALPHANUMERIC_LITERAL

  exp = var | abs | app | tpl

  var = id

  abs = `\` id '.' exp

  tpl = '(' commalist ')'

  commalist = exp
            | exp ',' commalist

We then have strings such as

  x
  \x.x
  (x,y,z)

and the more complex nested versions.

-}


type Id = String

x :: Id
x = "x"

--------------------------------------------------------------------------------
-- represent "\x.x"
--            0123
--------------------------------------------------------------------------------

-- The bare AST
eg_unannotated :: U.Exp Id
eg_unannotated = U.Abs x (U.Var x)

-- The AST in GHC Located format
eg_located :: LExp Id
eg_located
  = L (Span (1,0) (1,3))
         (Abs (L (Span (1,1) (1,2)) x )
              (L (Span (1,3) (1,4)) (Var x) )
         )

-- The AST in the new format, including existing locations and the requirements
-- for API Annotations
eg :: PI.Exp RowAnn ColAnn Id
eg = PI.Abs ann1 x (PI.Var ann2 x)


ann1 :: RowAnn 'AbsL
ann1 = AAbs (Span (1,0) (1,4)) -- full
            (Span (1,0) (1,1)) -- '\'
            (Span (1,1) (1,2)) -- id
            (Span (1,2) (1,3)) -- '.'

ann2 :: RowAnn 'VarL
ann2 = AVar (Span (1,3) (1,4))

data RowAnn lbl where
  AVar :: SrcSpan   -> RowAnn 'VarL
  AAbs :: { lextent :: SrcSpan -- full extent of the Abs
          , llambda :: SrcSpan -- Location of the '\'
          , lid     :: SrcSpan -- Location of the id
          , ldot    :: SrcSpan -- Location of the '.'
          } -> RowAnn 'AbsL
  AApp :: SrcSpan   -> RowAnn 'AppL
  ATpl :: [SrcSpan] -> RowAnn 'TplL

instance Show (RowAnn lbl) where
  show (AVar ss)          = "(AVar (" ++ show ss ++ "))"
  show (AAbs ss sl li ld) = "(AAbs (" ++ intercalate " " (map show [ss,sl,li,ld]) ++ "))"

data ColAnn exp = ColAnn deriving Show

type SrcLoc = (Int,Int)
data SrcSpan = Span SrcLoc SrcLoc deriving Show

deriving instance (Show id) => Show (U.Exp id)
deriving instance Show (PI.Exp RowAnn ColAnn Id)

--------------------------------------------------------------------------------

data AnnKeywordId
    = AnnLambda  -- ^ '\'
    | AnnDot     -- ^ '.'
    deriving (Show,Eq)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Equivalent to GHC Located AST for the example
--------------------------------------------------------------------------------

type LExp id = Located (Exp id)

data Exp id
  = Var id
  | Abs (Located id) (LExp id)
  | App (LExp id) (LExp id)
  | Tpl [LExp id]


data GenLocated l e = L l e
  deriving (Eq, Ord, Show)

type Located e = GenLocated SrcSpan e
