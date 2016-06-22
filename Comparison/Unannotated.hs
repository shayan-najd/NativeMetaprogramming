{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
module Unannotated where
--------------------------------------------------------------------------------
-- Unannotated expressions
--------------------------------------------------------------------------------

data Exp id
  = Var id
  | Abs id (Exp id)
  | App (Exp id) (Exp id)
  | Tpl [Exp id]
