{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
module HsBinds where
import {-# SOURCE #-} HsExpr
       (pprExpr, LHsExpr, MatchGroup, pprFunBind, GRHSs, pprPatBind)
import {-# SOURCE #-} HsPat (LPat)
import PlaceHolder (PostTc, PostRn, DataId)
import HsTypes
import PprCore ()
import CoreSyn
import TcEvidence
import Type
import Name
import NameSet
import BasicTypes
import Outputable
import SrcLoc
import Var
import Bag
import FastString
import BooleanFormula (LBooleanFormula)
import DynFlags
import Data.Data hiding (Fixity)
import Data.List hiding (foldr)
import Data.Ord
import Data.Foldable (Foldable(..))

type HsLocalBinds id = HsLocalBindsLR id id

data HsLocalBindsLR idL idR = HsValBinds (HsValBindsLR idL idR)
                            | HsIPBinds (HsIPBinds idR)
                            | EmptyLocalBinds
                            deriving Typeable

deriving instance (DataId idL, DataId idR) => Data
         (HsLocalBindsLR idL idR)

type HsValBinds id = HsValBindsLR id id

data HsValBindsLR idL idR = ValBindsIn (LHsBindsLR idL idR)
                                       [LSig idR]
                          | ValBindsOut [(RecFlag, LHsBinds idL)] [LSig Name]
                          deriving Typeable

deriving instance (DataId idL, DataId idR) => Data
         (HsValBindsLR idL idR)

type LHsBind id = LHsBindLR id id

type LHsBinds id = LHsBindsLR id id

type HsBind id = HsBindLR id id

type LHsBindsLR idL idR = Bag (LHsBindLR idL idR)

type LHsBindLR idL idR = Located (HsBindLR idL idR)

data HsBindLR idL idR = FunBind{fun_id :: Located idL,
                                fun_matches :: MatchGroup idR (LHsExpr idR),
                                fun_co_fn :: HsWrapper, bind_fvs :: PostRn idL NameSet,
                                fun_tick :: [Tickish Id]}
                      | PatBind{pat_lhs :: LPat idL, pat_rhs :: GRHSs idR (LHsExpr idR),
                                pat_rhs_ty :: PostTc idR Type, bind_fvs :: PostRn idL NameSet,
                                pat_ticks :: ([Tickish Id], [[Tickish Id]])}
                      | VarBind{var_id :: idL, var_rhs :: LHsExpr idR,
                                var_inline :: Bool}
                      | AbsBinds{abs_tvs :: [TyVar], abs_ev_vars :: [EvVar],
                                 abs_exports :: [ABExport idL], abs_ev_binds :: [TcEvBinds],
                                 abs_binds :: LHsBinds idL}
                      | AbsBindsSig{abs_tvs :: [TyVar], abs_ev_vars :: [EvVar],
                                    abs_sig_export :: idL, abs_sig_prags :: TcSpecPrags,
                                    abs_sig_ev_bind :: TcEvBinds, abs_sig_bind :: LHsBind idL}
                      | PatSynBind (PatSynBind idL idR)
                      deriving Typeable

deriving instance (DataId idL, DataId idR) => Data
         (HsBindLR idL idR)

data ABExport id = ABE{abe_poly :: id, abe_mono :: id,
                       abe_wrap :: HsWrapper, abe_prags :: TcSpecPrags}
                 deriving (Data, Typeable)

data PatSynBind idL idR = PSB{psb_id :: Located idL,
                              psb_fvs :: PostRn idR NameSet,
                              psb_args :: HsPatSynDetails (Located idR), psb_def :: LPat idR,
                              psb_dir :: HsPatSynDir idR}
                        deriving Typeable

deriving instance (DataId idL, DataId idR) => Data
         (PatSynBind idL idR)

instance (OutputableBndr idL, OutputableBndr idR) => Outputable
         (HsLocalBindsLR idL idR) where
        ppr (HsValBinds bs) = ppr bs
        ppr (HsIPBinds bs) = ppr bs
        ppr EmptyLocalBinds = empty

instance (OutputableBndr idL, OutputableBndr idR) => Outputable
         (HsValBindsLR idL idR) where
        ppr (ValBindsIn binds sigs)
          = pprDeclList (pprLHsBindsForUser binds sigs)
        ppr (ValBindsOut sccs sigs)
          = getPprStyle $
              \ sty ->
                if debugStyle sty then
                  vcat (map ppr sigs) $$ vcat (map ppr_scc sccs) else
                  pprDeclList
                    (pprLHsBindsForUser (unionManyBags (map snd sccs)) sigs)
          where ppr_scc (rec_flag, binds)
                  = pp_rec rec_flag <+> pprLHsBinds binds
                pp_rec Recursive = text "rec"
                pp_rec NonRecursive = text "nonrec"

pprLHsBinds ::
              (OutputableBndr idL, OutputableBndr idR) =>
              LHsBindsLR idL idR -> SDoc
pprLHsBinds binds
  | isEmptyLHsBinds binds = empty
  | otherwise = pprDeclList (map ppr (bagToList binds))

pprLHsBindsForUser ::
                     (OutputableBndr idL, OutputableBndr idR, OutputableBndr id2) =>
                     LHsBindsLR idL idR -> [LSig id2] -> [SDoc]
pprLHsBindsForUser binds sigs = map snd (sort_by_loc decls)
  where decls :: [(SrcSpan, SDoc)]
        decls
          = [(loc, ppr sig) | L loc sig <- sigs] ++
              [(loc, ppr bind) | L loc bind <- bagToList binds]
        sort_by_loc decls = sortBy (comparing fst) decls

pprDeclList :: [SDoc] -> SDoc
pprDeclList ds = pprDeeperList vcat ds

emptyLocalBinds :: HsLocalBindsLR a b
emptyLocalBinds = EmptyLocalBinds

isEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds) = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyValBinds :: HsValBindsLR a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)
  = isEmptyLHsBinds ds && null sigs
isEmptyValBinds (ValBindsOut ds sigs) = null ds && null sigs

emptyValBindsIn, emptyValBindsOut :: HsValBindsLR a b
emptyValBindsIn = ValBindsIn emptyBag []
emptyValBindsOut = ValBindsOut [] []

emptyLHsBinds :: LHsBindsLR idL idR
emptyLHsBinds = emptyBag

isEmptyLHsBinds :: LHsBindsLR idL idR -> Bool
isEmptyLHsBinds = isEmptyBag

plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
plusHsValBinds (ValBindsIn ds1 sigs1) (ValBindsIn ds2 sigs2)
  = ValBindsIn (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
plusHsValBinds (ValBindsOut ds1 sigs1) (ValBindsOut ds2 sigs2)
  = ValBindsOut (ds1 ++ ds2) (sigs1 ++ sigs2)
plusHsValBinds _ _ = panic "HsBinds.plusHsValBinds"

instance (OutputableBndr idL, OutputableBndr idR) => Outputable
         (HsBindLR idL idR) where
        ppr mbind = ppr_monobind mbind

ppr_monobind ::
               (OutputableBndr idL, OutputableBndr idR) =>
               HsBindLR idL idR -> SDoc
ppr_monobind (PatBind{pat_lhs = pat, pat_rhs = grhss})
  = pprPatBind pat grhss
ppr_monobind (VarBind{var_id = var, var_rhs = rhs})
  = sep
      [pprBndr CasePatBind var, nest 2 $ equals <+> pprExpr (unLoc rhs)]
ppr_monobind
  (FunBind{fun_id = fun, fun_co_fn = wrap, fun_matches = matches,
           fun_tick = ticks})
  = pprTicks empty
      (if null ticks then empty else text "-- ticks = " <> ppr ticks)
      $$ ifPprDebug (pprBndr LetBind (unLoc fun))
      $$ pprFunBind (unLoc fun) matches
      $$ ifPprDebug (ppr wrap)
ppr_monobind (PatSynBind psb) = ppr psb
ppr_monobind
  (AbsBinds{abs_tvs = tyvars, abs_ev_vars = dictvars,
            abs_exports = exports, abs_binds = val_binds,
            abs_ev_binds = ev_binds})
  = sdocWithDynFlags $
      \ dflags ->
        if gopt Opt_PrintTypecheckerElaboration dflags then
          hang
            (text "AbsBinds" <+> brackets (interpp'SP tyvars) <+>
               brackets (interpp'SP dictvars))
            2
            $
            braces $
              vcat
                [text "Exports:" <+>
                   brackets (sep (punctuate comma (map ppr exports))),
                 text "Exported types:" <+>
                   vcat [pprBndr LetBind (abe_poly ex) | ex <- exports],
                 text "Binds:" <+> pprLHsBinds val_binds,
                 text "Evidence:" <+> ppr ev_binds]
          else pprLHsBinds val_binds
ppr_monobind
  (AbsBindsSig{abs_tvs = tyvars, abs_ev_vars = dictvars,
               abs_sig_ev_bind = ev_bind, abs_sig_bind = bind})
  = sdocWithDynFlags $
      \ dflags ->
        if gopt Opt_PrintTypecheckerElaboration dflags then
          hang
            (text "AbsBindsSig" <+> brackets (interpp'SP tyvars) <+>
               brackets (interpp'SP dictvars))
            2
            $
            braces $
              vcat [text "Bind:" <+> ppr bind, text "Evidence:" <+> ppr ev_bind]
          else ppr bind

instance (OutputableBndr id) => Outputable (ABExport id) where
        ppr
          (ABE{abe_wrap = wrap, abe_poly = gbl, abe_mono = lcl,
               abe_prags = prags})
          = vcat
              [ppr gbl <+> text "<=" <+> ppr lcl, nest 2 (pprTcSpecPrags prags),
               nest 2 (text "wrap:" <+> ppr wrap)]

instance (OutputableBndr idL, OutputableBndr idR) => Outputable
         (PatSynBind idL idR) where
        ppr
          (PSB{psb_id = L _ psyn, psb_args = details, psb_def = pat,
               psb_dir = dir})
          = ppr_lhs <+> ppr_rhs
          where ppr_lhs = text "pattern" <+> ppr_details
                ppr_simple syntax = syntax <+> ppr pat
                ppr_details
                  = case details of
                        InfixPatSyn v1 v2 -> hsep [ppr v1, pprInfixOcc psyn, ppr v2]
                        PrefixPatSyn vs -> hsep (pprPrefixOcc psyn : map ppr vs)
                        RecordPatSyn vs -> pprPrefixOcc psyn <>
                                             braces (sep (punctuate comma (map ppr vs)))
                ppr_rhs
                  = case dir of
                        Unidirectional -> ppr_simple (text "<-")
                        ImplicitBidirectional -> ppr_simple equals
                        ExplicitBidirectional mg -> ppr_simple (text "<-") <+>
                                                      ptext (sLit "where")
                                                      $$ (nest 2 $ pprFunBind psyn mg)

pprTicks :: SDoc -> SDoc -> SDoc
pprTicks pp_no_debug pp_when_debug
  = getPprStyle
      (\ sty ->
         if debugStyle sty || dumpStyle sty then pp_when_debug else
           pp_no_debug)

data HsIPBinds id = IPBinds [LIPBind id] TcEvBinds
                  deriving Typeable

deriving instance (DataId id) => Data (HsIPBinds id)

isEmptyIPBinds :: HsIPBinds id -> Bool
isEmptyIPBinds (IPBinds is ds) = null is && isEmptyTcEvBinds ds

type LIPBind id = Located (IPBind id)

data IPBind id = IPBind (Either (Located HsIPName) id) (LHsExpr id)
               deriving Typeable

deriving instance (DataId name) => Data (IPBind name)

instance (OutputableBndr id) => Outputable (HsIPBinds id) where
        ppr (IPBinds bs ds)
          = pprDeeperList vcat (map ppr bs) $$ ifPprDebug (ppr ds)

instance (OutputableBndr id) => Outputable (IPBind id) where
        ppr (IPBind lr rhs) = name <+> equals <+> pprExpr (unLoc rhs)
          where name
                  = case lr of
                        Left (L _ ip) -> pprBndr LetBind ip
                        Right id -> pprBndr LetBind id

type LSig name = Located (Sig name)

data Sig name = TypeSig [Located name] (LHsSigWcType name)
              | PatSynSig (Located name) (LHsSigType name)
              | ClassOpSig Bool [Located name] (LHsSigType name)
              | IdSig Id
              | FixSig (FixitySig name)
              | InlineSig (Located name) InlinePragma
              | SpecSig (Located name) [LHsSigType name] InlinePragma
              | SpecInstSig SourceText (LHsSigType name)
              | MinimalSig SourceText (LBooleanFormula (Located name))
              deriving Typeable

deriving instance (DataId name) => Data (Sig name)

type LFixitySig name = Located (FixitySig name)

data FixitySig name = FixitySig [Located name] Fixity
                    deriving (Data, Typeable)

data TcSpecPrags = IsDefaultMethod
                 | SpecPrags [LTcSpecPrag]
                 deriving (Data, Typeable)

type LTcSpecPrag = Located TcSpecPrag

data TcSpecPrag = SpecPrag Id HsWrapper InlinePragma
                deriving (Data, Typeable)

noSpecPrags :: TcSpecPrags
noSpecPrags = SpecPrags []

hasSpecPrags :: TcSpecPrags -> Bool
hasSpecPrags (SpecPrags ps) = not (null ps)
hasSpecPrags IsDefaultMethod = False

isDefaultMethod :: TcSpecPrags -> Bool
isDefaultMethod IsDefaultMethod = True
isDefaultMethod (SpecPrags{}) = False

isFixityLSig :: LSig name -> Bool
isFixityLSig (L _ (FixSig{})) = True
isFixityLSig _ = False

isTypeLSig :: LSig name -> Bool
isTypeLSig (L _ (TypeSig{})) = True
isTypeLSig (L _ (ClassOpSig{})) = True
isTypeLSig (L _ (IdSig{})) = True
isTypeLSig _ = False

isSpecLSig :: LSig name -> Bool
isSpecLSig (L _ (SpecSig{})) = True
isSpecLSig _ = False

isSpecInstLSig :: LSig name -> Bool
isSpecInstLSig (L _ (SpecInstSig{})) = True
isSpecInstLSig _ = False

isPragLSig :: LSig name -> Bool
isPragLSig (L _ (SpecSig{})) = True
isPragLSig (L _ (InlineSig{})) = True
isPragLSig _ = False

isInlineLSig :: LSig name -> Bool
isInlineLSig (L _ (InlineSig{})) = True
isInlineLSig _ = False

isMinimalLSig :: LSig name -> Bool
isMinimalLSig (L _ (MinimalSig{})) = True
isMinimalLSig _ = False

hsSigDoc :: Sig name -> SDoc
hsSigDoc (TypeSig{}) = text "type signature"
hsSigDoc (PatSynSig{}) = text "pattern synonym signature"
hsSigDoc (ClassOpSig is_deflt _ _)
  | is_deflt = text "default type signature"
  | otherwise = text "class method signature"
hsSigDoc (IdSig{}) = text "id signature"
hsSigDoc (SpecSig{}) = text "SPECIALISE pragma"
hsSigDoc (InlineSig _ prag)
  = ppr (inlinePragmaSpec prag) <+> text "pragma"
hsSigDoc (SpecInstSig{}) = text "SPECIALISE instance pragma"
hsSigDoc (FixSig{}) = text "fixity declaration"
hsSigDoc (MinimalSig{}) = text "MINIMAL pragma"

instance (OutputableBndr name) => Outputable (Sig name) where
        ppr sig = ppr_sig sig

ppr_sig :: OutputableBndr name => Sig name -> SDoc
ppr_sig (TypeSig vars ty) = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (ClassOpSig is_deflt vars ty)
  | is_deflt = text "default" <+> pprVarSig (map unLoc vars) (ppr ty)
  | otherwise = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (IdSig id) = pprVarSig [id] (ppr (varType id))
ppr_sig (FixSig fix_sig) = ppr fix_sig
ppr_sig (SpecSig var ty inl)
  = pragBrackets (pprSpec (unLoc var) (interpp'SP ty) inl)
ppr_sig (InlineSig var inl)
  = pragBrackets (ppr inl <+> pprPrefixOcc (unLoc var))
ppr_sig (SpecInstSig _ ty)
  = pragBrackets (text "SPECIALIZE instance" <+> ppr ty)
ppr_sig (MinimalSig _ bf) = pragBrackets (pprMinimalSig bf)
ppr_sig (PatSynSig name sig_ty)
  = text "pattern" <+> pprPrefixOcc (unLoc name) <+> dcolon <+>
      ppr sig_ty

instance OutputableBndr name => Outputable (FixitySig name) where
        ppr (FixitySig names fixity) = sep [ppr fixity, pprops]
          where pprops
                  = hsep $ punctuate comma (map (pprInfixOcc . unLoc) names)

pragBrackets :: SDoc -> SDoc
pragBrackets doc = text "{-#" <+> doc <+> ptext (sLit "#-}")

pprVarSig :: (OutputableBndr id) => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

pprSpec ::
          (OutputableBndr id) => id -> SDoc -> InlinePragma -> SDoc
pprSpec var pp_ty inl
  = text "SPECIALIZE" <+> pp_inl <+> pprVarSig [var] pp_ty
  where pp_inl
          | isDefaultInlinePragma inl = empty
          | otherwise = ppr inl

pprTcSpecPrags :: TcSpecPrags -> SDoc
pprTcSpecPrags IsDefaultMethod = text "<default method>"
pprTcSpecPrags (SpecPrags ps) = vcat (map (ppr . unLoc) ps)

instance Outputable TcSpecPrag where
        ppr (SpecPrag var _ inl) = pprSpec var (text "<type>") inl

pprMinimalSig ::
                OutputableBndr name => LBooleanFormula (Located name) -> SDoc
pprMinimalSig (L _ bf) = text "MINIMAL" <+> ppr (fmap unLoc bf)

data HsPatSynDetails a = InfixPatSyn a a
                       | PrefixPatSyn [a]
                       | RecordPatSyn [RecordPatSynField a]
                       deriving (Typeable, Data)

data RecordPatSynField a = RecordPatSynField{recordPatSynSelectorId
                                             :: a,
                                             recordPatSynPatVar :: a}
                         deriving (Typeable, Data)

instance Functor RecordPatSynField where
        fmap f (RecordPatSynField visible hidden)
          = RecordPatSynField (f visible) (f hidden)

instance Outputable a => Outputable (RecordPatSynField a) where
        ppr (RecordPatSynField v _) = ppr v

instance Foldable RecordPatSynField where
        foldMap f (RecordPatSynField visible hidden)
          = f visible `mappend` f hidden

instance Traversable RecordPatSynField where
        traverse f (RecordPatSynField visible hidden)
          = RecordPatSynField <$> f visible <*> f hidden

instance Functor HsPatSynDetails where
        fmap f (InfixPatSyn left right) = InfixPatSyn (f left) (f right)
        fmap f (PrefixPatSyn args) = PrefixPatSyn (fmap f args)
        fmap f (RecordPatSyn args) = RecordPatSyn (map (fmap f) args)

instance Foldable HsPatSynDetails where
        foldMap f (InfixPatSyn left right) = f left `mappend` f right
        foldMap f (PrefixPatSyn args) = foldMap f args
        foldMap f (RecordPatSyn args) = foldMap (foldMap f) args
        foldl1 f (InfixPatSyn left right) = left `f` right
        foldl1 f (PrefixPatSyn args) = Data.List.foldl1 f args
        foldl1 f (RecordPatSyn args)
          = Data.List.foldl1 f (map (Data.Foldable.foldl1 f) args)
        foldr1 f (InfixPatSyn left right) = left `f` right
        foldr1 f (PrefixPatSyn args) = Data.List.foldr1 f args
        foldr1 f (RecordPatSyn args)
          = Data.List.foldr1 f (map (Data.Foldable.foldr1 f) args)
        length (InfixPatSyn _ _) = 2
        length (PrefixPatSyn args) = Data.List.length args
        length (RecordPatSyn args) = Data.List.length args
        null (InfixPatSyn _ _) = False
        null (PrefixPatSyn args) = Data.List.null args
        null (RecordPatSyn args) = Data.List.null args
        toList (InfixPatSyn left right) = [left, right]
        toList (PrefixPatSyn args) = args
        toList (RecordPatSyn args) = foldMap toList args

instance Traversable HsPatSynDetails where
        traverse f (InfixPatSyn left right)
          = InfixPatSyn <$> f left <*> f right
        traverse f (PrefixPatSyn args) = PrefixPatSyn <$> traverse f args
        traverse f (RecordPatSyn args)
          = RecordPatSyn <$> traverse (traverse f) args

data HsPatSynDir id = Unidirectional
                    | ImplicitBidirectional
                    | ExplicitBidirectional (MatchGroup id (LHsExpr id))
                    deriving Typeable

deriving instance (DataId id) => Data (HsPatSynDir id)