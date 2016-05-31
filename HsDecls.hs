{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module HsDecls
       (HsDecl(..), LHsDecl, HsDataDefn(..), HsDeriving, TyClDecl(..),
        LTyClDecl, TyClGroup(..), mkTyClGroup, emptyTyClGroup,
        tyClGroupTyClDecls, tyClGroupInstDecls, tyClGroupRoleDecls,
        isClassDecl, isDataDecl, isSynDecl, tcdName, isFamilyDecl,
        isTypeFamilyDecl, isDataFamilyDecl, isOpenTypeFamilyInfo,
        isClosedTypeFamilyInfo, tyFamInstDeclName, tyFamInstDeclLName,
        countTyClDecls, pprTyClDeclFlavour, tyClDeclLName, tyClDeclTyVars,
        hsDeclHasCusk, famDeclHasCusk, FamilyDecl(..), LFamilyDecl,
        InstDecl(..), LInstDecl, NewOrData(..), FamilyInfo(..),
        TyFamInstDecl(..), LTyFamInstDecl, instDeclDataFamInsts,
        DataFamInstDecl(..), LDataFamInstDecl, pprDataFamInstFlavour,
        TyFamEqn(..), TyFamInstEqn, LTyFamInstEqn, TyFamDefltEqn,
        LTyFamDefltEqn, HsTyPats, LClsInstDecl, ClsInstDecl(..),
        DerivDecl(..), LDerivDecl, LRuleDecls, RuleDecls(..), RuleDecl(..),
        LRuleDecl, RuleBndr(..), LRuleBndr, collectRuleBndrSigTys,
        flattenRuleDecls, pprFullRuleName, VectDecl(..), LVectDecl,
        lvectDeclName, lvectInstDecl, DefaultDecl(..), LDefaultDecl,
        SpliceExplicitFlag(..), SpliceDecl(..), LSpliceDecl,
        ForeignDecl(..), LForeignDecl, ForeignImport(..),
        ForeignExport(..), noForeignImportCoercionYet,
        noForeignExportCoercionYet, CImportSpec(..), ConDecl(..), LConDecl,
        HsConDeclDetails, hsConDeclArgTys, getConNames, getConDetails,
        gadtDeclDetails, DocDecl(..), LDocDecl, docDeclDoc, WarnDecl(..),
        LWarnDecl, WarnDecls(..), LWarnDecls, AnnDecl(..), LAnnDecl,
        AnnProvenance(..), annProvenanceName_maybe, RoleAnnotDecl(..),
        LRoleAnnotDecl, roleAnnotDeclName, FamilyResultSig(..),
        LFamilyResultSig, InjectivityAnn(..), LInjectivityAnn,
        resultVariableName, HsGroup(..), emptyRdrGroup, emptyRnGroup,
        appendGroups, hsGroupInstDecls)
       where
import {-# SOURCE #-} HsExpr
       (LHsExpr, HsExpr, HsSplice, pprExpr, pprSplice)
import HsBinds
import HsTypes
import HsDoc
import TyCon
import Name
import BasicTypes
import Coercion
import ForeignCall
import PlaceHolder (PostTc, PostRn, PlaceHolder(..), DataId)
import NameSet
import InstEnv
import Class
import Outputable
import Util
import SrcLoc
import Bag
import Maybes
import Data.Data hiding (TyCon, Fixity)

type LHsDecl id = Located (HsDecl id)

data HsDecl id = TyClD (TyClDecl id)
               | InstD (InstDecl id)
               | DerivD (DerivDecl id)
               | ValD (HsBind id)
               | SigD (Sig id)
               | DefD (DefaultDecl id)
               | ForD (ForeignDecl id)
               | WarningD (WarnDecls id)
               | AnnD (AnnDecl id)
               | RuleD (RuleDecls id)
               | VectD (VectDecl id)
               | SpliceD (SpliceDecl id)
               | DocD (DocDecl)
               | RoleAnnotD (RoleAnnotDecl id)
               deriving Typeable

deriving instance (DataId id) => Data (HsDecl id)

data HsGroup id = HsGroup{hs_valds :: HsValBinds id,
                          hs_splcds :: [LSpliceDecl id], hs_tyclds :: [TyClGroup id],
                          hs_derivds :: [LDerivDecl id], hs_fixds :: [LFixitySig id],
                          hs_defds :: [LDefaultDecl id], hs_fords :: [LForeignDecl id],
                          hs_warnds :: [LWarnDecls id], hs_annds :: [LAnnDecl id],
                          hs_ruleds :: [LRuleDecls id], hs_vects :: [LVectDecl id],
                          hs_docs :: [LDocDecl]}
                deriving Typeable

deriving instance (DataId id) => Data (HsGroup id)

emptyGroup, emptyRdrGroup, emptyRnGroup :: HsGroup a
emptyRdrGroup = emptyGroup{hs_valds = emptyValBindsIn}
emptyRnGroup = emptyGroup{hs_valds = emptyValBindsOut}

hsGroupInstDecls :: HsGroup id -> [LInstDecl id]
hsGroupInstDecls = (=<<) group_instds . hs_tyclds
emptyGroup
  = HsGroup{hs_tyclds = [], hs_derivds = [], hs_fixds = [],
            hs_defds = [], hs_annds = [], hs_fords = [], hs_warnds = [],
            hs_ruleds = [], hs_vects = [],
            hs_valds = error "emptyGroup hs_valds: Can't happen",
            hs_splcds = [], hs_docs = []}

appendGroups :: HsGroup a -> HsGroup a -> HsGroup a
appendGroups
  HsGroup{hs_valds = val_groups1, hs_splcds = spliceds1,
          hs_tyclds = tyclds1, hs_derivds = derivds1, hs_fixds = fixds1,
          hs_defds = defds1, hs_annds = annds1, hs_fords = fords1,
          hs_warnds = warnds1, hs_ruleds = rulds1, hs_vects = vects1,
          hs_docs = docs1}
  HsGroup{hs_valds = val_groups2, hs_splcds = spliceds2,
          hs_tyclds = tyclds2, hs_derivds = derivds2, hs_fixds = fixds2,
          hs_defds = defds2, hs_annds = annds2, hs_fords = fords2,
          hs_warnds = warnds2, hs_ruleds = rulds2, hs_vects = vects2,
          hs_docs = docs2}
  = HsGroup{hs_valds = val_groups1 `plusHsValBinds` val_groups2,
            hs_splcds = spliceds1 ++ spliceds2, hs_tyclds = tyclds1 ++ tyclds2,
            hs_derivds = derivds1 ++ derivds2, hs_fixds = fixds1 ++ fixds2,
            hs_annds = annds1 ++ annds2, hs_defds = defds1 ++ defds2,
            hs_fords = fords1 ++ fords2, hs_warnds = warnds1 ++ warnds2,
            hs_ruleds = rulds1 ++ rulds2, hs_vects = vects1 ++ vects2,
            hs_docs = docs1 ++ docs2}

instance OutputableBndr name => Outputable (HsDecl name) where
        ppr (TyClD dcl) = ppr dcl
        ppr (ValD binds) = ppr binds
        ppr (DefD def) = ppr def
        ppr (InstD inst) = ppr inst
        ppr (DerivD deriv) = ppr deriv
        ppr (ForD fd) = ppr fd
        ppr (SigD sd) = ppr sd
        ppr (RuleD rd) = ppr rd
        ppr (VectD vect) = ppr vect
        ppr (WarningD wd) = ppr wd
        ppr (AnnD ad) = ppr ad
        ppr (SpliceD dd) = ppr dd
        ppr (DocD doc) = ppr doc
        ppr (RoleAnnotD ra) = ppr ra

instance OutputableBndr name => Outputable (HsGroup name) where
        ppr
          (HsGroup{hs_valds = val_decls, hs_tyclds = tycl_decls,
                   hs_derivds = deriv_decls, hs_fixds = fix_decls,
                   hs_warnds = deprec_decls, hs_annds = ann_decls,
                   hs_fords = foreign_decls, hs_defds = default_decls,
                   hs_ruleds = rule_decls, hs_vects = vect_decls})
          = vcat_mb empty
              [ppr_ds fix_decls, ppr_ds default_decls, ppr_ds deprec_decls,
               ppr_ds ann_decls, ppr_ds rule_decls, ppr_ds vect_decls,
               if isEmptyValBinds val_decls then Nothing else
                 Just (ppr val_decls),
               ppr_ds (tyClGroupTyClDecls tycl_decls),
               ppr_ds (tyClGroupInstDecls tycl_decls), ppr_ds deriv_decls,
               ppr_ds foreign_decls]
          where ppr_ds :: Outputable a => [a] -> Maybe SDoc
                ppr_ds [] = Nothing
                ppr_ds ds = Just (vcat (map ppr ds))
                
                vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
                vcat_mb _ [] = empty
                vcat_mb gap (Nothing : ds) = vcat_mb gap ds
                vcat_mb gap (Just d : ds) = gap $$ d $$ vcat_mb blankLine ds

data SpliceExplicitFlag = ExplicitSplice
                        | ImplicitSplice
                        deriving (Data, Typeable)

type LSpliceDecl name = Located (SpliceDecl name)

data SpliceDecl id = SpliceDecl (Located (HsSplice id))
                                SpliceExplicitFlag
                   deriving Typeable

deriving instance (DataId id) => Data (SpliceDecl id)

instance OutputableBndr name => Outputable (SpliceDecl name) where
        ppr (SpliceDecl (L _ e) _) = pprSplice e

type LTyClDecl name = Located (TyClDecl name)

data TyClDecl name = FamDecl{tcdFam :: FamilyDecl name}
                   | SynDecl{tcdLName :: Located name, tcdTyVars :: LHsQTyVars name,
                             tcdRhs :: LHsType name, tcdFVs :: PostRn name NameSet}
                   | DataDecl{tcdLName :: Located name, tcdTyVars :: LHsQTyVars name,
                              tcdDataDefn :: HsDataDefn name, tcdDataCusk :: PostRn name Bool,
                              tcdFVs :: PostRn name NameSet}
                   | ClassDecl{tcdCtxt :: LHsContext name, tcdLName :: Located name,
                               tcdTyVars :: LHsQTyVars name,
                               tcdFDs :: [Located (FunDep (Located name))],
                               tcdSigs :: [LSig name], tcdMeths :: LHsBinds name,
                               tcdATs :: [LFamilyDecl name], tcdATDefs :: [LTyFamDefltEqn name],
                               tcdDocs :: [LDocDecl], tcdFVs :: PostRn name NameSet}
                   deriving Typeable

deriving instance (DataId id) => Data (TyClDecl id)

isDataDecl :: TyClDecl name -> Bool
isDataDecl (DataDecl{}) = True
isDataDecl _other = False

isSynDecl :: TyClDecl name -> Bool
isSynDecl (SynDecl{}) = True
isSynDecl _other = False

isClassDecl :: TyClDecl name -> Bool
isClassDecl (ClassDecl{}) = True
isClassDecl _ = False

isFamilyDecl :: TyClDecl name -> Bool
isFamilyDecl (FamDecl{}) = True
isFamilyDecl _other = False

isTypeFamilyDecl :: TyClDecl name -> Bool
isTypeFamilyDecl (FamDecl (FamilyDecl{fdInfo = info}))
  = case info of
        OpenTypeFamily -> True
        ClosedTypeFamily{} -> True
        _ -> False
isTypeFamilyDecl _ = False

isOpenTypeFamilyInfo :: FamilyInfo name -> Bool
isOpenTypeFamilyInfo OpenTypeFamily = True
isOpenTypeFamilyInfo _ = False

isClosedTypeFamilyInfo :: FamilyInfo name -> Bool
isClosedTypeFamilyInfo (ClosedTypeFamily{}) = True
isClosedTypeFamilyInfo _ = False

isDataFamilyDecl :: TyClDecl name -> Bool
isDataFamilyDecl (FamDecl (FamilyDecl{fdInfo = DataFamily})) = True
isDataFamilyDecl _other = False

tyFamInstDeclName :: TyFamInstDecl name -> name
tyFamInstDeclName = unLoc . tyFamInstDeclLName

tyFamInstDeclLName :: TyFamInstDecl name -> Located name
tyFamInstDeclLName
  (TyFamInstDecl{tfid_eqn = (L _ (TyFamEqn{tfe_tycon = ln}))}) = ln

tyClDeclLName :: TyClDecl name -> Located name
tyClDeclLName (FamDecl{tcdFam = FamilyDecl{fdLName = ln}}) = ln
tyClDeclLName decl = tcdLName decl

tcdName :: TyClDecl name -> name
tcdName = unLoc . tyClDeclLName

tyClDeclTyVars :: TyClDecl name -> LHsQTyVars name
tyClDeclTyVars (FamDecl{tcdFam = FamilyDecl{fdTyVars = tvs}}) = tvs
tyClDeclTyVars d = tcdTyVars d

countTyClDecls :: [TyClDecl name] -> (Int, Int, Int, Int, Int)
countTyClDecls decls
  = (count isClassDecl decls, count isSynDecl decls,
     count isDataTy decls, count isNewTy decls,
     count isFamilyDecl decls)
  where isDataTy DataDecl{tcdDataDefn = HsDataDefn{dd_ND = DataType}}
          = True
        isDataTy _ = False
        isNewTy DataDecl{tcdDataDefn = HsDataDefn{dd_ND = NewType}} = True
        isNewTy _ = False

hsDeclHasCusk :: TyClDecl Name -> Bool
hsDeclHasCusk (FamDecl{tcdFam = fam_decl})
  = famDeclHasCusk Nothing fam_decl
hsDeclHasCusk (SynDecl{tcdTyVars = tyvars, tcdRhs = rhs})
  = hsTvbAllKinded tyvars && rhs_annotated rhs
  where rhs_annotated (L _ ty)
          = case ty of
                HsParTy lty -> rhs_annotated lty
                HsKindSig{} -> True
                _ -> False
hsDeclHasCusk (DataDecl{tcdDataCusk = cusk}) = cusk
hsDeclHasCusk (ClassDecl{tcdTyVars = tyvars})
  = hsTvbAllKinded tyvars

instance OutputableBndr name => Outputable (TyClDecl name) where
        ppr (FamDecl{tcdFam = decl}) = ppr decl
        ppr (SynDecl{tcdLName = ltycon, tcdTyVars = tyvars, tcdRhs = rhs})
          = hang
              (text "type" <+> pp_vanilla_decl_head ltycon tyvars [] <+> equals)
              4
              (ppr rhs)
        ppr
          (DataDecl{tcdLName = ltycon, tcdTyVars = tyvars,
                    tcdDataDefn = defn})
          = pp_data_defn (pp_vanilla_decl_head ltycon tyvars) defn
        ppr
          (ClassDecl{tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                     tcdFDs = fds, tcdSigs = sigs, tcdMeths = methods, tcdATs = ats,
                     tcdATDefs = at_defs})
          | null sigs && isEmptyBag methods && null ats && null at_defs =
            top_matter
          | otherwise =
            vcat
              [top_matter <+> text "where",
               nest 2 $
                 pprDeclList
                   (map (pprFamilyDecl NotTopLevel . unLoc) ats ++
                      map ppr_fam_deflt_eqn at_defs ++ pprLHsBindsForUser methods sigs)]
          where top_matter
                  = text "class" <+>
                      pp_vanilla_decl_head lclas tyvars (unLoc context)
                      <+> pprFundeps (map unLoc fds)

instance OutputableBndr name => Outputable (TyClGroup name) where
        ppr
          (TyClGroup{group_tyclds = tyclds, group_roles = roles,
                     group_instds = instds})
          = ppr tyclds $$ ppr roles $$ ppr instds

pp_vanilla_decl_head ::
                       OutputableBndr name =>
                       Located name -> LHsQTyVars name -> HsContext name -> SDoc
pp_vanilla_decl_head thing tyvars context
  = hsep
      [pprHsContext context, pprPrefixOcc (unLoc thing), ppr tyvars]

pprTyClDeclFlavour :: TyClDecl a -> SDoc
pprTyClDeclFlavour (ClassDecl{}) = text "class"
pprTyClDeclFlavour (SynDecl{}) = text "type"
pprTyClDeclFlavour (FamDecl{tcdFam = FamilyDecl{fdInfo = info}})
  = pprFlavour info <+> text "family"
pprTyClDeclFlavour (DataDecl{tcdDataDefn = HsDataDefn{dd_ND = nd}})
  = ppr nd

data TyClGroup name = TyClGroup{group_tyclds :: [LTyClDecl name],
                                group_roles :: [LRoleAnnotDecl name],
                                group_instds :: [LInstDecl name]}
                    deriving Typeable

deriving instance (DataId id) => Data (TyClGroup id)

emptyTyClGroup :: TyClGroup name
emptyTyClGroup = TyClGroup [] [] []

tyClGroupTyClDecls :: [TyClGroup name] -> [LTyClDecl name]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup name] -> [LInstDecl name]
tyClGroupInstDecls = concatMap group_instds

tyClGroupRoleDecls :: [TyClGroup name] -> [LRoleAnnotDecl name]
tyClGroupRoleDecls = concatMap group_roles

mkTyClGroup ::
            [LTyClDecl name] -> [LInstDecl name] -> TyClGroup name
mkTyClGroup decls instds
  = TyClGroup{group_tyclds = decls, group_roles = [],
              group_instds = instds}

type LFamilyResultSig name = Located (FamilyResultSig name)

data FamilyResultSig name = NoSig
                          | KindSig (LHsKind name)
                          | TyVarSig (LHsTyVarBndr name)
                          deriving Typeable

deriving instance (DataId name) => Data (FamilyResultSig name)

type LFamilyDecl name = Located (FamilyDecl name)

data FamilyDecl name = FamilyDecl{fdInfo :: FamilyInfo name,
                                  fdLName :: Located name, fdTyVars :: LHsQTyVars name,
                                  fdResultSig :: LFamilyResultSig name,
                                  fdInjectivityAnn :: Maybe (LInjectivityAnn name)}
                     deriving Typeable

deriving instance (DataId id) => Data (FamilyDecl id)

type LInjectivityAnn name = Located (InjectivityAnn name)

data InjectivityAnn name = InjectivityAnn (Located name)
                                          [Located name]
                         deriving (Data, Typeable)

data FamilyInfo name = DataFamily
                     | OpenTypeFamily
                     | ClosedTypeFamily (Maybe [LTyFamInstEqn name])
                     deriving Typeable

deriving instance (DataId name) => Data (FamilyInfo name)

famDeclHasCusk :: Maybe Bool -> FamilyDecl name -> Bool
famDeclHasCusk _
  (FamilyDecl{fdInfo = ClosedTypeFamily _, fdTyVars = tyvars,
              fdResultSig = L _ resultSig})
  = hsTvbAllKinded tyvars && hasReturnKindSignature resultSig
famDeclHasCusk mb_class_cusk _ = mb_class_cusk `orElse` True

hasReturnKindSignature :: FamilyResultSig a -> Bool
hasReturnKindSignature NoSig = False
hasReturnKindSignature (TyVarSig (L _ (UserTyVar _))) = False
hasReturnKindSignature _ = True

resultVariableName :: FamilyResultSig a -> Maybe a
resultVariableName (TyVarSig sig) = Just $ hsLTyVarName sig
resultVariableName _ = Nothing

instance (OutputableBndr name) => Outputable (FamilyDecl name)
         where
        ppr = pprFamilyDecl TopLevel

pprFamilyDecl ::
                OutputableBndr name => TopLevelFlag -> FamilyDecl name -> SDoc
pprFamilyDecl top_level
  (FamilyDecl{fdInfo = info, fdLName = ltycon, fdTyVars = tyvars,
              fdResultSig = L _ result, fdInjectivityAnn = mb_inj})
  = vcat
      [pprFlavour info <+> pp_top_level <+>
         pp_vanilla_decl_head ltycon tyvars []
         <+> pp_kind
         <+> pp_inj
         <+> pp_where,
       nest 2 $ pp_eqns]
  where pp_top_level
          = case top_level of
                TopLevel -> text "family"
                NotTopLevel -> empty
        pp_kind
          = case result of
                NoSig -> empty
                KindSig kind -> dcolon <+> ppr kind
                TyVarSig tv_bndr -> text "=" <+> ppr tv_bndr
        pp_inj
          = case mb_inj of
                Just (L _ (InjectivityAnn lhs rhs)) -> hsep
                                                         [vbar, ppr lhs, text "->",
                                                          hsep (map ppr rhs)]
                Nothing -> empty
        (pp_where, pp_eqns)
          = case info of
                ClosedTypeFamily mb_eqns -> (text "where",
                                             case mb_eqns of
                                                 Nothing -> text ".."
                                                 Just eqns -> vcat $ map ppr_fam_inst_eqn eqns)
                _ -> (empty, empty)

pprFlavour :: FamilyInfo name -> SDoc
pprFlavour DataFamily = text "data"
pprFlavour OpenTypeFamily = text "type"
pprFlavour (ClosedTypeFamily{}) = text "type"

instance Outputable (FamilyInfo name) where
        ppr info = pprFlavour info <+> text "family"

data HsDataDefn name = HsDataDefn{dd_ND :: NewOrData,
                                  dd_ctxt :: LHsContext name, dd_cType :: Maybe (Located CType),
                                  dd_kindSig :: Maybe (LHsKind name), dd_cons :: [LConDecl name],
                                  dd_derivs :: HsDeriving name}
                     deriving Typeable

deriving instance (DataId id) => Data (HsDataDefn id)

type HsDeriving name = Maybe (Located [LHsSigType name])

data NewOrData = NewType
               | DataType
               deriving (Eq, Data, Typeable)

type LConDecl name = Located (ConDecl name)

data ConDecl name = ConDeclGADT{con_names :: [Located name],
                                con_type :: LHsSigType name, con_doc :: Maybe LHsDocString}
                  | ConDeclH98{con_name :: Located name,
                               con_qvars :: Maybe (LHsQTyVars name),
                               con_cxt :: Maybe (LHsContext name),
                               con_details :: HsConDeclDetails name,
                               con_doc :: Maybe LHsDocString}
                  deriving Typeable

deriving instance (DataId name) => Data (ConDecl name)

type HsConDeclDetails name =
     HsConDetails (LBangType name) (Located [LConDeclField name])

getConNames :: ConDecl name -> [Located name]
getConNames ConDeclH98{con_name = name} = [name]
getConNames ConDeclGADT{con_names = names} = names

getConDetails :: ConDecl name -> HsConDeclDetails name
getConDetails ConDeclH98{con_details = details} = details
getConDetails ConDeclGADT{con_type = ty} = details
  where (details, _, _, _) = gadtDeclDetails ty

gadtDeclDetails ::
                LHsSigType name ->
                  (HsConDeclDetails name, LHsType name, LHsContext name,
                   [LHsTyVarBndr name])
gadtDeclDetails HsIB{hsib_body = lbody_ty}
  = (details, res_ty, cxt, tvs)
  where (tvs, cxt, tau) = splitLHsSigmaTy lbody_ty
        (details, res_ty)
          = case tau of
                L _ (HsFunTy (L l (HsRecTy flds)) res_ty') -> (RecCon (L l flds),
                                                               res_ty')
                _other -> (PrefixCon [], tau)

hsConDeclArgTys :: HsConDeclDetails name -> [LBangType name]
hsConDeclArgTys (PrefixCon tys) = tys
hsConDeclArgTys (InfixCon ty1 ty2) = [ty1, ty2]
hsConDeclArgTys (RecCon flds)
  = map (cd_fld_type . unLoc) (unLoc flds)

pp_data_defn ::
               OutputableBndr name =>
               (HsContext name -> SDoc) -> HsDataDefn name -> SDoc
pp_data_defn pp_hdr
  (HsDataDefn{dd_ND = new_or_data, dd_ctxt = L _ context,
              dd_kindSig = mb_sig, dd_cons = condecls, dd_derivs = derivings})
  | null condecls = ppr new_or_data <+> pp_hdr context <+> pp_sig
  | otherwise =
    hang (ppr new_or_data <+> pp_hdr context <+> pp_sig) 2
      (pp_condecls condecls $$ pp_derivings)
  where pp_sig
          = case mb_sig of
                Nothing -> empty
                Just kind -> dcolon <+> ppr kind
        pp_derivings
          = case derivings of
                Nothing -> empty
                Just (L _ ds) -> hsep [text "deriving", parens (interpp'SP ds)]

instance OutputableBndr name => Outputable (HsDataDefn name) where
        ppr d = pp_data_defn (\ _ -> text "Naked HsDataDefn") d

instance Outputable NewOrData where
        ppr NewType = text "newtype"
        ppr DataType = text "data"

pp_condecls :: OutputableBndr name => [LConDecl name] -> SDoc
pp_condecls cs@(L _ ConDeclGADT{} : _)
  = hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs
  = equals <+> sep (punctuate (text " |") (map ppr cs))

instance (OutputableBndr name) => Outputable (ConDecl name) where
        ppr = pprConDecl

pprConDecl :: OutputableBndr name => ConDecl name -> SDoc
pprConDecl
  (ConDeclH98{con_name = L _ con, con_qvars = mtvs, con_cxt = mcxt,
              con_details = details, con_doc = doc})
  = sep [ppr_mbDoc doc, pprHsForAll tvs cxt, ppr_details details]
  where ppr_details (InfixCon t1 t2)
          = hsep [ppr t1, pprInfixOcc con, ppr t2]
        ppr_details (PrefixCon tys)
          = hsep (pprPrefixOcc con : map (pprParendHsType . unLoc) tys)
        ppr_details (RecCon fields)
          = pprPrefixOcc con <+> pprConDeclFields (unLoc fields)
        tvs
          = case mtvs of
                Nothing -> []
                Just (HsQTvs{hsq_explicit = tvs}) -> tvs
        cxt = fromMaybe (noLoc []) mcxt
pprConDecl
  (ConDeclGADT{con_names = cons, con_type = res_ty, con_doc = doc})
  = sep
      [ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon <+> ppr res_ty]

ppr_con_names :: (OutputableBndr name) => [Located name] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

type LTyFamInstEqn name = Located (TyFamInstEqn name)

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type HsTyPats name = HsImplicitBndrs name [LHsType name]

type TyFamInstEqn name = TyFamEqn name (HsTyPats name)

type TyFamDefltEqn name = TyFamEqn name (LHsQTyVars name)

data TyFamEqn name pats = TyFamEqn{tfe_tycon :: Located name,
                                   tfe_pats :: pats, tfe_rhs :: LHsType name}
                        deriving Typeable

deriving instance (DataId name, Data pats) => Data
         (TyFamEqn name pats)

type LTyFamInstDecl name = Located (TyFamInstDecl name)

data TyFamInstDecl name = TyFamInstDecl{tfid_eqn ::
                                        LTyFamInstEqn name,
                                        tfid_fvs :: PostRn name NameSet}
                        deriving Typeable

deriving instance (DataId name) => Data (TyFamInstDecl name)

type LDataFamInstDecl name = Located (DataFamInstDecl name)

data DataFamInstDecl name = DataFamInstDecl{dfid_tycon ::
                                            Located name,
                                            dfid_pats :: HsTyPats name,
                                            dfid_defn :: HsDataDefn name,
                                            dfid_fvs :: PostRn name NameSet}
                          deriving Typeable

deriving instance (DataId name) => Data (DataFamInstDecl name)

type LClsInstDecl name = Located (ClsInstDecl name)

data ClsInstDecl name = ClsInstDecl{cid_poly_ty :: LHsSigType name,
                                    cid_binds :: LHsBinds name, cid_sigs :: [LSig name],
                                    cid_tyfam_insts :: [LTyFamInstDecl name],
                                    cid_datafam_insts :: [LDataFamInstDecl name],
                                    cid_overlap_mode :: Maybe (Located OverlapMode)}
                      deriving Typeable

deriving instance (DataId id) => Data (ClsInstDecl id)

type LInstDecl name = Located (InstDecl name)

data InstDecl name = ClsInstD{cid_inst :: ClsInstDecl name}
                   | DataFamInstD{dfid_inst :: DataFamInstDecl name}
                   | TyFamInstD{tfid_inst :: TyFamInstDecl name}
                   deriving Typeable

deriving instance (DataId id) => Data (InstDecl id)

instance (OutputableBndr name) => Outputable (TyFamInstDecl name)
         where
        ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl ::
                   OutputableBndr name => TopLevelFlag -> TyFamInstDecl name -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl{tfid_eqn = eqn})
  = text "type" <+> ppr_instance_keyword top_lvl <+>
      ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel = text "instance"
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn ::
                   OutputableBndr name => LTyFamInstEqn name -> SDoc
ppr_fam_inst_eqn
  (L _ (TyFamEqn{tfe_tycon = tycon, tfe_pats = pats, tfe_rhs = rhs}))
  = pp_fam_inst_lhs tycon pats [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn ::
                    OutputableBndr name => LTyFamDefltEqn name -> SDoc
ppr_fam_deflt_eqn
  (L _ (TyFamEqn{tfe_tycon = tycon, tfe_pats = tvs, tfe_rhs = rhs}))
  = text "type" <+> pp_vanilla_decl_head tycon tvs [] <+> equals <+>
      ppr rhs

instance (OutputableBndr name) => Outputable (DataFamInstDecl name)
         where
        ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl ::
                     OutputableBndr name => TopLevelFlag -> DataFamInstDecl name -> SDoc
pprDataFamInstDecl top_lvl
  (DataFamInstDecl{dfid_tycon = tycon, dfid_pats = pats,
                   dfid_defn = defn})
  = pp_data_defn pp_hdr defn
  where pp_hdr ctxt
          = ppr_instance_keyword top_lvl <+> pp_fam_inst_lhs tycon pats ctxt

pprDataFamInstFlavour :: DataFamInstDecl name -> SDoc
pprDataFamInstFlavour
  (DataFamInstDecl{dfid_defn = (HsDataDefn{dd_ND = nd})}) = ppr nd

pp_fam_inst_lhs ::
                  OutputableBndr name =>
                  Located name -> HsTyPats name -> HsContext name -> SDoc
pp_fam_inst_lhs thing (HsIB{hsib_body = typats}) context
  = hsep
      [pprHsContext context, pprPrefixOcc (unLoc thing),
       hsep (map (pprParendHsType . unLoc) typats)]

instance (OutputableBndr name) => Outputable (ClsInstDecl name)
         where
        ppr
          (ClsInstDecl{cid_poly_ty = inst_ty, cid_binds = binds,
                       cid_sigs = sigs, cid_tyfam_insts = ats,
                       cid_overlap_mode = mbOverlap, cid_datafam_insts = adts})
          | null sigs, null ats, null adts, isEmptyBag binds = top_matter
          | otherwise =
            vcat
              [top_matter <+> text "where",
               nest 2 $
                 pprDeclList $
                   map (pprTyFamInstDecl NotTopLevel . unLoc) ats ++
                     map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
                       pprLHsBindsForUser binds sigs]
          where top_matter
                  = text "instance" <+> ppOverlapPragma mbOverlap <+> ppr inst_ty

ppOverlapPragma :: Maybe (Located OverlapMode) -> SDoc
ppOverlapPragma mb
  = case mb of
        Nothing -> empty
        Just (L _ (NoOverlap _)) -> text "{-# NO_OVERLAP #-}"
        Just (L _ (Overlappable _)) -> text "{-# OVERLAPPABLE #-}"
        Just (L _ (Overlapping _)) -> text "{-# OVERLAPPING #-}"
        Just (L _ (Overlaps _)) -> text "{-# OVERLAPS #-}"
        Just (L _ (Incoherent _)) -> text "{-# INCOHERENT #-}"

instance (OutputableBndr name) => Outputable (InstDecl name) where
        ppr (ClsInstD{cid_inst = decl}) = ppr decl
        ppr (TyFamInstD{tfid_inst = decl}) = ppr decl
        ppr (DataFamInstD{dfid_inst = decl}) = ppr decl

instDeclDataFamInsts :: [LInstDecl name] -> [DataFamInstDecl name]
instDeclDataFamInsts inst_decls = concatMap do_one inst_decls
  where do_one
          (L _
             (ClsInstD{cid_inst = ClsInstDecl{cid_datafam_insts = fam_insts}}))
          = map unLoc fam_insts
        do_one (L _ (DataFamInstD{dfid_inst = fam_inst})) = [fam_inst]
        do_one (L _ (TyFamInstD{})) = []

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl{deriv_type :: LHsSigType name,
                                deriv_overlap_mode :: Maybe (Located OverlapMode)}
                    deriving Typeable

deriving instance (DataId name) => Data (DerivDecl name)

instance (OutputableBndr name) => Outputable (DerivDecl name) where
        ppr (DerivDecl ty o)
          = hsep [text "deriving instance", ppOverlapPragma o, ppr ty]

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name = DefaultDecl [LHsType name]
                      deriving Typeable

deriving instance (DataId name) => Data (DefaultDecl name)

instance (OutputableBndr name) => Outputable (DefaultDecl name)
         where
        ppr (DefaultDecl tys) = text "default" <+> parens (interpp'SP tys)

type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name = ForeignImport{fd_name :: Located name,
                                      fd_sig_ty :: LHsSigType name, fd_co :: PostTc name Coercion,
                                      fd_fi :: ForeignImport}
                      | ForeignExport{fd_name :: Located name,
                                      fd_sig_ty :: LHsSigType name, fd_co :: PostTc name Coercion,
                                      fd_fe :: ForeignExport}
                      deriving Typeable

deriving instance (DataId name) => Data (ForeignDecl name)

noForeignImportCoercionYet :: PlaceHolder
noForeignImportCoercionYet = PlaceHolder

noForeignExportCoercionYet :: PlaceHolder
noForeignExportCoercionYet = PlaceHolder

data ForeignImport = CImport (Located CCallConv) (Located Safety)
                             (Maybe Header) CImportSpec (Located SourceText)
                   deriving (Data, Typeable)

data CImportSpec = CLabel CLabelString
                 | CFunction CCallTarget
                 | CWrapper
                 deriving (Data, Typeable)

data ForeignExport = CExport (Located CExportSpec)
                             (Located SourceText)
                   deriving (Data, Typeable)

instance OutputableBndr name => Outputable (ForeignDecl name) where
        ppr (ForeignImport{fd_name = n, fd_sig_ty = ty, fd_fi = fimport})
          = hang (text "foreign import" <+> ppr fimport <+> ppr n) 2
              (dcolon <+> ppr ty)
        ppr (ForeignExport{fd_name = n, fd_sig_ty = ty, fd_fe = fexport})
          = hang (text "foreign export" <+> ppr fexport <+> ppr n) 2
              (dcolon <+> ppr ty)

instance Outputable ForeignImport where
        ppr (CImport cconv safety mHeader spec _)
          = ppr cconv <+> ppr safety <+> char '"' <> pprCEntity spec <>
              char '"'
          where pp_hdr
                  = case mHeader of
                        Nothing -> empty
                        Just (Header _ header) -> ftext header
                pprCEntity (CLabel lbl)
                  = text "static" <+> pp_hdr <+> char '&' <> ppr lbl
                pprCEntity (CFunction (StaticTarget _ lbl _ isFun))
                  = text "static" <+> pp_hdr <+>
                      (if isFun then empty else text "value")
                      <+> ppr lbl
                pprCEntity (CFunction (DynamicTarget)) = text "dynamic"
                pprCEntity (CWrapper) = text "wrapper"

instance Outputable ForeignExport where
        ppr (CExport (L _ (CExportStatic _ lbl cconv)) _)
          = ppr cconv <+> char '"' <> ppr lbl <> char '"'

type LRuleDecls name = Located (RuleDecls name)

data RuleDecls name = HsRules{rds_src :: SourceText,
                              rds_rules :: [LRuleDecl name]}
                    deriving Typeable

deriving instance (DataId name) => Data (RuleDecls name)

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name = HsRule (Located (SourceText, RuleName))
                            Activation [LRuleBndr name] (Located (HsExpr name))
                            (PostRn name NameSet) (Located (HsExpr name)) (PostRn name NameSet)
                   deriving Typeable

deriving instance (DataId name) => Data (RuleDecl name)

flattenRuleDecls :: [LRuleDecls name] -> [LRuleDecl name]
flattenRuleDecls decls = concatMap (rds_rules . unLoc) decls

type LRuleBndr name = Located (RuleBndr name)

data RuleBndr name = RuleBndr (Located name)
                   | RuleBndrSig (Located name) (LHsSigWcType name)
                   deriving Typeable

deriving instance (DataId name) => Data (RuleBndr name)

collectRuleBndrSigTys :: [RuleBndr name] -> [LHsSigWcType name]
collectRuleBndrSigTys bndrs = [ty | RuleBndrSig _ ty <- bndrs]

pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (_, n)) = doubleQuotes $ ftext n

instance OutputableBndr name => Outputable (RuleDecls name) where
        ppr (HsRules _ rules) = ppr rules

instance OutputableBndr name => Outputable (RuleDecl name) where
        ppr (HsRule name act ns lhs _fv_lhs rhs _fv_rhs)
          = sep
              [text "{-# RULES" <+> pprFullRuleName name <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 4 (equals <+> pprExpr (unLoc rhs) <+> text "#-}")]
          where pp_forall
                  | null ns = empty
                  | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance OutputableBndr name => Outputable (RuleBndr name) where
        ppr (RuleBndr name) = ppr name
        ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty

type LVectDecl name = Located (VectDecl name)

data VectDecl name = HsVect SourceText (Located name)
                            (LHsExpr name)
                   | HsNoVect SourceText (Located name)
                   | HsVectTypeIn SourceText Bool (Located name)
                                  (Maybe (Located name))
                   | HsVectTypeOut Bool TyCon (Maybe TyCon)
                   | HsVectClassIn SourceText (Located name)
                   | HsVectClassOut Class
                   | HsVectInstIn (LHsSigType name)
                   | HsVectInstOut ClsInst
                   deriving Typeable

deriving instance (DataId name) => Data (VectDecl name)

lvectDeclName :: NamedThing name => LVectDecl name -> Name
lvectDeclName (L _ (HsVect _ (L _ name) _)) = getName name
lvectDeclName (L _ (HsNoVect _ (L _ name))) = getName name
lvectDeclName (L _ (HsVectTypeIn _ _ (L _ name) _)) = getName name
lvectDeclName (L _ (HsVectTypeOut _ tycon _)) = getName tycon
lvectDeclName (L _ (HsVectClassIn _ (L _ name))) = getName name
lvectDeclName (L _ (HsVectClassOut cls)) = getName cls
lvectDeclName (L _ (HsVectInstIn _))
  = panic "HsDecls.lvectDeclName: HsVectInstIn"
lvectDeclName (L _ (HsVectInstOut _))
  = panic "HsDecls.lvectDeclName: HsVectInstOut"

lvectInstDecl :: LVectDecl name -> Bool
lvectInstDecl (L _ (HsVectInstIn _)) = True
lvectInstDecl (L _ (HsVectInstOut _)) = True
lvectInstDecl _ = False

instance OutputableBndr name => Outputable (VectDecl name) where
        ppr (HsVect _ v rhs)
          = sep
              [text "{-# VECTORISE" <+> ppr v,
               nest 4 $ pprExpr (unLoc rhs) <+> text "#-}"]
        ppr (HsNoVect _ v)
          = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}"]
        ppr (HsVectTypeIn _ False t Nothing)
          = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}"]
        ppr (HsVectTypeIn _ False t (Just t'))
          = sep
              [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}"]
        ppr (HsVectTypeIn _ True t Nothing)
          = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}"]
        ppr (HsVectTypeIn _ True t (Just t'))
          = sep
              [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t',
               text "#-}"]
        ppr (HsVectTypeOut False t Nothing)
          = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}"]
        ppr (HsVectTypeOut False t (Just t'))
          = sep
              [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}"]
        ppr (HsVectTypeOut True t Nothing)
          = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}"]
        ppr (HsVectTypeOut True t (Just t'))
          = sep
              [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t',
               text "#-}"]
        ppr (HsVectClassIn _ c)
          = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}"]
        ppr (HsVectClassOut c)
          = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}"]
        ppr (HsVectInstIn ty)
          = sep
              [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}"]
        ppr (HsVectInstOut i)
          = sep
              [text "{-# VECTORISE SCALAR instance" <+> ppr i <+> text "#-}"]

type LDocDecl = Located (DocDecl)

data DocDecl = DocCommentNext HsDocString
             | DocCommentPrev HsDocString
             | DocCommentNamed String HsDocString
             | DocGroup Int HsDocString
             deriving (Data, Typeable)

instance Outputable DocDecl where
        ppr _ = text "<document comment>"

docDeclDoc :: DocDecl -> HsDocString
docDeclDoc (DocCommentNext d) = d
docDeclDoc (DocCommentPrev d) = d
docDeclDoc (DocCommentNamed _ d) = d
docDeclDoc (DocGroup _ d) = d

type LWarnDecls name = Located (WarnDecls name)

data WarnDecls name = Warnings{wd_src :: SourceText,
                               wd_warnings :: [LWarnDecl name]}
                    deriving (Data, Typeable)

type LWarnDecl name = Located (WarnDecl name)

data WarnDecl name = Warning [Located name] WarningTxt
                   deriving (Data, Typeable)

instance OutputableBndr name => Outputable (WarnDecls name) where
        ppr (Warnings _ decls) = ppr decls

instance OutputableBndr name => Outputable (WarnDecl name) where
        ppr (Warning thing txt)
          = hsep
              [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt),
               text "#-}"]

type LAnnDecl name = Located (AnnDecl name)

data AnnDecl name = HsAnnotation SourceText (AnnProvenance name)
                                 (Located (HsExpr name))
                  deriving Typeable

deriving instance (DataId name) => Data (AnnDecl name)

instance (OutputableBndr name) => Outputable (AnnDecl name) where
        ppr (HsAnnotation _ provenance expr)
          = hsep
              [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr),
               text "#-}"]

data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance
                        deriving (Data, Typeable, Functor)

deriving instance Foldable AnnProvenance

deriving instance Traversable AnnProvenance

annProvenanceName_maybe :: AnnProvenance name -> Maybe name
annProvenanceName_maybe (ValueAnnProvenance (L _ name)) = Just name
annProvenanceName_maybe (TypeAnnProvenance (L _ name)) = Just name
annProvenanceName_maybe ModuleAnnProvenance = Nothing

pprAnnProvenance ::
                   OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance = text "ANN module"
pprAnnProvenance (ValueAnnProvenance (L _ name))
  = text "ANN" <+> ppr name
pprAnnProvenance (TypeAnnProvenance (L _ name))
  = text "ANN type" <+> ppr name

type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

data RoleAnnotDecl name = RoleAnnotDecl (Located name)
                                        [Located (Maybe Role)]
                        deriving (Data, Typeable)

instance OutputableBndr name => Outputable (RoleAnnotDecl name)
         where
        ppr (RoleAnnotDecl ltycon roles)
          = text "type role" <+> ppr ltycon <+>
              hsep (map (pp_role . unLoc) roles)
          where pp_role Nothing = underscore
                pp_role (Just r) = ppr r

roleAnnotDeclName :: RoleAnnotDecl name -> name
roleAnnotDeclName (RoleAnnotDecl (L _ name) _) = name