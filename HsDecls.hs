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

data HsGroup id = HsGroup{hs_valds :: HsValBinds id,
                          hs_splcds :: [LSpliceDecl id], hs_tyclds :: [TyClGroup id],
                          hs_derivds :: [LDerivDecl id], hs_fixds :: [LFixitySig id],
                          hs_defds :: [LDefaultDecl id], hs_fords :: [LForeignDecl id],
                          hs_warnds :: [LWarnDecls id], hs_annds :: [LAnnDecl id],
                          hs_ruleds :: [LRuleDecls id], hs_vects :: [LVectDecl id],
                          hs_docs :: [LDocDecl]}

data SpliceExplicitFlag = ExplicitSplice
                        | ImplicitSplice

type LSpliceDecl name = Located (SpliceDecl name)

data SpliceDecl id = SpliceDecl (Located (HsSplice id))
                                SpliceExplicitFlag

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

data TyClGroup name = TyClGroup{group_tyclds :: [LTyClDecl name],
                                group_roles :: [LRoleAnnotDecl name],
                                group_instds :: [LInstDecl name]}

type LFamilyResultSig name = Located (FamilyResultSig name)

data FamilyResultSig name = NoSig
                          | KindSig (LHsKind name)
                          | TyVarSig (LHsTyVarBndr name)

type LFamilyDecl name = Located (FamilyDecl name)

data FamilyDecl name = FamilyDecl{fdInfo :: FamilyInfo name,
                                  fdLName :: Located name, fdTyVars :: LHsQTyVars name,
                                  fdResultSig :: LFamilyResultSig name,
                                  fdInjectivityAnn :: Maybe (LInjectivityAnn name)}

type LInjectivityAnn name = Located (InjectivityAnn name)

data InjectivityAnn name = InjectivityAnn (Located name)
                                          [Located name]

data FamilyInfo name = DataFamily
                     | OpenTypeFamily
                     | ClosedTypeFamily (Maybe [LTyFamInstEqn name])

data HsDataDefn name = HsDataDefn{dd_ND :: NewOrData,
                                  dd_ctxt :: LHsContext name, dd_cType :: Maybe (Located CType),
                                  dd_kindSig :: Maybe (LHsKind name), dd_cons :: [LConDecl name],
                                  dd_derivs :: HsDeriving name}

type HsDeriving name = Maybe (Located [LHsSigType name])

data NewOrData = NewType
               | DataType

type LConDecl name = Located (ConDecl name)

data ConDecl name = ConDeclGADT{con_names :: [Located name],
                                con_type :: LHsSigType name, con_doc :: Maybe LHsDocString}
                  | ConDeclH98{con_name :: Located name,
                               con_qvars :: Maybe (LHsQTyVars name),
                               con_cxt :: Maybe (LHsContext name),
                               con_details :: HsConDeclDetails name,
                               con_doc :: Maybe LHsDocString}

type HsConDeclDetails name =
     HsConDetails (LBangType name) (Located [LConDeclField name])

type LTyFamInstEqn name = Located (TyFamInstEqn name)

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type HsTyPats name = HsImplicitBndrs name [LHsType name]

type TyFamInstEqn name = TyFamEqn name (HsTyPats name)

type TyFamDefltEqn name = TyFamEqn name (LHsQTyVars name)

data TyFamEqn name pats = TyFamEqn{tfe_tycon :: Located name,
                                   tfe_pats :: pats, tfe_rhs :: LHsType name}

type LTyFamInstDecl name = Located (TyFamInstDecl name)

data TyFamInstDecl name = TyFamInstDecl{tfid_eqn ::
                                        LTyFamInstEqn name,
                                        tfid_fvs :: PostRn name NameSet}

type LDataFamInstDecl name = Located (DataFamInstDecl name)

data DataFamInstDecl name = DataFamInstDecl{dfid_tycon ::
                                            Located name,
                                            dfid_pats :: HsTyPats name,
                                            dfid_defn :: HsDataDefn name,
                                            dfid_fvs :: PostRn name NameSet}

type LClsInstDecl name = Located (ClsInstDecl name)

data ClsInstDecl name = ClsInstDecl{cid_poly_ty :: LHsSigType name,
                                    cid_binds :: LHsBinds name, cid_sigs :: [LSig name],
                                    cid_tyfam_insts :: [LTyFamInstDecl name],
                                    cid_datafam_insts :: [LDataFamInstDecl name],
                                    cid_overlap_mode :: Maybe (Located OverlapMode)}

type LInstDecl name = Located (InstDecl name)

data InstDecl name = ClsInstD{cid_inst :: ClsInstDecl name}
                   | DataFamInstD{dfid_inst :: DataFamInstDecl name}
                   | TyFamInstD{tfid_inst :: TyFamInstDecl name}

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl{deriv_type :: LHsSigType name,
                                deriv_overlap_mode :: Maybe (Located OverlapMode)}

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name = DefaultDecl [LHsType name]

type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name = ForeignImport{fd_name :: Located name,
                                      fd_sig_ty :: LHsSigType name, fd_co :: PostTc name Coercion,
                                      fd_fi :: ForeignImport}
                      | ForeignExport{fd_name :: Located name,
                                      fd_sig_ty :: LHsSigType name, fd_co :: PostTc name Coercion,
                                      fd_fe :: ForeignExport}

data ForeignImport = CImport (Located CCallConv) (Located Safety)
                             (Maybe Header) CImportSpec (Located SourceText)

data CImportSpec = CLabel CLabelString
                 | CFunction CCallTarget
                 | CWrapper

data ForeignExport = CExport (Located CExportSpec)
                             (Located SourceText)

type LRuleDecls name = Located (RuleDecls name)

data RuleDecls name = HsRules{rds_src :: SourceText,
                              rds_rules :: [LRuleDecl name]}

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name = HsRule (Located (SourceText, RuleName))
                            Activation [LRuleBndr name] (Located (HsExpr name))
                            (PostRn name NameSet) (Located (HsExpr name)) (PostRn name NameSet)

type LRuleBndr name = Located (RuleBndr name)

data RuleBndr name = RuleBndr (Located name)
                   | RuleBndrSig (Located name) (LHsSigWcType name)

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

type LDocDecl = Located (DocDecl)

data DocDecl = DocCommentNext HsDocString
             | DocCommentPrev HsDocString
             | DocCommentNamed String HsDocString
             | DocGroup Int HsDocString

type LWarnDecls name = Located (WarnDecls name)

data WarnDecls name = Warnings{wd_src :: SourceText,
                               wd_warnings :: [LWarnDecl name]}

type LWarnDecl name = Located (WarnDecl name)

data WarnDecl name = Warning [Located name] WarningTxt

type LAnnDecl name = Located (AnnDecl name)

data AnnDecl name = HsAnnotation SourceText (AnnProvenance name)
                                 (Located (HsExpr name))

data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance

type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

data RoleAnnotDecl name = RoleAnnotDecl (Located name)
                                        [Located (Maybe Role)]
