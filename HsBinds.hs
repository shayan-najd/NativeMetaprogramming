type HsLocalBinds id = HsLocalBindsLR id id

data HsLocalBindsLR idL idR = HsValBinds (HsValBindsLR idL idR)
                            | HsIPBinds (HsIPBinds idR)
                            | EmptyLocalBinds

type HsValBinds id = HsValBindsLR id id

data HsValBindsLR idL idR = ValBindsIn (LHsBindsLR idL idR)
                                       [LSig idR]
                          | ValBindsOut [(RecFlag, LHsBinds idL)] [LSig Name]

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

data ABExport id = ABE{abe_poly :: id, abe_mono :: id,
                       abe_wrap :: HsWrapper, abe_prags :: TcSpecPrags}

data PatSynBind idL idR = PSB{psb_id :: Located idL,
                              psb_fvs :: PostRn idR NameSet,
                              psb_args :: HsPatSynDetails (Located idR), psb_def :: LPat idR,
                              psb_dir :: HsPatSynDir idR}

data HsIPBinds id = IPBinds [LIPBind id] TcEvBinds

type LIPBind id = Located (IPBind id)

data IPBind id = IPBind (Either (Located HsIPName) id) (LHsExpr id)

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

type LFixitySig name = Located (FixitySig name)

data FixitySig name = FixitySig [Located name] Fixity

data TcSpecPrags = IsDefaultMethod
                 | SpecPrags [LTcSpecPrag]

type LTcSpecPrag = Located TcSpecPrag

data TcSpecPrag = SpecPrag Id HsWrapper InlinePragma

data HsPatSynDetails a = InfixPatSyn a a
                       | PrefixPatSyn [a]
                       | RecordPatSyn [RecordPatSynField a]

data RecordPatSynField a = RecordPatSynField{recordPatSynSelectorId
                                             :: a,
                                             recordPatSynPatVar :: a}

data HsPatSynDir id = Unidirectional
                    | ImplicitBidirectional
                    | ExplicitBidirectional (MatchGroup id (LHsExpr id))
