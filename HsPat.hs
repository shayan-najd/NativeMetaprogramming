type InPat id = LPat id

type OutPat id = LPat id

type LPat id = Located (Pat id)

data Pat id = WildPat (PostTc id Type)
            | VarPat (Located id)
            | LazyPat (LPat id)
            | AsPat (Located id) (LPat id)
            | ParPat (LPat id)
            | BangPat (LPat id)
            | ListPat [LPat id] (PostTc id Type)
                      (Maybe (PostTc id Type, SyntaxExpr id))
            | TuplePat [LPat id] Boxity [PostTc id Type]
            | PArrPat [LPat id] (PostTc id Type)
            | ConPatIn (Located id) (HsConPatDetails id)
            | ConPatOut{pat_con :: Located ConLike, pat_arg_tys :: [Type],
                        pat_tvs :: [TyVar], pat_dicts :: [EvVar], pat_binds :: TcEvBinds,
                        pat_args :: HsConPatDetails id, pat_wrap :: HsWrapper}
            | ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
            | SplicePat (HsSplice id)
            | LitPat HsLit
            | NPat (Located (HsOverLit id)) (Maybe (SyntaxExpr id))
                   (SyntaxExpr id) (PostTc id Type)
            | NPlusKPat (Located id) (Located (HsOverLit id)) (HsOverLit id)
                        (SyntaxExpr id) (SyntaxExpr id) (PostTc id Type)
            | SigPatIn (LPat id) (LHsSigWcType id)
            | SigPatOut (LPat id) Type
            | CoPat HsWrapper (Pat id) Type

type HsConPatDetails id =
     HsConDetails (LPat id) (HsRecFields id (LPat id))

data HsRecFields id arg = HsRecFields{rec_flds ::
                                      [LHsRecField id arg],
                                      rec_dotdot :: Maybe Int}

type LHsRecField' id arg = Located (HsRecField' id arg)

type LHsRecField id arg = Located (HsRecField id arg)

type LHsRecUpdField id = Located (HsRecUpdField id)

type HsRecField id arg = HsRecField' (FieldOcc id) arg

type HsRecUpdField id =
     HsRecField' (AmbiguousFieldOcc id) (LHsExpr id)

data HsRecField' id arg = HsRecField{hsRecFieldLbl :: Located id,
                                     hsRecFieldArg :: arg, hsRecPun :: Bool}
