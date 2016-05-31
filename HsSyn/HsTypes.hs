type LBangType name = Located (BangType name)

type BangType name = HsType name

type LHsContext name = Located (HsContext name)

type HsContext name = [LHsType name]

type LHsType name = Located (HsType name)

type HsKind name = HsType name

type LHsKind name = Located (HsKind name)

type LHsTyVarBndr name = Located (HsTyVarBndr name)

data LHsQTyVars name = HsQTvs{hsq_implicit :: PostRn name [Name],
                              hsq_explicit :: [LHsTyVarBndr name],
                              hsq_dependent :: PostRn name NameSet}

data HsImplicitBndrs name thing = HsIB{hsib_vars ::
                                       PostRn name [Name],
                                       hsib_body :: thing}

data HsWildCardBndrs name thing = HsWC{hswc_wcs ::
                                       PostRn name [Name],
                                       hswc_ctx :: Maybe SrcSpan, hswc_body :: thing}

type LHsSigType name = HsImplicitBndrs name (LHsType name)

type LHsWcType name = HsWildCardBndrs name (LHsType name)

type LHsSigWcType name = HsImplicitBndrs name (LHsWcType name)

newtype HsIPName = HsIPName FastString

data HsTyVarBndr name = UserTyVar (Located name)
                      | KindedTyVar (Located name) (LHsKind name)

data HsType name = HsForAllTy{hst_bndrs :: [LHsTyVarBndr name],
                              hst_body :: LHsType name}
                 | HsQualTy{hst_ctxt :: LHsContext name, hst_body :: LHsType name}
                 | HsTyVar (Located name)
                 | HsAppsTy [LHsAppType name]
                 | HsAppTy (LHsType name) (LHsType name)
                 | HsFunTy (LHsType name) (LHsType name)
                 | HsListTy (LHsType name)
                 | HsPArrTy (LHsType name)
                 | HsTupleTy HsTupleSort [LHsType name]
                 | HsOpTy (LHsType name) (Located name) (LHsType name)
                 | HsParTy (LHsType name)
                 | HsIParamTy HsIPName (LHsType name)
                 | HsEqTy (LHsType name) (LHsType name)
                 | HsKindSig (LHsType name) (LHsKind name)
                 | HsSpliceTy (HsSplice name) (PostTc name Kind)
                 | HsDocTy (LHsType name) LHsDocString
                 | HsBangTy HsSrcBang (LHsType name)
                 | HsRecTy [LConDeclField name]
                 | HsCoreTy Type
                 | HsExplicitListTy (PostTc name Kind) [LHsType name]
                 | HsExplicitTupleTy [PostTc name Kind] [LHsType name]
                 | HsTyLit HsTyLit
                 | HsWildCardTy (HsWildCardInfo name)

data HsTyLit = HsNumTy SourceText Integer
             | HsStrTy SourceText FastString

newtype HsWildCardInfo name = AnonWildCard (PostRn name
                                              (Located Name))


type LHsAppType name = Located (HsAppType name)

data HsAppType name = HsAppInfix (Located name)
                    | HsAppPrefix (LHsType name)

data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple

type LConDeclField name = Located (ConDeclField name)

data ConDeclField name = ConDeclField{cd_fld_names ::
                                      [LFieldOcc name],
                                      cd_fld_type :: LBangType name,
                                      cd_fld_doc :: Maybe LHsDocString}

data HsConDetails arg rec = PrefixCon [arg]
                          | RecCon rec
                          | InfixCon arg arg

type LFieldOcc name = Located (FieldOcc name)

data FieldOcc name = FieldOcc{rdrNameFieldOcc :: Located RdrName,
                              selectorFieldOcc :: PostRn name name}

data AmbiguousFieldOcc name = Unambiguous (Located RdrName)
                                          (PostRn name name)
                            | Ambiguous (Located RdrName) (PostTc name name)
