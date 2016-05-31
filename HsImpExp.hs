type LImportDecl name = Located (ImportDecl name)

data ImportDecl name = ImportDecl{ideclSourceSrc ::
                                  Maybe SourceText,
                                  ideclName :: Located ModuleName,
                                  ideclPkgQual :: Maybe StringLiteral, ideclSource :: Bool,
                                  ideclSafe :: Bool, ideclQualified :: Bool, ideclImplicit :: Bool,
                                  ideclAs :: Maybe ModuleName,

type LIE name = Located (IE name)

data IE name = IEVar (Located name)
             | IEThingAbs (Located name)
             | IEThingAll (Located name)
             | IEThingWith (Located name) IEWildcard [Located name]
                           [Located (FieldLbl name)]
             | IEModuleContents (Located ModuleName)
             | IEGroup Int HsDocString
             | IEDoc HsDocString
             | IEDocNamed String

data IEWildcard = NoIEWildcard
                | IEWildcard Int
