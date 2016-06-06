
module Syntax where

data Exp l
  = Var          l (QName l)
--  UnboundVar
  | Con          l (QName l)

  | Lit          l (Literal l)
--  OverLit

  | App          l (Exp l) (Exp l)

  | LeftSection  l (Exp l) (QOp l)

  | RightSection l (QOp l) (Exp l)

  | NegApp       l (Exp l)

  | InfixApp     l (Exp l) (QOp l) (Exp l)

-- AppType
-- AppTypeOut

  | Paren        l (Exp l)

  | If           l (Exp l) (Exp l) (Exp l)

  | MultiIf      l [GuardedRhs l]

  | Case         l (Exp l) [Alt l]

  | Lambda       l [Pat l] (Exp l)

  | LCase        l [Alt l]

  | Let          l (Binds l) (Exp l)

  | IPVar        l (IPName l)

-- RecFld

-- OverLabel

  | RecConstr    l (QName l) [FieldUpdate l]

  | RecUpdate    l (Exp l)   [FieldUpdate l]
-- \n

  | Tuple              l Boxed [Exp l]
  | TupleSection       l Boxed [Maybe (Exp l)]

  | List               l [Exp l]

  | ParArray           l [Exp l]

  | EnumFrom           l (Exp l)
  | EnumFromTo         l (Exp l) (Exp l)
  | EnumFromThen       l (Exp l) (Exp l)
  | EnumFromThenTo     l (Exp l) (Exp l) (Exp l)

--  ParArrayFrom       (missing?)
  | ParArrayFromTo     l (Exp l) (Exp l)
--  ParArrayFromThen   (missing?)
  | ParArrayFromThenTo l (Exp l) (Exp l) (Exp l)

  | ListComp           l (Exp l) [QualStmt l]
--  MonadComp          (missing? same as ListComp?)
  | ParArrayComp       l (Exp l) [[QualStmt l]]
  | Do                 l [Stmt l]
  | MDo                l [Stmt l]
--  ArrowExp            (?)
--  GhciStmtCtxt
--  PatGuard
  | ParComp            l (Exp l) [[QualStmt l]]
--  TransStmtCtxt

  | BracketExp l (Bracket l)
--  RnBracketOut
--  TcBracketOut
  | QuasiQuote l String String

  | VarQuote l (QName l)
  | TypQuote l (QName l)

  | SpliceExp l (Splice l)

  | ExpTypeSig         l (Exp l) (Type l)
--  ExpWithTySigOut

  | CorePragma l      String (Exp l)
  | SCCPragma  l      String (Exp l)
  | GenPragma  l      String (Int, Int) (Int, Int) (Exp l)
--                    \n
--                    \n

  | Proc            l (Pat l) (Exp l)

  | LeftArrApp      l (Exp l) (Exp l)
  | RightArrApp     l (Exp l) (Exp l)
  | LeftArrHighApp  l (Exp l) (Exp l)
  | RightArrHighApp l (Exp l) (Exp l)

  | ExprHole l

-- Static   (missing?)

-- Tick     (missing?)

-- BinTick  (missing?)

-- EAsPat   (missing?)

-- EViewPat (missing?)

-- ELazyPat (missing?)

-- Wrap     (missing?)

data Pat l
  = PWildCard   l

  | PVar        l (Name l)

  | PLit        l (Sign l) (Literal l)
--  NPat
--  \n

  | PNPlusK     l (Name l) Integer
-- \n

  | PTuple      l Boxed [Pat l]

  | PList       l [Pat l]
-- \n

  | PParen      l (Pat l)

  | PAsPat      l (Name l) (Pat l)

  | PViewPat    l (Exp l) (Pat l)

  | PBangPat    l (Pat l)

  | PIrrPat     l (Pat l)

  | PatTypeSig  l (Pat l) (Type l)
--  SigPatOut

--  PArrPat

  | PApp        l (QName l) [Pat l]
--  ConPatOut
--   \n
  | PRec        l (QName l) [PatField l]
  | PInfixApp   l (Pat l) (QName l) (Pat l)

  | PQuasiQuote l String String

-- CoPat

data Literal l
  = Char       l Char     String

  | PrimChar   l Char     String

  | String     l String   String

  | PrimString l String   String

  | Int        l Integer  String

  | PrimInt    l Integer  String

  | PrimWord   l Integer  String

--  Int64Prim  (missing?)

--  Word64Prim (missing?)

--  Integer    (?)

  | Frac       l Rational String

  | PrimFloat  l Rational String

  | PrimDouble l Rational String

data Decl l
  = RoleAnnotDecl     l (QName l) [Role l]

  | AnnPragma         l (Annotation l)

  | DerivDecl         l (Maybe (Overlap l)) (InstRule l)

  | WarnPragmaDecl    l [([Name l], String)]

  | RulePragmaDecl    l [Rule l]

  | DefaultDecl       l [Type l]

  | ForImp            l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l)
                        (Type l)
  | ForExp            l (CallConv l) (Maybe String) (Name l) (Type l)

  | SpliceDecl        l (Exp l)

--  DocD  (missing?)

  | TypeSig           l [Name l] (Type l)
  | PatSynSig         l (Name l) (Maybe [TyVarBind l]) (Maybe (Context l))
                        (Maybe (Context l)) (Type l)
--  ClassOpSig
--  IdSig
  | InfixDecl         l (Assoc l) (Maybe Int) [Op l]
  | InlineSig         l Bool (Maybe (Activation l)) (QName l)
  | SpecSig           l (Maybe (Activation l)) (QName l) [Type l]
  | InstSig           l (InstRule l)
  | MinimalPragma     l (Maybe (BooleanFormula l))
  | InlineConlikeSig  l (Maybe (Activation l)) (QName l)
  | SpecInlineSig     l Bool (Maybe (Activation l)) (QName l) [Type l]

--  HsSyn groups below
  | DataFamDecl       l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
  | TypeFamDecl       l (DeclHead l) (Maybe (Kind l))
  | ClosedTypeFamDecl l (DeclHead l) (Maybe (Kind l))  [TypeEqn l]
  | TypeDecl          l (DeclHead l) (Type l)
  | DataDecl          l (DataOrNew l) (Maybe (Context l)) (DeclHead l)
                        [QualConDecl l] (Maybe (Deriving l))
  | GDataDecl         l (DataOrNew l) (Maybe (Context l)) (DeclHead l)
                        (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
  | ClassDecl         l (Maybe (Context l)) (DeclHead l) [FunDep l]
                        (Maybe [ClassDecl l])

--  HsSyn groups below
  | InstDecl          l (Maybe (Overlap l)) (InstRule l) (Maybe [InstDecl l])
  | DataInsDecl       l (DataOrNew l) (Type l) [QualConDecl l]
                        (Maybe (Deriving l))
  | GDataInsDecl      l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]
                        (Maybe (Deriving l))
  | TypeInsDecl       l (Type l) (Type l)

-- HsSyn groups below
  | FunBind           l [Match l]
  | PatBind           l (Pat l) (Rhs l) (Maybe (Binds l)) --expanding Pat below
--  Pat.PVar
--  AbsBinds     (what is this?)
--  AbsBindsSig  (what is this?)
  | PatSyn            l (Pat l) (Pat l) (PatternSynDirection l)

  | DeprPragmaDecl    l [([Name l], String)]

--  VectD (missing)


------------ the rest is not compared -------------
data ModuleName l
  = ModuleName l String

data SpecialCon l
  = UnitCon l
  | ListCon l
  | FunCon  l
  | TupleCon l Boxed Int
  | Cons l
  | UnboxedSingleCon l

data QName l
  = Qual    l (ModuleName l) (Name l)
  | UnQual  l                (Name l)
  | Special l (SpecialCon l)

data Name l
  = Ident  l String
  | Symbol l String

data IPName l
  = IPDup l String
  | IPLin l String

data QOp l
  = QVarOp l (QName l)
  | QConOp l (QName l)

data Op l
  = VarOp l (Name l)
  | ConOp l (Name l)

data CName l
  = VarName l (Name l)
  | ConName l (Name l)

data Module l
  = Module  l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]

data ModuleHead l
  = ModuleHead l (ModuleName l) (Maybe (WarningText l))
                 (Maybe (ExportSpecList l))

data ExportSpecList l
  = ExportSpecList l [ExportSpec l]

data ExportSpec l
  = EVar            l (QName l)
  | EAbs            l (Namespace l) (QName l)
  | EThingAll       l (QName l)
  | EThingWith      l (QName l) [CName l]
  | EModuleContents l (ModuleName l)

data Namespace l
  = NoNamespace l
  | TypeNamespace l
  | PatternNamespace l

data ImportDecl l
  = ImportDecl l (ModuleName l) Bool Bool Bool (Maybe String)
                 (Maybe (ModuleName l)) (Maybe (ImportSpecList l))

data ImportSpecList l
  = ImportSpecList l Bool [ImportSpec l]

data ImportSpec l
  = IVar l (Name l)
  | IAbs l (Namespace l) (Name l)
  | IThingAll l (Name l)
  | IThingWith l (Name l) [CName l]

data Assoc l
  = AssocNone  l
  | AssocLeft  l
  | AssocRight l

data PatternSynDirection l
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional l [Decl l]

data TypeEqn l
  = TypeEqn l (Type l) (Type l)

data Annotation l
  = Ann       l (Name l) (Exp l)
  | TypeAnn   l (Name l) (Exp l)
  | ModuleAnn l (Exp l)

data BooleanFormula l
  = VarFormula   l (Name l)
  | AndFormula   l [BooleanFormula l]
  | OrFormula    l [BooleanFormula l]
  | ParenFormula l (BooleanFormula l)

data Role l
  = Nominal          l
  | Representational l
  | Phantom          l
  | RoleWildcard     l

data DataOrNew l
  = DataType l
  | NewType  l

data DeclHead l
  = DHead l (Name l)
  | DHInfix l (TyVarBind l) (Name l)
  | DHParen l (DeclHead l)
  | DHApp   l (DeclHead l) (TyVarBind l)

data InstRule l
  = IRule l (Maybe [TyVarBind l]) (Maybe (Context l)) (InstHead l)
  | IParen l (InstRule l)

data InstHead l
  = IHCon l (QName l)
  | IHInfix l (Type l) (QName l)
  | IHParen l (InstHead l)
  | IHApp   l (InstHead l) (Type l)

data Deriving l
  = Deriving l [InstRule l]

data Binds l
  = BDecls  l [Decl l]
  | IPBinds l [IPBind l]

data IPBind l
  = IPBind l (IPName l) (Exp l)

data Match l
  = Match      l (Name l) [Pat l] (Rhs l) (Maybe (Binds l))
  | InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) (Maybe (Binds l))

data QualConDecl l
  = QualConDecl l (Maybe [TyVarBind l]) (Maybe (Context l)) (ConDecl l)

data ConDecl l
  = ConDecl      l (Name l) [Type l]
  | InfixConDecl l (Type l) (Name l) (Type l)
  | RecDecl      l (Name l) [FieldDecl l]

data FieldDecl l
  = FieldDecl l [Name l] (Type l)

data GadtDecl l
  = GadtDecl l (Name l) (Maybe [FieldDecl l]) (Type l)

data ClassDecl l
  = ClsDecl    l (Decl l)
  | ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
  | ClsTyFam   l                     (DeclHead l) (Maybe (Kind l))
  | ClsTyDef   l (Type l) (Type l)
  | ClsDefSig  l (Name l) (Type l)

data InstDecl l
  = InsDecl   l (Decl l)
  | InsType   l (Type l) (Type l)
  | InsData   l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l))
  | InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]
                (Maybe (Deriving l))

data BangType l
  = BangedTy   l
  | UnpackedTy l
  | NoUnpackedTy l

data Rhs l
  = UnGuardedRhs l (Exp l)
  | GuardedRhss  l [GuardedRhs l]

data GuardedRhs l
  = GuardedRhs l [Stmt l] (Exp l)

data Type l
  = TyForall     l (Maybe [TyVarBind l]) (Maybe (Context l)) (Type l)
  | TyFun        l (Type l) (Type l)
  | TyTuple      l Boxed [Type l]
  | TyList       l (Type l)
  | TyParArray   l (Type l)
  | TyApp        l (Type l) (Type l)
  | TyVar        l (Name l)
  | TyCon        l (QName l)
  | TyParen      l (Type l)
  | TyInfix      l (Type l) (QName l) (Type l)
  | TyKind       l (Type l) (Kind l)
  | TyPromoted   l (Promoted l)
  | TyEquals     l (Type l) (Type l)
  | TySplice     l (Splice l)
  | TyBang       l (BangType l) (Type l)
  | TyWildCard   l (Maybe (Name l))
  | TyQuasiQuote l String String

data Promoted l
  = PromotedInteger l Integer String
  | PromotedString  l String String
  | PromotedCon     l Bool (QName l)
  | PromotedList    l Bool [Type l]
  | PromotedTuple   l [Type l]
  | PromotedUnit    l

data Boxed
  = Boxed
  | Unboxed

data TyVarBind l
  = KindedVar   l (Name l) (Kind l)
  | UnkindedVar l (Name l)

data Kind l
  = KindStar  l
  | KindFn    l (Kind l) (Kind l)
  | KindParen l (Kind l)
  | KindVar   l (QName l)
  | KindApp   l (Kind l) (Kind l)
  | KindTuple l [Kind l]
  | KindList  l (Kind l)

data FunDep l
  = FunDep l [Name l] [Name l]

data Context l
  = CxSingle l (Asst l)
  | CxTuple  l [Asst l]
  | CxEmpty  l

data Asst l
  = ClassA    l (QName l) [Type l]
  | AppA      l (Name l) [Type l]
  | InfixA    l (Type l) (QName l) (Type l)
  | IParam    l (IPName l) (Type l)
  | EqualP    l (Type l) (Type l)
  | ParenA    l (Asst l)
  | WildCardA l (Maybe (Name l))

data Sign l
  = Signless l
  | Negative l

data Bracket l
  = ExpBracket  l (Exp l)
  | PatBracket  l (Pat l)
  | TypeBracket l (Type l)
  | DeclBracket l [Decl l]

data Splice l
  = IdSplice    l String
  | ParenSplice l (Exp l)

data Safety l
  = PlayRisky         l
  | PlaySafe          l Bool
  | PlayInterruptible l

data CallConv l
  = StdCall    l
  | CCall      l
  | CPlusPlus  l
  | DotNet     l
  | Jvm        l
  | Js         l
  | JavaScript l
  | CApi       l

data ModulePragma l
  = LanguagePragma  l [Name l]
  | OptionsPragma   l (Maybe Tool) String
  | AnnModulePragma l (Annotation l)

data Tool
  = GHC
  | HUGS
  | NHC98
  | YHC
  | HADDOCK
  | UnknownTool String

data Overlap l
  = NoOverlap  l
  | Overlap    l
  | Incoherent l

data Activation l
  = ActiveFrom  l Int
  | ActiveUntil l Int

data Rule l
  = Rule l String (Maybe (Activation l)) (Maybe [RuleVar l]) (Exp l) (Exp l)

data RuleVar l
  = RuleVar      l (Name l)
  | TypedRuleVar l (Name l) (Type l)

data WarningText l
  = DeprText l String
  | WarnText l String

data PatField l
  = PFieldPat l (QName l) (Pat l)
  | PFieldPun l (QName l)
  | PFieldWildcard l

data Stmt l
  = Generator l (Pat l) (Exp l)
  | Qualifier l (Exp l)
  | LetStmt l (Binds l)
  | RecStmt l [Stmt l]

data QualStmt l
  = QualStmt     l (Stmt l)
  | ThenTrans    l (Exp l)
  | ThenBy       l (Exp l) (Exp l)
  | GroupBy      l (Exp l)
  | GroupUsing   l (Exp l)
  | GroupByUsing l (Exp l) (Exp l)

data FieldUpdate l
  = FieldUpdate l (QName l) (Exp l)
  | FieldPun l (QName l)
  | FieldWildcard l

data Alt l
  = Alt l (Pat l) (Rhs l) (Maybe (Binds l))
