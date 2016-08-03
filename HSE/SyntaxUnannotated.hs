{-# OPTIONS_GHC -Wall #-}
module SyntaxUnannotated where






data Exp
  = Var                QName

  | Con                QName

  | Lit                Literal

  | App                Exp Exp

  | LeftSection        Exp QOp

  | RightSection       QOp Exp

  | NegApp             Exp

  | InfixApp           Exp QOp Exp

-- HsSyn.AppType       (missing?)

  | Paren              Exp

  | If                 Exp Exp Exp

  | MultiIf            [GuardedRhs]

  | Case               Exp [Alt]

  | Lambda             [Pat] Exp

  | LCase              [Alt]

  | Let                Binds Exp

  | IPVar              IPName

-- HsSyn.RecFld        (missing?)

-- HsSyn.OverLabel     (missing?)

  | RecConstr          QName [FieldUpdate]

  | RecUpdate          Exp   [FieldUpdate]

  | Tuple              Boxed [Exp]

  | TupleSection       Boxed [TupArg]

  | List               [Exp]

  | ParArray           [Exp]

--  HsSyn groups below
  | EnumFrom           Exp
  | EnumFromTo         Exp Exp
  | EnumFromThen       Exp Exp
  | EnumFromThenTo     Exp Exp Exp

--  HsSyn groups below
--  ParArrayFrom       (missing?)
  | ParArrayFromTo     Exp Exp
--  ParArrayFromThen   (missing?)
  | ParArrayFromThenTo Exp Exp Exp

--  HsSyn groups below
  | ListComp           Exp [QualStmt]
--  HsSyn.MonadComp          (missing? same as ListComp?)
  | ParArrayComp       Exp [[QualStmt]]
  | Do                 [Stmt]
  | MDo                [Stmt]
--  HsSyn.ArrowExp            (?)
--  HsSyn.GhciStmtCtxt (missing?)
--  HsSyn.PatGuard  (missing?)
  | ParComp            Exp [[QualStmt]]
--  HsSyn.TransStmtCtxt (missing?)

  | BracketExp         Bracket

  | QuasiQuote         String String

  | VarQuote           QName

  | TypQuote           QName

  | SpliceExp          Splice

  | ExpTypeSig         Exp Type

  | CorePragma         String Exp

  | SCCPragma          String Exp

  | GenPragma          String (Int, Int) (Int, Int) Exp
--                     \n
--                     \n

  | Proc               Pat Exp

  | LeftArrApp         Exp Exp
  | RightArrApp        Exp Exp
  | LeftArrHighApp     Exp Exp
  | RightArrHighApp    Exp Exp

  | ExprHole

-- HsSyn.Static        (missing?)

-- HsSyn.BinTick       (missing?)

-- HsSyn.EAsPat        (missing?)

-- HsSyn.EViewPat      (missing?)

-- HsSyn.ELazyPat      (missing?)


data Pat
  = PWildCard

  | PVar        Name

  | PLit        Sign Literal

--  HsSyn.NPat (?)

  | PNPlusK     Name Integer

  | PTuple      Boxed [Pat]

  | PList       [Pat]

  | PParen      Pat

  | PAsPat      Name Pat

  | PViewPat    Exp Pat

  | PBangPat    Pat

  | PIrrPat     Pat

  | PatTypeSig  Pat Type

--  HsSyn.PArrPat (missing?)

-- HsSyb packs them together
  | PApp        QName [Pat]
  | PRec        QName [PatField]
  | PInfixApp   Pat QName Pat

  | PQuasiQuote String String

--  HsSyn.CoPat (missing?)



data Literal
  = Char       Char     String

  | PrimChar   Char     String

  | String     String   String

  | PrimString String   String

  | Int        Integer  String

  | PrimInt    Integer  String

  | PrimWord   Integer  String

--  HsSyn.Int64Prim  (missing?)

--  HsSyn.Word64Prim (missing?)

  | Frac       Rational String

  | PrimFloat  Rational String

  | PrimDouble Rational String


data Decl
  = RoleAnnotDecl     QName [Role] -- [Maybe Role] on HsSyn

  | AnnPragma         Annotation

  | DerivDecl         (Maybe Overlap) InstRule

  | WarnPragmaDecl    [([Name], String)]

  | RulePragmaDecl    [Rule]

  | DefaultDecl       [Type]

-- HsSyn groups below
  | ForImp            CallConv (Maybe Safety) (Maybe String) Name Type
  | ForExp            CallConv (Maybe String) Name Type

  | SpliceDecl        Exp

--  HsSyn.DocD  (missing?)

--  HsSyn groups below
  | TypeSig           [Name] Type
  | PatSynSig         Name (Maybe [TyVarBind]) (Maybe Context)
                      (Maybe Context) Type
--  HsSyn.ClassOpSig
--  HsSyn.IdSig
  | InfixDecl         Assoc (Maybe Int) [Op]
  | InlineSig         Bool (Maybe Activation) QName
  | SpecSig           (Maybe Activation) QName [Type]
  | InstSig           InstRule
  | MinimalPragma     (Maybe BooleanFormula)
  | InlineConlikeSig  (Maybe Activation) QName
  | SpecInlineSig     Bool (Maybe Activation) QName [Type]

--  HsSyn groups below
  | DataFamDecl       (Maybe Context) DeclHead (Maybe Kind)
  | TypeFamDecl       DeclHead (Maybe Kind)
  | ClosedTypeFamDecl DeclHead (Maybe Kind)  [TypeEqn]
  | TypeDecl          DeclHead Type
  | DataDecl          DataOrNew (Maybe Context) DeclHead
                        [QualConDecl] (Maybe Deriving)
  | GDataDecl         DataOrNew (Maybe Context) DeclHead
                        (Maybe Kind) [GadtDecl]    (Maybe Deriving)
  | ClassDecl         (Maybe Context) DeclHead [FunDep]
                        (Maybe [ClassDecl])

--  HsSyn groups below
  | InstDecl          (Maybe Overlap) InstRule (Maybe [InstDecl])
  | DataInsDecl       DataOrNew Type [QualConDecl]
                        (Maybe Deriving)
  | GDataInsDecl     DataOrNew Type (Maybe Kind) [GadtDecl]
                        (Maybe Deriving)
  | TypeInsDecl      Type Type

-- HsSyn groups below
  | FunBind          [Match]
  | PatBind          Pat Rhs (Maybe Binds) --expanding Pat below
--  Pat.PVar
--  HsSyn.AbsBinds     (what is this?)
--  HsSyn.AbsBindsSig  (what is this?)
  | PatSyn           Pat Pat PatternSynDirection

  | DeprPragmaDecl   [([Name], String)]

--  HsSyn.VectD (missing)


data Type
  = TyForall    (Maybe [TyVarBind]) (Maybe Context) Type

  | TyFun       Type Type

  | TyTuple     Boxed [Type]

  | TyList      Type

  | TyParArray  Type

  | TyApp       Type Type

  | TyVar       Name

  | TyCon       QName

  | TyParen     Type

  | TyKind      Type Kind

  | TyBang      (BangType) Type

  | TyWildCard  (Maybe Name)

  | TyEquals    Type Type

  | TySplice    Splice

  | TyInfix     Type QName Type

  | TyPromoted  Promoted
--  Promoted.PromotedInteger
--  Promoted.PromotedString
--  Promoted.PromotedCon
--  Promoted.PromotedList
--  Promoted.PromotedTuple
--  Promoted.PromotedUnit

  | TyQuasiQuoteString String

-- HsSyn.QualTy (missing)

-- HsSyn.IParamTy (missing)

-- HsSyn.DocTy (missing)

-- HsSyn.RecTy (missing)

data Boxed
  = Boxed
  | Unboxed

data ModuleName
  = ModuleName String

data ImportDecl
  = ImportDecl ModuleName Bool Bool Bool (Maybe String)
               (Maybe ModuleName) (Maybe ImportSpecList)
data ImportSpecList
  = ImportSpecList Bool [ImportSpec]

data ImportSpec
  = IVar Name
  | IAbs Namespace Name
  | IThingAll Name
  | IThingWith Name [CName]








type TupArg
  = Maybe Exp
--   ...

-- HsSyn.ArithSeqInfo is built-into Exp
--   ...
--   ...
--   ...
--   ...

-- HsSyn.StmtContext is built-into Exp
--   ...
--   ...
--   ...
--   ...
--   ...
--   ...
--   ...
--   ...
--   ...
--   ...

-- HSE defines it as `... QName [Role]` in AST
--   ...

data Role
  = Nominal
  | Representational
  | Phantom
  | RoleWildcard

data Annotation
  = Ann       Name Exp
  | TypeAnn   Name Exp
  | ModuleAnn Exp
--   ...
--   ...

-- HSE defines it as `... (Maybe Overlap) InstRule` in AST
--   ...

data Overlap
  = NoOverlap
--  Overlappable
--  Overlapping
  | Overlap
  | Incoherent

data GuardedRhs
  = GuardedRhs [Stmt] Exp

--  [Alt]
--   ...
--   ...
--   ...
--   ...

data Alt
  = Alt Pat Rhs (Maybe Binds)
data Rhs
  = UnGuardedRhs Exp
  | GuardedRhss  [GuardedRhs]
--   ...
--   ...


------------ the rest is not compared -------------



data SpecialCon
  = UnitCon
  | ListCon
  | FunCon
  | TupleCon Boxed Int
  | Cons
  | UnboxedSingleCon

data QName
  = Qual    ModuleName Name
  | UnQual  Name
  | Special SpecialCon

data Name
  = Ident  String
  | Symbol String

data IPName
  = IPDup String
  | IPLin String

data QOp
  = QVarOp QName
  | QConOp QName

data Op
  = VarOp Name
  | ConOp Name

data CName
  = VarName Name
  | ConName Name

data Module
  = Module (Maybe ModuleHead) [ModulePragma] [ImportDecl] [Decl]

data ModuleHead
  = ModuleHead ModuleName (Maybe WarningText) (Maybe ExportSpecList)

data ExportSpecList
  = ExportSpecList [ExportSpec]

data ExportSpec
  = EVar            QName
  | EAbs            Namespace QName
  | EThingAll       QName
  | EThingWith      QName [CName]
  | EModuleContents ModuleName

data Namespace
  = NoNamespace
  | TypeNamespace
  | PatternNamespace


data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight

data PatternSynDirection
  = Unidirectiona
  | ImplicitBidirectiona
  | ExplicitBidirectional [Decl]

data TypeEqn
  = TypeEqn Type Type

data BooleanFormula
  = VarFormula   Name
  | AndFormula   [BooleanFormula]
  | OrFormula    [BooleanFormula]
  | ParenFormula BooleanFormula


data DataOrNew
  = DataType
  | NewType

data DeclHead
  = DHead Name
  | DHInfix TyVarBind Name
  | DHParen DeclHead
  | DHApp   DeclHead TyVarBind

data InstRule
  = IRule (Maybe [TyVarBind]) (Maybe Context) InstHead
  | IParen InstRule

data InstHead
  = IHCon QName
  | IHInfix Type QName
  | IHParen InstHead
  | IHApp   InstHead Type

data Deriving
  = Deriving [InstRule]

data Binds
  = BDecls  [Decl]
  | IPBinds [IPBind]

data IPBind
  = IPBind IPName Exp

data Match
  = Match      Name [Pat] Rhs (Maybe Binds)
  | InfixMatch Pat Name [Pat] Rhs (Maybe Binds)

data QualConDecl
  = QualConDecl (Maybe [TyVarBind]) (Maybe Context) (ConDecl)

data ConDecl
  = ConDecl      Name [Type]
  | InfixConDecl Type Name Type
  | RecDecl      Name [FieldDecl]

data FieldDecl
  = FieldDecl [Name] Type

data GadtDecl
  = GadtDecl Name (Maybe [FieldDecl]) Type

data ClassDecl
  = ClsDecl    Decl
  | ClsDataFam (Maybe Context) DeclHead (Maybe Kind)
  | ClsTyFam                       DeclHead (Maybe Kind)
  | ClsTyDef   Type Type
  | ClsDefSig  Name Type

data InstDecl
  = InsDecl   Decl
  | InsType   Type Type
  | InsData   DataOrNew Type [QualConDecl] (Maybe Deriving)
  | InsGData  DataOrNew Type (Maybe Kind) [GadtDecl]
                (Maybe Deriving)

data BangType
  = BangedTy
  | UnpackedTy
  | NoUnpackedTy

data Promoted
  = PromotedInteger Integer String
  | PromotedString  String String
  | PromotedCon     Bool QName
  | PromotedList    Bool [Type]
  | PromotedTuple   [Type]
  | PromotedUnit



data TyVarBind
  = KindedVar   Name Kind
  | UnkindedVar Name

data Kind
  = KindStar
  | KindFn    Kind Kind
  | KindParen Kind
  | KindVar   QName
  | KindApp   Kind Kind
  | KindTuple [Kind]
  | KindList  Kind

data FunDep
  = FunDep [Name] [Name]

data Context
  = CxSingle Asst
  | CxTuple  [Asst]
  | CxEmpty

data Asst
  = ClassA    QName [Type]
  | AppA      Name [Type]
  | InfixA    Type QName Type
  | IParam    IPName Type
  | EqualP    Type Type
  | ParenA    Asst
  | WildCardA (Maybe Name)

data Sign
  = Signless
  | Negative

data Bracket
  = ExpBracket  Exp
  | PatBracket  Pat
  | TypeBracket Type
  | DeclBracket [Decl]

data Splice
  = IdSplice    String
  | ParenSplice Exp

data Safety
  = PlayRisky
  | PlaySafe          Bool
  | PlayInterruptible

data CallConv
  = StdCall
  | CCall
  | CPlusPlus
  | DotNet
  | Jvm
  | Js
  | JavaScript
  | CApi

data ModulePragma
  = LanguagePragma  [Name]
  | OptionsPragma   (Maybe Tool) String
  | AnnModulePragma Annotation

data Tool
  = GHC
  | HUGS
  | NHC98
  | YHC
  | HADDOCK
  | UnknownTool String

data Activation
  = ActiveFrom  Int
  | ActiveUntil Int

data Rule
  = Rule String (Maybe Activation) (Maybe [RuleVar]) Exp Exp

data RuleVar
  = RuleVar      Name
  | TypedRuleVar Name Type

data WarningText
  = DeprText String
  | WarnText String

data PatField
  = PFieldPat QName Pat
  | PFieldPun QName
  | PFieldWildcard

data Stmt
  = Generator Pat Exp
  | Qualifier Exp
  | LetStmt Binds
  | RecStmt [Stmt]

data QualStmt
  = QualStmt     Stmt
  | ThenTrans    Exp
  | ThenBy       Exp Exp
  | GroupBy      Exp
  | GroupUsing   Exp
  | GroupByUsing Exp Exp

data FieldUpdate
  = FieldUpdate QName Exp
  | FieldPun QName
  | FieldWildcard
