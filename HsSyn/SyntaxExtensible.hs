{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
module Syntax where

data Exp ext id
  = ExpExt          (ext "ExpExt")
  | Var             (ext "Var") id
  | UnboundVar      (ext "UnboundVar") UnboundVar
  | Lit             (ext "Lit") (Lit ext)
  | OverLit         (ext "OverLit") (OverLit ext id)
  | App             (ext "App") (Exp ext id) (Exp ext id)
  | SectionL        (ext "SectionL") (Exp ext id) (Exp ext id)
  | SectionR        (ext "SectionR") (Exp ext id) (Exp ext id)
  | NegApp          (ext "NegApp") (Exp ext id) (SyntaxExp ext id)
  | OpApp           (ext "OpApp") (Exp ext id) (Exp ext id) (PostRn id Fixity) (Exp ext id)
  | AppType         (ext "AppType") (Exp ext id) (WcType ext id)
  | AppTypeOut      (ext "AppTypeOut") (Exp ext id) (WcType ext Name)
  | Par             (ext "Par") (Exp ext id)
  | If              (ext "If") (Maybe (SyntaxExp ext id)) (Exp ext id) (Exp ext id) (Exp ext id)
  | MultiIf         (ext "MultiIf") (PostTc id TCRType) [GRHS ext id (Exp ext id)]
  | Case            (ext "Case") (Exp ext id) (MatchGroup ext id (Exp ext id))
  | Lam             (ext "Lam") (MatchGroup ext id (Exp ext id))
  | LamCase         (ext "LamCase") (MatchGroup ext id (Exp ext id))
  | Let             (ext "Let") (LocalBinds ext id) (Exp ext id)
  | IPVar           (ext "IPVar") IPName
  | RecFld          (ext "RecFld") (AmbiguousFieldOcc id)
  | OverLabel       (ext "OverLabel") FastString
  | RecordCon       (ext "RecordCon") id (PostTc id ConLike) (PostTcExp ext) (RecordBinds ext id)
  | RecordUpd       (ext "RecordUpd") (Exp ext id) [RecUpdField ext id] (PostTc id [ConLike]) (PostTc id [TCRType]) (PostTc id [TCRType]) (PostTc id Wrapper)
  | ExplicitTuple   (ext "ExplicitTuple") [TupArg ext id] Boxity
  | ExplicitList    (ext "ExplicitList") (PostTc id TCRType) (Maybe (SyntaxExp ext id)) [Exp ext id]
  | ExplicitPArr    (ext "ExplicitPArr") (PostTc id TCRType) [Exp ext id]
  | ArithSeq        (ext "ArithSeq") (PostTcExp ext) (Maybe (SyntaxExp ext id)) (ArithSeqInfo ext id)
  | PArrSeq         (ext "PArrSeq") (PostTcExp ext) (ArithSeqInfo ext id)
  | Do              (ext "Do") (StmtContext Name) [ExpStmt ext id] (PostTc id TCRType)
  | Bracket         (ext "Bracket") (Bracket ext id)
  | RnBracketOut    (ext "RnBracketOut") (Bracket ext Name) [PendingRnSplice ext]
  | TcBracketOut    (ext "TcBracketOut") (Bracket ext Name) [PendingTcSplice ext]
  | SpliceE         (ext "SpliceE") (Splice ext id)
  | ExpWithTySig    (ext "ExpWithTySig") (Exp ext id) (SigWcType ext id)
  | ExpWithTySigOut (ext "ExpWithTySigOut") (Exp ext id) (SigWcType ext Name)
  | CoreAnn         (ext "CoreAnn") SourceText StringLiteral (Exp ext id)
  | SCC             (ext "SCC") SourceText StringLiteral (Exp ext id)
  | TickPragma      (ext "TickPragma") SourceText (StringLiteral,
                          (Int, Int), (Int, Int)) ((SourceText, SourceText),
                                       (SourceText,
                                        SourceText)) (Exp ext id)
  | Proc            (ext "Proc") (Pat ext id) (CmdTop ext id)
  | ArrApp          (ext "ArrApp") (Exp ext id) (Exp ext id) (PostTc id TCRType) ArrAppType Bool
  | ArrForm         (ext "ArrForm") (Exp ext id) (Maybe Fixity) [CmdTop ext id]
  | EWildPat        (ext "EWildPat")
  | Static          (ext "Static") (Exp ext id)
  | Tick            (ext "Tick") (Tickish id) (Exp ext id)
  | BinTick         (ext "BinTick") Int Int (Exp ext id)
  | EAsPat          (ext "EAsPat") id (Exp ext id)
  | EViewPat        (ext "EViewPat") (Exp ext id) (Exp ext id)
  | ELazyPat        (ext "ELazyPat") (Exp ext id)
  | Wrap            (ext "Wrap") Wrapper (Exp ext id)
data Pat ext id
  = PatExt          (ext "PatExt")
  | WildPat         (ext "WildPat") (PostTc id TCRType)
  | VarPat          (ext "VarPat") id
  | LitPat          (ext "LitPat") (Lit ext)
  | NPat            (ext "NPat") (OverLit ext id) (Maybe (SyntaxExp ext id)) (SyntaxExp ext id) (PostTc id TCRType)
  | NPlusKPat       (ext "NPlusKPat") id (OverLit ext id) (OverLit ext id) (SyntaxExp ext id) (SyntaxExp ext id) (PostTc id TCRType)
  | TuplePat        (ext "TuplePat") [Pat ext id] Boxity [PostTc id TCRType]
  | ListPat         (ext "ListPat") [Pat ext id] (PostTc id TCRType) (Maybe (PostTc id TCRType,
                                     SyntaxExp ext id))
  | ParPat          (ext "ParPat") (Pat ext id)
  | AsPat           (ext "AsPat") id (Pat ext id)
  | ViewPat         (ext "ViewPat") (Exp ext id) (Pat ext id) (PostTc id TCRType)
  | BangPat         (ext "BangPat") (Pat ext id)
  | LazyPat         (ext "LazyPat") (Pat ext id)
  | SigPatIn        (ext "SigPatIn") (Pat ext id) (SigWcType ext id)
  | SigPatOut       (ext "SigPatOut") (Pat ext id) TCRType
  | PArrPat         (ext "PArrPat") [Pat ext id] (PostTc id TCRType)
  | ConPatIn        (ext "ConPatIn") id (ConPatDetails ext id)
  | ConPatOut       (ext "ConPatOut") ConLike [TCRType] [TyVar] [EvVar] TcEvBinds (ConPatDetails ext id) Wrapper
  | SplicePat       (ext "SplicePat") (Splice ext id)
  | CoPat           (ext "CoPat") Wrapper (Pat ext id) TCRType
data Lit ext
  = LitExt          (ext "LitExt")
  | Char            (ext "Char") SourceText Char
  | CharPrim        (ext "CharPrim") SourceText Char
  | String          (ext "String") SourceText FastString
  | StringPrim      (ext "StringPrim") SourceText ByteString
  | Int             (ext "Int") SourceText Integer
  | IntPrim         (ext "IntPrim") SourceText Integer
  | WordPrim        (ext "WordPrim") SourceText Integer
  | Int64Prim       (ext "Int64Prim") SourceText Integer
  | Word64Prim      (ext "Word64Prim") SourceText Integer
  | Integer         (ext "Integer") SourceText Integer TCRType
  | Rat             (ext "Rat") FractionalLit TCRType
  | FloatPrim       (ext "FloatPrim") FractionalLit
  | DoublePrim      (ext "DoublePrim") FractionalLit
data Decl ext id
  = DeclExt         (ext "DeclExt")
  | RoleAnnotD      (ext "RoleAnnotD") (RoleAnnotDecl id)
  | AnnD            (ext "AnnD") (AnnDecl ext id)
  | DerivD          (ext "DerivD") (DerivDecl ext id)
  | WarningD        (ext "WarningD") (WarnDecls id)
  | RuleD           (ext "RuleD") (RuleDecls ext id)
  | DefD            (ext "DefD") (DefaultDecl ext id)
  | ForD            (ext "ForD") (ForeignDecl ext id)
  | SpliceD         (ext "SpliceD") (SpliceDecl ext id)
  | DocD            (ext "DocD") DocDecl
  | SigD            (ext "SigD") (Sig ext id)
  | TyClD           (ext "TyClD") (TyClDecl ext id)
  | InstD           (ext "InstD") (InstDecl ext id)
  | ValD            (ext "ValD") (Bind ext id)
  | VectD           (ext "VectD") (VectDecl ext id)
data Type ext id
  = TypeExt         (ext "TypeExt")
  | ForAllTy        (ext "ForAllTy") [TyVarBndr ext id] (Type ext id)
  | FunTy           (ext "FunTy") (Type ext id) (Type ext id)
  | TupleTy         (ext "TupleTy") TupleSort [Type ext id]
  | ListTy          (ext "ListTy") (Type ext id)
  | PArrTy          (ext "PArrTy") (Type ext id)
  | AppTy           (ext "AppTy") (Type ext id) (Type ext id)
  | TyVar           (ext "TyVar") id
  | ParTy           (ext "ParTy") (Type ext id)
  | KindSig         (ext "KindSig") (Type ext id) (Kind ext id)
  | BangTy          (ext "BangTy") SrcBang (Type ext id)
  | WildCardTy      (ext "WildCardTy") (WildCardInfo id)
  | EqTy            (ext "EqTy") (Type ext id) (Type ext id)
  | SpliceTy        (ext "SpliceTy") (Splice ext id) (PostTc id TCRKind)
  | OpTy            (ext "OpTy") (Type ext id) id (Type ext id)
  | TyLit           (ext "TyLit") TyLit
  | AppsTy          (ext "AppsTy") [AppType ext id]
  | ExplicitListTy  (ext "ExplicitListTy") (PostTc id TCRKind) [Type ext id]
  | ExplicitTupleTy (ext "ExplicitTupleTy") [PostTc id TCRKind] [Type ext id]
  | QualTy          (ext "QualTy") (Context ext id) (Type ext id)
  | IParamTy        (ext "IParamTy") IPName (Type ext id)
  | DocTy           (ext "DocTy") (Type ext id) DocString
  | RecTy           (ext "RecTy") [ConDeclField ext id]
  | CoreTy          (ext "CoreTy") TCRType
data TCRType
type TCRKind = TCRType
data FractionalLit
data Id
data Name
data OccName
data Wrapper
data GlobalRdrEnv
data ByteString
data FastString
data PostRn a b
data PostTc a b
data Fixity
data Boxity
data ConLike
data SourceText
data StringLiteral
data Tickish a
data Origin
data SrcSpan
data TyVar
data EvVar
data TcEvBinds
data ModuleName
data FieldLbl a
data RecFlag
data Bag a
data NameSet
data InlinePragma
data BooleanFormula a
data FunDep a
data CType
data OverlapMode
data Coercion
data Safety
data TyCon
data Activation
data RuleName
data CCallConv
data Header
data Class
data ClsInst
data Role
data CLabelString
data CCallTarget
data CExportSpec
data WarningTxt
data SrcBang
data RdrName
data OverLit ext id
  = OverLit' OverLitVal (PostRn id Bool) (Exp ext id) (PostTc id TCRType)
data OverLitVal
  = Integral !SourceText !Integer
  | Fractional !FractionalLit
  | IsString !SourceText !FastString
type PostTcExp ext = Exp ext Id
data SyntaxExp ext id = SyntaxExp (Exp ext id) [Wrapper] Wrapper
type CmdSyntaxTable ext id = [(Name, Exp ext id)]
data UnboundVar
  = OutOfScope OccName GlobalRdrEnv | TrueExpHole OccName
data TupArg ext id
  = Present (Exp ext id) | Missing (PostTc id TCRType)
data Cmd ext id
  = CmdArrApp (Exp ext id) (Exp ext id) (PostTc id TCRType) ArrAppType Bool
  | CmdArrForm (Exp ext id) (Maybe Fixity) [CmdTop ext id]
  | CmdApp (Cmd ext id) (Exp ext id)
  | CmdLam (MatchGroup ext id (Cmd ext id))
  | CmdPar (Cmd ext id)
  | CmdCase (Exp ext id) (MatchGroup ext id (Cmd ext id))
  | CmdIf (Maybe (SyntaxExp ext id)) (Exp ext id) (Cmd ext id) (Cmd ext id)
  | CmdLet (LocalBinds ext id) (Cmd ext id)
  | CmdDo [CmdStmt ext id] (PostTc id TCRType)
  | CmdWrap Wrapper (Cmd ext id)
data ArrAppType = HigherOrderApp | FirstOrderApp
data CmdTop ext id
  = CmdTop (Cmd ext id) (PostTc id TCRType) (PostTc id TCRType) (CmdSyntaxTable ext id)
type RecordBinds ext id = RecFields id (Exp ext id)
data MatchGroup ext id body
  = MG [Match ext id body] [PostTc id TCRType] (PostTc id TCRType) Origin
data Match ext id body
  = Match (MatchFixity id) [Pat ext id] (Maybe (Type ext id)) (GRHSs ext id body)
data MatchFixity id = NonFunBindMatch | FunBindMatch id Bool
data GRHSs ext id body
  = GRHSs [GRHS ext id body] (LocalBinds ext id)
data GRHS ext id body = GRHS [GuardStmt ext id] body
type Stmt ext id body = StmtLR ext id id body
type CmdStmt ext id = Stmt ext id (Cmd ext id)
type ExpStmt ext id = Stmt ext id (Exp ext id)
type GuardStmt ext id = Stmt ext id (Exp ext id)
data StmtLR ext idL idR body
  = LastStmt body Bool (SyntaxExp ext idR)
  | BindStmt (Pat ext idL) body (SyntaxExp ext idR) (SyntaxExp ext idR) (PostTc idR TCRType)
  | ApplicativeStmt [(SyntaxExp ext idR,
              ApplicativeArg ext idL idR)] (Maybe (SyntaxExp ext idR)) (PostTc idR TCRType)
  | BodyStmt body (SyntaxExp ext idR) (SyntaxExp ext idR) (PostTc idR TCRType)
  | LetStmt (LocalBindsLR ext idL idR)
  | ParStmt [ParStmtBlock ext idL idR] (Exp ext idR) (SyntaxExp ext idR) (PostTc idR TCRType)
  | TransStmt TransForm [ExpStmt ext idL] [(idR,
                        idR)] (Exp ext idR) (Maybe (Exp ext idR)) (SyntaxExp ext idR) (SyntaxExp ext idR) (PostTc idR TCRType) (Exp ext idR)
  | RecStmt [StmtLR ext idL idR body] [idR] [idR] (SyntaxExp ext idR) (SyntaxExp ext idR) (SyntaxExp ext idR) (PostTc idR TCRType) [PostTcExp ext] [PostTcExp ext] (PostTc idR TCRType)
data TransForm = ThenForm | GroupForm
data ParStmtBlock ext idL idR
  = ParStmtBlock [ExpStmt ext idL] [idR] (SyntaxExp ext idR)
data ApplicativeArg ext idL idR
  = ApplicativeArgOne (Pat ext idL) (Exp ext idL)
  | ApplicativeArgMany [ExpStmt ext idL] (Exp ext idL) (Pat ext idL)
data Splice ext id
  = TypedSplice id (Exp ext id)
  | UntypedSplice id (Exp ext id)
  | QuasiQuote id id SrcSpan FastString
type SplicePointName = Name
data PendingRnSplice ext
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (Exp ext Name)
data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
data PendingTcSplice ext
  = PendingTcSplice SplicePointName (Exp ext Id)
data Bracket ext id
  = ExpBr (Exp ext id)
  | PatBr (Pat ext id)
  | DecBrL [Decl ext id]
  | DecBrG (Group ext id)
  | TypBr (Type ext id)
  | VarBr Bool id
  | TExpBr (Exp ext id)
data ArithSeqInfo ext id
  = From (Exp ext id)
  | FromThen (Exp ext id) (Exp ext id)
  | FromTo (Exp ext id) (Exp ext id)
  | FromThenTo (Exp ext id) (Exp ext id) (Exp ext id)
data MatchContext id
  = FunRhs id
  | LambdaExp
  | CaseAlt
  | IfAlt
  | ProcExp
  | PatBindRhs
  | RecUpd
  | StmtCtxt (StmtContext id)
  | ThPatSplice
  | ThPatQuote
  | PatSyn
data StmtContext id
  = ListComp
  | MonadComp
  | PArrComp
  | DoExp
  | MDoExp
  | ArrowExp
  | GhciStmtCtxt
  | PatGuard (MatchContext id)
  | ParStmtCtxt (StmtContext id)
  | TransStmtCtxt (StmtContext id)
type ConPatDetails ext id =
    ConDetails (Pat ext id) (RecFields id (Pat ext id))
data RecFields id arg = RecFields [RecField id arg] (Maybe Int)
type RecField id arg = RecField' (FieldOcc id) arg
type RecUpdField ext id =
    RecField' (AmbiguousFieldOcc id) (Exp ext id)
data RecField' id arg = RecField id arg Bool
data DocString = DocString FastString
data ImportDecl id
  = ImportDecl (Maybe SourceText) ModuleName (Maybe StringLiteral) Bool Bool Bool Bool (Maybe ModuleName) (Maybe (Bool,
                                                              [IE id]))
data IE id
  = IEVar id
  | IEThingAbs id
  | IEThingAll id
  | IEThingWith id IEWildcard [id] [FieldLbl id]
  | IEModuleContents ModuleName
  | IEGroup Int DocString
  | IEDoc DocString
  | IEDocNamed String
data IEWildcard = NoIEWildcard | IEWildcard Int
type Bind ext id = BindLR ext id id
type Binds ext id = BindsLR ext id id
type BindsLR ext idL idR = Bag (BindLR ext idL idR)
type LocalBinds ext id = LocalBindsLR ext id id
data LocalBindsLR ext idL idR
  = ValBinds (ValBindsLR ext idL idR)
  | IPBinds (IPBinds ext idR)
  | EmptyLocalBinds
type ValBinds ext id = ValBindsLR ext id id
data ValBindsLR ext idL idR
  = ValBindsIn (BindsLR ext idL idR) [Sig ext idR]
  | ValBindsOut [(RecFlag, Binds ext idL)] [Sig ext Name]
data BindLR ext idL idR
  = FunBind idL (MatchGroup ext idR (Exp ext idR)) Wrapper (PostRn idL NameSet) [Tickish Id]
  | PatBind (Pat ext idL) (GRHSs ext idR (Exp ext idR)) (PostTc idR TCRType) (PostRn idL NameSet) ([Tickish Id],
                                                       [[Tickish Id]])
  | VarBind idL (Exp ext idR) Bool
  | AbsBinds [TyVar] [EvVar] [ABExport idL] [TcEvBinds] (Binds ext idL)
  | AbsBindsSig [TyVar] [EvVar] idL TcSpecPrags TcEvBinds (Bind ext idL)
  | PatSynBind (PatSynBind ext idL idR)
data ABExport id = ABE id id Wrapper TcSpecPrags
data PatSynBind ext idL idR
  = PSB idL (PostRn idR NameSet) (PatSynDetails idR) (Pat ext idR) (PatSynDir ext idR)
data IPBinds ext id = IPBinds' [IPBind ext id] TcEvBinds
data IPBind ext id = IPBind (Either IPName id) (Exp ext id)
data Sig ext id
  = TypeSig [id] (SigWcType ext id)
  | PatSynSig id (SigType ext id)
  | ClassOpSig Bool [id] (SigType ext id)
  | IdSig Id
  | FixSig (FixitySig id)
  | InlineSig id InlinePragma
  | SpecSig id [SigType ext id] InlinePragma
  | SpecInstSig SourceText (SigType ext id)
  | MinimalSig SourceText (BooleanFormula id)
data FixitySig id = FixitySig [id] Fixity
data TcSpecPrags = IsDefaultMethod | SpecPrags [TcSpecPrag]
data TcSpecPrag = SpecPrag Id Wrapper InlinePragma
data PatSynDetails a
  = InfixPatSyn a a
  | PrefixPatSyn [a]
  | RecordPatSyn [RecordPatSynField a]
data RecordPatSynField a = RecordPatSynField a a
data PatSynDir ext id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup ext id (Exp ext id))
data Group ext id
  = Group (ValBinds ext id) [SpliceDecl ext id] [TyClGroup ext id] [DerivDecl ext id] [FixitySig id] [DefaultDecl ext id] [ForeignDecl ext id] [WarnDecls id] [AnnDecl ext id] [RuleDecls ext id] [VectDecl ext id] [DocDecl]
data SpliceExplicitFlag = ExplicitSplice | ImplicitSplice
data SpliceDecl ext id
  = SpliceDecl (Splice ext id) SpliceExplicitFlag
data TyClDecl ext id
  = FamDecl (FamilyDecl ext id)
  | SynDecl id (QTyVars ext id) (Type ext id) (PostRn id NameSet)
  | DataDecl id (QTyVars ext id) (DataDefn ext id) (PostRn id Bool) (PostRn id NameSet)
  | ClassDecl (Context ext id) id (QTyVars ext id) [FunDep id] [Sig ext id] (Binds ext id) [FamilyDecl ext id] [TyFamDefltEqn ext id] [DocDecl] (PostRn id NameSet)
data TyClGroup ext id
  = TyClGroup [TyClDecl ext id] [RoleAnnotDecl id] [InstDecl ext id]
data FamilyResultSig ext id
  = NoSig | KindSig' (Kind ext id) | TyVarSig (TyVarBndr ext id)
data FamilyDecl ext id
  = FamilyDecl (FamilyInfo ext id) id (QTyVars ext id) (FamilyResultSig ext id) (Maybe (InjectivityAnn id))
data InjectivityAnn id = InjectivityAnn id [id]
data FamilyInfo ext id
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [TyFamInstEqn ext id])
data DataDefn ext id
  = DataDefn NewOrData (Context ext id) (Maybe CType) (Maybe (Kind ext id)) [ConDecl ext id] (Deriving ext id)
type Deriving ext id = Maybe [SigType ext id]
data NewOrData = NewType | DataType
data ConDecl ext id
  = ConDeclGADT [id] (SigType ext id) (Maybe DocString)
  | ConDeclH98 id (Maybe (QTyVars ext id)) (Maybe (Context ext id)) (ConDeclDetails ext id) (Maybe DocString)
type ConDeclDetails ext id =
    ConDetails (BangType ext id) [ConDeclField ext id]
type TyPats ext id = ImplicitBndrs id [Type ext id]
type TyFamInstEqn ext id = TyFamEqn ext id (TyPats ext id)
type TyFamDefltEqn ext id = TyFamEqn ext id (QTyVars ext id)
data TyFamEqn ext id pats = TyFamEqn id pats (Type ext id)
data TyFamInstDecl ext id
  = TyFamInstDecl (TyFamInstEqn ext id) (PostRn id NameSet)
data DataFamInstDecl ext id
  = DataFamInstDecl id (TyPats ext id) (DataDefn ext id) (PostRn id NameSet)
data ClsInstDecl ext id
  = ClsInstDecl (SigType ext id) (Binds ext id) [Sig ext id] [TyFamInstDecl ext id] [DataFamInstDecl ext id] (Maybe OverlapMode)
data InstDecl ext id
  = ClsInstD (ClsInstDecl ext id)
  | DataFamInstD (DataFamInstDecl ext id)
  | TyFamInstD (TyFamInstDecl ext id)
data DerivDecl ext id
  = DerivDecl (SigType ext id) (Maybe OverlapMode)
data DefaultDecl ext id = DefaultDecl [Type ext id]
data ForeignDecl ext id
  = ForeignImport id (SigType ext id) (PostTc id Coercion) ForeignImport
  | ForeignExport id (SigType ext id) (PostTc id Coercion) ForeignExport
data ForeignImport
  = CImport CCallConv Safety (Maybe Header) CImportSpec SourceText
data CImportSpec
  = CLabel CLabelString | CFunction CCallTarget | CWrapper
data ForeignExport = CExport CExportSpec SourceText
data RuleDecls ext id = Rules SourceText [RuleDecl ext id]
data RuleDecl ext id
  = Rule (SourceText,
          RuleName) Activation [RuleBndr ext id] (Exp ext id) (PostRn id NameSet) (Exp ext id) (PostRn id NameSet)
data RuleBndr ext id
  = RuleBndr id | RuleBndrSig id (SigWcType ext id)
data VectDecl ext id
  = Vect SourceText id (Exp ext id)
  | NoVect SourceText id
  | VectTypeIn SourceText Bool id (Maybe id)
  | VectTypeOut Bool TyCon (Maybe TyCon)
  | VectClassIn SourceText id
  | VectClassOut Class
  | VectInstIn (SigType ext id)
  | VectInstOut ClsInst
data DocDecl
  = DocCommentNext DocString
  | DocCommentPrev DocString
  | DocCommentNamed String DocString
  | DocGroup Int DocString
data WarnDecls id = Warnings SourceText [WarnDecl id]
data WarnDecl id = Warning [id] WarningTxt
data AnnDecl ext id
  = Annotation SourceText (AnnProvenance id) (Exp ext id)
data AnnProvenance id
  = ValueAnnProvenance id
  | TypeAnnProvenance id
  | ModuleAnnProvenance
data RoleAnnotDecl id = RoleAnnotDecl id [Maybe Role]
type BangType ext id = Type ext id
type Context ext id = [Type ext id]
type Kind ext id = Type ext id
data QTyVars ext id
  = QTvs (PostRn id [Name]) [TyVarBndr ext id] (PostRn id NameSet)
data ImplicitBndrs id thing = IB (PostRn id [Name]) thing
data WildCardBndrs id thing
  = WC (PostRn id [Name]) (Maybe SrcSpan) thing
type SigType ext id = ImplicitBndrs id (Type ext id)
type WcType ext id = WildCardBndrs id (Type ext id)
type SigWcType ext id = ImplicitBndrs id (WcType ext id)
data IPName = IPName FastString
data TyVarBndr ext id = UserTyVar id | KindedTyVar id (Kind ext id)
data TyLit = NumTy SourceText Integer | StrTy SourceText FastString
data WildCardInfo id = AnonWildCard (PostRn id Name)
data AppType ext id = AppInfix id | AppPrefix (Type ext id)
data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple
data ConDeclField ext id
  = ConDeclField [FieldOcc id] (BangType ext id) (Maybe DocString)
data ConDetails arg rec
  = PrefixCon [arg] | RecCon rec | InfixCon arg arg
data FieldOcc id = FieldOcc RdrName (PostRn id id)
data AmbiguousFieldOcc id
  = Unambiguous RdrName (PostRn id id)
  | Ambiguous RdrName (PostTc id id)
