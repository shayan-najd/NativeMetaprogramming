{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds,TemplateHaskell #-}
module Syntax where
import Extensible

desugarExtensible "Exp" [d|
 {-# Ann type Exp Extensible #-}
 data Exp id
  = Var      id
 --           \n
  | UnboundVar UnboundVar
 --           \n
 --  HSE.Con (missing?)
 --           \n

  | Lit      Lit
 --           \n
  | OverLit  (OverLit id)
 --           \n

  | App      (Exp id) (Exp id)
 --           \n

  | SectionL (Exp id) (Exp id)
 --           \n

  | SectionR (Exp id) (Exp id)
 --           \n

  | NegApp   (Exp id) (SyntaxExp id)
 --           \n

  | OpApp    (Exp id) (Exp id) (PostRn id Fixity)
             (Exp id)
 --           \n

  | AppType    (Exp id) (WcType id)
 --           \n
  | AppTypeOut (Exp id) (WcType Name)
 --           \n

  | Par      (Exp id)
 --           \n

  | If       (Maybe (SyntaxExp id)) (Exp id) (Exp id) (Exp id)
 --           \n
 --           \n

  | MultiIf  (PostTc id TCRType) [GRHS id (Exp id)]
 --           \n

  | Case     (Exp id) (MatchGroup id (Exp id))
 --           \n

  | Lam      (MatchGroup id (Exp id))
 --           \n

  | LamCase  (MatchGroup id (Exp id))
 --           \n

  | Let      (LocalBinds id) (Exp  id)
 --           \n

  | IPVar    IPName
 --           \n

  | RecFld    (AmbiguousFieldOcc id)
 --           \n

  | OverLabel FastString
 --           \n

  | RecordCon id (PostTc id ConLike) PostTcExp
              (RecordBinds id)
 --           \n

  | RecordUpd (Exp id) [RecUpdField id]
              (PostTc id [ConLike]) (PostTc id [TCRType])
              (PostTc id [TCRType]) (PostTc id Wrapper)
 --           \n

  | ExplicitTuple [TupArg id] Boxity
 --           \n
 --  HSE.TupleSection (part of above?)
 --           \n

  | ExplicitList (PostTc id TCRType) (Maybe (SyntaxExp id))
                 [Exp id]
 --           \n

  | ExplicitPArr (PostTc id TCRType) [Exp id]
 --           \n

  | ArithSeq  PostTcExp (Maybe (SyntaxExp id))
              (ArithSeqInfo id)
 --           \n
 --  ArithSeqInfo.From
 --           \n
 --  ArithSeqInfo.FromTo
 --           \n
 --  ArithSeqInfo.FromThen
 --           \n
 --  ArithSeqInfo.FromThenTo
 --           \n

  | PArrSeq   PostTcExp
              (ArithSeqInfo id) -- expanding ArithSeqInfo below
 --           \n
 --  ArithSeqInfo.From
 --  ArithSeqInfo.FromTo
 --           \n
 --  ArithSeqInfo.FromThen
 --  ArithSeqInfo.FromThenTo
 --           \n

  | Do           (StmtContext Name) [ExpStmt id]
                 (PostTc id TCRType)
 --           \n
 --  StmtContext.ListComp
 --           \n
 --  StmtContext.MonadComp
 --  StmtContext.ParArrComp
 --           \n
 --  StmtContext.DoExp
 --           \n
 --  StmtContext.MDoExp
 --           \n
 --  StmtContext.ArrowExp
 --  StmtContext.GhciStmtCtxt
 --  StmtContext.PatGuard
 --  StmtContext.ParStmtCtxt
 --           \n
 --  StmtContext.TransStmtCtxt

  | Bracket      (Bracket id)
 --           \n
  | RnBracketOut (Bracket Name) [PendingRnSplice]
 --           \n
  | TcBracketOut (Bracket Name) [PendingTcSplice]
 --           \n
 --  HSE.QuasiQuote (missing?)
 --           \n

 --  HSE.VarQuote (missing?)
 --           \n
 --  HSE.TypQuote (missing?)
 --           \n

  | SpliceE  (Splice id)
 --           \n

  | ExpWithTySig (Exp id) (SigWcType id)
 --           \n
  | ExpWithTySigOut (Exp id) (SigWcType Name)
 --           \n

  | CoreAnn   SourceText StringLiteral (Exp id)
 --           \n
  | SCC       SourceText StringLiteral (Exp id)
 --           \n
  | TickPragma SourceText (StringLiteral,(Int,Int),(Int,Int))
               ((SourceText,SourceText),(SourceText,SourceText))
               (Exp id) -- (is it GenPragma?)
 --           \n

  | Proc      (Pat id) (CmdTop id)
 --           \n

  | ArrApp    (Exp id) (Exp id) (PostTc id TCRType)
              ArrAppType Bool
 --           \n
 --  HSE.LeftArrHighApp (don't know how they compare)
 --           \n
  | ArrForm   (Exp id) (Maybe Fixity) [CmdTop id]
 --           \n
 --  HSE.RightArrHighApp (don't know how they compare)

  | EWildPat -- (right comparison?)
 --           \n

  | Static    (Exp id)
 --           \n

  | Tick      (Tickish id) (Exp id)
 --           \n

  | BinTick   Int Int (Exp id)
 --           \n

  | EAsPat    id (Exp id)
 --           \n

  | EViewPat  (Exp id) (Exp id)
 --           \n

  | ELazyPat  (Exp id)
 --           \n

  | Wrap      Wrapper (Exp id)
 --           \n

 --
 --
 {-# Ann type Pat Extensible #-}
 data Pat id
  = WildPat   (PostTc id TCRType)

  | VarPat    id

  | LitPat    Lit
  | NPat      (OverLit id)
              (Maybe (SyntaxExp id))
              (SyntaxExp id) (PostTc id TCRType)

  | NPlusKPat id (OverLit id)
              (OverLit id) (SyntaxExp id)
              (SyntaxExp id) (PostTc id TCRType)

  | TuplePat  [Pat id] Boxity [PostTc id TCRType]

  | ListPat   [Pat id] (PostTc id TCRType)
              (Maybe (PostTc id TCRType, SyntaxExp id))

  | ParPat    (Pat id)

  | AsPat     id (Pat id)

  | ViewPat   (Exp id) (Pat id) (PostTc id TCRType)

  | BangPat   (Pat id)

  | LazyPat   (Pat id)

  | SigPatIn  (Pat id) (SigWcType id)
  | SigPatOut (Pat id) TCRType

  | PArrPat   [Pat id] (PostTc id TCRType)

  | ConPatIn  id (ConPatDetails id)
  | ConPatOut ConLike [TCRType] [TyVar] [EvVar] TcEvBinds
              (ConPatDetails id)
              Wrapper
 --  PRec
 --  PInfixApp

  | SplicePat (Splice id)

  | CoPat     Wrapper (Pat id) TCRType


 {-# Ann type Lit Extensible #-}
 data Lit
  = Char          SourceText Char

  | CharPrim      SourceText Char

  | String        SourceText FastString

  | StringPrim    SourceText ByteString

  | Int           SourceText Integer

  | IntPrim       SourceText Integer

  | WordPrim      SourceText Integer

  | Int64Prim     SourceText Integer

  | Word64Prim    SourceText Integer

  | Integer       SourceText Integer TCRType

  | Rat           FractionalLit TCRType

  | FloatPrim     FractionalLit

  | DoublePrim    FractionalLit

 {-# Ann type Decl Extensible #-}
 data Decl id
  = RoleAnnotD (RoleAnnotDecl id)

  | AnnD (AnnDecl id)

  | DerivD (DerivDecl id)

  | WarningD (WarnDecls id)

  | RuleD (RuleDecls id)

  | DefD (DefaultDecl id)

  | ForD (ForeignDecl id)
 --   \n
 --  ForExp (in ForeignDecl)

  | SpliceD (SpliceDecl id)

  | DocD (DocDecl)

  | SigD (Sig id)
 --  Sig.TypeSig
 --  Sig.PatSynSig
 --    \n
 --  Sig.ClassOpSig
 --  Sig.IdSig
 --  Sig.FixSig
 --  Sig.InlineSig
 --  Sig.SpecSig
 --  Sig.SpecInstSig
 --  Sig.MinimalSig
 --  HSe.InlineConlikeSig (what does it correspond to?)
 --  HSe.SpecInlineSig    (what does it correspond to?)

  | TyClD (TyClDecl id) -- expanding TyClDecl below
 --  TyClDecl.FamilyDecl.DataFamily
 --  TyClDecl.FamilyDecl.OpenTypeFamily
 --  TyClDecl.FamilyDecl.ClosedTypeFamily
 --  TyClDecl.SynDecl
 --  TyClDecl.DataDecl
 --             \n
 --           GDataDecl (grouped with above?)
 --             \n
 --  TyClDecl.ClassDecl
 --             \n

  | InstD (InstDecl id) -- expanding InstDecl below
 --  InstDecl.ClsInstD
 --  InstDecl.DataFamInstD
 --             \n
 --           GDataInsDecl (grouped with above?)
 --             \n
 --  InstDecl.TyFamInstD

  | ValD (Bind id)
 --  Bind.FunBind
 --  Bind.PatBind
 --  Bind.VarBind
 --  Bind.AbsBinds
 --  Bind.AbsBindsSig
 --  Bind.PatSynBind

 --  HSE.DeprPragmaDecl  (missing?)

  | VectD (VectDecl id)

 {-# Ann type Type Extensible #-}
 data Type id
  = ForAllTy [TyVarBndr id] (Type id)

  | FunTy (Type id) (Type id)

  | TupleTy TupleSort [Type id]

  | ListTy (Type id)

  | PArrTy (Type id)

  | AppTy (Type id) (Type id)

  | TyVar id

 -- HSE.TyCon (part of above)

  | ParTy (Type id)

  | KindSig (Type id) (Kind id)

  | BangTy SrcBang (Type id)

  | WildCardTy (WildCardInfo id)

  | EqTy (Type id) (Type id)

  | SpliceTy (Splice id) (PostTc id TCRKind)

  | OpTy (Type id) id (Type id) -- match?

  | TyLit TyLit
 --  TyLit.Num
 --  TyLit.String
  | AppsTy [AppType id] -- assuming it is promoted constructors
  | ExplicitListTy (PostTc id TCRKind) [Type id]
  | ExplicitTupleTy [PostTc id TCRKind] [Type id]
 --  HSE.Promoted.PromotedUnit (ExplicitTupleTy [] []?)

 --  HSE.TyQuasiQuote (missing?)

  | QualTy (Context id) (Type id)

  | IParamTy IPName (Type id)

  | DocTy (Type id) DocString

  | RecTy [ConDeclField id]

  | CoreTy TCRType

 ------------ the rest is not compared -------------

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

 data OverLit id
  = OverLit' OverLitVal (PostRn id Bool) (Exp id) (PostTc id TCRType)

 data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

 type PostTcExp   = Exp Id

 data SyntaxExp id
  = SyntaxExp (Exp id) [Wrapper] Wrapper

 type CmdSyntaxTable id = [(Name, Exp id)]

 data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

 data TupArg id
  = Present (Exp id)
  | Missing (PostTc id TCRType)

 data Cmd id
  = CmdArrApp  (Exp id) (Exp id) (PostTc id TCRType) ArrAppType Bool
  | CmdArrForm (Exp id) (Maybe Fixity) [CmdTop id]
  | CmdApp     (Cmd id)  (Exp id)
  | CmdLam     (MatchGroup id (Cmd id))
  | CmdPar     (Cmd id)
  | CmdCase    (Exp id) (MatchGroup id (Cmd id))
  | CmdIf      (Maybe (SyntaxExp id)) (Exp id) (Cmd id) (Cmd id)
  | CmdLet     (LocalBinds id) (Cmd  id)
  | CmdDo      [CmdStmt id] (PostTc id TCRType)
  | CmdWrap    Wrapper (Cmd id)

 data ArrAppType
  = HigherOrderApp
  | FirstOrderApp

 data CmdTop id
  = CmdTop (Cmd id) (PostTc id TCRType) (PostTc id TCRType)
           (CmdSyntaxTable id)

 type RecordBinds id = RecFields id (Exp id)

 data MatchGroup id body
  = MG [Match id body] [PostTc id TCRType] (PostTc id TCRType) Origin

 data Match id body
  = Match (MatchFixity id) [Pat id] (Maybe (Type id)) (GRHSs id body)

 data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch id Bool

 data GRHSs id body
  = GRHSs [GRHS id body] (LocalBinds id)

 data GRHS id body
  = GRHS [GuardStmt id] body

 type Stmt id body  = StmtLR id id body
 type CmdStmt    id = Stmt  id (Cmd  id)
 type ExpStmt    id = Stmt  id (Exp id)
 type GuardStmt  id = Stmt  id (Exp id)

 data StmtLR idL idR body
  = LastStmt body Bool (SyntaxExp idR)
  | BindStmt (Pat idL) body (SyntaxExp idR) (SyntaxExp idR)
             (PostTc idR TCRType)
  | ApplicativeStmt [(SyntaxExp idR, ApplicativeArg idL idR)]
                    (Maybe (SyntaxExp idR)) (PostTc idR TCRType)
  | BodyStmt body (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
  | LetStmt  (LocalBindsLR idL idR)
  | ParStmt  [ParStmtBlock idL idR] (Exp idR) (SyntaxExp idR)
             (PostTc idR TCRType)
  | TransStmt TransForm [ExpStmt idL] [(idR, idR)] (Exp idR)
              (Maybe (Exp idR))
              (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType) (Exp idR)
  | RecStmt [StmtLR idL idR body] [idR] [idR] (SyntaxExp idR)
            (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
            [PostTcExp] [PostTcExp] (PostTc idR TCRType)

 data TransForm
  = ThenForm
  | GroupForm

 data ParStmtBlock idL idR
  = ParStmtBlock [ExpStmt idL] [idR] (SyntaxExp idR)

 data ApplicativeArg idL idR
  = ApplicativeArgOne (Pat idL) (Exp idL)
  | ApplicativeArgMany [ExpStmt idL] (Exp idL) (Pat idL)

 data Splice id
  = TypedSplice id (Exp id)
  | UntypedSplice id (Exp id)
  | QuasiQuote id id SrcSpan FastString

 type SplicePointName = Name

 data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (Exp Name)

 data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

 data PendingTcSplice
  = PendingTcSplice SplicePointName (Exp Id)

 data Bracket id
  = ExpBr (Exp id)
  | PatBr (Pat id)
  | DecBrL [Decl id]
  | DecBrG (Group id)
  | TypBr (Type id)
  | VarBr Bool id
  | TExpBr (Exp id)

 data ArithSeqInfo id
  = From            (Exp id)
  | FromThen        (Exp id) (Exp id)
  | FromTo          (Exp id) (Exp id)
  | FromThenTo      (Exp id) (Exp id) (Exp id)

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

 -------------------------------------------------------------------------------
 -- Pat

 type ConPatDetails id = ConDetails (Pat id) (RecFields id (Pat id))

 data RecFields id arg
  = RecFields [RecField id arg] (Maybe Int)

 type RecField id arg = RecField' (FieldOcc id) arg

 type RecUpdField id = RecField' (AmbiguousFieldOcc id) (Exp id)

 data RecField' id arg
  = RecField id arg Bool

 --------------------------------------------------------------------------------
 -- Doc

 newtype DocString = DocString FastString

 --------------------------------------------------------------------------------
 -- ImpExp

 data ImportDecl id
  = ImportDecl (Maybe SourceText) ModuleName
               (Maybe StringLiteral) Bool Bool Bool Bool
               (Maybe ModuleName) (Maybe (Bool, [IE id]))

 data IE id
  = IEVar id
  | IEThingAbs id
  | IEThingAll id
  | IEThingWith id IEWildcard [id] [FieldLbl id]
  | IEModuleContents ModuleName
  | IEGroup Int DocString
  | IEDoc DocString
  | IEDocNamed String

 data IEWildcard
  = NoIEWildcard
  | IEWildcard Int

 -------------------------------------------------------------------------------
 -- Binds

 type Bind id              = BindLR id id
 type Binds        id      = BindsLR id id
 type BindsLR      idL idR = Bag (BindLR idL idR)

 type LocalBinds id = LocalBindsLR id id

 data LocalBindsLR idL idR
  = ValBinds (ValBindsLR idL idR)
  | IPBinds (IPBinds idR)
  | EmptyLocalBinds

 type ValBinds id = ValBindsLR id id

 data ValBindsLR idL idR
  = ValBindsIn (BindsLR idL idR) [Sig idR]
  | ValBindsOut [(RecFlag, Binds idL)] [Sig Name]

 data BindLR idL idR
  = FunBind (idL) (MatchGroup idR (Exp idR)) Wrapper
            (PostRn idL NameSet) [Tickish Id]
  | PatBind (Pat idL) (GRHSs idR (Exp idR)) (PostTc idR TCRType)
            (PostRn idL NameSet) ([Tickish Id], [[Tickish Id]])
  | VarBind idL (Exp idR) Bool
  | AbsBinds [TyVar] [EvVar] [ABExport idL] [TcEvBinds] (Binds idL)
  | AbsBindsSig [TyVar] [EvVar] idL TcSpecPrags TcEvBinds (Bind idL)
  | PatSynBind (PatSynBind idL idR)

 data ABExport id
  = ABE id id Wrapper TcSpecPrags

 data PatSynBind idL idR
  = PSB (idL) (PostRn idR NameSet) (PatSynDetails (idR))
        (Pat idR)    (PatSynDir idR)

 -- IPBinds'
 data IPBinds id
  = IPBinds' [IPBind id] TcEvBinds

 data IPBind id
  = IPBind (Either IPName id) (Exp id)

 data Sig id
  = TypeSig [id] (SigWcType id)
  | PatSynSig id (SigType id)
  | ClassOpSig Bool [id] (SigType id)
  | IdSig Id
  | FixSig (FixitySig id)
  | InlineSig id InlinePragma
  | SpecSig id [SigType id] InlinePragma
  | SpecInstSig SourceText (SigType id)
  | MinimalSig SourceText (BooleanFormula id)

 data FixitySig id
  = FixitySig [id] Fixity

 data TcSpecPrags
  = IsDefaultMethod
  | SpecPrags [TcSpecPrag]

 data TcSpecPrag
  = SpecPrag Id Wrapper InlinePragma

 data PatSynDetails a
  = InfixPatSyn a a
  | PrefixPatSyn [a]
  | RecordPatSyn [RecordPatSynField a]

 data RecordPatSynField a
  = RecordPatSynField a a

 data PatSynDir id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup id (Exp id))

 ------------------------------------------------------------------------------
 -- Decls

 data Group id
  = Group (ValBinds id) [SpliceDecl id] [TyClGroup id] [DerivDecl id]
          [FixitySig id] [DefaultDecl id] [ForeignDecl id] [WarnDecls id]
          [AnnDecl id] [RuleDecls id] [VectDecl id] [DocDecl]

 data SpliceExplicitFlag
  = ExplicitSplice
  | ImplicitSplice

 data SpliceDecl id
  = SpliceDecl (Splice id) SpliceExplicitFlag

 data TyClDecl id
  = FamDecl (FamilyDecl id)
  | SynDecl id (QTyVars id) (Type id) (PostRn id NameSet)
  | DataDecl id (QTyVars id) (DataDefn id) (PostRn id Bool)
             (PostRn id NameSet)
  | ClassDecl (Context id) id (QTyVars id)
              [FunDep id] [Sig id] (Binds id)
              [FamilyDecl id] [TyFamDefltEqn id] [DocDecl]
              (PostRn id NameSet)

 data TyClGroup id
  = TyClGroup [TyClDecl id] [RoleAnnotDecl id] [InstDecl id]

 -- KingSig --> KindSig'
 data FamilyResultSig id
  = NoSig
  | KindSig' (Kind id)
  | TyVarSig (TyVarBndr id)

 data FamilyDecl id
  = FamilyDecl (FamilyInfo id) id (QTyVars id)
               (FamilyResultSig id) (Maybe (InjectivityAnn id))

 data InjectivityAnn id
  = InjectivityAnn id [id]

 data FamilyInfo id
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [TyFamInstEqn id])

 data DataDefn id
  = DataDefn NewOrData (Context id) (Maybe CType)
             (Maybe (Kind id)) [ConDecl id] (Deriving id)

 type Deriving id = Maybe [SigType id]

 data NewOrData
  = NewType
  | DataType

 data ConDecl id
  = ConDeclGADT [id] (SigType id) (Maybe DocString)
  | ConDeclH98 id (Maybe (QTyVars id)) (Maybe (Context id))
               (ConDeclDetails id) (Maybe DocString)

 type ConDeclDetails id =
   ConDetails (BangType id) [ConDeclField id]

 type TyPats id = ImplicitBndrs id [Type id]

 type TyFamInstEqn id = TyFamEqn id (TyPats id)

 type TyFamDefltEqn id = TyFamEqn id (QTyVars id)

 data TyFamEqn id pats
  = TyFamEqn id pats (Type id)

 data TyFamInstDecl id
  = TyFamInstDecl (TyFamInstEqn id) (PostRn id NameSet)

 data DataFamInstDecl id
  = DataFamInstDecl id (TyPats id) (DataDefn id)
                    (PostRn id NameSet)

 data ClsInstDecl id
  = ClsInstDecl (SigType id) (Binds id) [Sig id] [TyFamInstDecl id]
                [DataFamInstDecl id] (Maybe OverlapMode)

 data InstDecl id
  = ClsInstD (ClsInstDecl id)
  | DataFamInstD (DataFamInstDecl id)
  | TyFamInstD (TyFamInstDecl id)

 data DerivDecl id
  = DerivDecl (SigType id) (Maybe OverlapMode)

 data DefaultDecl id
  = DefaultDecl [Type id]

 data ForeignDecl id
  = ForeignImport id (SigType id) (PostTc id Coercion)
                  ForeignImport
  | ForeignExport id (SigType id) (PostTc id Coercion)
                  ForeignExport

 data ForeignImport
  = CImport CCallConv Safety (Maybe Header) CImportSpec SourceText

 data CImportSpec
  = CLabel CLabelString
  | CFunction CCallTarget
  | CWrapper

 data ForeignExport
  = CExport CExportSpec SourceText

 data RuleDecls id
  = Rules SourceText [RuleDecl id]

 data RuleDecl id
  = Rule (SourceText, RuleName)
         Activation [RuleBndr id] (Exp id)
         (PostRn id NameSet) (Exp id) (PostRn id NameSet)

 data RuleBndr id
  = RuleBndr id
  | RuleBndrSig id (SigWcType id)

 data VectDecl id
  = Vect SourceText id (Exp id)
  | NoVect SourceText id
  | VectTypeIn SourceText Bool id (Maybe id)
  | VectTypeOut Bool TyCon (Maybe TyCon)
  | VectClassIn SourceText id
  | VectClassOut Class
  | VectInstIn (SigType id)
  | VectInstOut ClsInst

 data DocDecl
  = DocCommentNext DocString
  | DocCommentPrev DocString
  | DocCommentNamed String DocString
  | DocGroup Int DocString

 data WarnDecls id
  = Warnings SourceText [WarnDecl id]

 data WarnDecl id
  = Warning [id] WarningTxt

 data AnnDecl id
  = Annotation SourceText (AnnProvenance id) (Exp id)

 data AnnProvenance id
  = ValueAnnProvenance id
  | TypeAnnProvenance id
  | ModuleAnnProvenance

 data RoleAnnotDecl id
  = RoleAnnotDecl id [Maybe Role]

 -------------------------------------------------------------------------------
 -- Types

 type BangType id = Type id

 type Context id = [Type id]

 type Kind id = Type id

 data QTyVars id
  = QTvs (PostRn id [Name]) [TyVarBndr id] (PostRn id NameSet)

 data ImplicitBndrs id thing
  = IB (PostRn id [Name]) thing

 data WildCardBndrs id thing
  = WC (PostRn id [Name]) (Maybe SrcSpan) thing

 type SigType id = ImplicitBndrs id (Type id)

 type WcType id = WildCardBndrs id (Type id)

 type SigWcType id = ImplicitBndrs id (WcType id)

 newtype IPName = IPName FastString

 data TyVarBndr id
  = UserTyVar id
  | KindedTyVar id (Kind id)

 data TyLit
  = NumTy SourceText Integer
  | StrTy SourceText FastString

 newtype WildCardInfo id
  = AnonWildCard (PostRn id Name)

 data AppType id
  = AppInfix id
  | AppPrefix (Type id)

 data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple

 data ConDeclField id
  = ConDeclField [FieldOcc id] (BangType id) (Maybe DocString)

 data ConDetails arg rec
  = PrefixCon [arg]
  | RecCon rec
  | InfixCon arg arg

 data FieldOcc id
  = FieldOcc RdrName (PostRn id id)

 data AmbiguousFieldOcc id
  = Unambiguous RdrName (PostRn id id)
  | Ambiguous RdrName (PostTc id id)

 |]
