{-# LANGUAGE ExistentialQuantification #-}
module SyntaxMinimal where

data Exp id
  = Var      (Located id)
  | UnboundVar UnboundVar
--  HSE.Con (missing?)

  | Lit      Lit
  | OverLit  (OverLit id)

  | App      (LExp id) (LExp id)

  | SectionL (LExp id) (LExp id)

  | SectionR (LExp id) (LExp id)

  | NegApp   (LExp id) (SyntaxExp id)

  | OpApp    (LExp id) (LExp id) (LExp id)

  | AppType    (LExp id) (LWcType id)
  | AppTypeOut (LExp id) (LWcType Name)

  | Par      (LExp id)

  | If       (Maybe (SyntaxExp id)) (LExp id) (LExp id) (LExp id)

  | MultiIf  [LGRHS id (LExp id)]

  | Case     (LExp id) (MatchGroup id (LExp id))

  | Lam      (MatchGroup id (LExp id))

  | LamCase  (MatchGroup id (LExp id))

  | Let      (LLocalBinds id) (LExp  id)

  | IPVar    IPName

  | RecFld    (AmbiguousFieldOcc id)

  | OverLabel FastString

  | RecordCon (Located id) (RecordBinds id)

  | RecordUpd (LExp id) [LRecUpdField id]

  | ExplicitTuple [LTupArg id] Boxity
--  HSE.TupleSection (part of above?)

  | ExplicitList (Maybe (SyntaxExp id)) [LExp id]

  | ExplicitPArr [LExp id]

  | ArithSeq  (Maybe (SyntaxExp id)) (ArithSeqInfo id)
--  ArithSeqInfo.From
--  ArithSeqInfo.FromTo
--  ArithSeqInfo.FromThen
--  ArithSeqInfo.FromThenTo

  | PArrSeq  (ArithSeqInfo id) -- expanding ArithSeqInfo below
--  ArithSeqInfo.From
--  ArithSeqInfo.FromTo
--  ArithSeqInfo.FromThen
--  ArithSeqInfo.FromThenTo

  | Do           (StmtContext Name) (LExpLStmts id)
--  StmtContext.ListComp
--  StmtContext.MonadComp
--  StmtContext.ParArrComp
--  StmtContext.DoExp
--  StmtContext.MDoExp
--  StmtContext.ArrowExp
--  StmtContext.GhciStmtCtxt
--  StmtContext.PatGuard
--  StmtContext.ParStmtCtxt
--  StmtContext.TransStmtCtxt

  | Bracket      (Bracket id)
  | RnBracketOut (Bracket Name) [PendingRnSplice]
  | TcBracketOut (Bracket Name) [PendingTcSplice]
--  HSE.QuasiQuote (missing?)

--  HSE.VarQuote (missing?)
--  HSE.TypQuote (missing?)

  | SpliceE  (Splice id)

  | ExpWithTySig (LExp id) (LSigWcType id)
  | ExpWithTySigOut (LExp id) (LSigWcType Name)

  | CoreAnn   SourceText StringLiteral (LExp id)
  | SCC       SourceText StringLiteral (LExp id)
  | TickPragma SourceText (StringLiteral,(Int,Int),(Int,Int))
               ((SourceText,SourceText),(SourceText,SourceText))
               (LExp id) -- (is it GenPragma?)

  | Proc      (LPat id) (LCmdTop id)

  | ArrApp    (LExp id) (LExp id) ArrAppType Bool
--  HSE.LeftArrHighApp (don't know how they compare)
  | ArrForm   (LExp id) (Maybe Fixity) [LCmdTop id]
--  HSE.RightArrHighApp (don't know how they compare)

  | EWildPat -- (right comparison?)

  | Static    (LExp id)

  | Tick      (Tickish id) (LExp id)

  | BinTick   Int Int (LExp id)

  | EAsPat    (Located id) (LExp id)

  | EViewPat  (LExp id) (LExp id)

  | ELazyPat  (LExp id)

  | Wrap      Wrapper (Exp id)

data Pat id
  = WildPat

  | VarPat    (Located id)

  | LitPat    Lit
  | NPat      (LOverLit id) (Maybe (SyntaxExp id))  (SyntaxExp id)

  | NPlusKPat (Located id) (LOverLit id) (OverLit id) (SyntaxExp id)
              (SyntaxExp id)

  | TuplePat  [LPat id] Boxity

  | ListPat   [LPat id]
              (Maybe (SyntaxExp id))

  | ParPat    (LPat id)

  | AsPat     (Located id) (LPat id)

  | ViewPat   (LExp id) (LPat id)

  | BangPat   (LPat id)

  | LazyPat   (LPat id)

  | SigPatIn  (LPat id) (LSigWcType id)
  | SigPatOut (LPat id) TCRType

  | PArrPat   [LPat id]

  | ConPatIn  (Located id) (ConPatDetails id)
  | ConPatOut LConLike [TCRType] [TyVar] [EvVar] TcEvBinds (ConPatDetails id)
              Wrapper
--  PRec
--  PInfixApp

  | SplicePat (Splice id)

  | CoPat     Wrapper (Pat id) TCRType

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

data Type id
  = ForAllTy [LTyVarBndr id] (LType id)

  | FunTy (LType id) (LType id)

  | TupleTy TupleSort [LType id]

  | ListTy (LType id)

  | PArrTy (LType id)

  | AppTy (LType id) (LType id)

  | TyVar (Located id)

 -- HSE.TyCon (part of above)

  | ParTy (LType id)

  | KindSig (LType id) (LKind id)

  | BangTy SrcBang (LType id)

  | WildCardTy (WildCardInfo id)

  | EqTy (LType id) (LType id)

  | SpliceTy (Splice id)

  | OpTy (LType id) (Located id) (LType id) -- match?

  | TyLit TyLit
--  TyLit.Num
--  TyLit.String
  | AppsTy [LAppType id] -- assuming it is promoted constructors
  | ExplicitListTy  [LType id]
  | ExplicitTupleTy [LType id]
--  HSE.Promoted.PromotedUnit (ExplicitTupleTy [] []?)

--  HSE.TyQuasiQuote (missing?)

  | QualTy (LContext id) (LType id)

  | IParamTy IPName (LType id)

  | DocTy (LType id) LDocString

  | RecTy [LConDeclField id]

  | CoreTy TCRType

------------ the rest is not compared -------------


----------------------------------------------------------------------------
-- Located versions

type LExp id               = Located (Exp id)
type LTupArg id            = Located (TupArg id)
type LCmd id               = Located (Cmd id)
type LCmdTop id            = Located (CmdTop id)
type LMatch id body        = Located (Match id body)
type LDecl id              = Located (Decl id)
type LBangType id        = Located (BangType id)
type LContext id         = Located (Context id)
type LType id            = Located (Type id)
type LKind id            = Located (Kind id)
type LConDeclField id    = Located (ConDeclField id)
type LTyVarBndr id       = Located (TyVarBndr id)
type LAppType id         = Located (AppType id)
type LFieldOcc id        = Located (FieldOcc id)
type LGRHS id body         = Located (GRHS id body)
type LStmt id body         = Located (StmtLR id id body)
type LStmtLR idL idR body  = Located (StmtLR idL idR body)
type LPat id               = Located (Pat id)
type LRecField' id arg     = Located (RecField' id arg)
type LRecField id arg      = Located (RecField id arg)
type LRecUpdField id       = Located (RecUpdField id)
type LDocString            = Located DocString
type LImportDecl      id = Located (ImportDecl id)
type LIE id              = Located (IE id)
type LBindLR       idL idR = Located (BindLR idL idR)
type LIPBind            id = Located (IPBind id)
type LSig             id = Located (Sig id)
type LFixitySig       id = Located (FixitySig id)
type LTcSpecPrag           = Located TcSpecPrag
type LSpliceDecl      id = Located (SpliceDecl id)
type LTyClDecl        id = Located (TyClDecl id)
type LFamilyResultSig id = Located (FamilyResultSig id)
type LFamilyDecl      id = Located (FamilyDecl id)
type LInjectivityAnn  id = Located (InjectivityAnn id)
type LConDecl         id = Located (ConDecl id)
type LTyFamInstEqn    id = Located (TyFamInstEqn id)
type LTyFamDefltEqn   id = Located (TyFamDefltEqn id)
type LTyFamInstDecl   id = Located (TyFamInstDecl id)
type LDataFamInstDecl id = Located (DataFamInstDecl id)
type LClsInstDecl     id = Located (ClsInstDecl id)
type LInstDecl        id = Located (InstDecl id)
type LDerivDecl       id = Located (DerivDecl id)
type LDefaultDecl     id = Located (DefaultDecl id)
type LForeignDecl     id = Located (ForeignDecl id)
type LRuleDecls       id = Located (RuleDecls id)
type LRuleDecl        id = Located (RuleDecl id)
type LRuleBndr        id = Located (RuleBndr id)
type LVectDecl        id = Located (VectDecl id)
type LDocDecl              = Located (DocDecl)
type LWarnDecls       id = Located (WarnDecls id)
type LWarnDecl        id = Located (WarnDecl id)
type LAnnDecl         id = Located (AnnDecl id)
type LRoleAnnotDecl   id = Located (RoleAnnotDecl id)
type LModuleName           = Located ModuleName
type LIPName               = Located IPName
type LSplice id            = Located (Splice id)
type LCType                = Located CType
type LOverlapMode          = Located OverlapMode
type LCCallConv            = Located CCallConv
type LSafety               = Located Safety
type LSourceText           = Located SourceText
type LCExportSpec          = Located CExportSpec
type LOverLit id           = Located (OverLit id)
type LConLike              = Located ConLike
type LExpLStmts id         = Located [ExpLStmt id]
type LCmdLStmts id         = Located [CmdLStmt id]
type LLMatchs id body      = Located [LMatch id body]
type LLIEs id            = Located [LIE id]
type LFieldLbl id        = Located (FieldLbl id)
type LLSigTypes id       = Located [LSigType id]
type LLConDeclFields id  = Located [LConDeclField id]
type LRdrName              = Located RdrName
type LName                 = Located Name
type LFunDepL id         = Located (FunDep (Located id))
type LSrcTextRuleName      = Located (SourceText, RuleName)
type LMRole                = Located (Maybe Role)
type LLocalBinds id        = Located (LocalBinds id)
type LLocalBindsLR idL idR = Located (LocalBindsLR idL idR)

--------------------------------------------------------

data TCRType
type TCRKind = TCRType
data FractionalLit
data Located a
data Id
data Name
data OccName
data Wrapper
data GlobalRdrEnv
data ByteString
data FastString
data Fixity
data Boxity
data ConLike
data SourceText
data StringLiteral
data Tickish a
data Origin
data SrcSpan
class OutputableBndr a
data TyVar
data EvVar
data TcEvBinds
data ModuleName
data FieldLbl a
data RecFlag
data Bag a
data NameSet
data InlinePragma
data LBooleanFormula a
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
  = OverLit' OverLitVal (Exp id)

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

data SyntaxExp id
  = SyntaxExp (Exp id) [Wrapper] Wrapper

type CmdSyntaxTable id = [(Name, Exp id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

data TupArg id
  = Present (LExp id)
  | Missing

data LWcTypeX
  = forall id. OutputableBndr id => LWcTypeX (LWcType id)

data Cmd id
  = CmdArrApp  (LExp id) (LExp id) ArrAppType Bool
  | CmdArrForm (LExp id) (Maybe Fixity) [LCmdTop id]
  | CmdApp     (LCmd id)  (LExp id)
  | CmdLam     (MatchGroup id (LCmd id))
  | CmdPar     (LCmd id)
  | CmdCase    (LExp id) (MatchGroup id (LCmd id))
  | CmdIf      (Maybe (SyntaxExp id)) (LExp id) (LCmd id) (LCmd id)
  | CmdLet     (LLocalBinds id) (LCmd  id)
  | CmdDo      (LCmdLStmts id)
  | CmdWrap    Wrapper (Cmd id)

data ArrAppType
  = HigherOrderApp
  | FirstOrderApp

data CmdTop id
  = CmdTop (LCmd id)
           (CmdSyntaxTable id)

type RecordBinds id = RecFields id (LExp id)

data MatchGroup id body
  = MG (LLMatchs id body) Origin

data Match id body
  = Match (MatchFixity id) [LPat id] (Maybe (LType id)) (GRHSs id body)

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id) Bool

data GRHSs id body
  = GRHSs [LGRHS id body] (LLocalBinds id)

data GRHS id body
  = GRHS [GuardLStmt id] body

type Stmt id body  = StmtLR id id body
type CmdLStmt   id = LStmt id (LCmd  id)
type CmdStmt    id = Stmt  id (LCmd  id)
type ExpLStmt   id = LStmt id (LExp id)
type ExpStmt    id = Stmt  id (LExp id)
type GuardLStmt id = LStmt id (LExp id)
type GuardStmt  id = Stmt  id (LExp id)
type GhciLStmt  id = LStmt id (LExp id)
type GhciStmt   id = Stmt  id (LExp id)

data StmtLR idL idR body
  = LastStmt body Bool (SyntaxExp idR)
  | BindStmt (LPat idL) body (SyntaxExp idR) (SyntaxExp idR)
  | ApplicativeStmt [(SyntaxExp idR, ApplicativeArg idL idR)]
                    (Maybe (SyntaxExp idR))
  | BodyStmt body (SyntaxExp idR) (SyntaxExp idR)
  | LetStmt  (LLocalBindsLR idL idR)
  | ParStmt  [ParStmtBlock idL idR] (Exp idR) (SyntaxExp idR)

  | TransStmt TransForm [ExpLStmt idL] [(idR, idR)] (LExp idR)
              (Maybe (LExp idR)) (SyntaxExp idR) (SyntaxExp idR)  (Exp idR)
  | RecStmt [LStmtLR idL idR body] [idR] [idR] (SyntaxExp idR)
            (SyntaxExp idR) (SyntaxExp idR)

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock idL idR
  = ParStmtBlock [ExpLStmt idL] [idR] (SyntaxExp idR)

data ApplicativeArg idL idR
  = ApplicativeArgOne (LPat idL) (LExp idL)
  | ApplicativeArgMany [ExpLStmt idL] (Exp idL) (LPat idL)

data Splice id
  = TypedSplice id (LExp id)
  | UntypedSplice id (LExp id)
  | QuasiQuote id id SrcSpan FastString

type SplicePointName = Name

data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LExp Name)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data PendingTcSplice
  = PendingTcSplice SplicePointName (LExp Id)

data Bracket id
  = ExpBr (LExp id)
  | PatBr (LPat id)
  | DecBrL [LDecl id]
  | DecBrG (Group id)
  | TypBr (LType id)
  | VarBr Bool id
  | TExpBr (LExp id)

data ArithSeqInfo id
  = From            (LExp id)
  | FromThen        (LExp id) (LExp id)
  | FromTo          (LExp id) (LExp id)
  | FromThenTo      (LExp id) (LExp id) (LExp id)

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

type InPat id = LPat id

type OutPat id = LPat id

type ConPatDetails id = ConDetails (LPat id) (RecFields id (LPat id))

data RecFields id arg
  = RecFields [LRecField id arg] (Maybe Int)

type RecField id arg = RecField' (FieldOcc id) arg

type RecUpdField id = RecField' (AmbiguousFieldOcc id) (LExp id)

data RecField' id arg
  = RecField (Located id) arg Bool

--------------------------------------------------------------------------------
-- Doc

newtype DocString = DocString FastString

--------------------------------------------------------------------------------
-- ImpExp

data ImportDecl id
  = ImportDecl (Maybe SourceText) LModuleName
               (Maybe StringLiteral) Bool Bool Bool Bool
               (Maybe ModuleName) (Maybe (Bool, LLIEs id))

data IE id
  = IEVar (Located id)
  | IEThingAbs (Located id)
  | IEThingAll (Located id)
  | IEThingWith (Located id) IEWildcard [Located id] [LFieldLbl id]
  | IEModuleContents LModuleName
  | IEGroup Int DocString
  | IEDoc DocString
  | IEDocNamed String

data IEWildcard
  = NoIEWildcard
  | IEWildcard Int

-------------------------------------------------------------------------------
-- Binds

type LBind id              = LBindLR id id
type LBinds        id      = LBindsLR id id
type LBindsLR      idL idR = Bag (LBindLR idL idR)

type LocalBinds id = LocalBindsLR id id

data LocalBindsLR idL idR
  = ValBinds (ValBindsLR idL idR)
  | IPBinds (IPBinds idR)
  | EmptyLocalBinds

type ValBinds id = ValBindsLR id id

data ValBindsLR idL idR
  = ValBindsIn (LBindsLR idL idR) [LSig idR]
  | ValBindsOut [(RecFlag, LBinds idL)] [LSig Name]

type Bind id = BindLR id id


data BindLR idL idR
  = FunBind (Located idL) (MatchGroup idR (LExp idR)) Wrapper
             [Tickish Id]
  | PatBind (LPat idL) (GRHSs idR (LExp idR))
            ([Tickish Id], [[Tickish Id]])
  | VarBind idL (LExp idR) Bool
  | AbsBinds [TyVar] [EvVar] [ABExport idL] [TcEvBinds] (LBinds idL)
  | AbsBindsSig [TyVar] [EvVar] idL TcSpecPrags TcEvBinds (LBind idL)
  | PatSynBind (PatSynBind idL idR)

data ABExport id
  = ABE id id Wrapper TcSpecPrags

data PatSynBind idL idR
  = PSB (Located idL) (PatSynDetails (Located idR))
        (LPat idR)    (PatSynDir idR)

-- IPBinds'
data IPBinds id
  = IPBinds' [LIPBind id] TcEvBinds

data IPBind id
  = IPBind (Either LIPName id) (LExp id)

data Sig id
  = TypeSig [Located id] (LSigWcType id)
  | PatSynSig (Located id) (LSigType id)
  | ClassOpSig Bool [Located id] (LSigType id)
  | IdSig Id
  | FixSig (FixitySig id)
  | InlineSig (Located id) InlinePragma
  | SpecSig (Located id) [LSigType id] InlinePragma
  | SpecInstSig SourceText (LSigType id)
  | MinimalSig SourceText (LBooleanFormula (Located id))

data FixitySig id
  = FixitySig [Located id] Fixity

data TcSpecPrags
  = IsDefaultMethod
  | SpecPrags [LTcSpecPrag]

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
  | ExplicitBidirectional (MatchGroup id (LExp id))

------------------------------------------------------------------------------
-- Decls

data Group id
  = Group (ValBinds id) [LSpliceDecl id] [TyClGroup id] [LDerivDecl id]
          [LFixitySig id] [LDefaultDecl id] [LForeignDecl id] [LWarnDecls id]
          [LAnnDecl id] [LRuleDecls id] [LVectDecl id] [LDocDecl]

data SpliceExplicitFlag
  = ExplicitSplice
  | ImplicitSplice

data SpliceDecl id
  = SpliceDecl (LSplice id) SpliceExplicitFlag

data TyClDecl id
  = FamDecl (FamilyDecl id)
  | SynDecl (Located id) (LQTyVars id) (LType id)
  | DataDecl (Located id) (LQTyVars id) (DataDefn id)
  | ClassDecl (LContext id) (Located id) (LQTyVars id)
              [LFunDepL id] [LSig id] (LBinds id)
              [LFamilyDecl id] [LTyFamDefltEqn id] [LDocDecl]


data TyClGroup id
  = TyClGroup [LTyClDecl id] [LRoleAnnotDecl id] [LInstDecl id]

-- KingSig --> KindSig'
data FamilyResultSig id
  = NoSig
  | KindSig' (LKind id)
  | TyVarSig (LTyVarBndr id)

data FamilyDecl id
  = FamilyDecl (FamilyInfo id) (Located id) (LQTyVars id)
               (LFamilyResultSig id) (Maybe (LInjectivityAnn id))

data InjectivityAnn id
  = InjectivityAnn (Located id) [Located id]

data FamilyInfo id
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [LTyFamInstEqn id])

data DataDefn id
  = DataDefn NewOrData (LContext id) (Maybe LCType)
             (Maybe (LKind id)) [LConDecl id] (Deriving id)

type Deriving id = Maybe (LLSigTypes id)

data NewOrData
  = NewType
  | DataType

data ConDecl id
  = ConDeclGADT [Located id] (LSigType id) (Maybe LDocString)
  | ConDeclH98 (Located id) (Maybe (LQTyVars id)) (Maybe (LContext id))
               (ConDeclDetails id) (Maybe LDocString)

type ConDeclDetails id =
   ConDetails (LBangType id) (LLConDeclFields id)

type TyPats id = ImplicitBndrs id [LType id]

type TyFamInstEqn id = TyFamEqn id (TyPats id)

type TyFamDefltEqn id = TyFamEqn id (LQTyVars id)

data TyFamEqn id pats
  = TyFamEqn (Located id) pats (LType id)

data TyFamInstDecl id
  = TyFamInstDecl (LTyFamInstEqn id)

data DataFamInstDecl id
  = DataFamInstDecl (Located id) (TyPats id) (DataDefn id)

data ClsInstDecl id
  = ClsInstDecl (LSigType id) (LBinds id) [LSig id] [LTyFamInstDecl id]
                [LDataFamInstDecl id] (Maybe LOverlapMode)

data InstDecl id
  = ClsInstD (ClsInstDecl id)
  | DataFamInstD (DataFamInstDecl id)
  | TyFamInstD (TyFamInstDecl id)

data DerivDecl id
  = DerivDecl (LSigType id) (Maybe LOverlapMode)

data DefaultDecl id
  = DefaultDecl [LType id]

data ForeignDecl id
  = ForeignImport (Located id) (LSigType id) ForeignImport
  | ForeignExport (Located id) (LSigType id) ForeignExport

data ForeignImport
  = CImport LCCallConv LSafety (Maybe Header) CImportSpec LSourceText

data CImportSpec
  = CLabel CLabelString
  | CFunction CCallTarget
  | CWrapper

data ForeignExport
  = CExport LCExportSpec LSourceText

data RuleDecls id
  = Rules SourceText [LRuleDecl id]

data RuleDecl id
  = Rule LSrcTextRuleName
         Activation [LRuleBndr id] (LExp id)
         (LExp id)

data RuleBndr id
  = RuleBndr (Located id)
  | RuleBndrSig (Located id) (LSigWcType id)

data VectDecl id
  = Vect SourceText (Located id) (LExp id)
  | NoVect SourceText (Located id)
  | VectTypeIn SourceText Bool (Located id) (Maybe (Located id))
  | VectTypeOut Bool TyCon (Maybe TyCon)
  | VectClassIn SourceText (Located id)
  | VectClassOut Class
  | VectInstIn (LSigType id)
  | VectInstOut ClsInst

data DocDecl
  = DocCommentNext DocString
  | DocCommentPrev DocString
  | DocCommentNamed String DocString
  | DocGroup Int DocString

data WarnDecls id
  = Warnings SourceText [LWarnDecl id]

data WarnDecl id
  = Warning [Located id] WarningTxt

data AnnDecl id
  = Annotation SourceText (AnnProvenance id) (LExp id)

data AnnProvenance id
  = ValueAnnProvenance (Located id)
  | TypeAnnProvenance (Located id)
  | ModuleAnnProvenance

data RoleAnnotDecl id
  = RoleAnnotDecl (Located id) [LMRole]

-------------------------------------------------------------------------------
-- Types

type BangType id = Type id

type Context id = [LType id]

type Kind id = Type id

data LQTyVars id
  = QTvs [LTyVarBndr id]

data ImplicitBndrs id thing
  = IB thing

data WildCardBndrs id thing
  = WC (Maybe SrcSpan) thing

type LSigType id = ImplicitBndrs id (LType id)

type LWcType id = WildCardBndrs id (LType id)

type LSigWcType id = ImplicitBndrs id (LWcType id)

newtype IPName = IPName FastString

data TyVarBndr id
  = UserTyVar (Located id)
  | KindedTyVar (Located id) (LKind id)

data TyLit
  = NumTy SourceText Integer
  | StrTy SourceText FastString

data WildCardInfo id
  = AnonWildCard

data AppType id
  = AppInfix (Located id)
  | AppPrefix (LType id)

data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple

data ConDeclField id
  = ConDeclField [LFieldOcc id] (LBangType id) (Maybe LDocString)

data ConDetails arg rec
  = PrefixCon [arg]
  | RecCon rec
  | InfixCon arg arg

data FieldOcc id
  = FieldOcc LRdrName

data AmbiguousFieldOcc id
  = Unambiguous LRdrName
  | Ambiguous LRdrName
