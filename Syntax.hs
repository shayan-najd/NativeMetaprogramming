{-# LANGUAGE ExistentialQuantification #-}
module Syntax where

data Exp id
  = Var      (Located id)
  | UnboundVar UnboundVar
-- Con

  | Lit      Lit
  | OverLit  (OverLit id)

  | App      (LExp id) (LExp id)

  | SectionL (LExp id) (LExp id)

  | SectionR (LExp id) (LExp id)

  | NegApp   (LExp id) (SyntaxExp id)

  | OpApp    (LExp id) (LExp id) (PostRn id Fixity) (LExp id)

  | AppType    (LExp id) (LWcType id)
  | AppTypeOut (LExp id) (LWcType Name)

  | Par      (LExp id)

  | If       (Maybe (SyntaxExp id)) (LExp id) (LExp id) (LExp id)

  | MultiIf  (PostTc id TCRType) [LGRHS id (LExp id)]

  | Case     (LExp id) (MatchGroup id (LExp id))

  | Lam      (MatchGroup id (LExp id))

  | LamCase  (MatchGroup id (LExp id))

  | Let      (LLocalBinds id) (LExp  id)

  | IPVar    IPName

  | RecFld    (AmbiguousFieldOcc id)

  | OverLabel FastString

  | RecordCon (Located id) (PostTc id ConLike) PostTcExp (RecordBinds id)

  | RecordUpd (LExp id) [LRecUpdField id] (PostTc id [ConLike])
              (PostTc id [TCRType]) (PostTc id [TCRType]) (PostTc id Wrapper)

  | ExplicitTuple [LTupArg id] Boxity
--  TupleSection

  | ExplicitList (PostTc id TCRType) (Maybe (SyntaxExp id)) [LExp id]

  | ExplicitPArr (PostTc id TCRType) [LExp id]

  | ArithSeq  PostTcExp (Maybe (SyntaxExp id)) (ArithSeqInfo id)
--  EnumFromTo      (part of ArithSeqInfo)
--  EnumFromThen    (part of ArithSeqInfo)
--  EnumFromThenTo  (part of ArithSeqInfo)

  | PArrSeq   PostTcExp (ArithSeqInfo id)
--  ParArrayFromTo      (part of ArithSeqInfo)
--  ParArrayFromThen    (part of ArithSeqInfo)
--  ParArrayFromThenTo  (part of ArithSeqInfo)

-- ListComp      (part of StmtContext)
-- MonadComp     (part of StmtContext)
-- ParArrayComp  (part of StmtContext)
  | Do           (StmtContext Name) (LExpLStmts id) (PostTc id TCRType)
-- MDo           (part of StmtContext)
-- ArrowExp      (part of StmtContext)
-- GhciStmtCtxt  (part of StmtContext)
-- PatGuard      (part of StmtContext)
-- ParComp       (part of StmtContext)
-- TransStmtCtxt (part of StmtContext)

  | Bracket      (Bracket id)
  | RnBracketOut (Bracket Name) [PendingRnSplice]
  | TcBracketOut (Bracket Name) [PendingTcSplice]
--  QuasiQuote

--  VarQuote
--  TypQuote

  | SpliceE  (Splice id)

  | ExpWithTySig (LExp id) (LSigWcType id)
  | ExpWithTySigOut (LExp id) (LSigWcType Name)

  | CoreAnn   SourceText StringLiteral (LExp id)
  | SCC       SourceText StringLiteral (LExp id)
  | TickPragma SourceText (StringLiteral,(Int,Int),(Int,Int))
               ((SourceText,SourceText),(SourceText,SourceText))
               (LExp id) -- (is it GenPragma?)

  | Proc      (LPat id) (LCmdTop id)

  | ArrApp    (LExp id) (LExp id) (PostTc id TCRType) ArrAppType Bool
--  LeftArrHighApp (don't know how they compare)
  | ArrForm   (LExp id) (Maybe Fixity) [LCmdTop id]
--  RightArrHighApp (don't know how they compare)

  | EWildPat -- (right comparison?)

  | Static    (LExp id)

  | Tick      (Tickish id) (LExp id)

  | BinTick   Int Int (LExp id)

  | EAsPat    (Located id) (LExp id)

  | EViewPat  (LExp id) (LExp id)

  | ELazyPat  (LExp id)

  | Wrap      Wrapper (Exp id)

data Pat id
  = WildPat   (PostTc id TCRType)

  | VarPat    (Located id)

  | LitPat    Lit
  | NPat      (LOverLit id)
              (Maybe (SyntaxExp id))  (SyntaxExp id) (PostTc id TCRType)

  | NPlusKPat (Located id) (LOverLit id) (OverLit id) (SyntaxExp id)
              (SyntaxExp id) (PostTc id TCRType)

  | TuplePat  [LPat id] Boxity [PostTc id TCRType]

  | ListPat   [LPat id] (PostTc id TCRType)
              (Maybe (PostTc id TCRType, SyntaxExp id))

  | ParPat    (LPat id)

  | AsPat     (Located id) (LPat id)

  | ViewPat   (LExp id) (LPat id) (PostTc id TCRType)

  | BangPat   (LPat id)

  | LazyPat   (LPat id)

  | SigPatIn  (LPat id) (LSigWcType id)
  | SigPatOut (LPat id) TCRType

  | PArrPat   [LPat id] (PostTc id TCRType)

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


------------ the rest is not compared -------------


----------------------------------------------------------------------------
-- Located versions

type LExp id               = Located (Exp id)
type LTupArg id            = Located (TupArg id)
type LCmd id               = Located (Cmd id)
type LCmdTop id            = Located (CmdTop id)
type LMatch id body        = Located (Match id body)
type LDecl id              = Located (Decl id)
type LBangType name        = Located (BangType name)
type LContext name         = Located (Context name)
type LType name            = Located (Type name)
type LKind name            = Located (Kind name)
type LConDeclField name    = Located (ConDeclField name)
type LTyVarBndr name       = Located (TyVarBndr name)
type LAppType name         = Located (AppType name)
type LFieldOcc name        = Located (FieldOcc name)
type LGRHS id body         = Located (GRHS id body)
type LStmt id body         = Located (StmtLR id id body)
type LStmtLR idL idR body  = Located (StmtLR idL idR body)
type LPat id               = Located (Pat id)
type LRecField' id arg     = Located (RecField' id arg)
type LRecField id arg      = Located (RecField id arg)
type LRecUpdField id       = Located (RecUpdField id)
type LDocString            = Located DocString
type LImportDecl      name = Located (ImportDecl name)
type LIE name              = Located (IE name)
type LBindLR       idL idR = Located (BindLR idL idR)
type LIPBind            id = Located (IPBind id)
type LSig             name = Located (Sig name)
type LFixitySig       name = Located (FixitySig name)
type LTcSpecPrag           = Located TcSpecPrag
type LSpliceDecl      name = Located (SpliceDecl name)
type LTyClDecl        name = Located (TyClDecl name)
type LFamilyResultSig name = Located (FamilyResultSig name)
type LFamilyDecl      name = Located (FamilyDecl name)
type LInjectivityAnn  name = Located (InjectivityAnn name)
type LConDecl         name = Located (ConDecl name)
type LTyFamInstEqn    name = Located (TyFamInstEqn name)
type LTyFamDefltEqn   name = Located (TyFamDefltEqn name)
type LTyFamInstDecl   name = Located (TyFamInstDecl name)
type LDataFamInstDecl name = Located (DataFamInstDecl name)
type LClsInstDecl     name = Located (ClsInstDecl name)
type LInstDecl        name = Located (InstDecl name)
type LDerivDecl       name = Located (DerivDecl name)
type LDefaultDecl     name = Located (DefaultDecl name)
type LForeignDecl     name = Located (ForeignDecl name)
type LRuleDecls       name = Located (RuleDecls name)
type LRuleDecl        name = Located (RuleDecl name)
type LRuleBndr        name = Located (RuleBndr name)
type LVectDecl        name = Located (VectDecl name)
type LDocDecl              = Located (DocDecl)
type LWarnDecls       name = Located (WarnDecls name)
type LWarnDecl        name = Located (WarnDecl name)
type LAnnDecl         name = Located (AnnDecl name)
type LRoleAnnotDecl   name = Located (RoleAnnotDecl name)
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
type LLIEs name            = Located [LIE name]
type LFieldLbl name        = Located (FieldLbl name)
type LLSigTypes name       = Located [LSigType name]
type LLConDeclFields name  = Located [LConDeclField name]
type LRdrName              = Located RdrName
type LName                 = Located Name
type LFunDepL name         = Located (FunDep (Located name))
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
  = OverLit' OverLitVal (PostRn id Bool) (Exp id) (PostTc id TCRType)

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

type PostTcExp   = Exp Id

type PostTcTable = [(Name, PostTcExp)]

data SyntaxExp id
  = SyntaxExp (Exp id) [Wrapper] Wrapper

type CmdSyntaxTable id = [(Name, Exp id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

data TupArg id
  = Present (LExp id)
  | Missing (PostTc id TCRType)

data LWcTypeX
  = forall id. OutputableBndr id => LWcTypeX (LWcType id)

data Cmd id
  = CmdArrApp  (LExp id) (LExp id) (PostTc id TCRType) ArrAppType Bool
  | CmdArrForm (LExp id) (Maybe Fixity) [LCmdTop id]
  | CmdApp     (LCmd id)  (LExp id)
  | CmdLam     (MatchGroup id (LCmd id))
  | CmdPar     (LCmd id)
  | CmdCase    (LExp id) (MatchGroup id (LCmd id))
  | CmdIf      (Maybe (SyntaxExp id)) (LExp id) (LCmd id) (LCmd id)
  | CmdLet     (LLocalBinds id) (LCmd  id)
  | CmdDo      (LCmdLStmts id) (PostTc id TCRType)
  | CmdWrap    Wrapper (Cmd id)

data ArrAppType
  = HigherOrderApp
  | FirstOrderApp

data CmdTop id
  = CmdTop (LCmd id) (PostTc id TCRType) (PostTc id TCRType)
           (CmdSyntaxTable id)

type RecordBinds id = RecFields id (LExp id)

data MatchGroup id body
  = MG (LLMatchs id body) [PostTc id TCRType] (PostTc id TCRType) Origin

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
             (PostTc idR TCRType)
  | ApplicativeStmt [(SyntaxExp idR, ApplicativeArg idL idR)]
                    (Maybe (SyntaxExp idR)) (PostTc idR TCRType)
  | BodyStmt body (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
  | LetStmt  (LLocalBindsLR idL idR)
  | ParStmt  [ParStmtBlock idL idR] (Exp idR) (SyntaxExp idR)
             (PostTc idR TCRType)
  | TransStmt TransForm [ExpLStmt idL] [(idR, idR)] (LExp idR)
              (Maybe (LExp idR))
              (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType) (Exp idR)
  | RecStmt [LStmtLR idL idR body] [idR] [idR] (SyntaxExp idR)
            (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
            [PostTcExp] [PostTcExp] (PostTc idR TCRType)

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

data ImportDecl name
  = ImportDecl (Maybe SourceText) LModuleName
               (Maybe StringLiteral) Bool Bool Bool Bool
               (Maybe ModuleName) (Maybe (Bool, LLIEs name))

data IE name
  = IEVar (Located name)
  | IEThingAbs (Located name)
  | IEThingAll (Located name)
  | IEThingWith (Located name) IEWildcard [Located name] [LFieldLbl name]
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
            (PostRn idL NameSet) [Tickish Id]
  | PatBind (LPat idL) (GRHSs idR (LExp idR)) (PostTc idR TCRType)
            (PostRn idL NameSet) ([Tickish Id], [[Tickish Id]])
  | VarBind idL (LExp idR) Bool
  | AbsBinds [TyVar] [EvVar] [ABExport idL] [TcEvBinds] (LBinds idL)
  | AbsBindsSig [TyVar] [EvVar] idL TcSpecPrags TcEvBinds (LBind idL)
  | PatSynBind (PatSynBind idL idR)

data ABExport id
  = ABE id id Wrapper TcSpecPrags

data PatSynBind idL idR
  = PSB (Located idL) (PostRn idR NameSet) (PatSynDetails (Located idR))
        (LPat idR)    (PatSynDir idR)

-- IPBinds'
data IPBinds id
  = IPBinds' [LIPBind id] TcEvBinds

data IPBind id
  = IPBind (Either LIPName id) (LExp id)

data Sig name
  = TypeSig [Located name] (LSigWcType name)
  | PatSynSig (Located name) (LSigType name)
  | ClassOpSig Bool [Located name] (LSigType name)
  | IdSig Id
  | FixSig (FixitySig name)
  | InlineSig (Located name) InlinePragma
  | SpecSig (Located name) [LSigType name] InlinePragma
  | SpecInstSig SourceText (LSigType name)
  | MinimalSig SourceText (LBooleanFormula (Located name))

data FixitySig name
  = FixitySig [Located name] Fixity

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

data Decl id
  = TyClD (TyClDecl id)
  | InstD (InstDecl id)
  | DerivD (DerivDecl id)
  | ValD (Bind id)
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

data Group id
  = Group (ValBinds id) [LSpliceDecl id] [TyClGroup id] [LDerivDecl id]
          [LFixitySig id] [LDefaultDecl id] [LForeignDecl id] [LWarnDecls id]
          [LAnnDecl id] [LRuleDecls id] [LVectDecl id] [LDocDecl]

data SpliceExplicitFlag
  = ExplicitSplice
  | ImplicitSplice

data SpliceDecl id
  = SpliceDecl (LSplice id) SpliceExplicitFlag

data TyClDecl name
  = FamDecl (FamilyDecl name)
  | SynDecl (Located name) (LQTyVars name) (LType name) (PostRn name NameSet)
  | DataDecl (Located name) (LQTyVars name) (DataDefn name) (PostRn name Bool)
             (PostRn name NameSet)
  | ClassDecl (LContext name) (Located name) (LQTyVars name)
              [LFunDepL name] [LSig name] (LBinds name)
              [LFamilyDecl name] [LTyFamDefltEqn name] [LDocDecl]
              (PostRn name NameSet)

data TyClGroup name
  = TyClGroup [LTyClDecl name] [LRoleAnnotDecl name] [LInstDecl name]

-- KingSig --> KindSig'
data FamilyResultSig name
  = NoSig
  | KindSig' (LKind name)
  | TyVarSig (LTyVarBndr name)

data FamilyDecl name
  = FamilyDecl (FamilyInfo name) (Located name) (LQTyVars name)
               (LFamilyResultSig name) (Maybe (LInjectivityAnn name))

data InjectivityAnn name
  = InjectivityAnn (Located name) [Located name]

data FamilyInfo name
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [LTyFamInstEqn name])

data DataDefn name
  = DataDefn NewOrData (LContext name) (Maybe LCType)
             (Maybe (LKind name)) [LConDecl name] (Deriving name)

type Deriving name = Maybe (LLSigTypes name)

data NewOrData
  = NewType
  | DataType

data ConDecl name
  = ConDeclGADT [Located name] (LSigType name) (Maybe LDocString)
  | ConDeclH98 (Located name) (Maybe (LQTyVars name)) (Maybe (LContext name))
               (ConDeclDetails name) (Maybe LDocString)

type ConDeclDetails name =
   ConDetails (LBangType name) (LLConDeclFields name)

type TyPats name = ImplicitBndrs name [LType name]

type TyFamInstEqn name = TyFamEqn name (TyPats name)

type TyFamDefltEqn name = TyFamEqn name (LQTyVars name)

data TyFamEqn name pats
  = TyFamEqn (Located name) pats (LType name)

data TyFamInstDecl name
  = TyFamInstDecl (LTyFamInstEqn name) (PostRn name NameSet)

data DataFamInstDecl name
  = DataFamInstDecl (Located name) (TyPats name) (DataDefn name)
                    (PostRn name NameSet)

data ClsInstDecl name
  = ClsInstDecl (LSigType name) (LBinds name) [LSig name] [LTyFamInstDecl name]
                [LDataFamInstDecl name] (Maybe LOverlapMode)

data InstDecl name
  = ClsInstD (ClsInstDecl name)
  | DataFamInstD (DataFamInstDecl name)
  | TyFamInstD (TyFamInstDecl name)

data DerivDecl name
  = DerivDecl (LSigType name) (Maybe LOverlapMode)

data DefaultDecl name
  = DefaultDecl [LType name]

data ForeignDecl name
  = ForeignImport (Located name) (LSigType name) (PostTc name Coercion)
                  ForeignImport
  | ForeignExport (Located name) (LSigType name) (PostTc name Coercion)
                  ForeignExport

data ForeignImport
  = CImport LCCallConv LSafety (Maybe Header) CImportSpec LSourceText

data CImportSpec
  = CLabel CLabelString
  | CFunction CCallTarget
  | CWrapper

data ForeignExport
  = CExport LCExportSpec LSourceText

data RuleDecls name
  = Rules SourceText [LRuleDecl name]

data RuleDecl name
  = Rule LSrcTextRuleName
         Activation [LRuleBndr name] (LExp name)
         (PostRn name NameSet) (LExp name) (PostRn name NameSet)

data RuleBndr name
  = RuleBndr (Located name)
  | RuleBndrSig (Located name) (LSigWcType name)

data VectDecl name
  = Vect SourceText (Located name) (LExp name)
  | NoVect SourceText (Located name)
  | VectTypeIn SourceText Bool (Located name) (Maybe (Located name))
  | VectTypeOut Bool TyCon (Maybe TyCon)
  | VectClassIn SourceText (Located name)
  | VectClassOut Class
  | VectInstIn (LSigType name)
  | VectInstOut ClsInst

data DocDecl
  = DocCommentNext DocString
  | DocCommentPrev DocString
  | DocCommentNamed String DocString
  | DocGroup Int DocString

data WarnDecls name
  = Warnings SourceText [LWarnDecl name]

data WarnDecl name
  = Warning [Located name] WarningTxt

data AnnDecl name
  = Annotation SourceText (AnnProvenance name) (LExp name)

data AnnProvenance name
  = ValueAnnProvenance (Located name)
  | TypeAnnProvenance (Located name)
  | ModuleAnnProvenance

data RoleAnnotDecl name
  = RoleAnnotDecl (Located name) [LMRole]

-------------------------------------------------------------------------------
-- Types

type BangType name = Type name

type Context name = [LType name]

type Kind name = Type name

data LQTyVars name
  = QTvs (PostRn name [Name]) [LTyVarBndr name] (PostRn name NameSet)

data ImplicitBndrs name thing
  = IB (PostRn name [Name]) thing

data WildCardBndrs name thing
  = WC (PostRn name [Name]) (Maybe SrcSpan) thing

type LSigType name = ImplicitBndrs name (LType name)

type LWcType name = WildCardBndrs name (LType name)

type LSigWcType name = ImplicitBndrs name (LWcType name)

newtype IPName = IPName FastString

data TyVarBndr name
  = UserTyVar (Located name)
  | KindedTyVar (Located name) (LKind name)

data Type name
  = ForAllTy [LTyVarBndr name] (LType name)
  | QualTy (LContext name) (LType name)
  | TyVar (Located name)
  | AppsTy [LAppType name]
  | AppTy (LType name) (LType name)
  | FunTy (LType name) (LType name)
  | ListTy (LType name)
  | PArrTy (LType name)
  | TupleTy TupleSort [LType name]
  | OpTy (LType name) (Located name) (LType name)
  | ParTy (LType name)
  | IParamTy IPName (LType name)
  | EqTy (LType name) (LType name)
  | KindSig (LType name) (LKind name)
  | SpliceTy (Splice name) (PostTc name TCRKind)
  | DocTy (LType name) LDocString
  | BangTy SrcBang (LType name)
  | RecTy [LConDeclField name]
  | CoreTy TCRType
  | ExplicitListTy (PostTc name TCRKind) [LType name]
  | ExplicitTupleTy [PostTc name TCRKind] [LType name]
  | TyLit TyLit
  | WildCardTy (WildCardInfo name)

data TyLit
  = NumTy SourceText Integer
  | StrTy SourceText FastString

newtype WildCardInfo name
  = AnonWildCard (PostRn name LName)

data AppType name
  = AppInfix (Located name)
  | AppPrefix (LType name)

data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple

data ConDeclField name
  = ConDeclField [LFieldOcc name] (LBangType name) (Maybe LDocString)

data ConDetails arg rec
  = PrefixCon [arg]
  | RecCon rec
  | InfixCon arg arg

data FieldOcc name
  = FieldOcc LRdrName (PostRn name name)

data AmbiguousFieldOcc name
  = Unambiguous LRdrName (PostRn name name)
  | Ambiguous LRdrName (PostTc name name)
