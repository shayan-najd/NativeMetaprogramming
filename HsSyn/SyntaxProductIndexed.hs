{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds,GADTs,PatternSynonyms #-}
module SyntaxProductIndexed where

data Exp row col id
  = Var             (row 'VarL)
                    (Located id)
  | UnboundVar      (row 'UnboundVarL)
                    UnboundVar
--  HSE.Con (missing?)
--                  \n

  | Lit             (row 'LitL)
                    Lit
--  HsSyn.OverLit
--                  \n

  | App             (row 'AppL)
                    (Exp row col id) (Exp row col id)

  | SectionL        (row 'SectionLL)
                    (Exp row col id) (Exp row col id)

  | SectionR        (row 'SectionRL)
                    (Exp row col id) (Exp row col id)

  | NegApp          (row 'NegAppL)
                    (Exp row col id)

  | OpApp           (row 'OpAppL)
                    (Exp row col id) (Exp row col id) (Exp row col id)
--                  \n

  | AppType         (row 'AppTypeL)
                    (Exp row col id) (LWcType row col id)
-- HsSyn.AppTypeOut
--                  \n

  | Par             (row 'ParL)
                    (Exp row col id)

  | If              (row 'IfL)
                    (Exp row col id) (Exp row col id) (Exp row col id)
--                  \n

  | MultiIf         (row 'MultiIfL)
                    [LGRHS row col id (Exp row col id)]

  | Case            (row 'CaseL)
                    (Exp row col id) (MatchGroup row col id (Exp row col id))

  | Lam             (row 'LamL)
                    (MatchGroup row col id (Exp row col id))

  | LamCase         (row 'LamCaseL)
                    (MatchGroup row col id (Exp row col id))

  | Let             (row 'LetL)
                    (LLocalBinds row col id) (Exp row col id)

  | IPVar           (row 'IPVarL)
                    IPName

  | RecFld          (row 'RecFldL)
                    (AmbiguousFieldOcc id)

  | OverLabel       (row 'OverLabelL)
                    FastString

  | RecordCon       (row 'RecordConL)
                    (Located id) (RecordBinds row col id)
--                  \n

  | RecordUpd       (row 'RecordUpdL)
                    (Exp row col id) [LRecUpdField row col id]
--                  \n
--                  \n

  | ExplicitTuple   (row 'ExplicitTupleL)
                    [LTupArg row col id] Boxity
--  HSE.TupleSection (part of above?)
--                  \n

  | ExplicitList    (row 'ExplicitListL)
                    [Exp row col id]
--                  \n

  | ExplicitPArr    (row 'ExplicitPArrL)
                    [Exp row col id]

  | ArithSeq        (row 'ArithSeqL)
                    (ArithSeqInfo row col id)
--                  \n
--  ArithSeqInfo.From
--                  \n
--  ArithSeqInfo.FromTo
--                  \n
--  ArithSeqInfo.FromThen
--                  \n
--  ArithSeqInfo.FromThenTo
--                  \n

  | PArrSeq         (row 'PArrSeqL)
                    (ArithSeqInfo row col id) -- expanding ArithSeqInfo below
--                  \n
--  ArithSeqInfo.From
--  ArithSeqInfo.FromTo
--                  \n
--  ArithSeqInfo.FromThen
--  ArithSeqInfo.FromThenTo
--                  \n

  | Do              (row 'DoL)
                    (StmtContext Name) (LExpLStmts row col id)
--                  \n
--  StmtContext.ListComp
--                  \n
--  StmtContext.MonadComp
--  StmtContext.ParArrComp
--                  \n
--  StmtContext.DoExp
--                  \n
--  StmtContext.MDoExp
--                  \n
--  StmtContext.ArrowExp
--  StmtContext.GhciStmtCtxt
--  StmtContext.PatGuard
--  StmtContext.ParStmtCtxt
--                  \n
--  StmtContext.TransStmtCtxt

  | Bracket         (row 'BracketL)
                    (Bracket row col id)
--  HsSyn.RnBracketOut
--                     \n
--  HsSyn.TcBracketOut
--                     \n
--  HSE.QuasiQuote (missing?)
--                  \n

--  HSE.VarQuote (missing?)
--                  \n
--  HSE.TypQuote (missing?)
--                  \n

  | SpliceE         (row 'SpliceEL)
                    (Splice row col id)

  | ExpWithTySig    (row 'ExpWithTySigL)
                    (Exp row col id) (LSigWcType row col id)
--  HsSyn.ExpWithTySigOut
--                  \n

  | CoreAnn         (row 'CoreAnnL)
                    SourceText StringLiteral (Exp row col id)
  | SCC             (row 'SCCL)
                    SourceText StringLiteral (Exp row col id)
  | TickPragma      (row 'TickPragmaL)
                    SourceText (StringLiteral,(Int,Int),(Int,Int))
                    ((SourceText,SourceText),(SourceText,SourceText))
                    (Exp row col id) -- (is it GenPragma?)

  | Proc            (row 'ProcL)
                    (LPat row col id) (LCmdTop row col id)

  | ArrApp          (row 'ArrAppL)
                    (Exp row col id) (Exp row col id) (PostTc id TCRType)
                    ArrAppType Bool
--  HSE.LeftArrHighApp (don't know how they compare)
--                  \n
  | ArrForm         (row 'ArrFormL)
                    (Exp row col id) (Maybe Fixity) [LCmdTop row col id]
--  HSE.RightArrHighApp (don't know how they compare)

  | EWildPat        (row 'EWildPatL)
                    -- (right comparison?)

  | Static          (row 'StaticL)
                    (Exp row col id)

  | Tick            (row 'TickL)
                    (Tickish id) (Exp row col id)

  | BinTick         (row 'BinTickL)
                    Int Int (Exp row col id)

  | EAsPat          (row 'EAsPatL)
                    (Located id) (Exp row col id)

  | EViewPat        (row 'EViewPatL)
                    (Exp row col id) (Exp row col id)

  | ELazyPat        (row 'ELazyPatL)
                    (Exp row col id)

  | Wrap            (row 'WrapL)
                    Wrapper (Exp row col id)

  | Ext             (row 'ExtL)
                    (col (Exp row col id))

data Pat row col id
  = WildPat   (PostTc id TCRType)

  | VarPat    (Located id)

  | LitPat    Lit
  | NPat      (LOverLit row col id)
              (Maybe (SyntaxExp row col id))
              (SyntaxExp row col id) (PostTc id TCRType)

  | NPlusKPat (Located id) (LOverLit row col id)
              (OverLit row col id) (SyntaxExp row col id)
              (SyntaxExp row col id) (PostTc id TCRType)

  | TuplePat  [LPat row col id] Boxity [PostTc id TCRType]

  | ListPat   [LPat row col id] (PostTc id TCRType)
              (Maybe (PostTc id TCRType, SyntaxExp row col id))

  | ParPat    (LPat row col id)

  | AsPat     (Located id) (LPat row col id)

  | ViewPat   (Exp row col id) (LPat row col id) (PostTc id TCRType)

  | BangPat   (LPat row col id)

  | LazyPat   (LPat row col id)

  | SigPatIn  (LPat row col id) (LSigWcType row col id)
  | SigPatOut (LPat row col id) TCRType

  | PArrPat   [LPat row col id] (PostTc id TCRType)

  | ConPatIn  (Located id) (ConPatDetails row col id)
  | ConPatOut LConLike [TCRType] [TyVar] [EvVar] TcEvBinds
              (ConPatDetails row col id)
              Wrapper
--  HSE.PRec
--  HSE.PInfixApp

  | SplicePat (Splice row col id)

  | CoPat     Wrapper (Pat row col id) TCRType

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

data Decl row col id
  = RoleAnnotD (RoleAnnotDecl id)

  | AnnD (AnnDecl row col id)

  | DerivD (DerivDecl row col id)

  | WarningD (WarnDecls id)

  | RuleD (RuleDecls row col id)

  | DefD (DefaultDecl row col id)

  | ForD (ForeignDecl row col id)
--        \n
--  ForExp (in ForeignDecl)

  | SpliceD (SpliceDecl row col id)

  | DocD (DocDecl)

  | SigD (Sig row col id)
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

  | TyClD (TyClDecl row col id) -- expanding TyClDecl below
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

  | InstD (InstDecl row col id) -- expanding InstDecl below
--  InstDecl.ClsInstD
--  InstDecl.DataFamInstD
--             \n
--           GDataInsDecl (grouped with above?)
--             \n
--  InstDecl.TyFamInstD

  | ValD (Bind row col id)
--  Bind.FunBind
--  Bind.PatBind
--  Bind.VarBind
--  Bind.AbsBinds
--  Bind.AbsBindsSig
--  Bind.PatSynBind

--  HSE.DeprPragmaDecl  (missing?)

  | VectD (VectDecl row col id)

data Type row col id
  = ForAllTy [LTyVarBndr row col id] (LType row col id)

  | FunTy (LType row col id) (LType row col id)

  | TupleTy TupleSort [LType row col id]

  | ListTy (LType row col id)

  | PArrTy (LType row col id)

  | AppTy (LType row col id) (LType row col id)

  | TyVar (Located id)

 -- HSE.TyCon (part of above)

  | ParTy (LType row col id)

  | KindSig (LType row col id) (LKind row col id)

  | BangTy SrcBang (LType row col id)

  | WildCardTy (WildCardInfo id)

  | EqTy (LType row col id) (LType row col id)

  | SpliceTy (Splice row col id) (PostTc id TCRKind)

  | OpTy (LType row col id) (Located id) (LType row col id) -- match?

  | TyLit TyLit
--  TyLit.Num
--  TyLit.String
  | AppsTy [LAppType row col id] -- assuming it is promoted constructors
  | ExplicitListTy (PostTc id TCRKind) [LType row col id]
  | ExplicitTupleTy [PostTc id TCRKind] [LType row col id]
--  HSE.Promoted.PromotedUnit (ExplicitTupleTy [] []?)

--  HSE.TyQuasiQuote (missing?)

  | QualTy (LContext row col id) (LType row col id)

  | IParamTy IPName (LType row col id)

  | DocTy (LType row col id) LDocString

  | RecTy [LConDeclField row col id]

  | CoreTy TCRType

------------ the rest is not compared -------------


----------------------------------------------------------------------------
-- Located versions

type LTupArg            row col id = Located (TupArg row col id)
type LCmd               row col id = Located (Cmd  row col id)
type LCmdTop            row col id = Located (CmdTop  row col id)
type LMatch        row col id body = Located (Match  row col id body)
type LDecl              row col id = Located (Decl  row col id)
type LBangType          row col id = Located (BangType  row col id)
type LContext           row col id = Located (Context row col id)
type LType              row col id = Located (Type  row col id)
type LKind              row col id = Located (Kind  row col id)
type LConDeclField      row col id = Located (ConDeclField row col id)
type LTyVarBndr         row col id = Located (TyVarBndr row col id)
type LAppType           row col id = Located (AppType row col id)
type LFieldOcc                  id = Located (FieldOcc id)
type LGRHS         row col id body = Located (GRHS  row col id body)
type LStmt         row col id body = Located (StmtLR  row col id id body)
type LStmtLR  row col idL idR body = Located (StmtLR  row col idL idR body)
type LPat               row col id = Located (Pat  row col id)
type LRecField'             id arg = Located (RecField' id arg)
type LRecField              id arg = Located (RecField id arg)
type LRecUpdField       row col id = Located (RecUpdField row col id)
type LDocString                    = Located DocString
type LImportDecl                id = Located (ImportDecl id)
type LIE                        id = Located (IE id)
type LBindLR       row col idL idR = Located (BindLR row col idL idR)
type LIPBind            row col id = Located (IPBind row col id)
type LSig               row col id = Located (Sig row col id)
type LFixitySig                 id = Located (FixitySig id)
type LTcSpecPrag                   = Located TcSpecPrag
type LSpliceDecl        row col id = Located (SpliceDecl row col id)
type LTyClDecl          row col id = Located (TyClDecl row col id)
type LFamilyResultSig   row col id = Located (FamilyResultSig row col id)
type LFamilyDecl        row col id = Located (FamilyDecl row col id)
type LInjectivityAnn            id = Located (InjectivityAnn id)
type LConDecl           row col id = Located (ConDecl row col id)
type LTyFamInstEqn      row col id = Located (TyFamInstEqn row col id)
type LTyFamDefltEqn     row col id = Located (TyFamDefltEqn row col id)
type LTyFamInstDecl     row col id = Located (TyFamInstDecl row col id)
type LDataFamInstDecl   row col id = Located (DataFamInstDecl row col id)
type LClsInstDecl       row col id = Located (ClsInstDecl row col id)
type LInstDecl          row col id = Located (InstDecl row col id)
type LDerivDecl         row col id = Located (DerivDecl row col id)
type LDefaultDecl       row col id = Located (DefaultDecl row col id)
type LForeignDecl       row col id = Located (ForeignDecl row col id)
type LRuleDecls         row col id = Located (RuleDecls row col id)
type LRuleDecl          row col id = Located (RuleDecl row col id)
type LRuleBndr          row col id = Located (RuleBndr row col id)
type LVectDecl          row col id = Located (VectDecl row col id)
type LDocDecl                      = Located (DocDecl)
type LWarnDecls                 id = Located (WarnDecls id)
type LWarnDecl                  id = Located (WarnDecl id)
type LAnnDecl           row col id = Located (AnnDecl row col id)
type LRoleAnnotDecl             id = Located (RoleAnnotDecl id)
type LModuleName                   = Located ModuleName
type LIPName                       = Located IPName
type LSplice            row col id = Located (Splice row col id)
type LCType                        = Located CType
type LOverlapMode                  = Located OverlapMode
type LCCallConv                    = Located CCallConv
type LSafety                       = Located Safety
type LSourceText                   = Located SourceText
type LCExportSpec                  = Located CExportSpec
type LOverLit           row col id = Located (OverLit  row col id)
type LConLike                      = Located ConLike
type LExpLStmts         row col id = Located [ExpLStmt row col id]
type LCmdLStmts         row col id = Located [CmdLStmt row col id]
type LLMatchs      row col id body = Located [LMatch row col id body]
type LLIEs                      id = Located [LIE id]
type LFieldLbl                  id = Located (FieldLbl id)
type LLSigTypes         row col id = Located [LSigType row col id]
type LLConDeclFields    row col id = Located [LConDeclField row col id]
type LRdrName                      = Located RdrName
type LName                         = Located Name
type LFunDepL                   id = Located (FunDep (Located id))
type LSrcTextRuleName              = Located (SourceText, RuleName)
type LMRole                        = Located (Maybe Role)
type LLocalBinds   row col id      = Located (LocalBinds row col id)
type LLocalBindsLR row col idL idR = Located (LocalBindsLR row col idL idR)

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

data OverLit row col id
  = OverLit' OverLitVal (PostRn id Bool) (Exp row col id) (PostTc id TCRType)

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

type PostTcExp row col
  = Exp row col Id

type PostTcTable row col
  = [(Name, PostTcExp row col)]

data SyntaxExp row col id
  = SyntaxExp (Exp row col id) [Wrapper] Wrapper

type CmdSyntaxTable  row col id = [(Name, Exp row col id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

data TupArg row col id
  = Present (Exp row col id)
  | Missing (PostTc id TCRType)

data LWcTypeX row col
  = forall id. OutputableBndr id => LWcTypeX (LWcType row col id)

data Cmd row col id
  = CmdArrApp  (Exp row col id) (Exp row col id) (PostTc id TCRType)
               ArrAppType Bool
  | CmdArrForm (Exp row col id) (Maybe Fixity) [LCmdTop row col id]
  | CmdApp     (LCmd row col id)  (Exp row col id)
  | CmdLam     (MatchGroup row col id (LCmd row col id))
  | CmdPar     (LCmd row col id)
  | CmdCase    (Exp row col id) (MatchGroup row col id (LCmd row col id))
  | CmdIf      (Maybe (SyntaxExp row col id))
               (Exp row col id) (LCmd row col id) (LCmd row col id)
  | CmdLet     (LLocalBinds row col id) (LCmd row col id)
  | CmdDo      (LCmdLStmts row col id) (PostTc id TCRType)
  | CmdWrap    Wrapper (Cmd row col id)

data ArrAppType
  = HigherOrderApp
  | FirstOrderApp

data CmdTop row col id
  = CmdTop (LCmd row col id) (PostTc id TCRType) (PostTc id TCRType)
           (CmdSyntaxTable row col id)

type RecordBinds row col id = RecFields id (Exp row col id)

data MatchGroup row col id body
  = MG (LLMatchs row col id body) [PostTc id TCRType] (PostTc id TCRType) Origin

data Match row col id body
  = Match (MatchFixity id) [LPat row col id] (Maybe (LType row col id))
          (GRHSs row col id body)

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id) Bool

data GRHSs row col id body
  = GRHSs [LGRHS row col id body] (LLocalBinds row col id)

data GRHS  row col id body
  = GRHS [GuardLStmt row col id] body

type Stmt row col id body  = StmtLR row col id id body
type CmdLStmt   row col id = LStmt row col id (LCmd row col id)
type CmdStmt    row col id = Stmt  row col id (LCmd row col id)
type ExpLStmt   row col id = LStmt row col id (Exp row col id)
type ExpStmt    row col id = Stmt  row col id (Exp row col id)
type GuardLStmt row col id = LStmt row col id (Exp row col id)
type GuardStmt  row col id = Stmt  row col id (Exp row col id)
type GhciLStmt  row col id = LStmt row col id (Exp row col id)
type GhciStmt   row col id = Stmt  row col id (Exp row col id)

data StmtLR row col idL idR body
  = LastStmt body Bool (SyntaxExp row col idR)
  | BindStmt (LPat row col idL) body (SyntaxExp row col idR)
             (SyntaxExp row col idR)
             (PostTc idR TCRType)
  | ApplicativeStmt [(SyntaxExp row col idR, ApplicativeArg row col idL idR)]
                    (Maybe (SyntaxExp row col idR)) (PostTc idR TCRType)
  | BodyStmt body (SyntaxExp row col idR) (SyntaxExp row col idR)
                  (PostTc idR TCRType)
  | LetStmt  (LLocalBindsLR row col idL idR)
  | ParStmt  [ParStmtBlock row col idL idR] (Exp row col idR)
             (SyntaxExp row col idR) (PostTc idR TCRType)
  | TransStmt TransForm [ExpLStmt row col idL] [(idR, idR)] (Exp row col idR)
              (Maybe (Exp row col idR))
              (SyntaxExp row col idR) (SyntaxExp row col idR)
              (PostTc idR TCRType) (Exp row col idR)
  | RecStmt [LStmtLR row col idL idR body] [idR] [idR] (SyntaxExp row col idR)
            (SyntaxExp row col idR) (SyntaxExp row col idR) (PostTc idR TCRType)
            [PostTcExp row col] [PostTcExp row col] (PostTc idR TCRType)

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock row col idL idR
  = ParStmtBlock [ExpLStmt row col idL] [idR] (SyntaxExp row col idR)

data ApplicativeArg row col idL idR
  = ApplicativeArgOne (LPat row col idL) (Exp row col idL)
  | ApplicativeArgMany [ExpLStmt row col idL] (Exp row col idL) (LPat row col idL)

data Splice row col id
  = TypedSplice id (Exp row col id)
  | UntypedSplice id (Exp row col id)
  | QuasiQuote id id SrcSpan FastString

type SplicePointName = Name

data PendingRnSplice row col
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (Exp row col Name)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data PendingTcSplice row col
  = PendingTcSplice SplicePointName (Exp row col Id)

data Bracket row col id
  = ExpBr (Exp row col id)
  | PatBr (LPat row col id)
  | DecBrL [LDecl row col id]
  | DecBrG (Group row col id)
  | TypBr (LType row col id)
  | VarBr Bool id
  | TExpBr (Exp row col id)

data ArithSeqInfo row col id
  = From            (Exp row col id)
  | FromThen        (Exp row col id) (Exp row col id)
  | FromTo          (Exp row col id) (Exp row col id)
  | FromThenTo      (Exp row col id) (Exp row col id) (Exp row col id)

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

type InPat row col id
  = LPat row col id

type OutPat row col id
  = LPat row col id

type ConPatDetails row col id
  = ConDetails (LPat row col id) (RecFields id (LPat row col id))

data RecFields id arg
  = RecFields [LRecField id arg] (Maybe Int)

type RecField id arg
  = RecField' (FieldOcc id) arg

type RecUpdField row col id
  = RecField' (AmbiguousFieldOcc id) (Exp row col id)

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

type LBind      row col id      = LBindLR row col id id
type LBinds     row col id      = LBindsLR row col id id
type LBindsLR   row col idL idR = Bag (LBindLR row col idL idR)

type LocalBinds row col id = LocalBindsLR row col id id

data LocalBindsLR  row col idL idR
  = ValBinds (ValBindsLR row col idL idR)
  | IPBinds (IPBinds row col idR)
  | EmptyLocalBinds

type ValBinds row col id = ValBindsLR row col id id

data ValBindsLR row col idL idR
  = ValBindsIn (LBindsLR row col idL idR) [LSig row col idR]
  | ValBindsOut [(RecFlag, LBinds row col idL)] [LSig row col Name]

type Bind row col id = BindLR row col id id


data BindLR row col idL idR
  = FunBind (Located idL) (MatchGroup row col idR (Exp row col idR)) Wrapper
            (PostRn idL NameSet) [Tickish Id]
  | PatBind (LPat row col idL)
            (GRHSs row col idR (Exp row col idR)) (PostTc idR TCRType)
            (PostRn idL NameSet) ([Tickish Id], [[Tickish Id]])
  | VarBind idL (Exp row col idR) Bool
  | AbsBinds [TyVar] [EvVar] [ABExport idL] [TcEvBinds] (LBinds row col idL)
  | AbsBindsSig [TyVar] [EvVar] idL TcSpecPrags TcEvBinds (LBind row col idL)
  | PatSynBind (PatSynBind row col idL idR)

data ABExport id
  = ABE id id Wrapper TcSpecPrags

data PatSynBind row col idL idR
  = PSB (Located idL) (PostRn idR NameSet) (PatSynDetails (Located idR))
        (LPat row col idR)    (PatSynDir row col idR)

-- IPBinds'
data IPBinds row col id
  = IPBinds' [LIPBind row col id] TcEvBinds

data IPBind row col id
  = IPBind (Either LIPName id) (Exp row col id)

data Sig row col id
  = TypeSig [Located id] (LSigWcType row col id)
  | PatSynSig (Located id) (LSigType row col id)
  | ClassOpSig Bool [Located id] (LSigType row col id)
  | IdSig Id
  | FixSig (FixitySig id)
  | InlineSig (Located id) InlinePragma
  | SpecSig (Located id) [LSigType row col id] InlinePragma
  | SpecInstSig SourceText (LSigType row col id)
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

data PatSynDir row col id
  = Unidirectional
  | ImplicitBidirectional
  | ExplicitBidirectional (MatchGroup row col id (Exp row col id))

------------------------------------------------------------------------------
-- Decls

data Group row col id
  = Group (ValBinds row col id) [LSpliceDecl row col id] [TyClGroup row col id]
          [LDerivDecl row col id]
          [LFixitySig id] [LDefaultDecl row col id]
          [LForeignDecl row col id] [LWarnDecls id]
          [LAnnDecl row col id] [LRuleDecls row col id]
          [LVectDecl row col id] [LDocDecl]

data SpliceExplicitFlag
  = ExplicitSplice
  | ImplicitSplice

data SpliceDecl row col id
  = SpliceDecl (LSplice row col id) SpliceExplicitFlag

data TyClDecl row col id
  = FamDecl (FamilyDecl row col id)
  | SynDecl (Located id) (LQTyVars row col id)
            (LType row col id) (PostRn id NameSet)
  | DataDecl (Located id) (LQTyVars row col id)
             (DataDefn row col id) (PostRn id Bool)
             (PostRn id NameSet)
  | ClassDecl (LContext row col id) (Located id) (LQTyVars row col id)
              [LFunDepL id] [LSig row col id] (LBinds row col id)
              [LFamilyDecl row col id] [LTyFamDefltEqn row col id] [LDocDecl]
              (PostRn id NameSet)

data TyClGroup row col id
  = TyClGroup [LTyClDecl row col id] [LRoleAnnotDecl id] [LInstDecl row col id]

-- KingSig --> KindSig'
data FamilyResultSig row col id
  = NoSig
  | KindSig' (LKind row col id)
  | TyVarSig (LTyVarBndr row col id)

data FamilyDecl row col id
  = FamilyDecl (FamilyInfo row col id) (Located id) (LQTyVars row col id)
               (LFamilyResultSig row col id) (Maybe (LInjectivityAnn id))

data InjectivityAnn id
  = InjectivityAnn (Located id) [Located id]

data FamilyInfo row col id
  = DataFamily
  | OpenTypeFamily
  | ClosedTypeFamily (Maybe [LTyFamInstEqn row col id])

data DataDefn row col id
  = DataDefn NewOrData (LContext row col id) (Maybe LCType)
             (Maybe (LKind row col id)) [LConDecl row col id]
             (Deriving row col id)

type Deriving row col id = Maybe (LLSigTypes row col id)

data NewOrData
  = NewType
  | DataType

data ConDecl row col id
  = ConDeclGADT [Located id] (LSigType row col id) (Maybe LDocString)
  | ConDeclH98 (Located id) (Maybe (LQTyVars row col id))
               (Maybe (LContext row col id))
               (ConDeclDetails row col id) (Maybe LDocString)

type ConDeclDetails row col id =
   ConDetails (LBangType row col id) (LLConDeclFields row col id)

type TyPats row col id
  = ImplicitBndrs id [LType row col id]

type TyFamInstEqn row col id
  = TyFamEqn row col id (TyPats row col id)

type TyFamDefltEqn row col id
  = TyFamEqn row col id (LQTyVars row col id)

data TyFamEqn row col id pats
  = TyFamEqn (Located id) pats (LType row col id)

data TyFamInstDecl row col id
  = TyFamInstDecl (LTyFamInstEqn row col id) (PostRn id NameSet)

data DataFamInstDecl row col id
  = DataFamInstDecl (Located id) (TyPats row col id) (DataDefn row col id)
                    (PostRn id NameSet)

data ClsInstDecl row col id
  = ClsInstDecl (LSigType row col id)
                (LBinds row col id) [LSig row col id] [LTyFamInstDecl row col id]
                [LDataFamInstDecl row col id] (Maybe LOverlapMode)

data InstDecl row col id
  = ClsInstD (ClsInstDecl row col id)
  | DataFamInstD (DataFamInstDecl row col id)
  | TyFamInstD (TyFamInstDecl row col id)

data DerivDecl row col id
  = DerivDecl (LSigType row col id) (Maybe LOverlapMode)

data DefaultDecl row col id
  = DefaultDecl [LType row col id]

data ForeignDecl row col id
  = ForeignImport (Located id) (LSigType row col id) (PostTc id Coercion)
                  ForeignImport
  | ForeignExport (Located id) (LSigType row col id) (PostTc id Coercion)
                  ForeignExport

data ForeignImport
  = CImport LCCallConv LSafety (Maybe Header) CImportSpec LSourceText

data CImportSpec
  = CLabel CLabelString
  | CFunction CCallTarget
  | CWrapper

data ForeignExport
  = CExport LCExportSpec LSourceText

data RuleDecls row col id
  = Rules SourceText [LRuleDecl row col id]

data RuleDecl row col id
  = Rule LSrcTextRuleName
         Activation [LRuleBndr row col id] (Exp row col id)
         (PostRn id NameSet) (Exp row col id) (PostRn id NameSet)

data RuleBndr row col id
  = RuleBndr (Located id)
  | RuleBndrSig (Located id) (LSigWcType row col id)

data VectDecl row col id
  = Vect SourceText (Located id) (Exp row col id)
  | NoVect SourceText (Located id)
  | VectTypeIn SourceText Bool (Located id) (Maybe (Located id))
  | VectTypeOut Bool TyCon (Maybe TyCon)
  | VectClassIn SourceText (Located id)
  | VectClassOut Class
  | VectInstIn (LSigType row col id)
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

data AnnDecl row col id
  = Annotation SourceText (AnnProvenance id) (Exp row col id)

data AnnProvenance id
  = ValueAnnProvenance (Located id)
  | TypeAnnProvenance (Located id)
  | ModuleAnnProvenance

data RoleAnnotDecl id
  = RoleAnnotDecl (Located id) [LMRole]

-------------------------------------------------------------------------------
-- Types

type BangType id = Type id

type Context row col id
  = [LType row col id]

type Kind id = Type id

data LQTyVars row col id
  = QTvs (PostRn id [Name]) [LTyVarBndr row col id] (PostRn id NameSet)

data ImplicitBndrs id thing
  = IB (PostRn id [Name]) thing

data WildCardBndrs id thing
  = WC (PostRn id [Name]) (Maybe SrcSpan) thing

type LSigType row col id
  = ImplicitBndrs id (LType row col id)

type LWcType row col id
  = WildCardBndrs id (LType row col id)

type LSigWcType row col id
  = ImplicitBndrs id (LWcType row col id)

newtype IPName = IPName FastString

data TyVarBndr row col id
  = UserTyVar (Located id)
  | KindedTyVar (Located id) (LKind row col id)

data TyLit
  = NumTy SourceText Integer
  | StrTy SourceText FastString

newtype WildCardInfo id
  = AnonWildCard (PostRn id LName)

data AppType row col id
  = AppInfix (Located id)
  | AppPrefix (LType row col id)

data TupleSort
  = UnboxedTuple
  | BoxedTuple
  | ConstraintTuple
  | BoxedOrConstraintTuple

data ConDeclField row col id
  = ConDeclField [LFieldOcc id] (LBangType row col id) (Maybe LDocString)

data ConDetails arg rec
  = PrefixCon [arg]
  | RecCon rec
  | InfixCon arg arg

data FieldOcc id
  = FieldOcc LRdrName (PostRn id id)

data AmbiguousFieldOcc id
  = Unambiguous LRdrName (PostRn id id)
  | Ambiguous LRdrName (PostTc id id)


data ExpL
  = VarL

  | UnboundVarL

  | LitL

  | OverLitL

  | AppL

  | SectionLL

  | SectionRL

  | NegAppL

  | OpAppL

  | AppTypeL

  | AppTypeOutL

  | ParL

  | IfL

  | MultiIfL

  | CaseL

  | LamL

  | LamCaseL

  | LetL

  | IPVarL

  | RecFldL

  | OverLabelL

  | RecordConL

  | RecordUpdL

  | ExplicitTupleL

  | ExplicitListL

  | ExplicitPArrL

  | ArithSeqL

  | PArrSeqL

  | DoL

  | BracketL

  | RnBracketOutL

  | TcBracketOutL

  | SpliceEL

  | ExpWithTySigL

  | ExpWithTySigOutL

  | CoreAnnL

  | SCCL

  | TickPragmaL

  | ProcL

  | ArrAppL

  | ArrFormL

  | EWildPatL

  | StaticL

  | TickL

  | BinTickL

  | EAsPatL

  | EViewPatL

  | ELazyPatL

  | WrapL

  | ExtL

data Col id exp
  = OverLitC         (Row id 'OverLitL)
                     (OverLit (Row id) (Col id) id)
  | AppTypeOutC      (Row id 'AppTypeOutL)
                     (Exp (Row id) (Col id) id) (LWcType (Row id) (Col id) Name)
  | RnBracketOutC    (Row id 'RnBracketOutL)
                     (Bracket (Row id) (Col id) Name)
                     [PendingRnSplice (Row id) (Col id)]
  | TcBracketOutC    (Row id 'TcBracketOutL)
                     (Bracket (Row id) (Col id) Name)
                     [PendingTcSplice (Row id) (Col id)]
  | ExpWithTySigOutC (Row id 'ExpWithTySigOutL)
                     (Exp (Row id) (Col id) id)
                     (LSigWcType (Row id) (Col id) Name)

data Row id lbl where
  NegAppR       :: SyntaxExp (Row id) (Col id) id ->
                   Row id 'NegAppL
  OpAppR        :: PostRn id Fixity ->
                   Row id 'OpAppL
  IfR           :: Maybe (SyntaxExp (Row id) (Col id) id) ->
                   Row id 'IfL
  MultiIfR      :: PostTc id TCRType ->
                   Row id 'MultiIfL
  RecordConR    :: PostTc id ConLike -> PostTcExp (Row id) (Col id) ->
                   Row id 'RecordConL
  RecordUpdR    :: PostTc id [ConLike] -> PostTc id [TCRType] ->
                   PostTc id [TCRType] -> PostTc id Wrapper ->
                   Row id 'RecordUpdL
  ExplicitListR :: PostTc id TCRType -> Maybe (SyntaxExp (Row id) (Col id) id) ->
                   Row id 'ExplicitListL
  ExplicitPArrR :: PostTc id TCRType ->
                   Row id 'ExplicitPArrL
  ArithSeqR     :: PostTcExp (Row id) (Col id) ->
                   Maybe (SyntaxExp (Row id) (Col id) id) ->
                   Row id 'ArithSeqL
  PArrSeqR      :: PostTcExp (Row id) (Col id) ->
                   Row id 'PArrSeqL
  DoR           :: PostTc id TCRType ->
                   Row id 'DoL

type Expr id = Exp (Row id) (Col id) id
