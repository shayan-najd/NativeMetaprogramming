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

  | Let      (Located (LocalBinds id)) (LExp  id)

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
  | Do           (StmtContext Name) (Located [ExpLStmt id]) (PostTc id TCRType)
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

--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC

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
  | NPat      (Located (OverLit id))
              (Maybe (SyntaxExp id))  (SyntaxExp id) (PostTc id TCRType)

  | NPlusKPat (Located id) (Located (OverLit id)) (OverLit id) (SyntaxExp id)
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
  | ConPatOut (Located ConLike) [TCRType] [TyVar] [EvVar] TcEvBinds
              (ConPatDetails id) Wrapper
--  PRec
--  PInfixApp

  | SplicePat (Splice id)

  | CoPat     Wrapper (Pat id) TCRType

--  HSE Regex stuff missing from GHC

--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC
--  HSE XML stuff missing from GHC



------------ the rest is not compared -------------
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

data OverLit id
  = OverLit' OverLitVal (PostRn id Bool) (Exp id) (PostTc id TCRType)

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

type LExp id = Located (Exp id)

type PostTcExp  = Exp Id

type PostTcTable = [(Name, PostTcExp)]

data SyntaxExp id = SyntaxExp (Exp id) [Wrapper] Wrapper

type CmdSyntaxTable id = [(Name, Exp id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName


type LTupArg id = Located (TupArg id)

data TupArg id
  = Present (LExp id)
  | Missing (PostTc id TCRType)

data LWcTypeX = forall id. OutputableBndr id => LWcTypeX (LWcType id)

type LCmd id = Located (Cmd id)

data Cmd id
  = CmdArrApp
        (LExp id)
        (LExp id)
        (PostTc id TCRType)
        ArrAppType
        Bool

  | CmdArrForm
        (LExp id)

        (Maybe Fixity)
        [LCmdTop id]
  | CmdApp    (LCmd id)
                (LExp id)
  | CmdLam    (MatchGroup id (LCmd id))

  | CmdPar    (LCmd id)

  | CmdCase   (LExp id)
                (MatchGroup id (LCmd id))

  | CmdIf     (Maybe (SyntaxExp id))
                (LExp id)
                (LCmd id)
                (LCmd id)

  | CmdLet    (Located (LocalBinds id))
                (LCmd  id)

  | CmdDo     (Located [CmdLStmt id])
                (PostTc id TCRType)

  | CmdWrap   Wrapper
                (Cmd id)

data ArrAppType = HigherOrderApp | FirstOrderApp

type LCmdTop id = Located (CmdTop id)

data CmdTop id
  = CmdTop (LCmd id)
             (PostTc id TCRType)
             (PostTc id TCRType)
             (CmdSyntaxTable id)

type RecordBinds id = RecFields id (LExp id)

data MatchGroup id body
  = MG (Located [LMatch id body]) [PostTc id TCRType] (PostTc id TCRType) Origin

type LMatch id body = Located (Match id body)

data Match id body
  = Match (MatchFixity id) [LPat id] (Maybe (LType id)) (GRHSs id body)

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id)
                 Bool

data GRHSs id body
  = GRHSs [LGRHS id body] (Located (LocalBinds id))


type LGRHS id body = Located (GRHS id body)

data GRHS id body = GRHS [GuardLStmt id]
                         body

type LStmt id body = Located (StmtLR id id body)
type LStmtLR idL idR body = Located (StmtLR idL idR body)
type Stmt id body = StmtLR id id body
type CmdLStmt   id = LStmt id (LCmd  id)
type CmdStmt    id = Stmt  id (LCmd  id)
type ExpLStmt  id = LStmt id (LExp id)
type ExpStmt   id = Stmt  id (LExp id)
type GuardLStmt id = LStmt id (LExp id)
type GuardStmt  id = Stmt  id (LExp id)
type GhciLStmt  id = LStmt id (LExp id)
type GhciStmt   id = Stmt  id (LExp id)

data StmtLR idL idR body
  = LastStmt

          body
          Bool
          (SyntaxExp idR)

  | BindStmt (LPat idL)
             body
             (SyntaxExp idR)
             (SyntaxExp idR)

             (PostTc idR TCRType)

  | ApplicativeStmt
             [ ( SyntaxExp idR
               , ApplicativeArg idL idR) ]
             (Maybe (SyntaxExp idR))
             (PostTc idR TCRType)
  | BodyStmt body
             (SyntaxExp idR)
             (SyntaxExp idR)
             (PostTc idR TCRType)

  | LetStmt  (Located (LocalBindsLR idL idR))

  | ParStmt  [ParStmtBlock idL idR]
             (Exp idR)
             (SyntaxExp idR)
             (PostTc idR TCRType)

  | TransStmt TransForm [ExpLStmt idL] [(idR, idR)] (LExp idR) (Maybe (LExp idR))
              (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType) (Exp idR)

  | RecStmt [LStmtLR idL idR body] [idR] [idR] (SyntaxExp idR)
            (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
            [PostTcExp] [PostTcExp] (PostTc idR TCRType)

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock idL idR
  = ParStmtBlock
        [ExpLStmt idL]
        [idR]
        (SyntaxExp idR)

data ApplicativeArg idL idR
  = ApplicativeArgOne
      (LPat idL)
      (LExp idL)
  | ApplicativeArgMany
      [ExpLStmt idL]
      (Exp idL)
      (LPat idL)

data Splice id
   = TypedSplice
        id
        (LExp id)
   | UntypedSplice
        id
        (LExp id)
   | QuasiQuote
        id
        id
        SrcSpan
        FastString

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

data Bracket id = ExpBr (LExp id)
                  | PatBr (LPat id)
                  | DecBrL [LDecl id]
                  | DecBrG (Group id)
                  | TypBr (LType id)
                  | VarBr Bool id
                  | TExpBr (LExp id)

data ArithSeqInfo id
  = From            (LExp id)
  | FromThen        (LExp id)
                    (LExp id)
  | FromTo          (LExp id)
                    (LExp id)
  | FromThenTo      (LExp id)
                    (LExp id)
                    (LExp id)

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

type LPat id = Located (Pat id)

type ConPatDetails id = ConDetails (LPat id) (RecFields id (LPat id))

data RecFields id arg = RecFields [LRecField id arg] (Maybe Int)

type LRecField' id arg = Located (RecField' id arg)

type LRecField id arg = Located (RecField id arg)

type LRecUpdField id = Located (RecUpdField id)

type RecField id arg = RecField' (FieldOcc id) arg

type RecUpdField id = RecField' (AmbiguousFieldOcc id) (LExp id)

data RecField' id arg = RecField (Located id) arg Bool


--------------------------------------------------------------------------------
-- Doc

newtype DocString = DocString FastString

type LDocString = Located DocString

--------------------------------------------------------------------------------
-- ImpExp


type LImportDecl name = Located (ImportDecl name)

data ImportDecl name = ImportDecl (Maybe SourceText)
                                  (Located ModuleName)
                                  (Maybe StringLiteral) Bool
                                  Bool Bool Bool
                                  (Maybe ModuleName)
                                  (Maybe (Bool, Located [LIE name]))

type LIE name = Located (IE name)

data IE name = IEVar (Located name)
             | IEThingAbs (Located name)
             | IEThingAll (Located name)
             | IEThingWith (Located name) IEWildcard [Located name]
                           [Located (FieldLbl name)]
             | IEModuleContents (Located ModuleName)
             | IEGroup Int DocString
             | IEDoc DocString
             | IEDocNamed String

data IEWildcard = NoIEWildcard
                | IEWildcard Int

-------------------------------------------------------------------------------
-- Binds

type LocalBinds id = LocalBindsLR id id

data LocalBindsLR idL idR = ValBinds (ValBindsLR idL idR)
                            | IPBinds (IPBinds idR)
                            | EmptyLocalBinds

type ValBinds id = ValBindsLR id id

data ValBindsLR idL idR = ValBindsIn (LBindsLR idL idR)
                                       [LSig idR]
                          | ValBindsOut [(RecFlag, LBinds idL)] [LSig Name]

type LBind id = LBindLR id id

type LBinds id = LBindsLR id id

type Bind id = BindLR id id

type LBindsLR idL idR = Bag (LBindLR idL idR)

type LBindLR idL idR = Located (BindLR idL idR)

data BindLR idL idR = FunBind (Located idL)
                              (MatchGroup idR (LExp idR))
                              Wrapper (PostRn idL NameSet)
                              [Tickish Id]
                      | PatBind (LPat idL) (GRHSs idR (LExp idR))
                                (PostTc idR TCRType) (PostRn idL NameSet)
                                ([Tickish Id], [[Tickish Id]])
                      | VarBind idL (LExp idR)
                                Bool
                      | AbsBinds [TyVar] [EvVar]
                                 [ABExport idL] [TcEvBinds]
                                 (LBinds idL)
                      | AbsBindsSig [TyVar] [EvVar]
                                    idL TcSpecPrags
                                    TcEvBinds (LBind idL)
                      | PatSynBind (PatSynBind idL idR)

data ABExport id = ABE id id Wrapper TcSpecPrags

data PatSynBind idL idR = PSB (Located idL)
                              (PostRn idR NameSet)
                              (PatSynDetails (Located idR))
                              (LPat idR)
                              (PatSynDir idR)

-- IPBinds'
data IPBinds id = IPBinds' [LIPBind id] TcEvBinds

type LIPBind id = Located (IPBind id)

data IPBind id = IPBind (Either (Located IPName) id) (LExp id)

type LSig name = Located (Sig name)

data Sig name = TypeSig [Located name] (LSigWcType name)
              | PatSynSig (Located name) (LSigType name)
              | ClassOpSig Bool [Located name] (LSigType name)
              | IdSig Id
              | FixSig (FixitySig name)
              | InlineSig (Located name) InlinePragma
              | SpecSig (Located name) [LSigType name] InlinePragma
              | SpecInstSig SourceText (LSigType name)
              | MinimalSig SourceText (LBooleanFormula (Located name))

type LFixitySig name = Located (FixitySig name)

data FixitySig name = FixitySig [Located name] Fixity

data TcSpecPrags = IsDefaultMethod
                 | SpecPrags [LTcSpecPrag]

type LTcSpecPrag = Located TcSpecPrag

data TcSpecPrag = SpecPrag Id Wrapper InlinePragma

data PatSynDetails a = InfixPatSyn a a
                       | PrefixPatSyn [a]
                       | RecordPatSyn [RecordPatSynField a]

data RecordPatSynField a = RecordPatSynField a a

data PatSynDir id = Unidirectional
                    | ImplicitBidirectional
                    | ExplicitBidirectional (MatchGroup id (LExp id))

------------------------------------------------------------------------------
-- Decls

type LDecl id = Located (Decl id)

data Decl id = TyClD (TyClDecl id)
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

data Group id = Group (ValBinds id)
                      [LSpliceDecl id] [TyClGroup id]
                      [LDerivDecl id] [LFixitySig id]
                      [LDefaultDecl id] [LForeignDecl id]
                      [LWarnDecls id] [LAnnDecl id]
                      [LRuleDecls id] [LVectDecl id]
                      [LDocDecl]


data SpliceExplicitFlag = ExplicitSplice
                        | ImplicitSplice

type LSpliceDecl name = Located (SpliceDecl name)

data SpliceDecl id = SpliceDecl (Located (Splice id))
                                SpliceExplicitFlag

type LTyClDecl name = Located (TyClDecl name)

data TyClDecl name = FamDecl (FamilyDecl name)
                   | SynDecl (Located name) (LQTyVars name)
                             (LType name) (PostRn name NameSet)
                   | DataDecl (Located name) (LQTyVars name)
                              (DataDefn name) (PostRn name Bool)
                              (PostRn name NameSet)
                   | ClassDecl (LContext name) (Located name)
                               (LQTyVars name)
                               [Located (FunDep (Located name))]
                               [LSig name] (LBinds name)
                               [LFamilyDecl name] [LTyFamDefltEqn name]
                               [LDocDecl] (PostRn name NameSet)

data TyClGroup name
  = TyClGroup [LTyClDecl name] [LRoleAnnotDecl name] [LInstDecl name]

type LFamilyResultSig name
  = Located (FamilyResultSig name)

-- KingSig --> KindSig'
data FamilyResultSig name
  = NoSig
  | KindSig' (LKind name)
  | TyVarSig (LTyVarBndr name)

type LFamilyDecl name
  = Located (FamilyDecl name)

data FamilyDecl name = FamilyDecl (FamilyInfo name)
                                  (Located name) (LQTyVars name)
                                  (LFamilyResultSig name)
                                  (Maybe (LInjectivityAnn name))

type LInjectivityAnn name = Located (InjectivityAnn name)

data InjectivityAnn name = InjectivityAnn (Located name)
                                          [Located name]

data FamilyInfo name = DataFamily
                     | OpenTypeFamily
                     | ClosedTypeFamily (Maybe [LTyFamInstEqn name])

data DataDefn name = DataDefn NewOrData
                              (LContext name) (Maybe (Located CType))
                              (Maybe (LKind name)) [LConDecl name]
                              (Deriving name)

type Deriving name = Maybe (Located [LSigType name])

data NewOrData = NewType
               | DataType

type LConDecl name = Located (ConDecl name)

data ConDecl name = ConDeclGADT [Located name]
                                (LSigType name) (Maybe LDocString)
                  | ConDeclH98 (Located name)
                               (Maybe (LQTyVars name))
                               (Maybe (LContext name))
                               (ConDeclDetails name)
                               (Maybe LDocString)

type ConDeclDetails name =
     ConDetails (LBangType name) (Located [LConDeclField name])

type LTyFamInstEqn name = Located (TyFamInstEqn name)

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type TyPats name = ImplicitBndrs name [LType name]

type TyFamInstEqn name = TyFamEqn name (TyPats name)

type TyFamDefltEqn name = TyFamEqn name (LQTyVars name)

data TyFamEqn name pats = TyFamEqn (Located name)
                                   pats
                                   (LType name)

type LTyFamInstDecl name = Located (TyFamInstDecl name)

data TyFamInstDecl name
  = TyFamInstDecl (LTyFamInstEqn name) (PostRn name NameSet)

type LDataFamInstDecl name = Located (DataFamInstDecl name)

data DataFamInstDecl name = DataFamInstDecl (Located name)
                                            (TyPats name)
                                            (DataDefn name)
                                            (PostRn name NameSet)

type LClsInstDecl name = Located (ClsInstDecl name)

data ClsInstDecl name = ClsInstDecl (LSigType name)
                                    (LBinds name) [LSig name]
                                    [LTyFamInstDecl name]
                                    [LDataFamInstDecl name]
                                    (Maybe (Located OverlapMode))

type LInstDecl name = Located (InstDecl name)

data InstDecl name = ClsInstD (ClsInstDecl name)
                   | DataFamInstD (DataFamInstDecl name)
                   | TyFamInstD (TyFamInstDecl name)

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl (LSigType name)
                                (Maybe (Located OverlapMode))

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name = DefaultDecl [LType name]

type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name = ForeignImport (Located name)
                                      (LSigType name) (PostTc name Coercion)
                                      ForeignImport
                      | ForeignExport (Located name)
                                      (LSigType name) (PostTc name Coercion)
                                      ForeignExport

data ForeignImport = CImport (Located CCallConv) (Located Safety)
                             (Maybe Header) CImportSpec (Located SourceText)

data CImportSpec = CLabel CLabelString
                 | CFunction CCallTarget
                 | CWrapper

data ForeignExport = CExport (Located CExportSpec)
                             (Located SourceText)

type LRuleDecls name = Located (RuleDecls name)

data RuleDecls name = Rules SourceText [LRuleDecl name]

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name = Rule (Located (SourceText, RuleName))
                          Activation [LRuleBndr name] (Located (Exp name))
                          (PostRn name NameSet) (Located (Exp name))
                          (PostRn name NameSet)

type LRuleBndr name = Located (RuleBndr name)

data RuleBndr name = RuleBndr (Located name)
                   | RuleBndrSig (Located name) (LSigWcType name)

type LVectDecl name = Located (VectDecl name)

data VectDecl name = Vect SourceText (Located name)
                            (LExp name)
                   | NoVect SourceText (Located name)
                   | VectTypeIn SourceText Bool (Located name)
                                  (Maybe (Located name))
                   | VectTypeOut Bool TyCon (Maybe TyCon)
                   | VectClassIn SourceText (Located name)
                   | VectClassOut Class
                   | VectInstIn (LSigType name)
                   | VectInstOut ClsInst

type LDocDecl = Located (DocDecl)

data DocDecl = DocCommentNext DocString
             | DocCommentPrev DocString
             | DocCommentNamed String DocString
             | DocGroup Int DocString

type LWarnDecls name = Located (WarnDecls name)

data WarnDecls name = Warnings SourceText
                               [LWarnDecl name]

type LWarnDecl name = Located (WarnDecl name)

data WarnDecl name = Warning [Located name] WarningTxt

type LAnnDecl name = Located (AnnDecl name)

data AnnDecl name = Annotation SourceText (AnnProvenance name)
                                 (Located (Exp name))

data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance

type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

data RoleAnnotDecl name = RoleAnnotDecl (Located name)
                                        [Located (Maybe Role)]

-------------------------------------------------------------------------------
-- Types

type LBangType name = Located (BangType name)

type BangType name = Type name

type LContext name = Located (Context name)

type Context name = [LType name]

type LType name = Located (Type name)

type Kind name = Type name

type LKind name = Located (Kind name)

type LTyVarBndr name = Located (TyVarBndr name)

data LQTyVars name = QTvs (PostRn name [Name])
                          [LTyVarBndr name]
                          (PostRn name NameSet)

data ImplicitBndrs name thing = IB (PostRn name [Name]) thing

data WildCardBndrs name thing = WC (PostRn name [Name]) (Maybe SrcSpan) thing

type LSigType name = ImplicitBndrs name (LType name)

type LWcType name = WildCardBndrs name (LType name)

type LSigWcType name = ImplicitBndrs name (LWcType name)

newtype IPName = IPName FastString

data TyVarBndr name = UserTyVar (Located name)
                      | KindedTyVar (Located name) (LKind name)

data Type name = ForAllTy [LTyVarBndr name] (LType name)
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

data TyLit = NumTy SourceText Integer
             | StrTy SourceText FastString

newtype WildCardInfo name = AnonWildCard (PostRn name
                                              (Located Name))


type LAppType name = Located (AppType name)

data AppType name = AppInfix (Located name)
                    | AppPrefix (LType name)

data TupleSort = UnboxedTuple
                 | BoxedTuple
                 | ConstraintTuple
                 | BoxedOrConstraintTuple

type LConDeclField name = Located (ConDeclField name)

data ConDeclField name = ConDeclField [LFieldOcc name]
                                      (LBangType name)
                                      (Maybe LDocString)

data ConDetails arg rec = PrefixCon [arg]
                          | RecCon rec
                          | InfixCon arg arg

type LFieldOcc name = Located (FieldOcc name)

data FieldOcc name = FieldOcc (Located RdrName)
                              (PostRn name name)

data AmbiguousFieldOcc name = Unambiguous (Located RdrName)
                                          (PostRn name name)
                            | Ambiguous (Located RdrName) (PostTc name name)
