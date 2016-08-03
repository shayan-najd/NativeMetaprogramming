{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds,TemplateHaskell #-}
module SyntaxExtensibleAutoSplitted where
--       (Decl(..),ImportDecl(..),Module(..)) where
import Extensible
import Data.ByteString

desugarExtensible "Exp" [d|
 {-# Ann type Exp Extensible #-}
 data Exp id
   = Var      id

 --  HSE.Con (missing?)

   | Lit      Lit

   | App      (Exp id) (Exp id)

   | SectionL (Exp id) (Exp id)

   | SectionR (Exp id) (Exp id)

   | NegApp   (Exp id)

   | OpApp    (Exp id) (Exp id) (Exp id)

   | AppType  (Exp id)

   | Par      (Exp id)

   | If       (Exp id) (Exp id) (Exp id)

   | MultiIf  [GRHS id (Exp id)]

   | Case     (Exp id) (MatchGroup id (Exp id))

   | Lam      (MatchGroup id (Exp id))

   | LamCase  (MatchGroup id (Exp id))

   | Let      (LocalBindsLR id id) (Exp  id)

   | IPVar    IPName

   | RecFld   (AmbiguousFieldOcc id)

   | OverLabel FastString

   | RecordCon id (RecordBinds id)

   | RecordUpd (Exp id) [RecUpdField id]

   | ExplicitTuple [TupArg id] Boxity

 --  HSE.TupleSection (part of above?)

   | ExplicitList [Exp id]

   | ExplicitPArr [Exp id]

   | ArithSeq (ArithSeqInfo id)
 --  ArithSeqInfo.From
 --  ArithSeqInfo.FromTo
 --  ArithSeqInfo.FromThen
 --  ArithSeqInfo.FromThenTo

   | PArrSeq  (ArithSeqInfo id) -- expanding ArithSeqInfo below
 --  ArithSeqInfo.From
 --  ArithSeqInfo.FromTo
 --  ArithSeqInfo.FromThen
 --  ArithSeqInfo.FromThenTo

   | Do            [Stmt id (Exp id)]
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

 --  HSE.QuasiQuote (missing?)

 --  HSE.VarQuote (missing?)

 --  HSE.TypQuote (missing?)

   | SpliceE  (Splice id)

   | ExpWithTySig (Exp id) (SigWcType id) -- <-- SigWcType?!

   | CoreAnn   SourceText StringLiteral (Exp id)

   | SCC       SourceText StringLiteral (Exp id)

   | TickPragma SourceText (StringLiteral,(Int,Int),(Int,Int))
               ((SourceText,SourceText),(SourceText,SourceText))
               (Exp id) -- (is it GenPragma?)

   | Proc      (Pat id) (CmdTop id)

   | ArrApp
 --  HSE.LeftArrHighApp (don't know how they compare)
   | ArrForm   (Exp id) (Maybe Fixity) [CmdTop id]
 --  HSE.RightArrHighApp (don't know how they compare)

   | EWildPat -- (right comparison?)

   | Static    (Exp id)

   | BinTick   Int Int (Exp id)

   | EAsPat    id (Exp id)

   | EViewPat  (Exp id) (Exp id)

   | ELazyPat  (Exp id)

 {-# Ann type Pat Extensible #-}
 data Pat id
   = WildPat

   | VarPat    id

   | LitPat    Lit

   | NPat      (OverLit id)

   | NPlusKPat id (OverLit id) (OverLit id)

   | TuplePat  [Pat id] Boxity

   | ListPat   [Pat id]

   | ParPat    (Pat id)

   | AsPat     id (Pat id)

   | ViewPat   (Exp id) (Pat id)

   | BangPat   (Pat id)

   | LazyPat   (Pat id)

   | SigPatIn  (Pat id) (SigWcType id)

   | PArrPat   [Pat id]

   | ConPatIn  id (ConPatDetails id)
 --  ConDetails.PrefixCon
 --  ConDetails.RecCon
 --  ConDetails.InfixCon

   | SplicePat (Splice id)

   | CoPat     (Pat id)

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

--  HSE.Frac       Rational String

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
 --  ForImp
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

   | ValD (BindLR id id)
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

   | SpliceTy (Splice id)

   | OpTy (Type id) id (Type id) -- match?

   | TyLit TyLit
 --  TyLit.Num
 --  TyLit.String
   | AppsTy [AppType id] -- assuming it is promoted constructors
   | ExplicitListTy [Type id]
   | ExplicitTupleTy [Type id]
 --  HSE.Promoted.PromotedUnit (ExplicitTupleTy [] []?)

 --  HSE.TyQuasiQuote (missing?)

   | QualTy (Context id) (Type id)

   | IParamTy IPName (Type id)

   | DocTy (Type id) DocString

   | RecTy [ConDeclField id]

 data Boxity
   = Boxed
   | Unboxed

 data ModuleName
   = ModuleName FastString

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

 data TupArg id
   = Present (Exp id)
   | Missing

 data ArithSeqInfo id
   = From            (Exp id)
   | FromThen        (Exp id) (Exp id)
   | FromTo          (Exp id) (Exp id)
   | FromThenTo      (Exp id) (Exp id) (Exp id)

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

 data RoleAnnotDecl id
   = RoleAnnotDecl id [Maybe Role]

 data Role
   = Nominal
   | Representational
   | Phantom
   -- HSE.RoleWildcard is `Nothing` of `Maybe Role`

 data AnnDecl id
   = Annotation SourceText (AnnProvenance id) (Exp id)
 data AnnProvenance id
   = ValueAnnProvenance id
   | TypeAnnProvenance id
   | ModuleAnnProvenance

 data DerivDecl id
   = DerivDecl (SigType id) (Maybe OverlapMode)

 data OverlapMode
   = NoOverlap SourceText
   | Overlappable SourceText
   | Overlapping SourceText
   | Overlaps SourceText
   | Incoherent SourceText

 data GRHS id body
   = GRHS [Stmt id (Exp id)] body

 data MatchGroup id body
   = MG [Match id body] Origin
 data Origin
   = FromSource
   | Generated

 data Match id body
   = Match (MatchFixity id) [Pat id] (Maybe (Type id)) (GRHSs id body)
 data MatchFixity id
   = NonFunBindMatch
   | FunBindMatch id Bool
 data GRHSs id body
   = GRHSs [GRHS id body] (LocalBindsLR id id)

-- HSE.Promoted is expanded as constructors
--   TyLit.Num
--   TyLit.String
--   AppsTy
--   ExplicitListTy
--   ExplicitTupleTy
--    part of above

------------ the rest is not compared -------------

 data FastString
 data Bag a

 data FixityDirection
   = InfixL
   | InfixR
   | InfixN

 data Fixity
   = Fixity SourceText Int FixityDirection

 type SourceText
   = String

 type FieldLabelString
   = FastString

 data FieldLbl a
   = FieldLabel FieldLabelString Bool a

 -- Extensible
 data BooleanFormula a
   = Var' a  -- Var --> Var'
   | And [BooleanFormula a]
   | Or [BooleanFormula a]
   | Parens (BooleanFormula a)

 type RuleName
   = FastString

 data Header
   = Header SourceText FastString

 type FunDep a
   = ([a],[a])

 data Activation
   = NeverActive
   | AlwaysActive
   | ActiveBefore SourceText PhaseNum
   | ActiveAfter SourceText PhaseNum

 type Arity
   = Int

 type PhaseNum
   = Int

 data RuleMatchInfo
   = ConLike
   | FunLike

 data InlinePragma
   = InlinePragma SourceText InlineSpec (Maybe Arity) Activation RuleMatchInfo

 data InlineSpec
   = Inline
   | Inlinable
   | NoInline
   | EmptyInlineSpec

 data CType
   = CType SourceText (Maybe Header) (SourceText,FastString)

 data CExportSpec
   = CExportStatic SourceText CLabelString CCallConv

 data CCallConv
   = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv

 data CCallTarget
   = StaticTarget SourceText CLabelString (Maybe UnitId) Bool
   | DynamicTarget

 type CLabelString
   = FastString

 -- extensible
 data WarningTxt
   = WarningTxt SourceText [StringLiteral]
   | DeprecatedTxt SourceText [StringLiteral]

 data Safety
   = PlaySafe
   | PlayInterruptible
   | PlayRisky

 data SrcStrictness
   = SrcLazy
   | SrcStrict
   | NoSrcStrict

 data SrcUnpackedness
   = SrcUnpack
   | SrcNoUnpack
   | NoSrcUnpack

 data SrcBang
   = SrcBang (Maybe SourceText) SrcUnpackedness SrcStrictness

 data Module
   = Module !UnitId !ModuleName

 data UnitId
   = PId FastString

 data FractionalLit
   = FL String Rational

 data StringLiteral =
    StringLiteral SourceText FastString

 data RealSrcSpan
   = RealSrcSpan' !FastString !Int !Int !Int !Int

 data SrcSpan =
    RealSrcSpan !RealSrcSpan
   | UnhelpfulSpan !FastString

 data OverLit id
   = OverLit' OverLitVal (Exp id)

 data OverLitVal
   = Integral   !SourceText !Integer
   | Fractional !FractionalLit
   | IsString   !SourceText !FastString

 data Cmd id
   = CmdArrApp  (Exp id) (Exp id) ArrAppType Bool
   | CmdArrForm (Exp id) (Maybe Fixity) [CmdTop id]
   | CmdApp     (Cmd id)  (Exp id)
   | CmdLam     (MatchGroup id (Cmd id))
   | CmdPar     (Cmd id)
   | CmdCase    (Exp id) (MatchGroup id (Cmd id))
   | CmdIf      (Exp id) (Cmd id) (Cmd id)
   | CmdLet     (LocalBindsLR id id) (Cmd  id)
   | CmdDo      [CmdStmt id]

 data ArrAppType
   = HigherOrderApp
   | FirstOrderApp

 data CmdTop id
   = CmdTop (Cmd id)

 type Stmt id body  = StmtLR id id body
 type CmdStmt    id = Stmt  id (Cmd  id)
 type ExpStmt    id = Stmt  id (Exp id)

 data StmtLR idL idR body
   = LastStmt body Bool
   | BindStmt (Pat idL) body
   | ApplicativeStmt [( ApplicativeArg idL idR)]
   | BodyStmt body
   | LetStmt  (LocalBindsLR idL idR)
   | ParStmt  [ParStmtBlock idL idR] (Exp idR)
   | TransStmt TransForm [ExpStmt idL] [(idR, idR)] (Exp idR)
              (Maybe (Exp idR))
              (Exp idR)
   | RecStmt [StmtLR idL idR body] [idR] [idR]

 data TransForm
   = ThenForm
   | GroupForm

 data ParStmtBlock idL idR
   = ParStmtBlock [ExpStmt idL] [idR]

 data ApplicativeArg idL idR
   = ApplicativeArgOne (Pat idL) (Exp idL)
   | ApplicativeArgMany [ExpStmt idL] (Exp idL) (Pat idL)

 data Splice id
   = TypedSplice id (Exp id)
   | UntypedSplice id (Exp id)
   | QuasiQuote id id SrcSpan FastString

 data Bracket id
   = ExpBr (Exp id)
   | PatBr (Pat id)
   | DecBrL [Decl id]
   | DecBrG (Group id)
   | TypBr (Type id)
   | VarBr Bool id
   | TExpBr (Exp id)

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

 ----------------------------------------------------------------------------
 -- Pat

 type RecordBinds id
   = RecFields id (Exp id)

 type ConPatDetails id
   = ConDetails (Pat id) (RecFields id (Pat id))

 data RecFields id arg
   = RecFields [RecField (FieldOcc id) arg] (Maybe Int)

 type RecUpdField id
   = RecField (AmbiguousFieldOcc id) (Exp id)

 data RecField id arg
   = RecField id arg Bool

 data ConDetails arg rec
   = PrefixCon [arg]
   | RecCon rec
   | InfixCon arg arg


 ----------------------------------------------------------------------------
 -- Doc

 data DocString = DocString FastString

 ----------------------------------------------------------------------------
 -- Binds
 data LocalBindsLR idL idR
   = ValBinds (ValBindsLR idL idR)
   | IPBinds (IPBinds idR)
   | EmptyLocalBinds

--  type ValBinds id = ValBindsLR id id

 data ValBindsLR idL idR
   = ValBindsIn (Bag (BindLR idL idR)) [Sig idR]

 data BindLR idL idR
   = FunBind (idL) (MatchGroup idR (Exp idR))
   | PatBind (Pat idL) (GRHSs idR (Exp idR))
   | VarBind idL (Exp idR) Bool
   | AbsBinds [ABExport idL] (Bag (BindLR idL idL))
   | AbsBindsSig idL (BindLR idL idL)
   | PatSynBind (PatSynBind idL idR)

 data ABExport id
   = ABE id id

 data PatSynBind idL idR
   = PSB idL (PatSynDetails idR) (Pat idR) (PatSynDir idR)

 -- IPBinds'
 data IPBinds id
   = IPBinds' [IPBind id]

 data IPBind id
   = IPBind (Either IPName id) (Exp id)

 data Sig id
   = TypeSig [id] (SigWcType id)
   | PatSynSig id (SigType id)
   | ClassOpSig Bool [id] (SigType id)
   | FixSig (FixitySig id)
   | InlineSig id InlinePragma
   | SpecSig id [SigType id] InlinePragma
   | SpecInstSig SourceText (SigType id)
   | MinimalSig SourceText (BooleanFormula id)

 data FixitySig id
   = FixitySig [id] Fixity

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

 ----------------------------------------------------------------------------
 -- Decls

 data Group id
   = Group (ValBindsLR id id) [SpliceDecl id] [TyClGroup id] [DerivDecl id]
          [FixitySig id] [DefaultDecl id] [ForeignDecl id] [WarnDecls id]
          [AnnDecl id] [RuleDecls id] [VectDecl id] [DocDecl]

 data SpliceExplicitFlag
   = ExplicitSplice
   | ImplicitSplice

 data SpliceDecl id
   = SpliceDecl (Splice id) SpliceExplicitFlag

 data TyClDecl id
   = FamDecl (FamilyDecl id)
   | SynDecl id (QTyVars id) (Type id)
   | DataDecl id (QTyVars id) (DataDefn id)
   | ClassDecl (Context id) id (QTyVars id)
              [FunDep id] [Sig id] (Bag (BindLR id id))
              [FamilyDecl id] [TyFamDefltEqn id] [DocDecl]


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
   = TyFamInstDecl (TyFamInstEqn id)

 data DataFamInstDecl id
   = DataFamInstDecl id (TyPats id) (DataDefn id)


 data ClsInstDecl id
   = ClsInstDecl (SigType id) (Bag (BindLR id id)) [Sig id]
                [TyFamInstDecl id]
                [DataFamInstDecl id] (Maybe OverlapMode)

 data InstDecl id
   = ClsInstD (ClsInstDecl id)
   | DataFamInstD (DataFamInstDecl id)
   | TyFamInstD (TyFamInstDecl id)


 data DefaultDecl id
   = DefaultDecl [Type id]

 data ForeignDecl id
   = ForeignImport id (SigType id)
                  ForeignImport
   | ForeignExport id (SigType id)
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
         (Exp id)

 data RuleBndr id
   = RuleBndr id
   | RuleBndrSig id (SigWcType id)

 data VectDecl id
   = Vect SourceText id (Exp id)
   | NoVect SourceText id
   | VectTypeIn SourceText Bool id (Maybe id)
   | VectClassIn SourceText id
   | VectInstIn (SigType id)

 data DocDecl
   = DocCommentNext DocString
   | DocCommentPrev DocString
   | DocCommentNamed String DocString
   | DocGroup Int DocString

 data WarnDecls id
   = Warnings SourceText [WarnDecl id]

 data WarnDecl id
   = Warning [id] WarningTxt

 -----------------------------------------------------------------------------
 -- Types

 type BangType id = Type id

 type Context id = [Type id]

 type Kind id = Type id

 data QTyVars id
   = QTvs [TyVarBndr id]

 data ImplicitBndrs id thing
   = IB thing

 data WildCardBndrs id thing
   = WC (Maybe SrcSpan) thing

 type SigType id = ImplicitBndrs id (Type id)

 type WcType id = WildCardBndrs id (Type id)

 type SigWcType id = ImplicitBndrs id (WcType id)

 data IPName = IPName FastString

 data TyVarBndr id
   = UserTyVar id
   | KindedTyVar id (Kind id)

 data TyLit
   = NumTy SourceText Integer
   | StrTy SourceText FastString

 data WildCardInfo id
   = AnonWildCard

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

 data RdrName

 data FieldOcc id
   = FieldOcc RdrName

 data AmbiguousFieldOcc id
   = Unambiguous RdrName
   | Ambiguous RdrName

 |]

{-

data HsExp id
   = UnboundVar UnboundVar
   | OverLit  (OverLit id)
   | NegApp (SyntaxExp id)
   | OpApp  (PostRn id Fixity)
   | AppType (WcType id)
   | If      (Maybe (SyntaxExp id))
   | MultiIf (PostTc id TCRType)
   | RecordCon (PostTc id ConLike) PostTcExp
   | RecordUpd (PostTc id [ConLike]) (PostTc id [TCRType])
              (PostTc id [TCRType]) (PostTc id Wrapper)
   | ExplicitList (PostTc id TCRType) (Maybe (SyntaxExp id))
   | ExplicitPArr (PostTc id TCRType)
   | ArithSeq  PostTcExp (Maybe (SyntaxExp id))
   | PArrSeq   PostTcExp
   | Do        (StmtContext Name)  (PostTc id TCRType)
   | ExpWithTySig (SigWcType id)
   | ArrApp    (PostTc id TCRType)
---

   | ExpWithTySigOut (Exp id) (SigWcType Name)
   | RnBracketOut (Bracket Name) [PendingRnSplice]
   | TcBracketOut (Bracket Name) [PendingTcSplice]
   | AppTypeOut (Exp id) (WcType Name)
   | Wrap      Wrapper (Exp id)
   | Tick      (Tickish id) (Exp id)



data HsPat id
   = WildPat (PostTc id TCRType)
   | NPat      (Maybe (SyntaxExp id))
              (SyntaxExp id) (PostTc id TCRType)
   | NPlusKPat (SyntaxExp id)
              (SyntaxExp id) (PostTc id TCRType)
   | TuplePat  [PostTc id TCRType]
   | ListPat   (PostTc id TCRType)
              (Maybe (PostTc id TCRType, SyntaxExp id))
   | ViewPat   (PostTc id TCRType)

   | SigPatIn  (SigWcType id)

   | PArrPat   (PostTc id TCRType)

   | CoPat     TCRType  Wrapper

   | SigPatOut (Pat id) TCRType

   | ConPatOut ConLike [TCRType] [TyVar] [EvVar] TcEvBinds
              (ConPatDetails id)
              Wrapper


data Lit
   = Integer       SourceText Integer TCRType
   | Rat           FractionalLit TCRType


data Type id
   =  SpliceTy (PostTc id TCRKind)
   | ExplicitListTy (PostTc id TCRKind)
   | ExplicitTupleTy [PostTc id TCRKind]
   | CoreTy TCRType

 data StmtLR idL idR body
   = LastStmt (SyntaxExp idR)
   | BindStmt (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
   | ApplicativeStmt [(SyntaxExp idR, ApplicativeArg idL idR)]
                    (Maybe (SyntaxExp idR)) (PostTc idR TCRType)
   | BodyStmt (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
   | LetStmt
   | ParStmt  (SyntaxExp idR) (PostTc idR TCRType)
   | TransStmt (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
   | RecStmt (SyntaxExp idR) (SyntaxExp idR) (SyntaxExp idR) (PostTc idR TCRType)
            [PostTcExp] [PostTcExp] (PostTc idR TCRType)


 data MatchGroup id body
   = MG [PostTc id TCRType] (PostTc id TCRType)

 data CmdTop id
   = CmdTop (PostTc id TCRType) (PostTc id TCRType)


 data Cmd id
   = CmdArrApp  (PostTc id TCRType)
   | CmdIf      (Maybe (SyntaxExp id))
   | CmdDo      (PostTc id TCRType)
   | CmdWrap    Wrapper (Cmd id)

 data TupArg id
   = Present
   | Missing (PostTc id TCRType)


 data OverLit id
   = OverLit' (PostRn id Bool) (PostTc id TCRType)

 type PostTcExp = Exp Id

 data SyntaxExp id
   = SyntaxExp (Exp id) [Wrapper] Wrapper

 data ParStmtBlock idL idR
   = ParStmtBlock (SyntaxExp idR)


 data BindLR idL idR

 data BindLR idL idR
   = FunBind (PostRn idL NameSet) Wrapper [Tickish Id]
   | PatBind (PostTc idR TCRType) (PostRn idL NameSet)
            ([Tickish Id], [[Tickish Id]])
   | AbsBinds [TyVar] [EvVar] [TcEvBinds]
   | AbsBindsSig [TyVar] [EvVar] TcSpecPrags TcEvBinds
   | PatSynBind



 data ABExport id
   = ABE TcSpecPrags Wrapper


 data PendingTcSplice
   = PendingTcSplice SplicePointName (Exp Id)

 data TcEvBinds

 data IPBinds id
   = IPBinds' TcEvBinds

 data TcSpecPrags
   = IsDefaultMethod
   | SpecPrags [TcSpecPrag]

 data TcSpecPrag
   = SpecPrag Id Wrapper InlinePragma

 data PostRn a b
 data PostTc a b

 data FieldOcc id
   = FieldOcc RdrName (PostRn id id)

 data AmbiguousFieldOcc id
   = Unambiguous RdrName (PostRn id id)
   | Ambiguous RdrName (PostTc id id)

 data QTyVars id
   = QTvs (PostRn id [Name]) (PostRn id NameSet)

 data ImplicitBndrs id thing
   = IB (PostRn id [Name]) thing

 data WildCardBndrs id thing
   = WC (PostRn id [Name])

 data WildCardInfo id
   = AnonWildCard (PostRn id Name)


 data PatSynBind idL idR
   = PSB (PostRn idR NameSet)


 data TyClDecl id
   = SynDecl (PostRn id NameSet)
   | DataDecl(PostRn id Bool) (PostRn id NameSet)
   | ClassDecl (PostRn id NameSet)

 data TyFamInstDecl id
   = TyFamInstDecl (PostRn id NameSet)

 data DataFamInstDecl id
   = DataFamInstDecl (PostRn id NameSet)

 data ForeignDecl id
   = ForeignImport (PostTc id Coercion)
   | ForeignExport (PostTc id Coercion)

 data RuleDecl id
   = Rule (PostRn id NameSet) (PostRn id NameSet)

 type TCRKind = TCRType

 data TyVar
 data EvVar
 data TCRType
 data Wrapper
 data Tickish a
 data Id

 data Sig id
   = IdSig Id

 data PendingRnSplice
   = PendingRnSplice UntypedSpliceFlavour SplicePointName (Exp Name)

 data ValBindsLR idL idR
   =  ValBindsOut [(RecFlag, Binds idL)] [Sig Name]

 data Coercion


 data VectDecl id
   = VectTypeOut Bool TyCon (Maybe TyCon)
   | VectClassOut Class
   | VectInstOut ClsInst

 data Class
 data ClsInst
 data TyCon

 type NameSet
   = UniqSet Name

 data ConLike
   = RealDataCon DataCon
   | PatSynCon   PatSyn

 data UniqSet a

 data RecFlag
   = Recursive
   | NonRecursive

 type SplicePointName = Name

 data UntypedSpliceFlavour
   = UntypedExpSplice
   | UntypedPatSplice
   | UntypedTypeSplice
   | UntypedDeclSplice

 data CmdTop id
   = CmdTop (CmdSyntaxTable id)

 type CmdSyntaxTable id
   = [(Name, Exp id)]

 data UnboundVar
   = OutOfScope OccName GlobalRdrEnv
   | TrueExpHole OccName

 data GlobalRdrEnv
 data OccName
-}
