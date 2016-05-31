{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Syntax where

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
  = OverLit' {
        ol_val :: OverLitVal,
        ol_rebindable :: PostRn id Bool,
        ol_witness :: Exp id,
        ol_type :: PostTc id TCRType }

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString

type LExp id = Located (Exp id)

type PostTcExp  = Exp Id

type PostTcTable = [(Name, PostTcExp)]

data SyntaxExp id = SyntaxExp { syn_expr      :: Exp id
                                , syn_arg_wraps :: [Wrapper]
                                , syn_res_wrap  :: Wrapper }

type CmdSyntaxTable id = [(Name, Exp id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

data Exp id
  = Var     (Located id)

  | UnboundVar UnboundVar

  | RecFld (AmbiguousFieldOcc id)
  | OverLabel FastString
  | IPVar   IPName
  | OverLit (OverLit id)
  | Lit     Lit
  | Lam     (MatchGroup id (LExp id))

  | LamCase (MatchGroup id (LExp id))

  | App     (LExp id) (LExp id)
  | AppType (LExp id) (LWcType id)

  | AppTypeOut (LExp id) (LWcType Name)

  | OpApp       (LExp id)
                (LExp id)
                (PostRn id Fixity)
                (LExp id)

  | NegApp      (LExp id)
                (SyntaxExp id)

  | Par       (LExp id)
  | SectionL    (LExp id)
                (LExp id)
  | SectionR    (LExp id)
                (LExp id)

  | ExplicitTuple
        [LTupArg id]
        Boxity

  | Case      (LExp id)
                (MatchGroup id (LExp id))

  | If        (Maybe (SyntaxExp id))

                (LExp id)
                (LExp id)
                (LExp id)

  | MultiIf   (PostTc id TCRType) [LGRHS id (LExp id)]

  | Let       (Located (LocalBinds id))
                (LExp  id)

  | Do        (StmtContext Name)

                (Located [ExpLStmt id])
                (PostTc id TCRType)

  | ExplicitList
                (PostTc id TCRType)
                (Maybe (SyntaxExp id))
                [LExp id]

  | ExplicitPArr
                (PostTc id TCRType)
                [LExp id]

  | RecordCon
      { rcon_con_name :: Located id
      , rcon_con_like :: PostTc id ConLike
      , rcon_con_expr :: PostTcExp
      , rcon_flds     :: RecordBinds id }

  | RecordUpd
      { rupd_expr :: LExp id
      , rupd_flds :: [LRecUpdField id]
      , rupd_cons :: PostTc id [ConLike]

      , rupd_in_tys  :: PostTc id [TCRType]
      , rupd_out_tys :: PostTc id [TCRType]

      , rupd_wrap :: PostTc id Wrapper
      }

  | ExpWithTySig
                (LExp id)
                (LSigWcType id)
  | ExpWithTySigOut
                (LExp id)
                (LSigWcType Name)

  | ArithSeq
                PostTcExp
                (Maybe (SyntaxExp id))
                (ArithSeqInfo id)

  | PArrSeq
                PostTcExp
                (ArithSeqInfo id)

  | SCC       SourceText
                StringLiteral
                (LExp id)

  | CoreAnn   SourceText
                StringLiteral
                (LExp id)

  | Bracket    (Bracket id)

  | RnBracketOut
      (Bracket Name)
      [PendingRnSplice]
  | TcBracketOut
      (Bracket Name)
      [PendingTcSplice]

  | SpliceE  (Splice id)

  | Proc      (LPat id)
                (LCmdTop id)

  | Static    (LExp id)

  | ArrApp
        (LExp id)
        (LExp id)
        (PostTc id TCRType)
        ArrAppType
        Bool

  | ArrForm
        (LExp id)

        (Maybe Fixity)
        [LCmdTop id]

  | Tick
     (Tickish id)
     (LExp id)
  | BinTick
     Int
     Int
     (LExp id)

  | TickPragma
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))

     (LExp id)

  | EWildPat

  | EAsPat      (Located id)
                (LExp id)

  | EViewPat    (LExp id)
                (LExp id)

  | ELazyPat    (LExp id)

  |  Wrap     Wrapper
                (Exp id)
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
  = MG { mg_alts    :: Located [LMatch id body]
       , mg_arg_tys :: [PostTc id TCRType]
       , mg_res_ty  :: PostTc id TCRType
       , mg_origin  :: Origin }

type LMatch id body = Located (Match id body)

data Match id body
  = Match {
        m_fixity :: MatchFixity id,
        m_pats :: [LPat id],
        m_type :: (Maybe (LType id)),

        m_grhss :: (GRHSs id body)
  }

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id)
                 Bool

data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],
      grhssLocalBinds :: Located (LocalBinds id)
    }

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

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExpLStmt idL],

      trS_bndrs :: [(idR, idR)],
      trS_using :: LExp idR,
      trS_by :: Maybe (LExp idR),

      trS_ret :: SyntaxExp idR,
      trS_bind :: SyntaxExp idR,
      trS_bind_arg_ty :: PostTc idR TCRType,
      trS_fmap :: Exp idR

    }

  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

     , recS_later_ids :: [idR]

     , recS_rec_ids :: [idR]

     , recS_bind_fn :: SyntaxExp idR
     , recS_ret_fn  :: SyntaxExp idR
     , recS_mfix_fn :: SyntaxExp idR
     , recS_bind_ty :: PostTc idR TCRType

     , recS_later_rets :: [PostTcExp]
     , recS_rec_rets :: [PostTcExp]

      , recS_ret_ty :: PostTc idR TCRType

      }

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

data Pat id = WildPat (PostTc id TCRType)
            | VarPat (Located id)
            | LazyPat (LPat id)
            | AsPat (Located id) (LPat id)
            | ParPat (LPat id)
            | BangPat (LPat id)
            | ListPat [LPat id] (PostTc id TCRType)
                      (Maybe (PostTc id TCRType, SyntaxExp id))
            | TuplePat [LPat id] Boxity [PostTc id TCRType]
            | PArrPat [LPat id] (PostTc id TCRType)
            | ConPatIn (Located id) (ConPatDetails id)
            | ConPatOut{pat_con :: Located ConLike, pat_arg_tys :: [TCRType],
                        pat_tvs :: [TyVar], pat_dicts :: [EvVar], pat_binds :: TcEvBinds,
                        pat_args :: ConPatDetails id, pat_wrap :: Wrapper}
            | ViewPat (LExp id) (LPat id) (PostTc id TCRType)
            | SplicePat (Splice id)
            | LitPat Lit
            | NPat (Located (OverLit id)) (Maybe (SyntaxExp id))
                   (SyntaxExp id) (PostTc id TCRType)
            | NPlusKPat (Located id) (Located (OverLit id)) (OverLit id)
                        (SyntaxExp id) (SyntaxExp id) (PostTc id TCRType)
            | SigPatIn (LPat id) (LSigWcType id)
            | SigPatOut (LPat id) TCRType
            | CoPat Wrapper (Pat id) TCRType

type ConPatDetails id =
     ConDetails (LPat id) (RecFields id (LPat id))

data RecFields id arg = RecFields{rec_flds ::
                                      [LRecField id arg],
                                      rec_dotdot :: Maybe Int}

type LRecField' id arg = Located (RecField' id arg)

type LRecField id arg = Located (RecField id arg)

type LRecUpdField id = Located (RecUpdField id)

type RecField id arg = RecField' (FieldOcc id) arg

type RecUpdField id =
     RecField' (AmbiguousFieldOcc id) (LExp id)

data RecField' id arg = RecField{hsRecFieldLbl :: Located id,
                                     hsRecFieldArg :: arg, hsRecPun :: Bool}


--------------------------------------------------------------------------------
-- Doc

newtype DocString = DocString FastString

type LDocString = Located DocString

--------------------------------------------------------------------------------
-- ImpExp


type LImportDecl name = Located (ImportDecl name)

data ImportDecl name = ImportDecl{ideclSourceSrc ::
                                  Maybe SourceText,
                                  ideclName :: Located ModuleName,
                                  ideclPkgQual :: Maybe StringLiteral, ideclSource :: Bool,
                                  ideclSafe :: Bool, ideclQualified :: Bool, ideclImplicit :: Bool,
                                  ideclAs :: Maybe ModuleName,
                                        ideclHiding    :: Maybe (Bool, Located [LIE name])

    }


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

data BindLR idL idR = FunBind{fun_id :: Located idL,
                                fun_matches :: MatchGroup idR (LExp idR),
                                fun_co_fn :: Wrapper, bind_fvs :: PostRn idL NameSet,
                                fun_tick :: [Tickish Id]}
                      | PatBind{pat_lhs :: LPat idL, pat_rhs :: GRHSs idR (LExp idR),
                                pat_rhs_ty :: PostTc idR TCRType, bind_fvs :: PostRn idL NameSet,
                                pat_ticks :: ([Tickish Id], [[Tickish Id]])}
                      | VarBind{var_id :: idL, var_rhs :: LExp idR,
                                var_inline :: Bool}
                      | AbsBinds{abs_tvs :: [TyVar], abs_ev_vars :: [EvVar],
                                 abs_exports :: [ABExport idL], abs_ev_binds :: [TcEvBinds],
                                 abs_binds :: LBinds idL}
                      | AbsBindsSig{abs_tvs :: [TyVar], abs_ev_vars :: [EvVar],
                                    abs_sig_export :: idL, abs_sig_prags :: TcSpecPrags,
                                    abs_sig_ev_bind :: TcEvBinds, abs_sig_bind :: LBind idL}
                      | PatSynBind (PatSynBind idL idR)

data ABExport id = ABE{abe_poly :: id, abe_mono :: id,
                       abe_wrap :: Wrapper, abe_prags :: TcSpecPrags}

data PatSynBind idL idR = PSB{psb_id :: Located idL,
                              psb_fvs :: PostRn idR NameSet,
                              psb_args :: PatSynDetails (Located idR), psb_def :: LPat idR,
                              psb_dir :: PatSynDir idR}

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

data RecordPatSynField a = RecordPatSynField{recordPatSynSelectorId
                                             :: a,
                                             recordPatSynPatVar :: a}

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

data Group id = Group {hs_valds :: ValBinds id,
                       hs_splcds :: [LSpliceDecl id], hs_tyclds :: [TyClGroup id],
                       hs_derivds :: [LDerivDecl id], hs_fixds :: [LFixitySig id],
                       hs_defds :: [LDefaultDecl id], hs_fords :: [LForeignDecl id],
                       hs_warnds :: [LWarnDecls id], hs_annds :: [LAnnDecl id],
                       hs_ruleds :: [LRuleDecls id], hs_vects :: [LVectDecl id],
                       hs_docs :: [LDocDecl]}

data SpliceExplicitFlag = ExplicitSplice
                        | ImplicitSplice

type LSpliceDecl name = Located (SpliceDecl name)

data SpliceDecl id = SpliceDecl (Located (Splice id))
                                SpliceExplicitFlag

type LTyClDecl name = Located (TyClDecl name)

data TyClDecl name = FamDecl{tcdFam :: FamilyDecl name}
                   | SynDecl{tcdLName :: Located name, tcdTyVars :: LQTyVars name,
                             tcdRhs :: LType name, tcdFVs :: PostRn name NameSet}
                   | DataDecl{tcdLName :: Located name, tcdTyVars :: LQTyVars name,
                              tcdDataDefn :: DataDefn name, tcdDataCusk :: PostRn name Bool,
                              tcdFVs :: PostRn name NameSet}
                   | ClassDecl{tcdCtxt :: LContext name, tcdLName :: Located name,
                               tcdTyVars :: LQTyVars name,
                               tcdFDs :: [Located (FunDep (Located name))],
                               tcdSigs :: [LSig name], tcdMeths :: LBinds name,
                               tcdATs :: [LFamilyDecl name], tcdATDefs :: [LTyFamDefltEqn name],
                               tcdDocs :: [LDocDecl], tcdFVs :: PostRn name NameSet}

data TyClGroup name = TyClGroup{group_tyclds :: [LTyClDecl name],
                                group_roles :: [LRoleAnnotDecl name],
                                group_instds :: [LInstDecl name]}

type LFamilyResultSig name = Located (FamilyResultSig name)

-- KingSig --> KindSig'
data FamilyResultSig name = NoSig
                          | KindSig' (LKind name)
                          | TyVarSig (LTyVarBndr name)

type LFamilyDecl name = Located (FamilyDecl name)

data FamilyDecl name = FamilyDecl{fdInfo :: FamilyInfo name,
                                  fdLName :: Located name, fdTyVars :: LQTyVars name,
                                  fdResultSig :: LFamilyResultSig name,
                                  fdInjectivityAnn :: Maybe (LInjectivityAnn name)}

type LInjectivityAnn name = Located (InjectivityAnn name)

data InjectivityAnn name = InjectivityAnn (Located name)
                                          [Located name]

data FamilyInfo name = DataFamily
                     | OpenTypeFamily
                     | ClosedTypeFamily (Maybe [LTyFamInstEqn name])

data DataDefn name = DataDefn{dd_ND :: NewOrData,
                                  dd_ctxt :: LContext name, dd_cType :: Maybe (Located CType),
                                  dd_kindSig :: Maybe (LKind name), dd_cons :: [LConDecl name],
                                  dd_derivs :: Deriving name}

type Deriving name = Maybe (Located [LSigType name])

data NewOrData = NewType
               | DataType

type LConDecl name = Located (ConDecl name)

data ConDecl name = ConDeclGADT{con_names :: [Located name],
                                con_type :: LSigType name, con_doc :: Maybe LDocString}
                  | ConDeclH98{con_name :: Located name,
                               con_qvars :: Maybe (LQTyVars name),
                               con_cxt :: Maybe (LContext name),
                               con_details :: ConDeclDetails name,
                               con_doc :: Maybe LDocString}

type ConDeclDetails name =
     ConDetails (LBangType name) (Located [LConDeclField name])

type LTyFamInstEqn name = Located (TyFamInstEqn name)

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type TyPats name = ImplicitBndrs name [LType name]

type TyFamInstEqn name = TyFamEqn name (TyPats name)

type TyFamDefltEqn name = TyFamEqn name (LQTyVars name)

data TyFamEqn name pats = TyFamEqn{tfe_tycon :: Located name,
                                   tfe_pats :: pats, tfe_rhs :: LType name}

type LTyFamInstDecl name = Located (TyFamInstDecl name)

data TyFamInstDecl name = TyFamInstDecl{tfid_eqn ::
                                        LTyFamInstEqn name,
                                        tfid_fvs :: PostRn name NameSet}

type LDataFamInstDecl name = Located (DataFamInstDecl name)

data DataFamInstDecl name = DataFamInstDecl{dfid_tycon ::
                                            Located name,
                                            dfid_pats :: TyPats name,
                                            dfid_defn :: DataDefn name,
                                            dfid_fvs :: PostRn name NameSet}

type LClsInstDecl name = Located (ClsInstDecl name)

data ClsInstDecl name = ClsInstDecl{cid_poly_ty :: LSigType name,
                                    cid_binds :: LBinds name, cid_sigs :: [LSig name],
                                    cid_tyfam_insts :: [LTyFamInstDecl name],
                                    cid_datafam_insts :: [LDataFamInstDecl name],
                                    cid_overlap_mode :: Maybe (Located OverlapMode)}

type LInstDecl name = Located (InstDecl name)

data InstDecl name = ClsInstD{cid_inst :: ClsInstDecl name}
                   | DataFamInstD{dfid_inst :: DataFamInstDecl name}
                   | TyFamInstD{tfid_inst :: TyFamInstDecl name}

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl{deriv_type :: LSigType name,
                                deriv_overlap_mode :: Maybe (Located OverlapMode)}

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name = DefaultDecl [LType name]

type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name = ForeignImport{fd_name :: Located name,
                                      fd_sig_ty :: LSigType name, fd_co :: PostTc name Coercion,
                                      fd_fi :: ForeignImport}
                      | ForeignExport{fd_name :: Located name,
                                      fd_sig_ty :: LSigType name, fd_co :: PostTc name Coercion,
                                      fd_fe :: ForeignExport}

data ForeignImport = CImport (Located CCallConv) (Located Safety)
                             (Maybe Header) CImportSpec (Located SourceText)

data CImportSpec = CLabel CLabelString
                 | CFunction CCallTarget
                 | CWrapper

data ForeignExport = CExport (Located CExportSpec)
                             (Located SourceText)

type LRuleDecls name = Located (RuleDecls name)

data RuleDecls name = Rules{rds_src :: SourceText,
                              rds_rules :: [LRuleDecl name]}

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name = Rule (Located (SourceText, RuleName))
                            Activation [LRuleBndr name] (Located (Exp name))
                            (PostRn name NameSet) (Located (Exp name)) (PostRn name NameSet)

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

data WarnDecls name = Warnings{wd_src :: SourceText,
                               wd_warnings :: [LWarnDecl name]}

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

data LQTyVars name = QTvs{hsq_implicit :: PostRn name [Name],
                              hsq_explicit :: [LTyVarBndr name],
                              hsq_dependent :: PostRn name NameSet}

data ImplicitBndrs name thing = IB{hsib_vars ::
                                       PostRn name [Name],
                                       hsib_body :: thing}

data WildCardBndrs name thing = WC{hswc_wcs ::
                                       PostRn name [Name],
                                       hswc_ctx :: Maybe SrcSpan, hswc_body :: thing}

type LSigType name = ImplicitBndrs name (LType name)

type LWcType name = WildCardBndrs name (LType name)

type LSigWcType name = ImplicitBndrs name (LWcType name)

newtype IPName = IPName FastString

data TyVarBndr name = UserTyVar (Located name)
                      | KindedTyVar (Located name) (LKind name)

data Type name = ForAllTy{hst_bndrs :: [LTyVarBndr name],
                              hst_body :: LType name}
                 | QualTy{hst_ctxt :: LContext name, hst_body :: LType name}
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

data ConDeclField name = ConDeclField{cd_fld_names ::
                                      [LFieldOcc name],
                                      cd_fld_type :: LBangType name,
                                      cd_fld_doc :: Maybe LDocString}

data ConDetails arg rec = PrefixCon [arg]
                          | RecCon rec
                          | InfixCon arg arg

type LFieldOcc name = Located (FieldOcc name)

data FieldOcc name = FieldOcc{rdrNameFieldOcc :: Located RdrName,
                              selectorFieldOcc :: PostRn name name}

data AmbiguousFieldOcc name = Unambiguous (Located RdrName)
                                          (PostRn name name)
                            | Ambiguous (Located RdrName) (PostTc name name)
